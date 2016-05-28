{-# LANGUAGE CPP #-}
#ifdef TEST_SUITE
{-# OPTIONS_GHC -F -pgmF htfpp #-}
#endif
{-# LANGUAGE OverloadedStrings #-}
module Data.Text.Parser where

#ifdef TEST_SUITE
import Test.Framework
import Data.Char (isSpace)
#endif

import Data.Char (isDigit)
import Data.Maybe
import Data.Monoid
import Control.Applicative
import Control.Monad
import qualified Data.Text as T

type ParseError = T.Text
newtype Parser a
    = Parser { runParser :: T.Text -> (T.Text, Either ParseError a) }

instance Functor Parser where
    fmap f (Parser parse) =
        Parser $ \txt ->
        let (rest, result) = parse txt
        in (rest, fmap f result)

instance Applicative Parser where
    pure val = Parser $ \txt -> (txt, Right val)
    (Parser funParser) <*> continue =
        Parser $ \txt ->
        let (rest, result) = funParser txt
        in case result of
            Left err -> (rest, Left err)
            Right f -> runParser (fmap f continue) rest

instance Alternative Parser where
    empty = Parser $ \txt -> (txt, Left "Parsing failed!")
    (Parser pa) <|> otherParser =
        Parser $ \txt ->
        case pa txt of
          full@(_, Right _) -> full
          _ -> runParser otherParser txt

instance Monad Parser where
    return = pure
    fail errMsg = Parser $ \txt -> (txt, Left $ T.pack errMsg)
    (Parser parse) >>= next =
        Parser $ \txt ->
        let (leftOver, res) = parse txt
        in case res of
             Left errMsg -> (leftOver, Left errMsg)
             Right val -> runParser (next val) leftOver

satisfy :: (Char -> Bool) -> Parser T.Text
satisfy f =
    Parser $ \txt ->
    let (matches, rest) = T.span f txt
    in (rest, Right matches)

satisfy1 :: (Char -> Bool) -> Parser T.Text
satisfy1 f =
    satisfy f >>= \res ->
    do when (T.null res) $ fail "skipWhile1 didn't ready anything!"
       pure res

skipWhile :: (Char -> Bool) -> Parser ()
skipWhile = void . satisfy

skipWhile1 :: (Char -> Bool) -> Parser ()
skipWhile1 = void . satisfy1

char :: Char -> Parser Char
char c =
    Parser $ \txt ->
    case T.uncons txt of
      Just (firstC, rest) | firstC == c -> (rest, Right c)
      _ -> (txt, Left $ T.pack $ "Expected a " ++ show c)

string :: T.Text -> Parser T.Text
string t =
    Parser $ \txt ->
    let tlen = T.length t
    in if T.take tlen txt == t
       then (T.drop tlen txt, Right t)
       else (txt, Left $ T.pack $ "Expected " ++ show t)

numStarter :: Parser T.Text
numStarter =
    do optNeg <- optional (char '-')
       rest <- satisfy1 isDigit
       pure $ maybe rest (`T.cons` rest) optNeg

int :: Parser Int
int = fmap (read . T.unpack) numStarter

double :: Parser Double
double =
    do firstPart <- numStarter
       secondPart <-
           optional $
           do ch <- char '.'
              rest <- satisfy1 isDigit
              pure (ch `T.cons` rest)
       pure $ (read . T.unpack) (firstPart <> fromMaybe "" secondPart)

sepBy :: Parser val -> Parser sep -> Parser [val]
sepBy valP sepP =
    do listHead <- optional valP
       case listHead of
         Nothing -> pure []
         Just x ->
             do rest <- many (sepP *> valP)
                pure (x : rest)

endOfInput :: Parser ()
endOfInput =
    Parser $ \txt ->
    if T.null txt
    then (txt, Right ())
    else (txt, Left "Expecting endOfInput")

#ifdef TEST_SUITE

test_char :: IO ()
test_char =
    do assertEqual ("Fooo", Right 'c') (runParser (char 'c') "cFooo")
       assertEqual ("Fooo", Left "Expected a 'c'") (runParser (char 'c') "Fooo")
       assertEqual ("", Left "Expected a 'c'") (runParser (char 'c') "")

test_string :: IO ()
test_string =
    do assertEqual ("Fooo", Right "cc") (runParser (string "cc") "ccFooo")
       assertEqual ("Fooo", Left "Expected \"cc\"") (runParser (string "cc") "Fooo")
       assertEqual ("", Left "Expected \"cc\"") (runParser (string "cc") "")

test_int :: IO ()
test_int =
    do assertEqual ("bar", Right 23) (runParser int "23bar")
       assertEqual ("bar", Right (-23)) (runParser int "-23bar")
       assertEqual (".bar", Right 23) (runParser int "23.bar")
       assertEqual ("a23.bar", Left "skipWhile1 didn't ready anything!") (runParser int "a23.bar")
       assertEqual ("", Left "skipWhile1 didn't ready anything!") (runParser int "")

test_double :: IO ()
test_double =
    do assertEqual ("bar", Right 23) (runParser double "23bar")
       assertEqual ("bar", Right (-23)) (runParser double "-23bar")
       assertEqual ("bar", Right 23.2) (runParser double "23.2bar")
       assertEqual (".bar", Right 23) (runParser double "23.bar")
       assertEqual ("a23.bar", Left "skipWhile1 didn't ready anything!") (runParser double "a23.bar")
       assertEqual ("", Left "skipWhile1 didn't ready anything!") (runParser double "")


data LanguageType
   = LanguageTypeFunctional
   | LanguageTypeOOP
   deriving (Show, Eq)

data Language
   = Language
   { l_name :: T.Text
   , l_type :: LanguageType
   } deriving (Show, Eq)

type LangList = [Language]

langType :: Parser LanguageType
langType =
    LanguageTypeFunctional <$ string "functional"
    <|> LanguageTypeOOP <$ string "oop"

langName :: Parser T.Text
langName = satisfy1 (\c -> not (isSpace c) && c /= ';')

test_langType :: IO ()
test_langType =
    do assertEqual ("", Right LanguageTypeFunctional) (runParser langType "functional")
       assertEqual ("", Right LanguageTypeOOP) (runParser langType "oop")
       assertEqual ("foobar", Left "Expected \"oop\"") (runParser langType "foobar")

test_langName :: IO ()
test_langName =
    do assertEqual ("", Right "haskell") (runParser langName "haskell")
       assertEqual ("", Right "java") (runParser langName "java")
       assertEqual (" bar baz", Right "java") (runParser langName "java bar baz")

skipVertSpace :: Parser ()
skipVertSpace = skipWhile (\c -> c == ' ' || c == '\t')

lang :: Parser Language
lang =
    do void $ string "language:" *> skipVertSpace
       name <- langName
       skipVertSpace
       void $ char ';'
       skipVertSpace
       void $ string "type:" *> skipVertSpace
       ty <- langType
       skipVertSpace
       void $ char ';'
       skipVertSpace
       pure (Language name ty)

test_lang :: IO ()
test_lang =
    do assertEqual ("", Right $ Language "haskell" LanguageTypeFunctional)
           (runParser lang "language: haskell; type: functional;")
       assertEqual ("", Right $ Language "java" LanguageTypeOOP)
           (runParser lang "language:java; type:oop; ")
       assertEqual ("language1:!java; type:oop; ", Left "Expected \"language:\"")
           (runParser lang "language1:!java; type:oop; ")

langFile :: Parser LangList
langFile =
    (lang `sepBy` char '\n') <* skipWhile isSpace <* endOfInput

test_langFile :: IO ()
test_langFile =
    assertEqual ("", Right langList) (runParser langFile sampleFile)
    where
      langList =
          [ Language "haskell" LanguageTypeFunctional
          , Language "purescript" LanguageTypeFunctional
          , Language "java" LanguageTypeOOP
          ]
      sampleFile =
          T.unlines
          [ "language: haskell; type: functional;"
          , "language: purescript; type: functional;"
          , "language: java; type: oop;"
          ]
#endif
