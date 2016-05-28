{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Data.Text.Parser

main :: IO ()
main = htfMain htf_importedTests
