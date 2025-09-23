{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified MyLib (someFunc)

import Control.Monad (foldM)
import Data.List.NonEmpty
import Data.Text     (Text)
import Text.Printf   (printf)

import qualified BlackHoleProvider
import           SubFSM         ( )

data BlackHoleState
    = Collapse
    |

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
