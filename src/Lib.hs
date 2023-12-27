module Lib
    ( someFunc
    ) where

import Prelude (putStrLn)
import RIO

someFunc :: IO ()
someFunc = putStrLn "someFunc"
