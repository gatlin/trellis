{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Foreign.C.Types (CInt (..))

foreign export javascript "hs_add sync" hsAdd :: CInt -> CInt -> CInt
hsAdd :: CInt -> CInt -> CInt
hsAdd a b = a + b

main :: IO ()
main = return ()
