{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Asterius.Aeson
import Asterius.ByteString
import Asterius.Text
import Asterius.Types
import PCalc (p)

foreign import javascript "prompt($2)" input :: JSString -> JSString -> IO JSString
foreign import javascript "confirm($1)" quest :: JSString -> IO JSVal

main :: IO ()
main = repl where
    repl :: IO ()
    repl = do
        jsstr <- input (toJSString "Enter m:") (toJSString "Enter m")
        jsbool <- quest (toJSString ("p(m) = " ++ show (p (read (fromJSString jsstr))) ++ "; Compute more?"))
        case jsonFromJSVal jsbool of
            Left str -> error str
            Right b -> if b then repl else return ()
