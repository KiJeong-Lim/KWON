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
        js_m_rep <- input (toJSString "Enter m:") (toJSString "Enter m")
        js_ComputeMore <- quest (toJSString ("p(m) = " ++ showsp (read (fromJSString js_m_rep)) "; Compute more?"))
        case jsonFromJSVal js_ComputeMore of
            Left str -> error str
            Right b -> if b then repl else return ()
