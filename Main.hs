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
        js_m_rep <- input (toJSString "Enter m (>= 2.0)") (toJSString "Enter m (>= 2.0)")
        js_compute_more <- quest (toJSString ("p(m) = " ++ showsp (read (fromJSString js_m_rep)) "; Compute more?"))
        case jsonFromJSVal js_ComputeMore of
            Left errmsg1 -> error errmsg1
            Right compute_more -> if compute_more then repl else return ()
