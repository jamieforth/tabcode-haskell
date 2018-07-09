-- TabCode - A parser for the Tabcode lute tablature language
--
-- Copyright (C) 2015-2017 Richard Lewis
-- Author: Richard Lewis <richard@rjlewis.me.uk>

-- This file is part of TabCode

-- TabCode is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- TabCode is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with TabCode.  If not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, CPP, ScopedTypeVariables #-}

module Main where

import Data.ByteString.Char8
import TabCode.Serialiser.MEIXML.Converter
import TabCode.Serialiser.MEIXML.Serialiser (meiDoc)
import TabCode.Options
import TabCode.Parser (parseTabcode)
import Text.XML.Generator

import GHCJS.Marshal(fromJSVal)
import GHCJS.Foreign.Callback (Callback, syncCallback1')
import Data.JSString (JSString, pack, unpack)
import GHCJS.Types (JSRef, JSVal, jsval)
import JavaScript.Object (Object, create, setProp)
import Data.Maybe (fromJust)

-- blog: http://weblog.luite.com/wordpress/?p=14
-- atom: http://edsko.net/2015/02/14/atom-haskell/
-- https://www.reddit.com/r/haskell/comments/59jy6h/how_to_use_haskell_as_a_library_from_js_via_ghcjs/
-- parser tutorial: https://www.futurelearn.com/courses/functional-programming-haskell/0/steps/27222

#ifdef __GHCJS__
foreign import javascript unsafe
  "console.log($1);"
  consoleLog :: JSString -> IO ()

foreign import javascript unsafe
  "tc2mei = $1"
  set_callback :: Callback (JSVal -> IO JSVal) -> IO ()
#else
consoleLog = error "consoleLog: only available from JavaScript"
set_callback = error "set_callback: only available from JavaScript"
#endif

main :: IO ()
main = do
    callback <- syncCallback1' $ \jv -> do
      (str :: String) <- Data.JSString.unpack . fromJust <$> fromJSVal jv
      let mei = tabcodeToMeiXml str
      (o :: Object) <- create
      setProp (Data.JSString.pack "mei" :: JSString) (jsval . Data.JSString.pack $ mei) o
      return $ jsval o
    set_callback callback

tabcodeToMeiXml :: String -> String
tabcodeToMeiXml tabcodeStr =
  Data.ByteString.Char8.unpack $ xrender $ doc defaultDocInfo $ meiDoc meiXml (xmlIds options)
  where
    options = TCOptions
      { parseMode = Permissive
      , structure = BarLines
      , xmlIds = WithXmlIds
      }
    Right tabcode = parseTabcode options tabcodeStr
    Right meiXml = mei (structure options) defaultDoc "" tabcode
