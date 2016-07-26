-- TabCode - A parser for the Tabcode lute tablature language
--
-- Copyright (C) 2015, 2016 Richard Lewis, Goldsmiths' College
-- Author: Richard Lewis <richard.lewis@gold.ac.uk>

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

module Main where

import Data.ByteString.Char8 (putStrLn)
import Options.Applicative
import Prelude hiding (putStrLn)
import TabCode.MEI.Serialiser (meiDoc)
import TabCode.Options
import TabCode.Parser (parseTabcodeStdIn)
import Text.XML.Generator

opts :: ParserInfo TCOptions
opts = info ( helper <*> config )
       ( fullDesc
         <> progDesc "TabCode MEI XML converter"
         <> header "tc2mei" )

main :: IO ()
main = execParser opts >>= runSerialiser
  where
    runSerialiser o = do
      tc <- parseTabcodeStdIn o
      putStrLn $ xrender $ doc defaultDocInfo $ meiDoc tc
