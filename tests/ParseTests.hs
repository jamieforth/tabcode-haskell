-- TabCode - A parser for the Tabcode lute tablature language
--
-- Copyright (C) 2016 Richard Lewis, Goldsmiths' College
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

module ParseTests where

import Distribution.TestSuite

import TabCode
import TabCode.Parser (parseTabcode)

tryParseWord :: String -> TabWord -> Result
tryParseWord tc tw =
  case parseTabcode tc of
    Right (TabCode rls wrds) -> check wrds tw
    Left err                 -> Fail $ show err
  where
    check [] twExp = Error $ "Could not parse " ++ tc ++ " as " ++ (show twExp)
    check (twAct:_) twExp | twExp == twAct = Pass
                          | otherwise      = Fail $ "For \"" ++ tc ++ "\", expected " ++ (show twExp) ++ "; got " ++ (show twAct)

mkParseTest :: String -> TabWord -> TestInstance
mkParseTest tc tw = TestInstance {
    run = return $ Finished $ tryParseWord tc tw
  , name = show tw
  , tags = []
  , options = []
  , setOption = \_ _ -> Right $ mkParseTest tc tw
  }
