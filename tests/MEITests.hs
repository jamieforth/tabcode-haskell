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

module MEITests where

import Distribution.TestSuite

import qualified Data.ByteString.Char8 as C
import           TabCode
import           TabCode.MEISerialiser
import           TabCode.Options (TCOptions(..), ParseMode(..))
import           TabCode.Parser (parseTabcode)
import           Text.XML.Generator
import           Text.XML.HaXml.Parse (xmlParse', xmlParse)
import           Text.XML.HaXml.Pretty (document)

mkMEITest :: String -> String -> TestInstance
mkMEITest tc xml = TestInstance {
    run = return $ Finished $ tryMEISerialise tc xml
  , name = "MEI " ++ tc
  , tags = []
  , options = []
  , setOption = \_ _ -> Right $ mkMEITest tc xml
  }

tryMEISerialise :: String -> String -> Result
tryMEISerialise tcStrIn meiStrIn =
  equal
    (parseTabcode (TCOptions { parseMode = Strict }) tcStrIn)
    (xmlParse' meiStrIn meiStrIn)

  where
    equal (Right tc) (Right xml) | tcXML == xml = Pass
                                 | otherwise    = Fail $ "Expected " ++ (show $ document xml)
                                                         ++ "; got: " ++ (show $ document tcXML)
      where
        tcXML      = tcMEI . tcMEIStr $ tc
        tcMEIStr t = xrender $ doc defaultDocInfo $ staff t
        tcMEI xml  = case xmlParse' ("input: " ++ tcStrIn) $ C.unpack xml of
                       Right mei -> mei
                       Left _    -> xmlParse "fail" "<empty/>"

    equal (Left e) _ = Fail $ "Invalid tabcode: " ++ tcStrIn ++ "; " ++ (show e)
    equal _ (Left e) = Fail $ "Un-parsable serialisation for " ++ tcStrIn ++ "; " ++ (show e)

asStaff :: String -> String
asStaff s = "<?xml version='1.0' encoding='UTF-8' standalone='yes' ?><staff xmlns='http://www.music-encoding.org/ns/mei'><layer>" ++ s ++ "</layer></staff>"

tests :: IO [Test]
tests = return $ []
