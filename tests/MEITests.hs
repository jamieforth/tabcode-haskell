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

import qualified Data.ByteString.Char8  as C
import           TabCode
import           TabCode.MEI
import           TabCode.MEI.Serialiser
import           TabCode.Options        (TCOptions(..), ParseMode(..))
import           TabCode.Parser         (parseTabcode)
import           Text.XML.Generator
import           Text.XML.HaXml.Parse   (xmlParse', xmlParse)
import           Text.XML.HaXml.Pretty  (document)

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
        tcMEIStr t = xrender $ doc defaultDocInfo $ meiDoc $ meiXml t
        meiXml t   = case mei defaultDoc ("input: " ++ tcStrIn) t of
                       Right m  -> m
                       Left err -> error $ "Could not generate MEI tree for " ++ tcStrIn
        tcMEI xml  = case xmlParse' ("input: " ++ tcStrIn) $ C.unpack xml of
                       Right m -> m
                       Left _  -> xmlParse "fail" "<empty/>"

    equal (Left e) _ = Fail $ "Invalid tabcode: " ++ tcStrIn ++ "; " ++ (show e)
    equal _ (Left e) = Fail $ "Un-parsable serialisation for " ++ tcStrIn ++ "; " ++ (show e)

asStaff :: String -> String
asStaff s = "<?xml version='1.0' encoding='UTF-8' standalone='yes' ?><staff xmlns='http://www.music-encoding.org/ns/mei'><layer>" ++ s ++ "</layer></staff>"

asMEIDoc :: String -> String
asMEIDoc s = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><music xmlns=\"http://www.music-encoding.org/ns/mei\"><body><mdiv><parts><part><section><staff><layer>" ++ s ++ "</layer></staff></section></part></parts></mdiv></body></music>"

meterSigns :: [Test]
meterSigns =
  [ Test $ mkMEITest
    "M(O.)\n" $ asMEIDoc "<staffDef prolatio='3' tempus='3'><mensur sign='O' dot='true'/></staffDef>"
  , Test $ mkMEITest
    "M(O)\n" $ asMEIDoc "<staffDef prolatio='3' tempus='2'><mensur sign='O' dot='false'/></staffDef>"
  , Test $ mkMEITest
    "M(C.)\n" $ asMEIDoc "<staffDef prolatio='2' tempus='3'><mensur sign='C' dot='true'/></staffDef>"
  , Test $ mkMEITest
    "M(C)\n" $ asMEIDoc "<staffDef prolatio='2' tempus='2'><mensur sign='C' dot='false'/></staffDef>"
  , Test $ mkMEITest
    "M(O/.)\n" $ asMEIDoc "<staffDef prolatio='3' tempus='3' mensur.slash='1'><mensur sign='O' dot='true' slash='1'/></staffDef>"
  , Test $ mkMEITest
    "M(O/)\n" $ asMEIDoc "<staffDef prolatio='3' tempus='2' mensur.slash='1'><mensur sign='O' dot='false' slash='1'/></staffDef>"
  , Test $ mkMEITest
    "M(C/.)\n" $ asMEIDoc "<staffDef prolatio='2' tempus='3' mensur.slash='1'><mensur sign='C' dot='true' slash='1'/></staffDef>"
  , Test $ mkMEITest
    "M(C/)\n" $ asMEIDoc "<staffDef prolatio='2' tempus='2' mensur.slash='1'><mensur sign='C' dot='false' slash='1'/></staffDef>"
  , Test $ mkMEITest
    "M(D.)\n" $ asMEIDoc "<staffDef prolatio='2' tempus='3' mensur.slash='1'><mensur sign='C' dot='true' slash='1'/></staffDef>"
  , Test $ mkMEITest
    "M(D)\n" $ asMEIDoc "<staffDef prolatio='2' tempus='2' mensur.slash='1'><mensur sign='C' dot='false' slash='1'/></staffDef>"
  , Test $ mkMEITest
    "M(3:4)\n" $ asMEIDoc "<staffDef num.default='3' numbase.default='4'><meterSig count='3' unit='4'/></staffDef>"
  , Test $ mkMEITest
    "M(3;4)\n" $ asMEIDoc "<staffDef num.default='3' numbase.default='4'><meterSig count='3' unit='4'/></staffDef>"
  , Test $ mkMEITest
    "M(3)\n" $ asMEIDoc "<staffDef tempus='3'/>"
  ]

rests :: [Test]
rests =
  [ Test $ mkMEITest
    "F\n" $ asMEIDoc "<fermata/>"
  , Test $ mkMEITest
    "B\n" $ asMEIDoc "<rest dur='breve'><rhythmGlyph symbol='B'/></rest>"
  , Test $ mkMEITest
    "W\n" $ asMEIDoc "<rest dur='1'><rhythmGlyph symbol='W'/></rest>"
  , Test $ mkMEITest
    "W.\n" $ asMEIDoc "<rest dur='1' dots='1'><rhythmGlyph symbol='W.' dots='1'/></rest>"
  , Test $ mkMEITest
    "H\n" $ asMEIDoc "<rest dur='2'><rhythmGlyph symbol='H'/></rest>"
  , Test $ mkMEITest
    "H.\n" $ asMEIDoc "<rest dur='2' dots='1'><rhythmGlyph symbol='H.' dots='1'/></rest>"
  ]

chords :: [Test]
chords =
  [ Test $ mkMEITest
    "c1\n" $ asMEIDoc "<chord><note tab.course='1' tab.fret='2'/></chord>"
  , Test $ mkMEITest
    "c1a2\n" $ asMEIDoc "<chord><note tab.course='1' tab.fret='2'/><note tab.course='2' tab.fret='o'/></chord>"
  , Test $ mkMEITest
    "Qc1\n" $ asMEIDoc "<chord dur='4'><rhythmGlyph symbol='Q'/><note tab.course='1' tab.fret='2'/></chord>"
  , Test $ mkMEITest
    "Q.c1\n" $ asMEIDoc "<chord dur='4' dots='1'><rhythmGlyph symbol='Q.' dots='1'/><note tab.course='1' tab.fret='2'/></chord>"
  , Test $ mkMEITest
    "E3c1\nc1\nc1\n" $ asMEIDoc "<tuplet num='3' numbase='2'><chord dur='8'><rhythmGlyph symbol='E3'/><note tab.course='1' tab.fret='2'/></chord><chord dur='8'><note tab.course='1' tab.fret='2'/></chord><chord dur='8'><note tab.course='1' tab.fret='2'/></chord></tuplet>"
  ]

tests :: IO [Test]
tests = return $ meterSigns
  ++ rests
  ++ chords
