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
import           Data.Text (pack)
import           TabCode
import           TabCode.MEI
import           TabCode.MEI.Serialiser
import           TabCode.Options        (TCOptions(..), ParseMode(..), Structure(..))
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
    (parseTabcode (TCOptions { parseMode = Strict, structure = BarLines }) tcStrIn)
    (xmlParse' meiStrIn meiStrIn)

  where
    equal (Right tc) (Right xml) | tcXML == xml = Pass
                                 | otherwise    = Fail $ "Expected " ++ (show $ document xml)
                                                         ++ "; got: " ++ (show $ document tcXML)
      where
        tcXML      = tcMEI . tcMEIStr $ tc
        tcMEIStr t = xrender $ doc defaultDocInfo $ meiDoc $ meiXml t
        meiXml t   = case mei BarLines testDoc ("input: " ++ tcStrIn) t of
                       Right m  -> m
                       Left err -> XMLComment $ pack $ "Could not generate MEI tree for " ++ tcStrIn ++ ": " ++ (show err)
        tcMEI xml  = case xmlParse' ("input: " ++ tcStrIn) $ C.unpack xml of
                       Right m  -> m
                       Left err -> xmlParse "fail" $ "<fail>" ++ (show err) ++ "</fail>"
        testDoc st staves = MEI noMEIAttrs staves

    equal (Left e) _ = Fail $ "Invalid tabcode: " ++ tcStrIn ++ "; " ++ (show e)
    equal _ (Left e) = Fail $ "Un-parsable serialisation for " ++ tcStrIn ++ "; " ++ (show e)

asStaff :: String -> String -> String
asStaff def s = "<?xml version='1.0' encoding='UTF-8' standalone='yes' ?><mei xmlns='http://www.music-encoding.org/ns/mei'><staff n='1' def='" ++ def ++ "'><layer n='1'>" ++ s ++ "</layer></staff></mei>"

meterSigns :: [Test]
meterSigns =
  [ Test $ mkMEITest
    "M(O.)\n" $ asStaff "#staff-1" "<staffDef xml:id='staff-1' prolatio='3' tempus='3'><mensur sign='O' dot='true'/></staffDef>"
  , Test $ mkMEITest
    "M(O)\n" $ asStaff "#staff-1" "<staffDef xml:id='staff-1' prolatio='3' tempus='2'><mensur sign='O' dot='false'/></staffDef>"
  , Test $ mkMEITest
    "M(C.)\n" $ asStaff "#staff-1" "<staffDef xml:id='staff-1' prolatio='2' tempus='3'><mensur sign='C' dot='true'/></staffDef>"
  , Test $ mkMEITest
    "M(C)\n" $ asStaff "#staff-1" "<staffDef xml:id='staff-1' prolatio='2' tempus='2'><mensur sign='C' dot='false'/></staffDef>"
  , Test $ mkMEITest
    "M(O/.)\n" $ asStaff "#staff-1" "<staffDef xml:id='staff-1' prolatio='3' tempus='3' mensur.slash='1'><mensur sign='O' dot='true' slash='1'/></staffDef>"
  , Test $ mkMEITest
    "M(O/)\n" $ asStaff "#staff-1" "<staffDef xml:id='staff-1' prolatio='3' tempus='2' mensur.slash='1'><mensur sign='O' dot='false' slash='1'/></staffDef>"
  , Test $ mkMEITest
    "M(C/.)\n" $ asStaff "#staff-1" "<staffDef xml:id='staff-1' prolatio='2' tempus='3' mensur.slash='1'><mensur sign='C' dot='true' slash='1'/></staffDef>"
  , Test $ mkMEITest
    "M(C/)\n" $ asStaff "#staff-1" "<staffDef xml:id='staff-1' prolatio='2' tempus='2' mensur.slash='1'><mensur sign='C' dot='false' slash='1'/></staffDef>"
  , Test $ mkMEITest
    "M(D.)\n" $ asStaff "#staff-1" "<staffDef xml:id='staff-1' prolatio='2' tempus='3' mensur.slash='1'><mensur sign='C' dot='true' slash='1'/></staffDef>"
  , Test $ mkMEITest
    "M(D)\n" $ asStaff "#staff-1" "<staffDef xml:id='staff-1' prolatio='2' tempus='2' mensur.slash='1'><mensur sign='C' dot='false' slash='1'/></staffDef>"
  , Test $ mkMEITest
    "M(3:4)\n" $ asStaff "#staff-1" "<staffDef xml:id='staff-1' num.default='3' numbase.default='4'><meterSig count='3' unit='4'/></staffDef>"
  , Test $ mkMEITest
    "M(3;4)\n" $ asStaff "#staff-1" "<staffDef xml:id='staff-1' num.default='3' numbase.default='4'><meterSig count='3' unit='4'/></staffDef>"
  , Test $ mkMEITest
    "M(3)\n" $ asStaff "#staff-1" "<staffDef xml:id='staff-1' tempus='3'/>"
  ]

rests :: [Test]
rests =
  [ Test $ mkMEITest
    "F\n" $ asStaff "#staff-0" "<fermata xml:id='r1'/>"
  , Test $ mkMEITest
    "B\n" $ asStaff "#staff-0" "<rest xml:id='r1' dur='breve'><rhythmGlyph xml:id='rg1' symbol='B'/></rest>"
  , Test $ mkMEITest
    "W\n" $ asStaff "#staff-0" "<rest xml:id='r1' dur='1'><rhythmGlyph xml:id='rg1' symbol='W'/></rest>"
  , Test $ mkMEITest
    "W.\n" $ asStaff "#staff-0" "<rest xml:id='r1' dur='1' dots='1'><rhythmGlyph xml:id='rg1' symbol='W.' dots='1'/></rest>"
  , Test $ mkMEITest
    "H\n" $ asStaff "#staff-0" "<rest xml:id='r1' dur='2'><rhythmGlyph xml:id='rg1' symbol='H'/></rest>"
  , Test $ mkMEITest
    "H.\n" $ asStaff "#staff-0" "<rest xml:id='r1' dur='2' dots='1'><rhythmGlyph xml:id='rg1' symbol='H.' dots='1'/></rest>"
  ]

chords :: [Test]
chords =
  [ Test $ mkMEITest
    "c1\n" $ asStaff "#staff-0" "<chord xml:id='c1'><note xml:id='n1' tab.course='1' tab.fret='2'/></chord>"
  , Test $ mkMEITest
    "c1a2\n" $ asStaff "#staff-0" "<chord xml:id='c1'><note xml:id='n1' tab.course='1' tab.fret='2'/><note xml:id='n2' tab.course='2' tab.fret='0'/></chord>"
  , Test $ mkMEITest
    "Qc1\n" $ asStaff "#staff-0" "<chord xml:id='c1' dur='4'><rhythmGlyph xml:id='rg1' symbol='Q'/><note xml:id='n1' tab.course='1' tab.fret='2'/></chord>"
  , Test $ mkMEITest
    "Q.c1\n" $ asStaff "#staff-0" "<chord xml:id='c1' dur='4' dots='1'><rhythmGlyph xml:id='rg1' symbol='Q.' dots='1'/><note xml:id='n1' tab.course='1' tab.fret='2'/></chord>"
  , Test $ mkMEITest
    "E3c1\nc1\nc1\n" $ asStaff "#staff-0" "<tuplet num='3' numbase='2'><chord xml:id='c1' dur='8'><rhythmGlyph xml:id='rg1' symbol='E3'/><note xml:id='n1' tab.course='1' tab.fret='2'/></chord><chord xml:id='c2' dur='8'><note xml:id='n2' tab.course='1' tab.fret='2'/></chord><chord xml:id='c3' dur='8'><note xml:id='n3' tab.course='1' tab.fret='2'/></chord></tuplet>"
  ]

phrases :: [Test]
phrases =
  [ Test $ mkMEITest
    "Q.c1\nEc1\n" $ asStaff "#staff-0" "<chord xml:id='c1' dur='4' dots='1'><rhythmGlyph xml:id='rg1' symbol='Q.' dots='1'/><note xml:id='n1' tab.course='1' tab.fret='2'/></chord><chord xml:id='c2' dur='8'><rhythmGlyph xml:id='rg2' symbol='E'/><note xml:id='n2' tab.course='1' tab.fret='2'/></chord>"
  , Test $ mkMEITest
    "Ec1\nc1\n" $ asStaff "#staff-0" "<chord xml:id='c1' dur='8'><rhythmGlyph xml:id='rg1' symbol='E'/><note xml:id='n1' tab.course='1' tab.fret='2'/></chord><chord xml:id='c2' dur='8'><note xml:id='n2' tab.course='1' tab.fret='2'/></chord>"
  , Test $ mkMEITest
    "E.c1\nc1\n" $ asStaff "#staff-0" "<chord xml:id='c1' dur='8' dots='1'><rhythmGlyph xml:id='rg1' symbol='E.' dots='1'/><note xml:id='n1' tab.course='1' tab.fret='2'/></chord><chord xml:id='c2' dur='8' dots='1'><note xml:id='n2' tab.course='1' tab.fret='2'/></chord>"
  ]

tests :: IO [Test]
tests = return $ meterSigns
  ++ rests
  ++ chords
  ++ phrases
