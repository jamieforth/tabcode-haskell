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

module ParseTests where

import Distribution.TestSuite

import qualified Data.Vector as V
import TabCode.Types
import TabCode.Options (TCOptions(..), ParseMode(..), Structure(..), XmlIds(..))
import TabCode.Parser (parseTabcode)

tryParsePhrase :: String -> [TabWord] -> Result
tryParsePhrase tc phrase =
  case (parseTabcode parsingOptions tc) of
    Right (TabCode _ wrds) -> check wrds phrase
    Left err -> Fail $ show err
  where
    parsingOptions = TCOptions
      { parseMode = Strict
      , structure = BarLines
      , xmlIds = WithXmlIds }
    check ws expected
      | V.length ws == 0 =
          Error $ "Could not parse " ++ tc ++ " as " ++ (show expected)
      | otherwise =
          if (V.toList ws) == expected
          then Pass
          else Fail $ "For \"" ++ tc ++ "\", expected " ++ (show expected) ++ "; got " ++ (show $ V.toList ws)

tryParseWord :: String -> TabWord -> Result
tryParseWord tc tw = tryParsePhrase tc [tw]

mkParseTest :: String -> TabWord -> TestInstance
mkParseTest tc tw = TestInstance
  { run = return $ Finished $ tryParseWord tc tw
  , name = show tc
  , tags = []
  , options = []
  , setOption = \_ _ -> Right $ mkParseTest tc tw
  }

mkParsePhraseTest :: String -> [TabWord] -> TestInstance
mkParsePhraseTest tc phrase = TestInstance
  { run = return $ Finished $ tryParsePhrase tc phrase
  , name = show tc
  , tags = []
  , options = []
  , setOption = \_ _ -> Right $ mkParsePhraseTest tc phrase
  }

tryParseInvalidWord :: String -> Result
tryParseInvalidWord tc =
  case parseTabcode (TCOptions { parseMode = Strict, structure = BarLines, xmlIds = WithXmlIds }) tc of
    Right (TabCode _ wrds) -> Fail $ show wrds
    Left _                 -> Pass

mkInvalidTest :: String -> TestInstance
mkInvalidTest tc = TestInstance
  { run = return $ Finished $ tryParseInvalidWord tc
  , name = show $ tc ++ " [invalid]"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right $ mkInvalidTest tc
  }

tests :: IO [Test]
tests = return $ meterSigns
  ++ rests
  ++ barLines
  ++ simpleChords
  ++ chordsWithBass
  ++ failChords
  ++ articulations
  ++ fingerings
  ++ ornaments
  ++ comments
  ++ structuredComments
  ++ phrases

meterSigns :: [Test]
meterSigns =
  [ Test $ mkParseTest "M(O.)\n" (Meter 1 1 (SingleMeterSign PerfectMajor) Nothing)
  , Test $ mkParseTest "M(O)\n" (Meter 1 1 (SingleMeterSign PerfectMinor) Nothing)
  , Test $ mkParseTest "M(C.)\n" (Meter 1 1 (SingleMeterSign ImperfectMajor) Nothing)
  , Test $ mkParseTest "M(C)\n" (Meter 1 1 (SingleMeterSign ImperfectMinor) Nothing)
  , Test $ mkParseTest "M(O/.)\n" (Meter 1 1 (SingleMeterSign HalfPerfectMajor) Nothing)
  , Test $ mkParseTest "M(O/)\n" (Meter 1 1 (SingleMeterSign HalfPerfectMinor) Nothing)
  , Test $ mkParseTest "M(C/.)\n" (Meter 1 1 (SingleMeterSign HalfImperfectMajor) Nothing)
  , Test $ mkParseTest "M(C/)\n" (Meter 1 1 (SingleMeterSign HalfImperfectMinor) Nothing)
  , Test $ mkParseTest "M(D.)\n" (Meter 1 1 (SingleMeterSign HalfImperfectMajor) Nothing)
  , Test $ mkParseTest "M(D)\n" (Meter 1 1 (SingleMeterSign HalfImperfectMinor) Nothing)
  , Test $ mkParseTest "M(3)\n" (Meter 1 1 (SingleMeterSign (Beats 3)) Nothing)
  , Test $ mkParseTest "M(4:4)\n" (Meter 1 1 (VerticalMeterSign (Beats 4) (Beats 4)) Nothing)
  , Test $ mkParseTest "M(4;4)\n" (Meter 1 1 (HorizontalMeterSign (Beats 4) (Beats 4)) Nothing)
  ]

rests :: [Test]
rests =
  [ Test $ mkParseTest "F\n" (Rest 1 1 (RhythmSign Fermata Simple NoDot Nothing) Nothing)
  , Test $ mkParseTest "B\n" (Rest 1 1 (RhythmSign Breve Simple NoDot Nothing) Nothing)
  , Test $ mkParseTest "W\n" (Rest 1 1 (RhythmSign Semibreve Simple NoDot Nothing) Nothing)
  , Test $ mkParseTest "W.\n" (Rest 1 1 (RhythmSign Semibreve Simple Dot Nothing) Nothing)
  , Test $ mkParseTest "W3\n" (Rest 1 1 (RhythmSign Semibreve Compound NoDot Nothing) Nothing)
  , Test $ mkParseTest "H\n" (Rest 1 1 (RhythmSign Minim Simple NoDot Nothing) Nothing)
  , Test $ mkParseTest "H.\n" (Rest 1 1 (RhythmSign Minim Simple Dot Nothing) Nothing)
  , Test $ mkParseTest "H3\n" (Rest 1 1 (RhythmSign Minim Compound NoDot Nothing) Nothing)
  , Test $ mkParseTest "Q\n" (Rest 1 1 (RhythmSign Crotchet Simple NoDot Nothing) Nothing)
  , Test $ mkParseTest "Q.\n" (Rest 1 1 (RhythmSign Crotchet Simple Dot Nothing) Nothing)
  , Test $ mkParseTest "Q3\n" (Rest 1 1 (RhythmSign Crotchet Compound NoDot Nothing) Nothing)
  , Test $ mkParseTest "E\n" (Rest 1 1 (RhythmSign Quaver Simple NoDot Nothing) Nothing)
  , Test $ mkParseTest "E.\n" (Rest 1 1 (RhythmSign Quaver Simple Dot Nothing) Nothing)
  , Test $ mkParseTest "E3\n" (Rest 1 1 (RhythmSign Quaver Compound NoDot Nothing) Nothing)
  , Test $ mkParseTest "S\n" (Rest 1 1 (RhythmSign Semiquaver Simple NoDot Nothing) Nothing)
  , Test $ mkParseTest "S.\n" (Rest 1 1 (RhythmSign Semiquaver Simple Dot Nothing) Nothing)
  , Test $ mkParseTest "S3\n" (Rest 1 1 (RhythmSign Semiquaver Compound NoDot Nothing) Nothing)
  , Test $ mkParseTest "T\n" (Rest 1 1 (RhythmSign Demisemiquaver Simple NoDot Nothing) Nothing)
  , Test $ mkParseTest "T.\n" (Rest 1 1 (RhythmSign Demisemiquaver Simple Dot Nothing) Nothing)
  , Test $ mkParseTest "T3\n" (Rest 1 1 (RhythmSign Demisemiquaver Compound NoDot Nothing) Nothing)
  , Test $ mkParseTest "Y\n" (Rest 1 1 (RhythmSign Hemidemisemiquaver Simple NoDot Nothing) Nothing)
  , Test $ mkParseTest "Y.\n" (Rest 1 1 (RhythmSign Hemidemisemiquaver Simple Dot Nothing) Nothing)
  , Test $ mkParseTest "Y3\n" (Rest 1 1 (RhythmSign Hemidemisemiquaver Compound NoDot Nothing) Nothing)
  , Test $ mkParseTest "Z\n" (Rest 1 1 (RhythmSign Semihemidemisemiquaver Simple NoDot Nothing) Nothing)
  , Test $ mkParseTest "Z.\n" (Rest 1 1 (RhythmSign Semihemidemisemiquaver Simple Dot Nothing) Nothing)
  , Test $ mkParseTest "Z3\n" (Rest 1 1 (RhythmSign Semihemidemisemiquaver Compound NoDot Nothing) Nothing)
  ]

barLines :: [Test]
barLines =
  [ Test $ mkParseTest "|\n" (BarLine 1 1 (SingleBar Nothing Nothing NotDashed Counting) Nothing)
  , Test $ mkParseTest "||\n" (BarLine 1 1 (DoubleBar Nothing Nothing NotDashed Counting) Nothing)
  , Test $ mkParseTest ":|\n" (BarLine 1 1 (SingleBar (Just RepeatLeft) Nothing NotDashed Counting) Nothing)
  , Test $ mkParseTest ":||\n" (BarLine 1 1 (DoubleBar (Just RepeatLeft) Nothing NotDashed Counting) Nothing)
  , Test $ mkParseTest "|:\n" (BarLine 1 1 (SingleBar (Just RepeatRight) Nothing NotDashed Counting) Nothing)
  , Test $ mkParseTest "||:\n" (BarLine 1 1 (DoubleBar (Just RepeatRight) Nothing NotDashed Counting) Nothing)
  , Test $ mkParseTest ":|:\n" (BarLine 1 1 (SingleBar (Just RepeatBoth) Nothing NotDashed Counting) Nothing)
  , Test $ mkParseTest ":||:\n" (BarLine 1 1 (DoubleBar (Just RepeatBoth) Nothing NotDashed Counting) Nothing)
  , Test $ mkParseTest "|=\n" (BarLine 1 1 (SingleBar Nothing Nothing Dashed Counting) Nothing)
  , Test $ mkParseTest "||=\n" (BarLine 1 1 (DoubleBar Nothing Nothing Dashed Counting) Nothing)
  , Test $ mkParseTest ":|=\n" (BarLine 1 1 (SingleBar (Just RepeatLeft) Nothing Dashed Counting) Nothing)
  , Test $ mkParseTest ":||=\n" (BarLine 1 1 (DoubleBar (Just RepeatLeft) Nothing Dashed Counting) Nothing)
  , Test $ mkParseTest "|:=\n" (BarLine 1 1 (SingleBar (Just RepeatRight) Nothing Dashed Counting) Nothing)
  , Test $ mkParseTest "||:=\n" (BarLine 1 1 (DoubleBar (Just RepeatRight) Nothing Dashed Counting) Nothing)
  , Test $ mkParseTest ":|:=\n" (BarLine 1 1 (SingleBar (Just RepeatBoth) Nothing Dashed Counting) Nothing)
  , Test $ mkParseTest ":||:=\n" (BarLine 1 1 (DoubleBar (Just RepeatBoth) Nothing Dashed Counting) Nothing)
  , Test $ mkParseTest "|0\n" (BarLine 1 1 (SingleBar Nothing Nothing NotDashed NotCounting) Nothing)
  , Test $ mkParseTest "||0\n" (BarLine 1 1 (DoubleBar Nothing Nothing NotDashed NotCounting) Nothing)
  , Test $ mkParseTest ":|0\n" (BarLine 1 1 (SingleBar (Just RepeatLeft) Nothing NotDashed NotCounting) Nothing)
  , Test $ mkParseTest ":||0\n" (BarLine 1 1 (DoubleBar (Just RepeatLeft) Nothing NotDashed NotCounting) Nothing)
  , Test $ mkParseTest "|:0\n" (BarLine 1 1 (SingleBar (Just RepeatRight) Nothing NotDashed NotCounting) Nothing)
  , Test $ mkParseTest "||:0\n" (BarLine 1 1 (DoubleBar (Just RepeatRight) Nothing NotDashed NotCounting) Nothing)
  , Test $ mkParseTest ":|:0\n" (BarLine 1 1 (SingleBar (Just RepeatBoth) Nothing NotDashed NotCounting) Nothing)
  , Test $ mkParseTest ":||:0\n" (BarLine 1 1 (DoubleBar (Just RepeatBoth) Nothing NotDashed NotCounting) Nothing)
  , Test $ mkParseTest "|(T=:\\R)\n" (BarLine 1 1 (SingleBar Nothing (Just Reprise) NotDashed Counting) Nothing)
  , Test $ mkParseTest "||(T=:\\R)\n" (BarLine 1 1 (DoubleBar Nothing (Just Reprise) NotDashed Counting) Nothing)
  , Test $ mkParseTest "|(T+:\\1)\n" (BarLine 1 1 (SingleBar Nothing (Just $ NthTime 1) NotDashed Counting) Nothing)
  , Test $ mkParseTest "||(T+:\\1)\n" (BarLine 1 1 (DoubleBar Nothing (Just $ NthTime 1) NotDashed Counting) Nothing)
  ]

simpleChords :: [Test]
simpleChords =
  [ Test $ mkParseTest "c1\n" (Chord 1 1 Nothing [Note One C (Nothing, Nothing) Nothing Nothing Nothing] Nothing)
  , Test $ mkParseTest "c1a2\n" (Chord 1 1 Nothing [ Note One C (Nothing, Nothing) Nothing Nothing Nothing
                                                   , Note Two A (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "Qc1\n" (Chord 1 1 (Just (RhythmSign Crotchet Simple NoDot Nothing)) [Note One C (Nothing, Nothing) Nothing Nothing Nothing] Nothing)
  , Test $ mkParseTest "Q.c1\n" (Chord 1 1 (Just (RhythmSign Crotchet Simple Dot Nothing)) [Note One C (Nothing, Nothing) Nothing Nothing Nothing] Nothing)
  , Test $ mkParseTest "Q3c1\n" (Chord 1 1 (Just (RhythmSign Crotchet Compound NoDot Nothing)) [Note One C (Nothing, Nothing) Nothing Nothing Nothing] Nothing)
  , Test $ mkParseTest "Qc1a2\n" (Chord 1 1 (Just (RhythmSign Crotchet Simple NoDot Nothing)) [ Note One C (Nothing, Nothing) Nothing Nothing Nothing
                                                                                              , Note Two A (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  ]

chordsWithBass :: [Test]
chordsWithBass =
  [ Test $ mkParseTest "Xa\n" (Chord 1 1 Nothing [Note (Bass 1) A (Nothing, Nothing) Nothing Nothing Nothing] Nothing)
  , Test $ mkParseTest "Xa/\n" (Chord 1 1 Nothing [Note (Bass 2) A (Nothing, Nothing) Nothing Nothing Nothing] Nothing)
  , Test $ mkParseTest "Xb//\n" (Chord 1 1 Nothing [Note (Bass 3) B (Nothing, Nothing) Nothing Nothing Nothing] Nothing)
  , Test $ mkParseTest "X1\n" (Chord 1 1 Nothing [Note (Bass 1) A (Nothing, Nothing) Nothing Nothing Nothing] Nothing)
  , Test $ mkParseTest "X4\n" (Chord 1 1 Nothing [Note (Bass 4) A (Nothing, Nothing) Nothing Nothing Nothing] Nothing)
  , Test $ mkParseTest "c1Xa\n" (Chord 1 1 Nothing [ Note One C (Nothing, Nothing) Nothing Nothing Nothing
                                                   , Note (Bass 1) A (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "c1Xa/\n" (Chord 1 1 Nothing [ Note One C (Nothing, Nothing) Nothing Nothing Nothing
                                                    , Note (Bass 2) A (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "c1Xb//\n" (Chord 1 1 Nothing [ Note One C (Nothing, Nothing) Nothing Nothing Nothing
                                                     , Note (Bass 3) B (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "c1X1\n" (Chord 1 1 Nothing [ Note One C (Nothing, Nothing) Nothing Nothing Nothing
                                                   , Note (Bass 1) A (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "c1X4\n" (Chord 1 1 Nothing [ Note One C (Nothing, Nothing) Nothing Nothing Nothing
                                                   , Note (Bass 4) A (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "Q.Xa\n" (Chord 1 1 (Just (RhythmSign Crotchet Simple Dot Nothing))
                                  [Note (Bass 1) A (Nothing, Nothing) Nothing Nothing Nothing] Nothing)
  , Test $ mkParseTest "Q.Xa/\n" (Chord 1 1 (Just (RhythmSign Crotchet Simple Dot Nothing))
                                   [Note (Bass 2) A (Nothing, Nothing) Nothing Nothing Nothing] Nothing)
  , Test $ mkParseTest "Q.Xb//\n" (Chord 1 1 (Just (RhythmSign Crotchet Simple Dot Nothing))
                                   [Note (Bass 3) B (Nothing, Nothing) Nothing Nothing Nothing] Nothing)
  , Test $ mkParseTest "Q.X1\n" (Chord 1 1 (Just (RhythmSign Crotchet Simple Dot Nothing))
                                  [Note (Bass 1) A (Nothing, Nothing) Nothing Nothing Nothing] Nothing)
  , Test $ mkParseTest "Q.X4\n" (Chord 1 1 (Just (RhythmSign Crotchet Simple Dot Nothing))
                                  [Note (Bass 4) A (Nothing, Nothing) Nothing Nothing Nothing] Nothing)
  , Test $ mkParseTest "Q.c1Xa\n" (Chord 1 1 (Just (RhythmSign Crotchet Simple Dot Nothing))
                                    [ Note One C (Nothing, Nothing) Nothing Nothing Nothing
                                    , Note (Bass 1) A (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "Q.c1Xa/\n" (Chord 1 1 (Just (RhythmSign Crotchet Simple Dot Nothing))
                                     [ Note One C (Nothing, Nothing) Nothing Nothing Nothing
                                     , Note (Bass 2) A (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "Q.c1Xb//\n" (Chord 1 1 (Just (RhythmSign Crotchet Simple Dot Nothing))
                                      [ Note One C (Nothing, Nothing) Nothing Nothing Nothing
                                      , Note (Bass 3) B (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "Q.c1X1\n" (Chord 1 1 (Just (RhythmSign Crotchet Simple Dot Nothing))
                                    [ Note One C (Nothing, Nothing) Nothing Nothing Nothing
                                    , Note (Bass 1) A (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "Q.c1X4\n" (Chord 1 1 (Just (RhythmSign Crotchet Simple Dot Nothing))
                                    [ Note One C (Nothing, Nothing) Nothing Nothing Nothing
                                    , Note (Bass 4) A (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  ]

failChords :: [Test]
failChords =
  [ Test $ mkInvalidTest "a1b1\n"
  , Test $ mkInvalidTest "a1a2a1\n"
  , Test $ mkInvalidTest "Qa1b1\n"
  , Test $ mkInvalidTest "Qa1a2a1\n"
  , Test $ mkInvalidTest "Q.a1b1\n"
  , Test $ mkInvalidTest "Q.a1a2a1\n"
  , Test $ mkInvalidTest "o1\n"
  , Test $ mkInvalidTest "a1\no1\n"
  , Test $ mkInvalidTest "o1\na1\n"
  , Test $ mkInvalidTest "So1\n"
  , Test $ mkInvalidTest "Sa1\no1\n"
  , Test $ mkInvalidTest "So1\na1\n"
  , Test $ mkInvalidTest "E.p1\n"
  ]

articulations :: [Test]
articulations =
  [ Test $ mkParseTest "b3(E)d4\n" (Chord 1 1 Nothing [ Note Three B (Nothing, Nothing) Nothing (Just Ensemble) Nothing
                                                      , Note Four D (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "Hc1c2d3(E)d6\n" (Chord 1 1 (Just (RhythmSign Minim Simple NoDot Nothing))
                                          [ Note One C (Nothing, Nothing) Nothing Nothing Nothing
                                          , Note Two C (Nothing, Nothing) Nothing Nothing Nothing
                                          , Note Three D (Nothing, Nothing) Nothing (Just Ensemble) Nothing
                                          , Note Six D (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "c1(E)X4" (Chord 1 1 Nothing [ Note One C (Nothing, Nothing) Nothing (Just Ensemble) Nothing
                                                    , Note (Bass 4) A (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "b3(S)d4\n" (Chord 1 1 Nothing [ Note Three B (Nothing, Nothing) Nothing (Just $ Separee Nothing Nothing) Nothing
                                                      , Note Four D (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "Hc1c2d3(S)d6\n" (Chord 1 1 (Just (RhythmSign Minim Simple NoDot Nothing))
                                          [ Note One C (Nothing, Nothing) Nothing Nothing Nothing
                                          , Note Two C (Nothing, Nothing) Nothing Nothing Nothing
                                          , Note Three D (Nothing, Nothing) Nothing (Just $ Separee Nothing Nothing) Nothing
                                          , Note Six D (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "c1(S)X4" (Chord 1 1 Nothing [ Note One C (Nothing, Nothing) Nothing (Just $ Separee Nothing Nothing) Nothing
                                                    , Note (Bass 4) A (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "b3(Sd)d4\n" (Chord 1 1 Nothing [ Note Three B (Nothing, Nothing) Nothing (Just $ Separee (Just SepareeDown) Nothing) Nothing
                                                       , Note Four D (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "b3(Su)d4\n" (Chord 1 1 Nothing [ Note Three B (Nothing, Nothing) Nothing (Just $ Separee (Just SepareeUp) Nothing) Nothing
                                                       , Note Four D (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "b3(Sd:l)d4\n" (Chord 1 1 Nothing [ Note Three B (Nothing, Nothing) Nothing (Just $ Separee (Just SepareeDown) (Just SepareeLeft)) Nothing
                                                         , Note Four D (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "b3(Su:r)d4\n" (Chord 1 1 Nothing [ Note Three B (Nothing, Nothing) Nothing (Just $ Separee (Just SepareeUp) (Just SepareeRight)) Nothing
                                                         , Note Four D (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "Hc1(E)c2d3(S)d6\n" (Chord 1 1 (Just (RhythmSign Minim Simple NoDot Nothing))
                                            [ Note One C (Nothing, Nothing) Nothing (Just Ensemble) Nothing
                                            , Note Two C (Nothing, Nothing) Nothing Nothing Nothing
                                            , Note Three D (Nothing, Nothing) Nothing (Just $ Separee Nothing Nothing) Nothing
                                            , Note Six D (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkInvalidTest "b3(E)(S)d4\n"
  , Test $ mkInvalidTest "b3(S)(E)d4\n"
  , Test $ mkInvalidTest "b3(E)(Su)d4\n"
  , Test $ mkInvalidTest "b3(Su)(E)d4\n"
  ]

fingerings :: [Test]
fingerings =
  [ Test $ mkParseTest "c1(Fr...:7)\n" (Chord 1 1 Nothing [ Note One C (Just $ FingeringRight FingerThree (Just PosBelow), Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "c1(F...)\n" (Chord 1 1 Nothing [ Note One C (Just $ FingeringRight FingerThree Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "c1(F...:6)\n" (Chord 1 1 Nothing [ Note One C (Just $ FingeringRight FingerThree (Just PosBelowLeft), Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "c1(F2:6)\n" (Chord 1 1 Nothing [ Note One C (Just $ FingeringLeft FingerTwo (Just PosBelowLeft), Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "c1(F2)\n" (Chord 1 1 Nothing [ Note One C (Just $ FingeringLeft FingerTwo Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "c1(Fl2:6)\n" (Chord 1 1 Nothing [ Note One C (Just $ FingeringLeft FingerTwo (Just PosBelowLeft), Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "c1(Fr2:6)\n" (Chord 1 1 Nothing [ Note One C (Just $ FingeringRight FingerTwo (Just PosBelowLeft), Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "c1(Fr2)\n" (Chord 1 1 Nothing [ Note One C (Just $ FingeringRight FingerTwo Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "c1.\n" (Chord 1 1 Nothing [ Note One C (Just $ FingeringRight FingerOne Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "c1:\n" (Chord 1 1 Nothing [ Note One C (Just $ FingeringRight FingerTwo Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "c1!\n" (Chord 1 1 Nothing [ Note One C (Just $ FingeringRight Thumb Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "c1\"\n" (Chord 1 1 Nothing [ Note One C (Just $ FingeringRight FingerTwo Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "c1(Fl2)!\n" (Chord 1 1 Nothing [ Note One C (Just $ FingeringLeft FingerTwo Nothing, Just $ FingeringRight Thumb Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "c1(Fl2)\"\n" (Chord 1 1 Nothing [ Note One C (Just $ FingeringLeft FingerTwo Nothing, Just $ FingeringRight FingerTwo Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "c1!(Fl2)\n" (Chord 1 1 Nothing [ Note One C (Just $ FingeringRight Thumb Nothing, Just $ FingeringLeft FingerTwo Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "c1\"(Fl2)\n" (Chord 1 1 Nothing [ Note One C (Just $ FingeringRight FingerTwo Nothing, Just $ FingeringLeft FingerTwo Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "c1(Fr!)(Fl2)\n" (Chord 1 1 Nothing [ Note One C (Just $ FingeringRight Thumb Nothing, Just $ FingeringLeft FingerTwo Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkInvalidTest "c1(Fl..:7)\n"
  , Test $ mkInvalidTest "c1(Fr...:9)\n"
  , Test $ mkInvalidTest "c1(F.....)\n"
  , Test $ mkInvalidTest "c1(F5)\n"
  , Test $ mkInvalidTest "c1(Fx1)\n"
  , Test $ mkInvalidTest "c1!.\n"
  , Test $ mkInvalidTest "c1\":\n"
  ]

ornaments :: [Test]
ornaments =
  [ Test $ mkParseTest "b2(Oa)\n" (Chord 1 1 Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnA Nothing Nothing) Nothing Nothing ] Nothing)
  , Test $ mkParseTest "b2(Oa1)\n" (Chord 1 1 Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnA (Just 1) Nothing) Nothing Nothing ] Nothing)
  , Test $ mkParseTest "b2(Oa:1)\n" (Chord 1 1 Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnA Nothing (Just PosAboveLeft)) Nothing Nothing ] Nothing)
  , Test $ mkParseTest "b2(Oa2:5)\n" (Chord 1 1 Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnA (Just 2) (Just PosRight)) Nothing Nothing ] Nothing)
  , Test $ mkParseTest "b2,\n" (Chord 1 1 Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnA (Just 1) Nothing) Nothing Nothing ] Nothing)
  , Test $ mkParseTest "b2(C)\n" (Chord 1 1 Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnB Nothing Nothing) Nothing Nothing ] Nothing)
  , Test $ mkParseTest "b2u\n" (Chord 1 1 Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnC (Just 1) Nothing) Nothing Nothing ] Nothing)
  , Test $ mkParseTest "b2<\n" (Chord 1 1 Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnC (Just 2) Nothing) Nothing Nothing ] Nothing)
  , Test $ mkParseTest "b2#\n" (Chord 1 1 Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnE Nothing Nothing) Nothing Nothing ] Nothing)
  , Test $ mkParseTest "b2x\n" (Chord 1 1 Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnF Nothing Nothing) Nothing Nothing ] Nothing)
  , Test $ mkParseTest "b2~\n" (Chord 1 1 Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnH Nothing Nothing) Nothing Nothing ] Nothing)
  , Test $ mkParseTest "b2(Oa)a3\n" (Chord 1 1 Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnA Nothing Nothing) Nothing Nothing
                                                       , Note Three A (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "b2,a3\n" (Chord 1 1 Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnA (Just 1) Nothing) Nothing Nothing
                                                    , Note Three A (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "b2(C)a3\n" (Chord 1 1 Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnB Nothing Nothing) Nothing Nothing
                                                      , Note Three A (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "b2ua3\n" (Chord 1 1 Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnC (Just 1) Nothing) Nothing Nothing
                                                    , Note Three A (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "b2<a3\n" (Chord 1 1 Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnC (Just 2) Nothing) Nothing Nothing
                                                    , Note Three A (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "b2#a3\n" (Chord 1 1 Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnE Nothing Nothing) Nothing Nothing
                                                    , Note Three A (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "b2xa3\n" (Chord 1 1 Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnF Nothing Nothing) Nothing Nothing
                                                    , Note Three A (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkParseTest "b2~a3\n" (Chord 1 1 Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnH Nothing Nothing) Nothing Nothing
                                                    , Note Three A (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
  , Test $ mkInvalidTest "b2(On)\n"
  , Test $ mkInvalidTest "b2(Oa)(Ob)\n"
  , Test $ mkInvalidTest "b2(Oa),\n"
  , Test $ mkInvalidTest "b2,u\n"
  ]

comments :: [Test]
comments =
  [ Test $ mkParseTest "{foo bar}\n" (CommentWord 1 1 (Comment "foo bar"))
  , Test $ mkParseTest "{foo {bar}\n" (CommentWord 1 1 (Comment "foo {bar"))
  , Test $ mkParseTest "{Qc1a2}" (CommentWord 1 1 (Comment "Qc1a2"))
  , Test $ mkParseTest "{}" (CommentWord 1 1 (Comment ""))
  , Test $ mkParseTest "{\n}" (CommentWord 1 1 (Comment "\n"))
  , Test $ mkParseTest "{foo\nbar}" (CommentWord 1 1 (Comment "foo\nbar"))
  , Test $ mkParseTest "Q.{foo}" (Rest 1 1 (RhythmSign Crotchet Simple Dot Nothing) (Just $ Comment "foo"))
  , Test $ mkParseTest "|{foo}" (BarLine 1 1 (SingleBar Nothing Nothing NotDashed Counting) (Just $ Comment "foo"))
  , Test $ mkParseTest "M(3){foo}" (Meter 1 1 (SingleMeterSign (Beats 3)) (Just $ Comment "foo"))
  , Test $ mkParseTest "a1c3{foo}" (Chord 1 1 Nothing [ Note One A (Nothing, Nothing) Nothing Nothing Nothing
                                                      , Note Three C (Nothing, Nothing) Nothing Nothing Nothing ] (Just $ Comment "foo"))
  , Test $ mkParseTest "a1c3 {foo}" (Chord 1 1 Nothing [ Note One A (Nothing, Nothing) Nothing Nothing Nothing
                                                       , Note Three C (Nothing, Nothing) Nothing Nothing Nothing ] (Just $ Comment "foo"))
  , Test $ mkParsePhraseTest "a1c3\n{foo}" [ (Chord 1 1 Nothing [ Note One A (Nothing, Nothing) Nothing Nothing Nothing
                                                                , Note Three C (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
                                           , (CommentWord 2 1 (Comment "foo")) ]
  , Test $ mkParseTest "{^}{foo}" (SystemBreak 1 1 (Just $ Comment "foo"))
  , Test $ mkParseTest "{>}{^}{foo}" (PageBreak 1 1 (Just $ Comment "foo"))
  ]

structuredComments :: [Test]
structuredComments =
  [ Test $ mkParseTest "{^}\n" (SystemBreak 1 1 Nothing)
  , Test $ mkParseTest "{>}{^}\n" (PageBreak 1 1 Nothing)
  ]

phrases :: [Test]
phrases =
  [ Test $ mkParsePhraseTest "a1c3\na1c3"
    [ (Chord 1 1 Nothing [ Note One A (Nothing, Nothing) Nothing Nothing Nothing
                         , Note Three C (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
    , (Chord 2 1 Nothing [ Note One A (Nothing, Nothing) Nothing Nothing Nothing
                         , Note Three C (Nothing, Nothing) Nothing Nothing Nothing ] Nothing) ]
  , Test $ mkParsePhraseTest "a1c3 a1c3"
    [ (Chord 1 1 Nothing [ Note One A (Nothing, Nothing) Nothing Nothing Nothing
                         , Note Three C (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
    , (Chord 1 6 Nothing [ Note One A (Nothing, Nothing) Nothing Nothing Nothing
                         , Note Three C (Nothing, Nothing) Nothing Nothing Nothing ] Nothing) ]
  , Test $ mkParsePhraseTest "a1c3 a1c3 {foo}"
    [ (Chord 1 1 Nothing [ Note One A (Nothing, Nothing) Nothing Nothing Nothing
                         , Note Three C (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
    , (Chord 1 6 Nothing [ Note One A (Nothing, Nothing) Nothing Nothing Nothing
                         , Note Three C (Nothing, Nothing) Nothing Nothing Nothing ] (Just $ Comment "foo")) ]
  , Test $ mkParsePhraseTest "a1c3 a1c3{foo}"
    [ (Chord 1 1 Nothing [ Note One A (Nothing, Nothing) Nothing Nothing Nothing
                         , Note Three C (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
    , (Chord 1 6 Nothing [ Note One A (Nothing, Nothing) Nothing Nothing Nothing
                         , Note Three C (Nothing, Nothing) Nothing Nothing Nothing ] (Just $ Comment "foo")) ]
  , Test $ mkParsePhraseTest "a1c3\na1c3 {foo}"
    [ (Chord 1 1 Nothing [ Note One A (Nothing, Nothing) Nothing Nothing Nothing
                         , Note Three C (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
    , (Chord 2 1 Nothing [ Note One A (Nothing, Nothing) Nothing Nothing Nothing
                         , Note Three C (Nothing, Nothing) Nothing Nothing Nothing ] (Just $ Comment "foo")) ]
  , Test $ mkParsePhraseTest "a1c3 Q {foo}"
    [ (Chord 1 1 Nothing [ Note One A (Nothing, Nothing) Nothing Nothing Nothing
                         , Note Three C (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
    , (Rest 1 6 (RhythmSign Crotchet Simple NoDot Nothing) (Just $ Comment "foo")) ]
  , Test $ mkParsePhraseTest "a1c3 | {foo}"
    [ (Chord 1 1 Nothing [ Note One A (Nothing, Nothing) Nothing Nothing Nothing
                         , Note Three C (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
    , (BarLine 1 6 (SingleBar Nothing Nothing NotDashed Counting) (Just $ Comment "foo")) ]
  , Test $ mkParsePhraseTest "a1c3 M(O) {foo}"
    [ (Chord 1 1 Nothing [ Note One A (Nothing, Nothing) Nothing Nothing Nothing
                         , Note Three C (Nothing, Nothing) Nothing Nothing Nothing ] Nothing)
    , (Meter 1 6 (SingleMeterSign PerfectMinor) (Just $ Comment "foo")) ]
  , Test $ mkParsePhraseTest "M(O.) {foo} a1c3"
    [ (Meter 1 1 (SingleMeterSign PerfectMajor) (Just $ Comment "foo"))
    , (Chord 1 13 Nothing [ Note One A (Nothing, Nothing) Nothing Nothing Nothing
                          , Note Three C (Nothing, Nothing) Nothing Nothing Nothing ] Nothing) ]
  ]
