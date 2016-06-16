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
import TabCode.Options (TCOptions(..), ParseMode(..))
import TabCode.Parser (parseTabcode)

tryParseWord :: String -> TabWord -> Result
tryParseWord tc tw =
  case parseTabcode (TCOptions { parseMode = Strict }) tc of
    Right (TabCode rls wrds) -> check wrds tw
    Left err                 -> Fail $ show err
  where
    check [] twExp = Error $ "Could not parse " ++ tc ++ " as " ++ (show twExp)
    check (twAct:_) twExp | twExp == twAct = Pass
                          | otherwise      = Fail $ "For \"" ++ tc ++ "\", expected " ++ (show twExp) ++ "; got " ++ (show twAct)

mkParseTest :: String -> TabWord -> TestInstance
mkParseTest tc tw = TestInstance {
    run = return $ Finished $ tryParseWord tc tw
  , name = show tc
  , tags = []
  , options = []
  , setOption = \_ _ -> Right $ mkParseTest tc tw
  }

tryParseInvalidWord :: String -> Result
tryParseInvalidWord tc =
  case parseTabcode (TCOptions { parseMode = Strict }) tc of
    Right (TabCode rls wrds) -> Fail $ show wrds
    Left err                 -> Pass

mkInvalidTest :: String -> TestInstance
mkInvalidTest tc = TestInstance {
    run = return $ Finished $ tryParseInvalidWord tc
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

meterSigns :: [Test]
meterSigns =
  [ Test $ mkParseTest "M(O.)\n" (Meter (SingleMeterSign PerfectMajor))
  , Test $ mkParseTest "M(O)\n" (Meter (SingleMeterSign PerfectMinor))
  , Test $ mkParseTest "M(C.)\n" (Meter (SingleMeterSign ImperfectMajor))
  , Test $ mkParseTest "M(C)\n" (Meter (SingleMeterSign ImperfectMinor))
  , Test $ mkParseTest "M(O/.)\n" (Meter (SingleMeterSign HalfPerfectMajor))
  , Test $ mkParseTest "M(O/)\n" (Meter (SingleMeterSign HalfPerfectMinor))
  , Test $ mkParseTest "M(C/.)\n" (Meter (SingleMeterSign HalfImperfectMajor))
  , Test $ mkParseTest "M(C/)\n" (Meter (SingleMeterSign HalfImperfectMinor))
  , Test $ mkParseTest "M(D.)\n" (Meter (SingleMeterSign HalfImperfectMajor))
  , Test $ mkParseTest "M(D)\n" (Meter (SingleMeterSign HalfImperfectMinor))
  , Test $ mkParseTest "M(3)\n" (Meter (SingleMeterSign (Beats 3)))
  , Test $ mkParseTest "M(4:4)\n" (Meter (VerticalMeterSign (Beats 4) (Beats 4)))
  , Test $ mkParseTest "M(4;4)\n" (Meter (HorizontalMeterSign (Beats 4) (Beats 4)))
  ]

rests :: [Test]
rests =
  [ Test $ mkParseTest "F\n" (Rest (RhythmSign Fermata Simple NoDot Nothing))
  , Test $ mkParseTest "B\n" (Rest (RhythmSign Breve Simple NoDot Nothing))
  , Test $ mkParseTest "W\n" (Rest (RhythmSign Semibreve Simple NoDot Nothing))
  , Test $ mkParseTest "W.\n" (Rest (RhythmSign Semibreve Simple Dot Nothing))
  , Test $ mkParseTest "W3\n" (Rest (RhythmSign Semibreve Compound NoDot Nothing))
  , Test $ mkParseTest "H\n" (Rest (RhythmSign Minim Simple NoDot Nothing))
  , Test $ mkParseTest "H.\n" (Rest (RhythmSign Minim Simple Dot Nothing))
  , Test $ mkParseTest "H3\n" (Rest (RhythmSign Minim Compound NoDot Nothing))
  , Test $ mkParseTest "Q\n" (Rest (RhythmSign Crotchet Simple NoDot Nothing))
  , Test $ mkParseTest "Q.\n" (Rest (RhythmSign Crotchet Simple Dot Nothing))
  , Test $ mkParseTest "Q3\n" (Rest (RhythmSign Crotchet Compound NoDot Nothing))
  , Test $ mkParseTest "E\n" (Rest (RhythmSign Quaver Simple NoDot Nothing))
  , Test $ mkParseTest "E.\n" (Rest (RhythmSign Quaver Simple Dot Nothing))
  , Test $ mkParseTest "E3\n" (Rest (RhythmSign Quaver Compound NoDot Nothing))
  , Test $ mkParseTest "S\n" (Rest (RhythmSign Semiquaver Simple NoDot Nothing))
  , Test $ mkParseTest "S.\n" (Rest (RhythmSign Semiquaver Simple Dot Nothing))
  , Test $ mkParseTest "S3\n" (Rest (RhythmSign Semiquaver Compound NoDot Nothing))
  , Test $ mkParseTest "T\n" (Rest (RhythmSign Demisemiquaver Simple NoDot Nothing))
  , Test $ mkParseTest "T.\n" (Rest (RhythmSign Demisemiquaver Simple Dot Nothing))
  , Test $ mkParseTest "T3\n" (Rest (RhythmSign Demisemiquaver Compound NoDot Nothing))
  , Test $ mkParseTest "Y\n" (Rest (RhythmSign Hemidemisemiquaver Simple NoDot Nothing))
  , Test $ mkParseTest "Y.\n" (Rest (RhythmSign Hemidemisemiquaver Simple Dot Nothing))
  , Test $ mkParseTest "Y3\n" (Rest (RhythmSign Hemidemisemiquaver Compound NoDot Nothing))
  , Test $ mkParseTest "Z\n" (Rest (RhythmSign Semihemidemisemiquaver Simple NoDot Nothing))
  , Test $ mkParseTest "Z.\n" (Rest (RhythmSign Semihemidemisemiquaver Simple Dot Nothing))
  , Test $ mkParseTest "Z3\n" (Rest (RhythmSign Semihemidemisemiquaver Compound NoDot Nothing))
  ]

barLines :: [Test]
barLines =
  [ Test $ mkParseTest "|\n" (BarLine (SingleBar Nothing Nothing NotDashed Counting))
  , Test $ mkParseTest "||\n" (BarLine (DoubleBar Nothing Nothing NotDashed Counting))
  , Test $ mkParseTest ":|\n" (BarLine (SingleBar (Just RepeatLeft) Nothing NotDashed Counting))
  , Test $ mkParseTest ":||\n" (BarLine (DoubleBar (Just RepeatLeft) Nothing NotDashed Counting))
  , Test $ mkParseTest "|:\n" (BarLine (SingleBar (Just RepeatRight) Nothing NotDashed Counting))
  , Test $ mkParseTest "||:\n" (BarLine (DoubleBar (Just RepeatRight) Nothing NotDashed Counting))
  , Test $ mkParseTest ":|:\n" (BarLine (SingleBar (Just RepeatBoth) Nothing NotDashed Counting))
  , Test $ mkParseTest ":||:\n" (BarLine (DoubleBar (Just RepeatBoth) Nothing NotDashed Counting))
  , Test $ mkParseTest "|=\n" (BarLine (SingleBar Nothing Nothing Dashed Counting))
  , Test $ mkParseTest "||=\n" (BarLine (DoubleBar Nothing Nothing Dashed Counting))
  , Test $ mkParseTest ":|=\n" (BarLine (SingleBar (Just RepeatLeft) Nothing Dashed Counting))
  , Test $ mkParseTest ":||=\n" (BarLine (DoubleBar (Just RepeatLeft) Nothing Dashed Counting))
  , Test $ mkParseTest "|:=\n" (BarLine (SingleBar (Just RepeatRight) Nothing Dashed Counting))
  , Test $ mkParseTest "||:=\n" (BarLine (DoubleBar (Just RepeatRight) Nothing Dashed Counting))
  , Test $ mkParseTest ":|:=\n" (BarLine (SingleBar (Just RepeatBoth) Nothing Dashed Counting))
  , Test $ mkParseTest ":||:=\n" (BarLine (DoubleBar (Just RepeatBoth) Nothing Dashed Counting))
  , Test $ mkParseTest "|0\n" (BarLine (SingleBar Nothing Nothing NotDashed NotCounting))
  , Test $ mkParseTest "||0\n" (BarLine (DoubleBar Nothing Nothing NotDashed NotCounting))
  , Test $ mkParseTest ":|0\n" (BarLine (SingleBar (Just RepeatLeft) Nothing NotDashed NotCounting))
  , Test $ mkParseTest ":||0\n" (BarLine (DoubleBar (Just RepeatLeft) Nothing NotDashed NotCounting))
  , Test $ mkParseTest "|:0\n" (BarLine (SingleBar (Just RepeatRight) Nothing NotDashed NotCounting))
  , Test $ mkParseTest "||:0\n" (BarLine (DoubleBar (Just RepeatRight) Nothing NotDashed NotCounting))
  , Test $ mkParseTest ":|:0\n" (BarLine (SingleBar (Just RepeatBoth) Nothing NotDashed NotCounting))
  , Test $ mkParseTest ":||:0\n" (BarLine (DoubleBar (Just RepeatBoth) Nothing NotDashed NotCounting))
  , Test $ mkParseTest "|(T=:\\R)\n" (BarLine (SingleBar Nothing (Just Reprise) NotDashed Counting))
  , Test $ mkParseTest "||(T=:\\R)\n" (BarLine (DoubleBar Nothing (Just Reprise) NotDashed Counting))
  , Test $ mkParseTest "|(T+:\\1)\n" (BarLine (SingleBar Nothing (Just $ NthTime 1) NotDashed Counting))
  , Test $ mkParseTest "||(T+:\\1)\n" (BarLine (DoubleBar Nothing (Just $ NthTime 1) NotDashed Counting))
  ]

simpleChords :: [Test]
simpleChords =
  [ Test $ mkParseTest "c1\n" (Chord Nothing [Note One C (Nothing, Nothing) Nothing Nothing Nothing])
  , Test $ mkParseTest "c1a2\n" (Chord Nothing [ Note One C (Nothing, Nothing) Nothing Nothing Nothing
                                               , Note Two A (Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "Qc1\n" (Chord (Just (RhythmSign Crotchet Simple NoDot Nothing)) [Note One C (Nothing, Nothing) Nothing Nothing Nothing])
  , Test $ mkParseTest "Q.c1\n" (Chord (Just (RhythmSign Crotchet Simple Dot Nothing)) [Note One C (Nothing, Nothing) Nothing Nothing Nothing])
  , Test $ mkParseTest "Q3c1\n" (Chord (Just (RhythmSign Crotchet Compound NoDot Nothing)) [Note One C (Nothing, Nothing) Nothing Nothing Nothing])
  , Test $ mkParseTest "Qc1a2\n" (Chord (Just (RhythmSign Crotchet Simple NoDot Nothing)) [ Note One C (Nothing, Nothing) Nothing Nothing Nothing
                                                                                          , Note Two A (Nothing, Nothing) Nothing Nothing Nothing ])
  ]

chordsWithBass :: [Test]
chordsWithBass =
  [ Test $ mkParseTest "Xa\n" (Chord Nothing [Note (Bass 1) A (Nothing, Nothing) Nothing Nothing Nothing])
  , Test $ mkParseTest "Xa/\n" (Chord Nothing [Note (Bass 2) A (Nothing, Nothing) Nothing Nothing Nothing])
  , Test $ mkParseTest "Xb//\n" (Chord Nothing [Note (Bass 3) B (Nothing, Nothing) Nothing Nothing Nothing])
  , Test $ mkParseTest "X1\n" (Chord Nothing [Note (Bass 1) A (Nothing, Nothing) Nothing Nothing Nothing])
  , Test $ mkParseTest "X4\n" (Chord Nothing [Note (Bass 4) A (Nothing, Nothing) Nothing Nothing Nothing])
  , Test $ mkParseTest "c1Xa\n" (Chord Nothing [ Note One C (Nothing, Nothing) Nothing Nothing Nothing
                                               , Note (Bass 1) A (Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "c1Xa/\n" (Chord Nothing [ Note One C (Nothing, Nothing) Nothing Nothing Nothing
                                                , Note (Bass 2) A (Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "c1Xb//\n" (Chord Nothing [ Note One C (Nothing, Nothing) Nothing Nothing Nothing
                                                 , Note (Bass 3) B (Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "c1X1\n" (Chord Nothing [ Note One C (Nothing, Nothing) Nothing Nothing Nothing
                                               , Note (Bass 1) A (Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "c1X4\n" (Chord Nothing [ Note One C (Nothing, Nothing) Nothing Nothing Nothing
                                               , Note (Bass 4) A (Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "Q.Xa\n" (Chord (Just (RhythmSign Crotchet Simple Dot Nothing))
                                  [Note (Bass 1) A (Nothing, Nothing) Nothing Nothing Nothing])
  , Test $ mkParseTest "Q.Xa/\n" (Chord (Just (RhythmSign Crotchet Simple Dot Nothing))
                                   [Note (Bass 2) A (Nothing, Nothing) Nothing Nothing Nothing])
  , Test $ mkParseTest "Q.Xb//\n" (Chord (Just (RhythmSign Crotchet Simple Dot Nothing))
                                   [Note (Bass 3) B (Nothing, Nothing) Nothing Nothing Nothing])
  , Test $ mkParseTest "Q.X1\n" (Chord (Just (RhythmSign Crotchet Simple Dot Nothing))
                                  [Note (Bass 1) A (Nothing, Nothing) Nothing Nothing Nothing])
  , Test $ mkParseTest "Q.X4\n" (Chord (Just (RhythmSign Crotchet Simple Dot Nothing))
                                  [Note (Bass 4) A (Nothing, Nothing) Nothing Nothing Nothing])
  , Test $ mkParseTest "Q.c1Xa\n" (Chord (Just (RhythmSign Crotchet Simple Dot Nothing))
                                    [ Note One C (Nothing, Nothing) Nothing Nothing Nothing
                                    , Note (Bass 1) A (Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "Q.c1Xa/\n" (Chord (Just (RhythmSign Crotchet Simple Dot Nothing))
                                     [ Note One C (Nothing, Nothing) Nothing Nothing Nothing
                                     , Note (Bass 2) A (Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "Q.c1Xb//\n" (Chord (Just (RhythmSign Crotchet Simple Dot Nothing))
                                      [ Note One C (Nothing, Nothing) Nothing Nothing Nothing
                                      , Note (Bass 3) B (Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "Q.c1X1\n" (Chord (Just (RhythmSign Crotchet Simple Dot Nothing))
                                    [ Note One C (Nothing, Nothing) Nothing Nothing Nothing
                                    , Note (Bass 1) A (Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "Q.c1X4\n" (Chord (Just (RhythmSign Crotchet Simple Dot Nothing))
                                    [ Note One C (Nothing, Nothing) Nothing Nothing Nothing
                                    , Note (Bass 4) A (Nothing, Nothing) Nothing Nothing Nothing ])
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
  [ Test $ mkParseTest "b3(E)d4\n" (Chord Nothing [ Note Three B (Nothing, Nothing) Nothing (Just Ensemble) Nothing
                                                  , Note Four D (Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "Hc1c2d3(E)d6\n" (Chord (Just (RhythmSign Minim Simple NoDot Nothing))
                                          [ Note One C (Nothing, Nothing) Nothing Nothing Nothing
                                          , Note Two C (Nothing, Nothing) Nothing Nothing Nothing
                                          , Note Three D (Nothing, Nothing) Nothing (Just Ensemble) Nothing
                                          , Note Six D (Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "c1(E)X4" (Chord Nothing [ Note One C (Nothing, Nothing) Nothing (Just Ensemble) Nothing
                                                , Note (Bass 4) A (Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "b3(S)d4\n" (Chord Nothing [ Note Three B (Nothing, Nothing) Nothing (Just $ Separee Nothing Nothing) Nothing
                                                  , Note Four D (Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "Hc1c2d3(S)d6\n" (Chord (Just (RhythmSign Minim Simple NoDot Nothing))
                                          [ Note One C (Nothing, Nothing) Nothing Nothing Nothing
                                          , Note Two C (Nothing, Nothing) Nothing Nothing Nothing
                                          , Note Three D (Nothing, Nothing) Nothing (Just $ Separee Nothing Nothing) Nothing
                                          , Note Six D (Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "c1(S)X4" (Chord Nothing [ Note One C (Nothing, Nothing) Nothing (Just $ Separee Nothing Nothing) Nothing
                                                , Note (Bass 4) A (Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "b3(Sd)d4\n" (Chord Nothing [ Note Three B (Nothing, Nothing) Nothing (Just $ Separee (Just SepareeDown) Nothing) Nothing
                                                  , Note Four D (Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "b3(Su)d4\n" (Chord Nothing [ Note Three B (Nothing, Nothing) Nothing (Just $ Separee (Just SepareeUp) Nothing) Nothing
                                                  , Note Four D (Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "b3(Sd:l)d4\n" (Chord Nothing [ Note Three B (Nothing, Nothing) Nothing (Just $ Separee (Just SepareeDown) (Just SepareeLeft)) Nothing
                                                  , Note Four D (Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "b3(Su:r)d4\n" (Chord Nothing [ Note Three B (Nothing, Nothing) Nothing (Just $ Separee (Just SepareeUp) (Just SepareeRight)) Nothing
                                                  , Note Four D (Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "Hc1(E)c2d3(S)d6\n" (Chord (Just (RhythmSign Minim Simple NoDot Nothing))
                                            [ Note One C (Nothing, Nothing) Nothing (Just Ensemble) Nothing
                                            , Note Two C (Nothing, Nothing) Nothing Nothing Nothing
                                            , Note Three D (Nothing, Nothing) Nothing (Just $ Separee Nothing Nothing) Nothing
                                            , Note Six D (Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkInvalidTest "b3(E)(S)d4\n"
  , Test $ mkInvalidTest "b3(S)(E)d4\n"
  , Test $ mkInvalidTest "b3(E)(Su)d4\n"
  , Test $ mkInvalidTest "b3(Su)(E)d4\n"
  ]

fingerings :: [Test]
fingerings =
  [ Test $ mkParseTest "c1(Fr...:7)\n" (Chord Nothing [ Note One C (Just $ FingeringRight FingerThree (Just PosBelow), Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "c1(F...)\n" (Chord Nothing [ Note One C (Just $ FingeringRight FingerThree Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "c1(F...:6)\n" (Chord Nothing [ Note One C (Just $ FingeringRight FingerThree (Just PosBelowLeft), Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "c1(F2:6)\n" (Chord Nothing [ Note One C (Just $ FingeringLeft FingerTwo (Just PosBelowLeft), Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "c1(F2)\n" (Chord Nothing [ Note One C (Just $ FingeringLeft FingerTwo Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "c1(Fl2:6)\n" (Chord Nothing [ Note One C (Just $ FingeringLeft FingerTwo (Just PosBelowLeft), Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "c1(Fr2:6)\n" (Chord Nothing [ Note One C (Just $ FingeringRight FingerTwo (Just PosBelowLeft), Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "c1(Fr2)\n" (Chord Nothing [ Note One C (Just $ FingeringRight FingerTwo Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "c1.\n" (Chord Nothing [ Note One C (Just $ FingeringRight FingerOne Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "c1:\n" (Chord Nothing [ Note One C (Just $ FingeringRight FingerTwo Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "c1!\n" (Chord Nothing [ Note One C (Just $ FingeringRight Thumb Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "c1\"\n" (Chord Nothing [ Note One C (Just $ FingeringRight FingerTwo Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "c1(Fl2)!\n" (Chord Nothing [ Note One C (Just $ FingeringLeft FingerTwo Nothing, Just $ FingeringRight Thumb Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "c1(Fl2)\"\n" (Chord Nothing [ Note One C (Just $ FingeringLeft FingerTwo Nothing, Just $ FingeringRight FingerTwo Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "c1!(Fl2)\n" (Chord Nothing [ Note One C (Just $ FingeringRight Thumb Nothing, Just $ FingeringLeft FingerTwo Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "c1\"(Fl2)\n" (Chord Nothing [ Note One C (Just $ FingeringRight FingerTwo Nothing, Just $ FingeringLeft FingerTwo Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "c1(Fr!)(Fl2)\n" (Chord Nothing [ Note One C (Just $ FingeringRight Thumb Nothing, Just $ FingeringLeft FingerTwo Nothing) Nothing Nothing Nothing ])
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
  [ Test $ mkParseTest "b2(Oa)\n" (Chord Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnA Nothing Nothing) Nothing Nothing ])
  , Test $ mkParseTest "b2(Oa1)\n" (Chord Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnA (Just 1) Nothing) Nothing Nothing ])
  , Test $ mkParseTest "b2(Oa:1)\n" (Chord Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnA Nothing (Just PosAboveLeft)) Nothing Nothing ])
  , Test $ mkParseTest "b2(Oa2:5)\n" (Chord Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnA (Just 2) (Just PosRight)) Nothing Nothing ])
  , Test $ mkParseTest "b2,\n" (Chord Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnA (Just 1) Nothing) Nothing Nothing ])
  , Test $ mkParseTest "b2(C)\n" (Chord Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnB Nothing Nothing) Nothing Nothing ])
  , Test $ mkParseTest "b2u\n" (Chord Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnC (Just 1) Nothing) Nothing Nothing ])
  , Test $ mkParseTest "b2<\n" (Chord Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnC (Just 2) Nothing) Nothing Nothing ])
  , Test $ mkParseTest "b2#\n" (Chord Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnE Nothing Nothing) Nothing Nothing ])
  , Test $ mkParseTest "b2x\n" (Chord Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnF Nothing Nothing) Nothing Nothing ])
  , Test $ mkParseTest "b2~\n" (Chord Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnH Nothing Nothing) Nothing Nothing ])
  , Test $ mkParseTest "b2(Oa)a3\n" (Chord Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnA Nothing Nothing) Nothing Nothing
                                                   , Note Three A (Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "b2,a3\n" (Chord Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnA (Just 1) Nothing) Nothing Nothing
                                                , Note Three A (Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "b2(C)a3\n" (Chord Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnB Nothing Nothing) Nothing Nothing
                                                  , Note Three A (Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "b2ua3\n" (Chord Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnC (Just 1) Nothing) Nothing Nothing
                                                , Note Three A (Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "b2<a3\n" (Chord Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnC (Just 2) Nothing) Nothing Nothing
                                                , Note Three A (Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "b2#a3\n" (Chord Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnE Nothing Nothing) Nothing Nothing
                                                , Note Three A (Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "b2xa3\n" (Chord Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnF Nothing Nothing) Nothing Nothing
                                                , Note Three A (Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkParseTest "b2~a3\n" (Chord Nothing [ Note Two B (Nothing, Nothing) (Just $ OrnH Nothing Nothing) Nothing Nothing
                                                , Note Three A (Nothing, Nothing) Nothing Nothing Nothing ])
  , Test $ mkInvalidTest "b2(On)\n"
  , Test $ mkInvalidTest "b2(Oa)(Ob)\n"
  , Test $ mkInvalidTest "b2(Oa),\n"
  , Test $ mkInvalidTest "b2,u\n"
  ]

comments :: [Test]
comments =
  [ Test $ mkParseTest "{foo bar}\n" (Comment "foo bar")
  , Test $ mkParseTest "{foo {bar}\n" (Comment "foo {bar")
  , Test $ mkParseTest "{Qc1a2}" (Comment "Qc1a2")
  , Test $ mkParseTest "{}" (Comment "")
  , Test $ mkParseTest "{\n}" (Comment "\n")
  , Test $ mkParseTest "{foo\nbar}" (Comment "foo\nbar")
  ]
