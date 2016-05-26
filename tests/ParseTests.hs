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
  , name = show tc
  , tags = []
  , options = []
  , setOption = \_ _ -> Right $ mkParseTest tc tw
  }

tryParseInvalidWord :: String -> Result
tryParseInvalidWord tc =
  case parseTabcode tc of
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

meterSigns :: [Test]
meterSigns =
  [ Test $ mkParseTest "M(O.)" (Meter (SingleMeterSign PerfectMajor))
  , Test $ mkParseTest "M(O)" (Meter (SingleMeterSign PerfectMinor))
  , Test $ mkParseTest "M(C.)" (Meter (SingleMeterSign ImperfectMajor))
  , Test $ mkParseTest "M(C)" (Meter (SingleMeterSign ImperfectMinor))
  , Test $ mkParseTest "M(O/.)" (Meter (SingleMeterSign HalfPerfectMajor))
  , Test $ mkParseTest "M(O/)" (Meter (SingleMeterSign HalfPerfectMinor))
  , Test $ mkParseTest "M(C/.)" (Meter (SingleMeterSign HalfImperfectMajor))
  , Test $ mkParseTest "M(C/)" (Meter (SingleMeterSign HalfImperfectMinor))
  , Test $ mkParseTest "M(D.)" (Meter (SingleMeterSign HalfImperfectMajor))
  , Test $ mkParseTest "M(D)" (Meter (SingleMeterSign HalfImperfectMinor))
  , Test $ mkParseTest "M(3)" (Meter (SingleMeterSign (Beats 3)))
  , Test $ mkParseTest "M(4:4)" (Meter (VerticalMeterSign (Beats 4) (Beats 4)))
  , Test $ mkParseTest "M(4;4)" (Meter (HorizontalMeterSign (Beats 4) (Beats 4)))
  ]

rests :: [Test]
rests =
  [ Test $ mkParseTest "F" (Rest (RhythmSign Fermata Simple NoDot Nothing))
  , Test $ mkParseTest "B" (Rest (RhythmSign Breve Simple NoDot Nothing))
  , Test $ mkParseTest "W" (Rest (RhythmSign Semibreve Simple NoDot Nothing))
  , Test $ mkParseTest "W." (Rest (RhythmSign Semibreve Simple Dot Nothing))
  , Test $ mkParseTest "W3" (Rest (RhythmSign Semibreve Compound NoDot Nothing))
  , Test $ mkParseTest "H" (Rest (RhythmSign Minim Simple NoDot Nothing))
  , Test $ mkParseTest "H." (Rest (RhythmSign Minim Simple Dot Nothing))
  , Test $ mkParseTest "H3" (Rest (RhythmSign Minim Compound NoDot Nothing))
  , Test $ mkParseTest "Q" (Rest (RhythmSign Crotchet Simple NoDot Nothing))
  , Test $ mkParseTest "Q." (Rest (RhythmSign Crotchet Simple Dot Nothing))
  , Test $ mkParseTest "Q3" (Rest (RhythmSign Crotchet Compound NoDot Nothing))
  , Test $ mkParseTest "E" (Rest (RhythmSign Quaver Simple NoDot Nothing))
  , Test $ mkParseTest "E." (Rest (RhythmSign Quaver Simple Dot Nothing))
  , Test $ mkParseTest "E3" (Rest (RhythmSign Quaver Compound NoDot Nothing))
  , Test $ mkParseTest "S" (Rest (RhythmSign Semiquaver Simple NoDot Nothing))
  , Test $ mkParseTest "S." (Rest (RhythmSign Semiquaver Simple Dot Nothing))
  , Test $ mkParseTest "S3" (Rest (RhythmSign Semiquaver Compound NoDot Nothing))
  , Test $ mkParseTest "T" (Rest (RhythmSign Demisemiquaver Simple NoDot Nothing))
  , Test $ mkParseTest "T." (Rest (RhythmSign Demisemiquaver Simple Dot Nothing))
  , Test $ mkParseTest "T3" (Rest (RhythmSign Demisemiquaver Compound NoDot Nothing))
  , Test $ mkParseTest "Y" (Rest (RhythmSign Hemidemisemiquaver Simple NoDot Nothing))
  , Test $ mkParseTest "Y." (Rest (RhythmSign Hemidemisemiquaver Simple Dot Nothing))
  , Test $ mkParseTest "Y3" (Rest (RhythmSign Hemidemisemiquaver Compound NoDot Nothing))
  , Test $ mkParseTest "Z" (Rest (RhythmSign Semihemidemisemiquaver Simple NoDot Nothing))
  , Test $ mkParseTest "Z." (Rest (RhythmSign Semihemidemisemiquaver Simple Dot Nothing))
  , Test $ mkParseTest "Z3" (Rest (RhythmSign Semihemidemisemiquaver Compound NoDot Nothing))
  ]

barLines :: [Test]
barLines =
  [ Test $ mkParseTest "|" (BarLine (SingleBar Nothing Nothing NotDashed Counting))
  , Test $ mkParseTest "||" (BarLine (DoubleBar Nothing Nothing NotDashed Counting))
  , Test $ mkParseTest ":|" (BarLine (SingleBar (Just RepeatLeft) Nothing NotDashed Counting))
  , Test $ mkParseTest ":||" (BarLine (DoubleBar (Just RepeatLeft) Nothing NotDashed Counting))
  , Test $ mkParseTest "|:" (BarLine (SingleBar (Just RepeatRight) Nothing NotDashed Counting))
  , Test $ mkParseTest "||:" (BarLine (DoubleBar (Just RepeatRight) Nothing NotDashed Counting))
  , Test $ mkParseTest ":|:" (BarLine (SingleBar (Just RepeatBoth) Nothing NotDashed Counting))
  , Test $ mkParseTest ":||:" (BarLine (DoubleBar (Just RepeatBoth) Nothing NotDashed Counting))
  , Test $ mkParseTest "|=" (BarLine (SingleBar Nothing Nothing Dashed Counting))
  , Test $ mkParseTest "||=" (BarLine (DoubleBar Nothing Nothing Dashed Counting))
  , Test $ mkParseTest ":|=" (BarLine (SingleBar (Just RepeatLeft) Nothing Dashed Counting))
  , Test $ mkParseTest ":||=" (BarLine (DoubleBar (Just RepeatLeft) Nothing Dashed Counting))
  , Test $ mkParseTest "|:=" (BarLine (SingleBar (Just RepeatRight) Nothing Dashed Counting))
  , Test $ mkParseTest "||:=" (BarLine (DoubleBar (Just RepeatRight) Nothing Dashed Counting))
  , Test $ mkParseTest ":|:=" (BarLine (SingleBar (Just RepeatBoth) Nothing Dashed Counting))
  , Test $ mkParseTest ":||:=" (BarLine (DoubleBar (Just RepeatBoth) Nothing Dashed Counting))
  , Test $ mkParseTest "|0" (BarLine (SingleBar Nothing Nothing NotDashed NotCounting))
  , Test $ mkParseTest "||0" (BarLine (DoubleBar Nothing Nothing NotDashed NotCounting))
  , Test $ mkParseTest ":|0" (BarLine (SingleBar (Just RepeatLeft) Nothing NotDashed NotCounting))
  , Test $ mkParseTest ":||0" (BarLine (DoubleBar (Just RepeatLeft) Nothing NotDashed NotCounting))
  , Test $ mkParseTest "|:0" (BarLine (SingleBar (Just RepeatRight) Nothing NotDashed NotCounting))
  , Test $ mkParseTest "||:0" (BarLine (DoubleBar (Just RepeatRight) Nothing NotDashed NotCounting))
  , Test $ mkParseTest ":|:0" (BarLine (SingleBar (Just RepeatBoth) Nothing NotDashed NotCounting))
  , Test $ mkParseTest ":||:0" (BarLine (DoubleBar (Just RepeatBoth) Nothing NotDashed NotCounting))
  , Test $ mkParseTest "|(T=:\\R)" (BarLine (SingleBar Nothing (Just Reprise) NotDashed Counting))
  , Test $ mkParseTest "||(T=:\\R)" (BarLine (DoubleBar Nothing (Just Reprise) NotDashed Counting))
  , Test $ mkParseTest "|(T+:\\1)" (BarLine (SingleBar Nothing (Just $ NthTime 1) NotDashed Counting))
  , Test $ mkParseTest "||(T+:\\1)" (BarLine (DoubleBar Nothing (Just $ NthTime 1) NotDashed Counting))
  ]
