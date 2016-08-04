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

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TabCode.MEI
  ( TabWordsToMEI
  , mei
  , defaultDoc
  , module TabCode.MEI.Elements
  , module TabCode.MEI.Types
  , (<>) ) where

import           Data.Monoid                   ((<>))
import           Data.Text                     (pack)
import qualified Data.Vector                   as V
import           Data.Vector                   (Vector)
import           TabCode
import           TabCode.MEI.Elements
import           TabCode.MEI.Types
import           Text.Parsec
import           Text.Parsec.Combinator

instance (Monad m) => Stream (Vector a) m a where
  uncons v | V.null v  = return Nothing
           | otherwise = return $ Just (V.unsafeHead v, V.unsafeTail v)

type TabWordsToMEI = Parsec (Vector TabWord) ([Rule], MEIAttrs) MEI

defaultDoc :: [MEI] -> MEI
defaultDoc staves = MEIMusic noMEIAttrs [body]
  where
    body    = MEIBody    noMEIAttrs [mdiv]
    mdiv    = MEIMDiv    noMEIAttrs [parts]
    parts   = MEIParts   noMEIAttrs [part]
    part    = MEIPart    noMEIAttrs [section]
    section = MEISection noMEIAttrs staves

mei :: ([MEI] -> MEI) -> String -> TabCode -> Either ParseError MEI
mei doc source (TabCode rls tws) = runParser (containers doc) (rls, noMEIAttrs) source tws

containers :: ([MEI] -> MEI) -> TabWordsToMEI
containers doc = do
  s <- staff <|> justChords
  return $ doc $ [s]

staff :: TabWordsToMEI
staff = do
  staffDef <- meter
  ms       <- many anyMeasure
  trailing <- many $ tuple <|> chord <|> rest <|> systemBreak <|> pageBreak <|> comment <|> invalid
  eof
  return $ MEIStaff noMEIAttrs $ staffDef : ms ++ trailing

justChords :: TabWordsToMEI
justChords = do
  ms       <- many anyMeasure
  trailing <- many $ tuple <|> chord <|> rest <|> systemBreak <|> pageBreak <|> comment <|> invalid
  eof
  return $ MEIStaff noMEIAttrs $ ms ++ trailing

measureP :: TabWordsToMEI -> MEIAttrs -> TabWordsToMEI
measureP barlineP attrs = do
  chords <- many1 $ tuple <|> chord <|> rest <|> systemBreak <|> pageBreak <|> comment <|> invalid
  barlineP
  return $ MEIMeasure attrs chords

measureSng    = measureP barLineSng ( atRight "single" )
measureDbl    = measureP barLineDbl ( atRight "double" )
measureRptEnd = measureP barLineRptL ( atRight "rptend" )
measureRptStr = measureP barLineRptR ( atRight "rptstart" )
measureRptBth = measureP barLineRptB ( atRight "rptboth" )

anyMeasure = (try measureSng) <|> (try measureDbl) <|> (try measureRptEnd) <|> (try measureRptStr) <|> (try measureRptBth)

barLineSng :: TabWordsToMEI
barLineSng = tokenPrim show updatePos getBarLine
  where
    getBarLine bl@(BarLine l c (SingleBar Nothing Nothing NotDashed _)) = Just $ MEIBarLine noMEIAttrs []
    getBarLine _ = Nothing

barLineDbl :: TabWordsToMEI
barLineDbl = tokenPrim show updatePos getBarLine
  where
    getBarLine bl@(BarLine l c (DoubleBar Nothing Nothing NotDashed _)) = Just $ MEIBarLine noMEIAttrs []
    getBarLine _ = Nothing

barLineRptL :: TabWordsToMEI
barLineRptL = tokenPrim show updatePos getBarLine
  where
    getBarLine bl@(BarLine l c (SingleBar (Just RepeatLeft) Nothing NotDashed _)) = Just $ MEIBarLine noMEIAttrs []
    getBarLine bl@(BarLine l c (DoubleBar (Just RepeatLeft) Nothing NotDashed _)) = Just $ MEIBarLine noMEIAttrs []
    getBarLine _ = Nothing

barLineRptR :: TabWordsToMEI
barLineRptR = tokenPrim show updatePos getBarLine
  where
    getBarLine bl@(BarLine l c (SingleBar (Just RepeatRight) Nothing NotDashed _)) = Just $ MEIBarLine noMEIAttrs []
    getBarLine bl@(BarLine l c (DoubleBar (Just RepeatRight) Nothing NotDashed _)) = Just $ MEIBarLine noMEIAttrs []
    getBarLine _ = Nothing

barLineRptB :: TabWordsToMEI
barLineRptB = tokenPrim show updatePos getBarLine
  where
    getBarLine bl@(BarLine l c (SingleBar (Just RepeatBoth) Nothing NotDashed _)) = Just $ MEIBarLine noMEIAttrs []
    getBarLine bl@(BarLine l c (DoubleBar (Just RepeatBoth) Nothing NotDashed _)) = Just $ MEIBarLine noMEIAttrs []
    getBarLine _ = Nothing

tuple :: TabWordsToMEI
tuple = do
  c  <- many1 chordCompound
  cs <- many chordNoRS
  return $ MEITuple ( atNum 3 <> atNumbase 2 ) $ c ++ cs

chordLike :: ([Rule] -> MEIAttrs -> TabWord -> Maybe MEI) -> TabWordsToMEI
chordLike getChord = do
  (rules, cDur) <- getState
  c <- tokenPrim show updatePos (getChord rules cDur)
  putState (rules, durOf c)
  return c

  where
    durOf (MEIChord attrs _) = someAttrs [pack "dur", pack "dots"] attrs
    durOf _                  = noMEIAttrs

chord :: TabWordsToMEI
chord = chordLike getChord
  where
    getChord rls dur ch@(Chord l c r ns) =
      Just $ MEIChord ( updateAttrs dur (atDur <$:> r) ) $ ( elRhythmSign <$:> r ) <> ( concat $ (elNote rls) <$> ns )
    getChord _ _ _ = Nothing

chordCompound :: TabWordsToMEI
chordCompound = chordLike getChord
  where
    getChord rls dur ch@(Chord l c r@(Just (RhythmSign _ Compound _ _)) ns) =
      Just $ MEIChord ( updateAttrs dur (atDur <$:> r) ) $ ( elRhythmSign <$:> r ) <> ( concat $ (elNote rls) <$> ns )
    getChord _ _ _ = Nothing

chordNoRS :: TabWordsToMEI
chordNoRS = chordLike getChord
  where
    getChord rls dur ch@(Chord l c Nothing ns) =
      Just $ MEIChord dur $ ( concat $ (elNote rls) <$> ns )
    getChord _ _ _ = Nothing

rest :: TabWordsToMEI
rest = tokenPrim show updatePos getRest
  where
    getRest re@(Rest l c (RhythmSign Fermata _ _ _)) =
      Just $ MEIFermata noMEIAttrs []
    getRest re@(Rest l c r) =
      Just $ MEIRest ( atDur r ) $ ( elRhythmSign r )
    getRest _ = Nothing

meter :: TabWordsToMEI
meter = tokenPrim show updatePos getMeter
  where
    getMeter me@(Meter l c m) = case m of
      (SingleMeterSign PerfectMajor)
        -> Just $ MEIStaffDef ( atProlation 3 <> atTempus 3 ) [ MEIMensur ( atSign 'O' <> atDot True ) [] ]
      (SingleMeterSign PerfectMinor)
        -> Just $ MEIStaffDef ( atProlation 3 <> atTempus 2 ) [ MEIMensur ( atSign 'O' <> atDot False ) [] ]
      (SingleMeterSign ImperfectMajor)
        -> Just $ MEIStaffDef ( atProlation 2 <> atTempus 3 ) [ MEIMensur ( atSign 'C' <> atDot True ) [] ]
      (SingleMeterSign ImperfectMinor)
        -> Just $ MEIStaffDef ( atProlation 2 <> atTempus 2 ) [ MEIMensur ( atSign 'C' <> atDot False ) [] ]
      (SingleMeterSign HalfPerfectMajor)
        -> Just $ MEIStaffDef ( atProlation 3 <> atTempus 3 <> atSlash 1 ) [ MEIMensur ( atSign 'O' <> atDot True <> atCut 1 ) [] ]
      (SingleMeterSign HalfPerfectMinor)
        -> Just $ MEIStaffDef ( atProlation 3 <> atTempus 2 <> atSlash 1 ) [ MEIMensur ( atSign 'O' <> atDot False <> atCut 1 ) [] ]
      (SingleMeterSign HalfImperfectMajor)
        -> Just $ MEIStaffDef ( atProlation 2 <> atTempus 3 <> atSlash 1 ) [ MEIMensur ( atSign 'C' <> atDot True <> atCut 1 ) [] ]
      (SingleMeterSign HalfImperfectMinor)
        -> Just $ MEIStaffDef ( atProlation 2 <> atTempus 2 <> atSlash 1 ) [ MEIMensur ( atSign 'C' <> atDot False <> atCut 1 ) [] ]
      (VerticalMeterSign (Beats n) (Beats b))
        -> Just $ MEIStaffDef ( atNumDef n <> atNumbaseDef b ) [ MEIMeterSig ( atCount n <> atUnit b ) [] ]
      (SingleMeterSign (Beats 3))
        -> Just $ MEIStaffDef ( atTempus 3 ) []
      _
        -> Just $ XMLComment $ pack $ " tc2mei: Un-implemented mensuration sign: " ++ (show m) ++ " "

    getMeter _ = Nothing

systemBreak :: TabWordsToMEI
systemBreak = tokenPrim show updatePos getSystemBreak
  where
    getSystemBreak re@(SystemBreak l c) = Just $ MEISystemBreak noMEIAttrs []
    getSystemBreak _                    = Nothing

pageBreak :: TabWordsToMEI
pageBreak = tokenPrim show updatePos getPageBreak
  where
    getPageBreak re@(PageBreak l c) = Just $ MEIPageBreak noMEIAttrs []
    getPageBreak _                  = Nothing

comment :: TabWordsToMEI
comment = tokenPrim show updatePos getComment
  where
    getComment re@(Comment l c cmt) = Just $ XMLComment $ pack cmt
    getComment _                    = Nothing

invalid :: TabWordsToMEI
invalid = tokenPrim show updatePos getInvalid
  where
    getInvalid re@(Invalid src l c word) =
      Just $ XMLComment $ pack $ " tc2mei: Invalid tabword in source '" ++ src ++ "' (line: " ++ (show l) ++ "; col: " ++ (show c) ++ "): \"" ++ word ++ "\" "
    getInvalid _ =
      Nothing

updatePos :: SourcePos -> TabWord -> Vector TabWord -> SourcePos
updatePos pos _ v
  | V.null v  = pos
  | otherwise = setSourceLine (setSourceColumn pos (twColumn tok)) (twLine tok)
  where
    tok = V.head v
