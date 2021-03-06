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

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TabCode.Serialiser.MEIXML.Converter
  ( TabWordsToMEI
  , mei
  , defaultDoc
  , module TabCode.Serialiser.MEIXML.Elements
  , module TabCode.Serialiser.MEIXML.Types
  , (<>) ) where

import Control.Applicative ((<$>))
import Data.Monoid ((<>))
import Data.Text (pack, append)
import qualified Data.Vector as V
import Data.Vector (Vector)
import TabCode.Types
import TabCode.Options (Structure(..))
import TabCode.Serialiser.MEIXML.Elements
import TabCode.Serialiser.MEIXML.Types
import Text.Parsec

newtype WrapVector a = WrapVector { unWrapVector :: Vector a }

instance (Monad m) => Stream (WrapVector a) m a where
  uncons (WrapVector { unWrapVector = v })
    | V.null v = return Nothing
    | otherwise = return $ Just (V.unsafeHead v, WrapVector { unWrapVector = V.unsafeTail v})

type TabWordsToMEI = Parsec (WrapVector TabWord) MEIState MEI

defaultDoc :: MEIState -> [MEI] -> MEI
defaultDoc st staves = MEI ( atMeiVersion ) [header, music]
  where
    header = meiHead $ stRules st
    music = MEIMusic noMEIAttrs [body]
    body = MEIBody noMEIAttrs [mdiv]
    mdiv = MEIMDiv (stMdiv st) [parts]
    parts = MEIParts noMEIAttrs [part]
    part = MEIPart (stPart st) [section]
    section = MEISection (stSection st) staves

mei :: Structure -> (MEIState -> [MEI] -> MEI) -> String -> TabCode -> Either ParseError MEI
mei BarLines doc source (TabCode rls tws) = runParser (withBarLines doc) (initialState { stRules = rls }) source (WrapVector { unWrapVector = tws })
mei Measures doc source (TabCode rls tws) = runParser (withMeasures doc) (initialState { stRules = rls }) source (WrapVector { unWrapVector = tws })

meiHead :: [Rule] -> MEI
meiHead rls =
  MEIHead noMEIAttrs $ ( elFileDesc noMEIAttrs <> elWorkDesc noMEIAttrs rls )

withMeasures :: (MEIState -> [MEI] -> MEI) -> TabWordsToMEI
withMeasures doc = do
  ms <- many1 $ anyMeasure
  trailing <- many $ tuplet <|> chord <|> rest <|> meter <|> systemBreak <|> pageBreak <|> comment <|> invalid
  eof
  st <- getState
  return $ doc st ( ms ++ trailing )

staffIDAsDef :: MEIAttrs -> MEIAttrs
staffIDAsDef staffAttrs =
  mutateAttr (pack "xml:id") ((updateStrAttrValue (\s -> append (pack "#") s)) . intAttrToStrAttr . renameAttr (pack "def")) staffAttrs

withBarLines :: (MEIState -> [MEI] -> MEI) -> TabWordsToMEI
withBarLines doc = do
  cs <- many1 $ tuplet <|> chord <|> rest <|> barLine <|> meter <|> systemBreak <|> pageBreak <|> comment <|> invalid
  eof
  st <- getState
  return $ doc st [ MEIStaff ( stStaff st <> staffIDAsDef (stStaffDef st)) [ MEILayer (stLayer st) cs ] ]

measureP :: TabWordsToMEI -> MEIAttrs -> TabWordsToMEI
measureP barlineP _ = do
  chords <- many $ tuplet <|> chord <|> rest <|> meter <|> systemBreak <|> pageBreak <|> comment <|> invalid
  bl <- barlineP
  st <- getState
  let nextSt = st { stMeasure = mutateAttr (pack "n") (incIntAttr 1) (stMeasure st)
                  , stMeasureId = mutateAttr (pack "xml:id") (incIntAttr 1) (stMeasureId st)
                  }
      measureAttrs = stMeasureId nextSt <> stMeasure nextSt
      measureChildren = [ MEIStaff (stStaff nextSt <> staffIDAsDef (stStaffDef nextSt)) [ MEILayer (stLayer nextSt) chords ] ] ++ getChildren bl
  putState nextSt

  return $ MEIMeasure measureAttrs measureChildren

measureSng :: TabWordsToMEI
measureSng = measureP barLineSng ( atRight "single" )

measureDbl :: TabWordsToMEI
measureDbl = measureP barLineDbl ( atRight "double" )

measureRptEnd :: TabWordsToMEI
measureRptEnd = measureP barLineRptL ( atRight "rptend" )

measureRptStr :: TabWordsToMEI
measureRptStr = measureP barLineRptR ( atRight "rptstart" )

measureRptBth :: TabWordsToMEI
measureRptBth = measureP barLineRptB ( atRight "rptboth" )

anyMeasure :: TabWordsToMEI
anyMeasure = (try measureSng) <|> (try measureDbl) <|> (try measureRptEnd) <|> (try measureRptStr) <|> (try measureRptBth)

barLineSng :: TabWordsToMEI
barLineSng = tokenPrim show updatePos getBarLine
  where
    getBarLine (BarLine _ _ (SingleBar Nothing Nothing NotDashed _) cmt) = Just $ MEIBarLine ( atForm "single" ) (attachedComment cmt)
    getBarLine _ = Nothing

barLineDbl :: TabWordsToMEI
barLineDbl = tokenPrim show updatePos getBarLine
  where
    getBarLine (BarLine _ _ (DoubleBar Nothing Nothing NotDashed _) cmt) = Just $ MEIBarLine ( atForm "double" ) (attachedComment cmt)
    getBarLine _ = Nothing

barLineRptL :: TabWordsToMEI
barLineRptL = tokenPrim show updatePos getBarLine
  where
    getBarLine (BarLine _ _ (SingleBar (Just RepeatLeft) Nothing NotDashed _) cmt) = Just $ MEIBarLine ( atForm "single-rptend" ) (attachedComment cmt)
    getBarLine (BarLine _ _ (DoubleBar (Just RepeatLeft) Nothing NotDashed _) cmt) = Just $ MEIBarLine ( atForm "double-rptend" ) (attachedComment cmt)
    getBarLine _ = Nothing

barLineRptR :: TabWordsToMEI
barLineRptR = tokenPrim show updatePos getBarLine
  where
    getBarLine (BarLine _ _ (SingleBar (Just RepeatRight) Nothing NotDashed _) cmt) = Just $ MEIBarLine ( atForm "single-rptstart" ) (attachedComment cmt)
    getBarLine (BarLine _ _ (DoubleBar (Just RepeatRight) Nothing NotDashed _) cmt) = Just $ MEIBarLine ( atForm "double-rptstart" ) (attachedComment cmt)
    getBarLine _ = Nothing

barLineRptB :: TabWordsToMEI
barLineRptB = tokenPrim show updatePos getBarLine
  where
    getBarLine (BarLine _ _ (SingleBar (Just RepeatBoth) Nothing NotDashed _) cmt) = Just $ MEIBarLine ( atForm "single-rptboth" ) (attachedComment cmt)
    getBarLine (BarLine _ _ (DoubleBar (Just RepeatBoth) Nothing NotDashed _) cmt) = Just $ MEIBarLine ( atForm "double-rptboth" ) (attachedComment cmt)
    getBarLine _ = Nothing

barLine :: TabWordsToMEI
barLine = do
  bl <- barLineSng <|> barLineDbl <|> barLineRptL <|> barLineRptR <|> barLineRptB
  st <- getState
  let blWithN = MEIBarLine blAttrs (getChildren bl)
      blAttrs = updateAttrs (updateAttrs (getAttrs bl) (stBarLine nextSt)) (stBarLineId nextSt)
      nextSt  = st { stBarLine = mutateAttr (pack "n") (incIntAttr 1) (stBarLine st)
                   , stBarLineId = mutateAttr (pack "xml:id") (incIntAttr 1) (stBarLineId st)
                   }
  putState nextSt
  return $ blWithN

tuplet :: TabWordsToMEI
tuplet = do
  c <- many1 chordCompound
  cs <- many chordNoRS
  return $ MEITuplet ( atNum 3 <> atNumbase 2 ) $ c ++ cs

chordLike :: (MEIState -> TabWord -> Maybe (MEIState, MEI)) -> TabWordsToMEI
chordLike getChord = do
  st <- getState
  (newState, c) <- tokenPrim show updatePos $ getChord st
  putState $ newState { stChordId = atXmlIdNext $ stChordId newState,
                        stChord = durOf c }
  return c

  where
    durOf (MEIChord attributes _) = someAttrs [pack "dur", pack "dots"] attributes
    durOf _ = noMEIAttrs

chord :: TabWordsToMEI
chord = chordLike getChord
  where
    getChord st (Chord _ _ (Just r) ns cmt) = Just (newState, meiChord)
      where
        newState = st { stRhythmGlyphId = atXmlIdNext $ stRhythmGlyphId st
                      , stNoteId = mutateAttr (pack "xml:id") (incIntAttr $ length ns) (stNoteId st)
                      }
        meiChord = MEIChord attributes ( rhythmSign <> notes <> (attachedComment cmt) )
        attributes = (stChordId st) <> replaceAttrs (stChord st) (atDur r)
        rhythmSign = elRhythmSign (stRhythmGlyphId st) r
        notes = concat $ (\(note, idx) -> (elNote (atXmlId "n" idx) (stRules st) note)) <$> notesWithIds
        notesWithIds = zip ns [(xmlIdNumber $ (stNoteId st))..]

    getChord st (Chord _ _ Nothing ns cmt) = Just (newState, meiChord)
      where
        newState = st { stNoteId = mutateAttr (pack "xml:id") (incIntAttr $ length ns) (stNoteId st) }
        meiChord = MEIChord attributes ( notes <> (attachedComment cmt) )
        attributes = (stChordId st) <> (stChord st)
        notes = concat $ (\(note, idx) -> (elNote (atXmlId "n" idx) (stRules st) note)) <$> notesWithIds
        notesWithIds = zip ns [(xmlIdNumber $ (stNoteId st))..]

    getChord _ _ = Nothing

chordCompound :: TabWordsToMEI
chordCompound = chordLike getChord
  where
    getChord st (Chord _ _ (Just r@(RhythmSign _ Compound _ _)) ns cmt) = Just (newState, meiChord)
      where
        newState = st { stRhythmGlyphId = atXmlIdNext $ stRhythmGlyphId st
                      , stNoteId = mutateAttr (pack "xml:id") (incIntAttr $ length ns) (stNoteId st)
                      }
        meiChord = MEIChord attributes ( rhythmSign <> notes <> (attachedComment cmt) )
        attributes = (stChordId st) <> replaceAttrs (stChord st) (atDur r)
        rhythmSign = elRhythmSign (stRhythmGlyphId st) r
        notes = concat $ (\(note, idx) -> (elNote (atXmlId "n" idx) (stRules st) note)) <$> notesWithIds
        notesWithIds = zip ns [(xmlIdNumber $ (stNoteId st))..]

    getChord _ _ = Nothing

chordNoRS :: TabWordsToMEI
chordNoRS = chordLike getChord
  where
    getChord st (Chord _ _ Nothing ns cmt) = Just (newState, meiChord)
      where
        newState = st { stNoteId = mutateAttr (pack "xml:id") (incIntAttr $ length ns) (stNoteId st) }
        meiChord = MEIChord attributes ( notes <> (attachedComment cmt) )
        attributes = (stChordId st) <> (stChord st)
        notes = concat $ (\(note, idx) -> (elNote (atXmlId "n" idx) (stRules st) note)) <$> notesWithIds
        notesWithIds = zip ns [(xmlIdNumber $ (stNoteId st))..]

    getChord _ _ = Nothing

rest :: TabWordsToMEI
rest = do
  st <- getState
  (newState, r) <- tokenPrim show updatePos $ getRest st
  putState newState
  return r

  where
    getRest st (Rest _ _ (RhythmSign Fermata _ _ _) cmt) = Just (newState, meiFermata)
      where
        newState = st { stRestId = atXmlIdNext $ stRestId st }
        meiFermata = MEIFermata (stRestId st) (attachedComment cmt)

    getRest st (Rest _ _ r cmt) = Just (newState, meiRest)
      where
        newState = st { stRhythmGlyphId = atXmlIdNext $ stRhythmGlyphId st
                      , stRestId = atXmlIdNext $ stRestId st }
        meiRest = MEIRest ( (stRestId st) <> atDur r ) $ ( (elRhythmSign (stRhythmGlyphId st) r) <> (attachedComment cmt) )

    getRest _ _ = Nothing

meter :: TabWordsToMEI
meter = do
  st <- getState
  let newSt = st { stStaffDef = mutateAttr (pack "xml:id") (incIntAttr 1) (stStaffDef st) }
  m  <- tokenPrim show updatePos (getMeter $ stStaffDef newSt)
  putState $ newSt
  return m
  where
    getMeter atts (Meter _ _ m cmt) = case m of
      (SingleMeterSign PerfectMajor)
        -> Just $ MEIStaffDef ( atts <> atProlation 3 <> atTempus 3 ) [ MEIMensur ( atSign 'O' <> atDot True ) (attachedComment cmt) ]
      (SingleMeterSign PerfectMinor)
        -> Just $ MEIStaffDef ( atts <> atProlation 3 <> atTempus 2 ) [ MEIMensur ( atSign 'O' <> atDot False ) (attachedComment cmt) ]
      (SingleMeterSign ImperfectMajor)
        -> Just $ MEIStaffDef ( atts <> atProlation 2 <> atTempus 3 ) [ MEIMensur ( atSign 'C' <> atDot True ) (attachedComment cmt) ]
      (SingleMeterSign ImperfectMinor)
        -> Just $ MEIStaffDef ( atts <> atProlation 2 <> atTempus 2 ) [ MEIMensur ( atSign 'C' <> atDot False ) (attachedComment cmt) ]
      (SingleMeterSign HalfPerfectMajor)
        -> Just $ MEIStaffDef ( atts <> atProlation 3 <> atTempus 3 <> atSlash 1 ) [ MEIMensur ( atSign 'O' <> atDot True <> atCut 1 ) (attachedComment cmt) ]
      (SingleMeterSign HalfPerfectMinor)
        -> Just $ MEIStaffDef ( atts <> atProlation 3 <> atTempus 2 <> atSlash 1 ) [ MEIMensur ( atSign 'O' <> atDot False <> atCut 1 ) (attachedComment cmt) ]
      (SingleMeterSign HalfImperfectMajor)
        -> Just $ MEIStaffDef ( atts <> atProlation 2 <> atTempus 3 <> atSlash 1 ) [ MEIMensur ( atSign 'C' <> atDot True <> atCut 1 ) (attachedComment cmt) ]
      (SingleMeterSign HalfImperfectMinor)
        -> Just $ MEIStaffDef ( atts <> atProlation 2 <> atTempus 2 <> atSlash 1 ) [ MEIMensur ( atSign 'C' <> atDot False <> atCut 1 ) (attachedComment cmt) ]
      (VerticalMeterSign (Beats n) (Beats b))
        -> Just $ MEIStaffDef ( atts <> atNumDef n <> atNumbaseDef b ) [ MEIMeterSig ( atCount n <> atUnit b ) (attachedComment cmt) ]
      (HorizontalMeterSign (Beats n) (Beats b))
        -> Just $ MEIStaffDef ( atts <> atNumDef n <> atNumbaseDef b ) [ MEIMeterSig ( atCount n <> atUnit b ) (attachedComment cmt) ]
      (SingleMeterSign (Beats 3))
        -> Just $ MEIStaffDef ( atts <> atTempus 3 ) (attachedComment cmt)
      _
        -> Just $ XMLComment $ pack $ " tc2mei: Un-implemented mensuration sign: " ++ (show m) ++ " "

    getMeter _ _ = Nothing

systemBreak :: TabWordsToMEI
systemBreak = tokenPrim show updatePos getSystemBreak
  where
    getSystemBreak (SystemBreak _ _ cmt) = Just $ MEISystemBreak noMEIAttrs (attachedComment cmt)
    getSystemBreak _ = Nothing

pageBreak :: TabWordsToMEI
pageBreak = tokenPrim show updatePos getPageBreak
  where
    getPageBreak (PageBreak _ _ cmt) = Just $ MEIPageBreak noMEIAttrs (attachedComment cmt)
    getPageBreak _ = Nothing

comment :: TabWordsToMEI
comment = tokenPrim show updatePos getComment
  where
    getComment (CommentWord _ _ (Comment cmt)) = Just $ XMLComment $ pack cmt
    getComment _ = Nothing

attachedComment :: Maybe Comment -> [MEI]
attachedComment (Just (Comment cmt)) = [ XMLComment $ pack cmt ]
attachedComment Nothing = []

invalid :: TabWordsToMEI
invalid = tokenPrim show updatePos getInvalid
  where
    getInvalid (Invalid src l c word) =
      Just $ XMLComment $ pack $ " tc2mei: Invalid tabword in source '" ++ src ++ "' (line: " ++ (show l) ++ "; col: " ++ (show c) ++ "): \"" ++ word ++ "\" "
    getInvalid _ =
      Nothing

updatePos :: SourcePos -> TabWord -> WrapVector TabWord -> SourcePos
updatePos pos _ (WrapVector { unWrapVector = v })
  | V.null v = pos
  | otherwise = setSourceLine (setSourceColumn pos (twColumn tok)) (twLine tok)
  where
    tok = V.head v
