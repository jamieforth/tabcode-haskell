-- TabCode - A parser for the Tabcode lute tablature language
--
-- Copyright (C) 2015 Richard Lewis, Goldsmiths' College
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

{-# LANGUAGE OverloadedStrings #-}

module TabCode.MEISerialiser (staff) where

import Data.Maybe (catMaybes)
import Data.Text (pack, Text(..))
import TabCode
import Text.XML.Generator

mei :: Namespace
mei = namespace "" "http://www.music-encoding.org/ns/mei"

staff :: TabCode -> Xml Elem
staff (TabCode rls tws) =
  xelemQ mei "staff" $ xelems $ map (tabWord rls) tws

tabWord :: [Rule] -> TabWord -> Xml Elem
tabWord rls (Chord (Just rs) ns) =
  xelemQ mei "chord" $ (durAttr rs) <#> ((rhythmSign rs) <> (xelems $ map (note rls) ns))

tabWord rls (Chord Nothing ns) =
  xelemQ mei "chord" $ xelems $ map (note rls) ns

tabWord rls (Rest rs) =
  xelemQ mei "rest" $ (durAttr rs) <#> (rhythmSign rs)

tabWord rls (BarLine b) =
  xelemQEmpty mei "barLine"

tabWord rls (Meter ms) =
  xelemQEmpty mei "timeSig"

tabWord rls (Comment c) =
  xelemQ mei "comment" $ xtext $ pack c

tabWord rls SystemBreak =
  xelemQEmpty mei "sb"

tabWord rls PageBreak =
  xelemQEmpty mei "pb"

rhythmSign :: RhythmSign -> Xml Elem
rhythmSign (RhythmSign dur _ dt _) =
  xelemQ mei "rhythmGlyph" $ durSymbAttr dur

note :: [Rule] -> Note -> Xml Elem
note rls (Note crs frt fngrg orn artic conn) =
  xelemQ mei "note" $ xattrs [ (tabCourseAttr crs)
                             , (tabFretAttr frt) ]
                    <#> (xelems $ catMaybes [ (fretGlyph rls frt)
                                            , (fingering <$> fngrg)
                                            , (ornament <$> orn)
                                            , (articulation <$> artic)
                                            , (connectingLine <$> conn) ])

fretGlyph :: [Rule] -> Fret -> Maybe (Xml Elem)
fretGlyph rls frt = el <$> glyph
  where
    el g  = xelemQ mei "fretGlyph" $ xtext g
    glyph = case notation rls of
      Just "italian" -> Just $ fretGlyphIt frt
      Just "french"  -> Just $ fretGlyphFr frt
      otherwise      -> Nothing

fingering :: Fingering -> Xml Elem
fingering (Fingering (Just hnd) fngr _) =
  xelemQ mei "fingering" $ xattrs [ (playingHandAttr hnd)
                                  , (playingFingerAttr fngr) ]

fingering (Fingering Nothing fngr _) =
  xelemQ mei "fingering" $ xattr "playingFinger" (finger fngr)

playingHandAttr :: Hand -> Xml Attr
playingHandAttr hnd = xattr "playingHand" (hand hnd)

hand :: Hand -> Text
hand RH = pack "right"
hand LH = pack "left"

playingFingerAttr :: Finger -> Xml Attr
playingFingerAttr fngr = xattr "playingFinger" (finger fngr)

tc :: Namespace
tc = namespace "tc" "http://data.t-mus.org/ns/tabcode"

ornament :: Ornament -> Xml Elem
ornament _ = xelemQEmpty tc "ornament"

articulation :: Articulation -> Xml Elem
articulation _ = xelemQEmpty mei "artic"

connectingLine :: Connecting -> Xml Elem
connectingLine _ = xelemQEmpty tc "connectingLine"

finger :: Finger -> Text
finger FingerOne = pack "1"
finger FingerTwo = pack "2"
finger FingerThree = pack "3"
finger FingerFour = pack "4"
finger Thumb = pack "t"

tabCourseAttr :: Course -> Xml Attr
tabCourseAttr crs = xattr "tab.course" (course crs)

course :: Course -> Text
course One = pack "1"
course Two = pack "2"
course Three = pack "3"
course Four = pack "4"
course Five = pack "5"
course Six = pack "6"
course (Bass n) = pack $ show $ 6 + n

tabFretAttr :: Fret -> Xml Attr
tabFretAttr frt = xattr "tab.fret" (fretNo frt)

fretNo :: Fret -> Text
fretNo A = pack "o"
fretNo B = pack "1"
fretNo C = pack "2"
fretNo D = pack "3"
fretNo E = pack "4"
fretNo F = pack "5"
fretNo G = pack "6"
fretNo H = pack "7"
fretNo I = pack "8"
fretNo J = pack "9"
fretNo K = pack "10"
fretNo L = pack "11"
fretNo M = pack "12"
fretNo N = pack "13"

fretGlyphIt :: Fret -> Text
fretGlyphIt = fretNo

fretGlyphFr :: Fret -> Text
fretGlyphFr A = pack "a"
fretGlyphFr B = pack "b"
fretGlyphFr C = pack "c"
fretGlyphFr D = pack "d"
fretGlyphFr E = pack "e"
fretGlyphFr F = pack "f"
fretGlyphFr G = pack "g"
fretGlyphFr H = pack "h"
fretGlyphFr I = pack "i"
fretGlyphFr J = pack "j"
fretGlyphFr K = pack "k"
fretGlyphFr L = pack "l"
fretGlyphFr M = pack "m"
fretGlyphFr N = pack "n"

durSymbAttr :: Duration -> Xml Attr
durSymbAttr dur = xattr "symbol" (durSymb dur)

durSymb :: Duration -> Text
durSymb Fermata = pack "F"
durSymb Breve = pack "B"
durSymb Semibreve = pack "W"
durSymb Minim = pack "H"
durSymb Crotchet = pack "Q"
durSymb Quaver = pack "E"
durSymb Semiquaver = pack "S"
durSymb Demisemiquaver = pack "T"
durSymb Hemidemisemiquaver = pack "Y"
durSymb Semihemidemisemiquaver = pack "Z"

durAttr :: RhythmSign -> Xml Attr
durAttr rs = xattr "dur" (rsMEIDur rs)

rsMEIDur :: RhythmSign -> Text
rsMEIDur (RhythmSign Fermata _ dot _) =
  if dot == Dot then (error "Dotted fermata not allowed.") else pack "fermata"
rsMEIDur (RhythmSign Breve _ dot _) =
  if dot == Dot then pack "breve." else pack "breve"
rsMEIDur (RhythmSign Semibreve _ dot _) =
  if dot == Dot then pack "1." else pack "1"
rsMEIDur (RhythmSign Minim _ dot _) =
  if dot == Dot then pack "2." else pack "2"
rsMEIDur (RhythmSign Crotchet _ dot _) =
  if dot == Dot then pack "4." else pack "4"
rsMEIDur (RhythmSign Quaver _ dot _) =
  if dot == Dot then pack "8." else pack "8"
rsMEIDur (RhythmSign Semiquaver _ dot _) =
  if dot == Dot then pack "16." else pack "16"
rsMEIDur (RhythmSign Demisemiquaver _ dot _) =
  if dot == Dot then pack "32." else pack "32"
rsMEIDur (RhythmSign Hemidemisemiquaver _ dot _) =
  if dot == Dot then pack "64." else pack "64"
rsMEIDur (RhythmSign Semihemidemisemiquaver _ dot _) =
  if dot == Dot then pack "128." else pack "128"
