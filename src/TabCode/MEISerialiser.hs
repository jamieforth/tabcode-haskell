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

import Data.Text (pack, Text(..))
import TabCode
import Text.XML.Generator

mei :: Namespace
mei = namespace "" "http://www.music-encoding.org/ns/mei"

staff :: TabCode -> Xml Elem
staff (TabCode tws) =
  xelemQ mei "staff" $ xelems $ map tabWord tws

tabWord :: TabWord -> Xml Elem
tabWord (Chord (Just rs) ns) =
  xelemQ mei "chord" $ (xattr "dur" (rsMEIDur rs)) <#> ((rhythmSign rs) <> (xelems $ map note ns))

tabWord (Chord Nothing ns) =
  xelemQ mei "chord" $ xelems $ map note ns

tabWord (Rest rs) =
  xelemQ mei "rest" $ (xattr "dur" (rsMEIDur rs)) <#> (rhythmSign rs)

tabWord (BarLine b) =
  xelemQEmpty mei "barLine"

tabWord (Meter ms) =
  xelemQEmpty mei "timeSig"

tabWord (Comment c) =
  xelemQ mei "comment" $ xtext $ pack c

tabWord SystemBreak =
  xelemQEmpty mei "sb"

tabWord PageBreak =
  xelemQEmpty mei "pb"

rhythmSign :: RhythmSign -> Xml Elem
rhythmSign (RhythmSign dur _ dt _) =
  xelemQ mei "rhythmGlyph" $ xattr "symbol" (durSymb dur)

note :: Note -> Xml Elem
note (Note crs frt (Just fngrg) _ _ _) =
  xelemQ mei "note" $ (xattrs [ (xattr "tab.course" (course crs))
                              , (xattr "tab.fret" (fret frt)) ])
                      <#> (fingering fngrg)

note (Note crs frt Nothing _ _ _) =
  xelemQ mei "note" $ xattrs [ (xattr "tab.course" (course crs))
                             , (xattr "tab.fret" (fret frt)) ]

fingering :: Fingering -> Xml Elem
fingering (Fingering (Just hnd) fngr _) =
  xelemQ mei "fingering" $ xattrs [ (xattr "playingHand" (hand hnd))
                                  , (xattr "playingFinger" (finger fngr)) ]

fingering (Fingering Nothing fngr _) =
  xelemQ mei "fingering" $ xattr "playingFinger" (finger fngr)

hand :: Hand -> Text
hand RH = pack "right"
hand LH = pack "left"

finger :: Finger -> Text
finger FingerOne = pack "1"
finger FingerTwo = pack "2"
finger FingerThree = pack "3"
finger FingerFour = pack "4"
finger Thumb = pack "t"

course :: Course -> Text
course One = pack "1"
course Two = pack "2"
course Three = pack "3"
course Four = pack "4"
course Five = pack "5"
course Six = pack "6"
course (Bass n) = pack $ show $ 6 + n

fret :: Fret -> Text
fret A = pack "o"
fret B = pack "1"
fret C = pack "2"
fret D = pack "3"
fret E = pack "4"
fret F = pack "5"
fret G = pack "6"
fret H = pack "7"
fret I = pack "8"
fret J = pack "9"
fret K = pack "10"
fret L = pack "11"
fret M = pack "12"
fret N = pack "13"

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

meiDurRS :: Text -> RhythmSign
meiDurRS "fermata" = RhythmSign Fermata Simple NoDot Nothing
meiDurRS "breve" = RhythmSign Breve Simple NoDot Nothing
meiDurRS "breve." = RhythmSign Breve Simple Dot Nothing
meiDurRS "1" = RhythmSign Semibreve Simple NoDot Nothing
meiDurRS "1." = RhythmSign Semibreve Simple Dot Nothing
meiDurRS "2" = RhythmSign Minim Simple NoDot Nothing
meiDurRS "2." = RhythmSign Minim Simple Dot Nothing
meiDurRS "4" = RhythmSign Crotchet Simple NoDot Nothing
meiDurRS "4." = RhythmSign Crotchet Simple Dot Nothing
meiDurRS "8" = RhythmSign Quaver Simple NoDot Nothing
meiDurRS "8." = RhythmSign Quaver Simple Dot Nothing
meiDurRS "16" = RhythmSign Semiquaver Simple NoDot Nothing
meiDurRS "16." = RhythmSign Semiquaver Simple Dot Nothing
meiDurRS "32" = RhythmSign Demisemiquaver Simple NoDot Nothing
meiDurRS "32." = RhythmSign Demisemiquaver Simple Dot Nothing
meiDurRS "64" = RhythmSign Hemidemisemiquaver Simple NoDot Nothing
meiDurRS "64." = RhythmSign Hemidemisemiquaver Simple Dot Nothing
meiDurRS "128" = RhythmSign Semihemidemisemiquaver Simple NoDot Nothing
meiDurRS "128." = RhythmSign Semihemidemisemiquaver Simple Dot Nothing
