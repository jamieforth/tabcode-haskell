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

{-# LANGUAGE OverloadedStrings #-}

module TabCode.MEI.Elements where

import Data.Monoid (mempty, (<>))
import Data.Text   (Text, pack, unpack)
import TabCode
import TabCode.MEI.Types

noMEIAttrs :: MEIAttrs
noMEIAttrs = mempty

children :: Maybe [a] -> [a]
children (Just xs) = xs
children Nothing   = []

(<$:>) :: (a -> [b]) -> Maybe a -> [b]
f <$:> Just x  = f x
f <$:> Nothing = []

attrs :: MEIAttrs -> MEIAttrs -> MEIAttrs
attrs xs ys = xs <> ys

boundedIntAttr :: Int -> (Int, Int) -> Text -> MEIAttrs
boundedIntAttr i (l, u) n | i >= l && i <= u = [(n, (pack $ show i))]
                          | otherwise        = error $ "Invalid " ++ (unpack n) ++ ": " ++ (show i)

atBase :: Int -> MEIAttrs
atBase b = [("numbase.default", (pack $ show b))]

atCount :: Int -> MEIAttrs
atCount c = [("count", (pack $ show c))]

atCut :: Int -> MEIAttrs
atCut n = boundedIntAttr n (1,6) "slash"

atDur :: RhythmSign -> MEIAttrs
atDur (RhythmSign s _ Dot _)   = [("dur", meiDur s), ("dots", "1")]
atDur (RhythmSign s _ NoDot _) = [("dur", meiDur s)]

atDurSymb :: Duration -> Dot -> MEIAttrs
atDurSymb dur Dot   = [ ("symbol", durSymb dur Dot)
                      , ("dots", "1") ]
atDurSymb dur NoDot = [("symbol", durSymb dur NoDot)]

atDot :: Bool -> MEIAttrs
atDot True  = [("dot", "true")]
atDot False = [("dot", "false")]

atNum :: Int -> MEIAttrs
atNum n = [("num.default", (pack $ show n))]

atPlayingFinger :: Finger -> MEIAttrs
atPlayingFinger fngr = [("playingFinger", finger fngr)]

atProlation :: Int -> MEIAttrs
atProlation p = boundedIntAttr p (2,3) "prolatio"

atSign :: Char -> MEIAttrs
atSign 'O' = [("sign", "O")]
atSign 'C' = [("sign", "C")]
atSign c   = error $ "Invalid mensuration symbol: " ++ (show c)

atSlash :: Int -> MEIAttrs
atSlash n = [("mensur.slash", (pack $ show n))]

atTabCourse :: Course -> MEIAttrs
atTabCourse crs = [("tab.course", course crs)]

atTabFret :: Fret -> MEIAttrs
atTabFret frt = [("tab.fret", fretNo frt)]

atTempus :: Int -> MEIAttrs
atTempus t = boundedIntAttr t (2,3) "tempus"

atUnit :: Int -> MEIAttrs
atUnit u = [("unit", (pack $ show u))]

elArticulation :: Articulation -> [MEI]
elArticulation artic = [XMLComment ""]

elConnectingLine :: Connecting -> [MEI]
elConnectingLine conn = [XMLComment ""]

elFingering :: Fingering -> [MEI]
elFingering (FingeringLeft fngr _)  = [ MEIFingering ([("playingHand", "left")]  <> (atPlayingFinger fngr)) [] ]
elFingering (FingeringRight fngr _) = [ MEIFingering ([("playingHand", "right")] <> (atPlayingFinger fngr)) [] ]

elFretGlyph :: [Rule] -> Fret -> Maybe [MEI]
elFretGlyph rls frt = m <$> glyph
  where
    m g  = [ MEIFretGlyph noMEIAttrs [ XMLText g ] ]
    glyph = case notation rls of
      Just "italian" -> Just $ fretGlyphIt frt
      Just "french"  -> Just $ fretGlyphFr frt
      _              -> Nothing

elNote :: [Rule] -> Note -> [MEI]
elNote rls (Note crs frt (fng1, fng2) orn artic conn) =
  [ MEINote ( atTabCourse crs <> atTabFret frt ) $ children ( (elFretGlyph rls frt)
                                                              <> (elFingering <$> fng1)
                                                              <> (elFingering <$> fng2) ) ]
  <> children ( (elOrnament <$> orn)
                <> (elArticulation <$> artic)
                <> (elConnectingLine <$> conn ) )

elOrnament :: Ornament -> [MEI]
elOrnament o =
  case o of
    (OrnA s _) -> orn "a" s
    (OrnB s _) -> orn "b" s
    (OrnC s _) -> orn "c" s
    (OrnD s _) -> orn "d" s
    (OrnE s _) -> orn "e" s
    (OrnF s _) -> orn "f" s
    (OrnG s _) -> orn "g" s
    (OrnH s _) -> orn "h" s
    (OrnI s _) -> orn "i" s
    (OrnJ s _) -> orn "j" s
    (OrnK s _) -> orn "k" s
    (OrnL s _) -> orn "l" s
    (OrnM s _) -> orn "m" s

  where
    orn t s  = [ TCOrnament ([("type", t)] <> (ornST <$:> s)) [] ]
    ornST st = [("sub-type", pack $ show st)]

elRhythmSign :: RhythmSign -> [MEI]
elRhythmSign (RhythmSign Fermata _ _ _) = [ MEIFermata noMEIAttrs [] ]
elRhythmSign (RhythmSign dur _ dt _)    = [ MEIRhythmSign ( atDurSymb dur dt ) [] ]

course :: Course -> Text
course One = pack "1"
course Two = pack "2"
course Three = pack "3"
course Four = pack "4"
course Five = pack "5"
course Six = pack "6"
course (Bass n) = pack $ show $ 6 + n

finger :: Finger -> Text
finger FingerOne = pack "1"
finger FingerTwo = pack "2"
finger FingerThree = pack "3"
finger FingerFour = pack "4"
finger Thumb = pack "t"

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
fretGlyphIt A = pack "0"
fretGlyphIt f = fretNo f

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

durSymb :: Duration -> Dot -> Text
durSymb Fermata Dot = error "Dotted fermata not allowed"
durSymb Fermata NoDot = pack "F"
durSymb Breve Dot = pack "B."
durSymb Breve NoDot = pack "B"
durSymb Semibreve Dot = pack "W."
durSymb Semibreve NoDot = pack "W"
durSymb Minim Dot = pack "H."
durSymb Minim NoDot = pack "H"
durSymb Crotchet Dot = pack "Q."
durSymb Crotchet NoDot = pack "Q"
durSymb Quaver Dot = pack "E."
durSymb Quaver NoDot = pack "E"
durSymb Semiquaver Dot = pack "S."
durSymb Semiquaver NoDot = pack "S"
durSymb Demisemiquaver Dot = pack "T."
durSymb Demisemiquaver NoDot = pack "T"
durSymb Hemidemisemiquaver Dot = pack "Y."
durSymb Hemidemisemiquaver NoDot = pack "Y"
durSymb Semihemidemisemiquaver Dot = pack "Z."
durSymb Semihemidemisemiquaver NoDot = pack "Z"

meiDur :: Duration -> Text
meiDur Fermata = pack "fermata"
meiDur Breve = pack "breve"
meiDur Semibreve = pack "1"
meiDur Minim = pack "2"
meiDur Crotchet = pack "4"
meiDur Quaver = pack "8"
meiDur Semiquaver = pack "16"
meiDur Demisemiquaver = pack "32"
meiDur Hemidemisemiquaver = pack "64"
meiDur Semihemidemisemiquaver = pack "128"
