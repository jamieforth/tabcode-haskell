-- TabCode - A parser for the Tabcode lute tablature language
--
-- Copyright (C) 2015, 2016 Richard Lewis, Goldsmiths' College
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

module TabCode.MEI.Serialiser ( meiDoc
                              , staff ) where

import           Data.Maybe (catMaybes)
import           Data.Text (pack, unpack, Text)
import           Data.Traversable()
import qualified Data.Vector as V
import           TabCode
import           Text.XML.Generator

mei :: Namespace
mei = namespace "" "http://www.music-encoding.org/ns/mei"

meiDoc :: TabCode -> Xml Elem
meiDoc tc =
  xelemQ mei "mei" $ (xattr "meiversion" "3.0.0") <#>
  ( xelemQEmpty mei "meiHead" <>
    ( xelemQ mei "music" $
      xelemQ mei "body" $
      xelemQ mei "mdiv" $
      xelemQ mei "parts" $
      xelemQ mei "part" $
      xelemQ mei "section" $ staff tc ) )

txelems :: Traversable t => t (Xml Elem) -> Xml Elem
txelems = foldr mappend mempty

staff :: TabCode -> Xml Elem
staff (TabCode rls tws) =
  xelemQ mei "staff" $ xelemQ mei "layer" $ txelems $ V.map (tabWord rls) tws

boundedIntAttr :: Int -> (Int, Int) -> Text -> Xml Attr
boundedIntAttr i (l, u) n | i >= l && i <= u = xattr n (pack $ show i)
                          | otherwise        = error $ "Invalid " ++ (unpack n) ++ ": " ++ (show i)

prolation :: Int -> Xml Attr
prolation p = boundedIntAttr p (2,3) "prolatio"

tempus :: Int -> Xml Attr
tempus t = boundedIntAttr t (2,3) "tempus"

slash :: Int -> Xml Attr
slash n = xattr "mensur.slash" (pack $ show n)

staffDef :: [Xml Attr] -> Xml Elem -> Xml Elem
staffDef attrs cs = xelemQ mei "staffDef" $ (xattrs attrs, cs)

sign :: Char -> Xml Attr
sign 'O' = xattr "sign" "O"
sign 'C' = xattr "sign" "C"
sign c   = error $ "Invalid mensuration symbol: " ++ (show c)

dot :: Bool -> Xml Attr
dot True  = xattr "dot" "true"
dot False = xattr "dot" "false"

cut :: Int -> Xml Attr
cut n = boundedIntAttr n (1,6) "slash"

mensur :: [Xml Attr] -> Xml Elem
mensur attrs = xelemQ mei "mensur" $ xattrs attrs

num :: Int -> Xml Attr
num n = xattr "num.default" (pack $ show n)

base :: Int -> Xml Attr
base b = xattr "numbase.default" (pack $ show b)

count :: Int -> Xml Attr
count c = xattr "count" (pack $ show c)

unit :: Int -> Xml Attr
unit u = xattr "unit" (pack $ show u)

meterSig :: [Xml Attr] -> Xml Elem
meterSig attrs = xelemQ mei "meterSig" $ xattrs attrs

tabWord :: [Rule] -> TabWord -> Xml Elem
tabWord rls (Chord _ _ (Just rs) ns) =
  xelemQ mei "chord" $ (durAttr rs) <#> ((rhythmSign rs) <> (xelems $ concat $ map (note rls) ns))

tabWord rls (Chord _ _ Nothing ns) =
  xelemQ mei "chord" $ xelems $ concat $ map (note rls) ns

tabWord _ (Rest _ _ (RhythmSign Fermata _ _ _)) =
  xelemQEmpty mei "fermata"

tabWord _ (Rest _ _ rs) =
  xelemQ mei "rest" $ (durAttr rs) <#> (rhythmSign rs)

tabWord _ (BarLine _ _ b) =
  xelemQEmpty mei "barLine"

tabWord _ (Meter _ _ (SingleMeterSign PerfectMajor)) =
  staffDef [ prolation 3 , tempus 3 ] $ mensur [ sign 'O' , dot True ]

tabWord _ (Meter _ _ (SingleMeterSign PerfectMinor)) =
  staffDef [ prolation 3 , tempus 2 ] $ mensur [ sign 'O' , dot False ]

tabWord _ (Meter _ _ (SingleMeterSign ImperfectMajor)) =
  staffDef [ prolation 2 , tempus 3 ] $ mensur [ sign 'C' , dot True ]

tabWord _ (Meter _ _ (SingleMeterSign ImperfectMinor)) =
  staffDef [ prolation 2 , tempus 2 ] $ mensur [ sign 'C' , dot False ]

tabWord _ (Meter _ _ (SingleMeterSign HalfPerfectMajor)) =
  staffDef [ prolation 3 , tempus 3 , slash 1 ] $ mensur [ sign 'O' , dot True , cut 1 ]

tabWord _ (Meter _ _ (SingleMeterSign HalfPerfectMinor)) =
  staffDef [ prolation 3 , tempus 2 , slash 1 ] $ mensur [ sign 'O' , dot False , cut 1 ]

tabWord _ (Meter _ _ (SingleMeterSign HalfImperfectMajor)) =
  staffDef [ prolation 2 , tempus 3 , slash 1 ] $ mensur [ sign 'C' , dot True , cut 1 ]

tabWord _ (Meter _ _ (SingleMeterSign HalfImperfectMinor)) =
  staffDef [ prolation 2 , tempus 2 , slash 1 ] $ mensur [ sign 'C' , dot False , cut 1 ]

tabWord _ (Meter _ _ (VerticalMeterSign (Beats n) (Beats b))) =
  staffDef [ num n , base b ] $ meterSig [ count n , unit b ]

tabWord _ (Meter _ _ (SingleMeterSign (Beats 3))) =
  staffDef [ tempus 3 ] $ noElems

tabWord _ (Meter _ _ m) = xcomment $ " tc2mei: Un-implemented mensuration sign: " ++ (show m) ++ " "

tabWord _ (Comment _ _ c) =
  xcomment c

tabWord _ (SystemBreak _ _) =
  xelemQEmpty mei "sb"

tabWord _ (PageBreak _ _) =
  xelemQEmpty mei "pb"

tabWord _ (Invalid src line col word) =
  xcomment $ " tc2mei: Invalid tabword in source '" ++ src ++ "' (line: " ++ (show line) ++ "; col: " ++ (show col) ++ "): \"" ++ word ++ "\" "

rhythmSign :: RhythmSign -> Xml Elem
rhythmSign (RhythmSign Fermata _ _ _) =
  xelemQEmpty mei "fermata"

rhythmSign (RhythmSign dur _ dt _) =
  xelemQ mei "rhythmGlyph" $ durSymbAttr dur dt

note :: [Rule] -> Note -> [Xml Elem]
note rls (Note crs frt (fng1, fng2) orn artic conn) =
  [ xelemQ mei "note" $ xattrs [ (tabCourseAttr crs)
                               , (tabFretAttr frt) ]
    <#> (xelems $ catMaybes [ (fretGlyph rls frt)
                            , (fingering <$> fng1)
                            , (fingering <$> fng2) ]) ]
  <> catMaybes [ (ornament <$> orn)
               , (articulation <$> artic)
               , (connectingLine <$> conn) ]

fretGlyph :: [Rule] -> Fret -> Maybe (Xml Elem)
fretGlyph rls frt = el <$> glyph
  where
    el g  = xelemQ mei "fretGlyph" $ xtext g
    glyph = case notation rls of
      Just "italian" -> Just $ fretGlyphIt frt
      Just "french"  -> Just $ fretGlyphFr frt
      _              -> Nothing

fingering :: Fingering -> Xml Elem
fingering (FingeringLeft fngr _) =
  xelemQ mei "fingering" $ xattrs [ (xattr "playingHand" "left")
                                  , (playingFingerAttr fngr) ]
fingering (FingeringRight fngr _) =
  xelemQ mei "fingering" $ xattrs [ (xattr "playingHand" "right")
                                  , (playingFingerAttr fngr) ]

playingFingerAttr :: Finger -> Xml Attr
playingFingerAttr fngr = xattr "playingFinger" (finger fngr)

tc :: Namespace
tc = namespace "tc" "http://data.t-mus.org/ns/tabcode"

ornament :: Ornament -> Xml Elem
ornament o =
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
    orn t s  = xelemQ tc "ornament" $ xattrs $ catMaybes [ ( Just $ xattr "type" t ), ornST <$> s ]
    ornST st = xattr "sub-type" (pack $ show st)

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

durSymbAttr :: Duration -> Dot -> Xml Attr
durSymbAttr dur Dot   = xattrs [ xattr "symbol" (durSymb dur Dot)
                               , xattr "dots" "1" ]
durSymbAttr dur NoDot = xattr "symbol" (durSymb dur NoDot)

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

durAttr :: RhythmSign -> Xml Attr
durAttr (RhythmSign s _ Dot _)   = xattrs $ [ xattr "dur" (meiDur s)
                                            , xattr "dots" "1" ]
durAttr (RhythmSign s _ NoDot _) = xattr "dur" (meiDur s)

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
