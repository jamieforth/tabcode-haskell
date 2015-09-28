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

module TabCode.MEISerialiser where

import TabCode
import Text.XML.HXT.Core

instance XmlPickler TabCode where
  xpickle = xpWrap ( \words -> TabCode words , \(TabCode words) -> words ) $
            xpElem "staff" $
            xpTabWords

xpTabWords :: PU [TabWord]
xpTabWords = xpList xpickle

instance XmlPickler TabWord where
  xpickle =
    xpAlt tag ps
    where
      tag (Chord (Just rs) ns) = 0
      tag (Chord Nothing ns)   = 1
      tag (Rest rs)            = 2
      tag (BarLine bl)         = 3
      tag (Meter m)            = 4
      tag (Comment c)          = 5
      tag SystemBreak          = 6
      tag PageBreak            = 7
      ps = [ xpChord
           , xpChordNoRS
           , xpRest
           , xpBarLine
           , xpMeter
           , xpComment
           , xpSystemBreak
           , xpPageBreak
           ]

xpChord :: PU TabWord
xpChord = xpElem "tabChord" xpickle

xpChordNoRS :: PU TabWord
xpChordNoRS = xpElem "tabChord" xpickle

xpRest :: PU TabWord
xpRest = xpElem "rest" xpickle

xpBarLine :: PU TabWord
xpBarLine = xpickle

instance XmlPickler Bar where
  xpickle = xpBar

xpBar :: PU Bar
xpBar = xpAlt tag ps
  where
    tag (SingleBar rm r d c) = 0
    tag (DoubleBar rm r d c) = 1
    ps = [ xpSingleBar
         , xpDoubleBar
         ]

xpSingleBar :: PU Bar
xpSingleBar = xpElem "barLine" $
              xpWrap ( \((rend, rm, r, d, c)) -> SingleBar rm r d c , \(SingleBar rm r d c) -> ("single", rm, r, d, c) ) $
              xp5Tuple
              (xpAttr "rend" (xpTextAttr "single"))
              xpRepeatMark
              xpRepetition
              xpDashed
              xpCounting

xpDoubleBar :: PU Bar
xpDoubleBar = xpElem "barLine" $
              xpWrap ( \((rend, rm, r, d, c)) -> DoubleBar rm r d c , \(DoubleBar rm r d c) -> ("double", rm, r, d, c) ) $
              xp5Tuple
              (xpAttr "rend" (xpTextAttr "double"))
              xpRepeatMark
              xpRepetition
              xpDashed
              xpCounting

xpRepeatMark :: PU (Maybe RepeatMark)
xpRepeatMark = xpOption $ xpRepeat

xpRepeat :: PU RepeatMark
xpRepeat = xpAlt tag ps
  where
    tag RepeatLeft  = 0
    tag RepeatRight = 1
    tag RepeatBoth  = 2
    ps = [ xpWrap ( const RepeatLeft, const "left" )   $ xpTextAttr "repeat"
         , xpWrap ( const RepeatRight, const "right" ) $ xpTextAttr "repeat"
         , xpWrap ( const RepeatBoth, const "both" )   $ xpTextAttr "repeat"
         ]

xpRepetition :: PU (Maybe Repetition)
xpRepetition = xpOption $ xpAttr "repetition" xpickle

xpDashed :: PU Dashed
xpDashed = xpAttr "dashed" xpickle

xpCounting :: PU Counting
xpCounting = xpAttr "counting" xpickle

xpMeter :: PU TabWord
xpMeter = xpElem "meter" xpickle

xpComment :: PU TabWord
xpComment = xpElem "comment" xpickle

xpSystemBreak :: PU TabWord
xpSystemBreak = xpElem "break" xpickle

xpPageBreak :: PU TabWord
xpPageBreak = xpElem "pb" xpickle
