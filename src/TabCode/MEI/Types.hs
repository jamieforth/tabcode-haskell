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

module TabCode.MEI.Types
  ( MEI(..)
  , MEIAttrs
  , MEIState(..) ) where

import Data.Text
import TabCode (Rule)

type MEIAttrs = [(Text, Text)]

data MEI
  = MEI MEIAttrs [MEI]
  | MEIBarLine MEIAttrs [MEI]
  | MEIBeam MEIAttrs [MEI]
  | MEIBody MEIAttrs [MEI]
  | MEIChord MEIAttrs [MEI]
  | MEIFermata MEIAttrs [MEI]
  | MEIFingering MEIAttrs [MEI]
  | MEIFretGlyph MEIAttrs [MEI]
  | MEIHead MEIAttrs [MEI]
  | MEILayer MEIAttrs [MEI]
  | MEIMDiv MEIAttrs [MEI]
  | MEIMeasure MEIAttrs [MEI]
  | MEIMensur MEIAttrs [MEI]
  | MEIMeterSig MEIAttrs [MEI]
  | MEIMusic MEIAttrs [MEI]
  | MEINote MEIAttrs [MEI]
  | TCOrnament MEIAttrs [MEI]
  | MEIPageBreak MEIAttrs [MEI]
  | MEIPart MEIAttrs [MEI]
  | MEIParts MEIAttrs [MEI]
  | MEIRest MEIAttrs [MEI]
  | MEIRhythmSign MEIAttrs [MEI]
  | MEISection MEIAttrs [MEI]
  | MEIStaff MEIAttrs [MEI]
  | MEIStaffDef MEIAttrs [MEI]
  | MEISystemBreak MEIAttrs [MEI]
  | MEITuplet MEIAttrs [MEI]
  | XMLText Text
  | XMLComment Text
  deriving (Eq, Show)

data MEIState = MEIState {
    stRules     :: [Rule]
  , stMdiv      :: MEIAttrs
  , stPart      :: MEIAttrs
  , stSection   :: MEIAttrs
  , stStaff     :: MEIAttrs
  , stStaffDef  :: MEIAttrs
  , stLayer     :: MEIAttrs
  , stMeasure   :: MEIAttrs
  , stChord     :: MEIAttrs
  }
  deriving (Eq)
