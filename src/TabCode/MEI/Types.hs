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
  , MEIAttrs ) where

import Data.Text

-- This is a sub-set of MEI! FIXME We really need a library which
-- generates a complete ADT for MEI from the ODD schema. But anyway,
-- the justification of this is that it's a type-safe version of what
-- you've started doing in MEI.Serialiser (which is not type-safe in
-- that it just works with Xml Elem and Xml Attr)

type MEIAttrs = [(Text, Text)]

data MEI
  = MEIBeam MEIAttrs [MEI]
  | MEIBody MEIAttrs [MEI]
  | MEIChord MEIAttrs [MEI]
  | MEIFermata MEIAttrs [MEI]
  | MEIFingering MEIAttrs [MEI]
  | MEIFretGlyph MEIAttrs [MEI]
  | MEILayer MEIAttrs [MEI]
  | MEIMDiv MEIAttrs [MEI]
  | MEIMeasure MEIAttrs [MEI]
  | MEIMensur MEIAttrs [MEI]
  | MEIMeterSig MEIAttrs [MEI]
  | MEIMusic MEIAttrs [MEI]
  | MEINote MEIAttrs [MEI]
  | MEIPart MEIAttrs [MEI]
  | MEIParts MEIAttrs [MEI]
  | MEIRest MEIAttrs [MEI]
  | MEIRhythmSign MEIAttrs [MEI]
  | MEISection MEIAttrs [MEI]
  | MEIStaff MEIAttrs [MEI]
  | MEIStaffDef MEIAttrs [MEI]
  | MEITuple MEIAttrs [MEI]
  | XMLText Text
  | XMLComment Text
  deriving (Eq, Show)
