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

module TabCode.Options where

import Control.Applicative ((<$>))
import Data.Monoid ((<>))
import Options.Applicative

data ParseMode
  = Strict
  | Permissive
  deriving (Eq, Show)

data Structure
  = BarLines
  | Measures
  deriving (Eq, Show)

data XmlIds
  = WithXmlIds
  | WithoutXmlIds
  deriving (Eq, Show)

data TCOptions = TCOptions
  { parseMode :: ParseMode
  , structure :: Structure
  , xmlIds :: XmlIds
  }
  deriving (Eq)

config :: Parser TCOptions
config = TCOptions
  <$> flag Strict Permissive
    ( long "permissive"
      <> help "Ignore invalid tabwords" )
  <*> flag BarLines Measures
    ( long "with-measures"
      <> help "Structure the MEI with <measure> elements. The default is to use <barLine>s." )
  <*> flag WithoutXmlIds WithXmlIds
    ( long "with-xml-ids"
      <> help "Add automatic, sequential xml:id attributes to some of the output elements. By default no xml:id attributes are included." )
