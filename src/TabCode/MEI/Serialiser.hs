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

module TabCode.MEI.Serialiser
  ( meiDoc ) where

import           Data.Text          (unpack, Text)
import           TabCode
import           TabCode.MEI.Types
import           Text.XML.Generator

mei :: Namespace
mei = namespace "" "http://www.music-encoding.org/ns/mei"

tc :: Namespace
tc = namespace "tc" "http://data.t-mus.org/ns/tabcode"

meiAttrs :: MEIAttrs -> Xml Attr
meiAttrs as = xattrs $ map (\(n,v) -> xattr n v) as

meiXml :: Text -> MEIAttrs -> [MEI] -> Xml Elem
meiXml n as cs = xelemQ mei n $ (xattrs (map (\(n,v) -> xattr n v) as)) <#> (xelems $ map meiDoc cs)

meiXml_ :: Text -> MEIAttrs -> Xml Elem
meiXml_ n as = xelemQ mei n $ xattrs $ map (\(n,v) -> xattr n v) as

tcXml :: Text -> MEIAttrs -> [MEI] -> Xml Elem
tcXml n as cs = xelemQ tc n $ (xattrs (map (\(n,v) -> xattr n v) as)) <#> (xelems $ map meiDoc cs)

tcXml_ :: Text -> MEIAttrs -> Xml Elem
tcXml_ n as = xelemQ tc n $ xattrs $ map (\(n,v) -> xattr n v) as

meiDoc :: MEI -> Xml Elem
meiDoc (MEIBarLine      attrs [])       = meiXml_ "barLine" attrs
meiDoc (MEIBarLine      attrs children) = meiXml  "barLine" attrs children
meiDoc (MEIBeam         attrs [])       = meiXml_ "beam" attrs
meiDoc (MEIBeam         attrs children) = meiXml  "beam" attrs children
meiDoc (MEIBody         attrs [])       = meiXml_ "body" attrs
meiDoc (MEIBody         attrs children) = meiXml  "body" attrs children
meiDoc (MEIChord        attrs [])       = meiXml_ "chord" attrs
meiDoc (MEIChord        attrs children) = meiXml  "chord" attrs children
meiDoc (MEIFermata      attrs [])       = meiXml_ "fermata" attrs
meiDoc (MEIFermata      attrs children) = meiXml  "fermata" attrs children
meiDoc (MEIFingering    attrs [])       = meiXml_ "fingering" attrs
meiDoc (MEIFingering    attrs children) = meiXml  "fingering" attrs children
meiDoc (MEIFretGlyph    attrs [])       = meiXml_ "fretGlyph" attrs
meiDoc (MEIFretGlyph    attrs children) = meiXml  "fretGlyph" attrs children
meiDoc (MEILayer        attrs [])       = meiXml_ "layer" attrs
meiDoc (MEILayer        attrs children) = meiXml  "layer" attrs children
meiDoc (MEIMDiv         attrs [])       = meiXml_ "mdiv" attrs
meiDoc (MEIMDiv         attrs children) = meiXml  "mdiv" attrs children
meiDoc (MEIMeasure      attrs [])       = meiXml_ "measure" attrs
meiDoc (MEIMeasure      attrs children) = meiXml  "measure" attrs children
meiDoc (MEIMensur       attrs [])       = meiXml_ "mensur" attrs
meiDoc (MEIMensur       attrs children) = meiXml  "mensur" attrs children
meiDoc (MEIMeterSig     attrs [])       = meiXml_ "meterSig" attrs
meiDoc (MEIMeterSig     attrs children) = meiXml  "meterSig" attrs children
meiDoc (MEIMusic        attrs [])       = meiXml_ "music" attrs
meiDoc (MEIMusic        attrs children) = meiXml  "music" attrs children
meiDoc (MEINote         attrs [])       = meiXml_ "note" attrs
meiDoc (MEINote         attrs children) = meiXml  "note" attrs children
meiDoc (TCOrnament      attrs [])       = tcXml_  "ornament" attrs
meiDoc (TCOrnament      attrs children) = tcXml   "ornament" attrs children
meiDoc (MEIPageBreak    attrs [])       = meiXml_ "pb" attrs
meiDoc (MEIPageBreak    attrs children) = meiXml  "pb" attrs children
meiDoc (MEIPart         attrs [])       = meiXml_ "part" attrs
meiDoc (MEIPart         attrs children) = meiXml  "part" attrs children
meiDoc (MEIParts        attrs [])       = meiXml_ "parts" attrs
meiDoc (MEIParts        attrs children) = meiXml  "parts" attrs children
meiDoc (MEIRest         attrs [])       = meiXml_ "rest" attrs
meiDoc (MEIRest         attrs children) = meiXml  "rest" attrs children
meiDoc (MEIRhythmSign   attrs [])       = meiXml_ "rhythmGlyph" attrs
meiDoc (MEIRhythmSign   attrs children) = meiXml  "rhythmGlyph" attrs children
meiDoc (MEISection      attrs [])       = meiXml_ "section" attrs
meiDoc (MEISection      attrs children) = meiXml  "section" attrs children
meiDoc (MEIStaff        attrs [])       = meiXml_ "staff" attrs
meiDoc (MEIStaff        attrs children) = meiXml  "staff" attrs children
meiDoc (MEIStaffDef     attrs [])       = meiXml_ "staffDef" attrs
meiDoc (MEIStaffDef     attrs children) = meiXml  "staffDef" attrs children
meiDoc (MEISystemBreak  attrs [])       = meiXml_ "sb" attrs
meiDoc (MEISystemBreak  attrs children) = meiXml  "sb" attrs children
meiDoc (MEITuplet       attrs [])       = meiXml_ "tuplet" attrs
meiDoc (MEITuplet       attrs children) = meiXml  "tuplet" attrs children
meiDoc (XMLText         txt)            = xtext txt
meiDoc (XMLComment      cmt)            = xcomment $ unpack cmt
