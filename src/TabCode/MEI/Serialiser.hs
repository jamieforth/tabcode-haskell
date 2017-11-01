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

{-# LANGUAGE OverloadedStrings #-}

module TabCode.MEI.Serialiser
  ( meiDoc ) where

import Data.Text (append, pack, unpack, Text)
import TabCode.MEI.Types
import TabCode.Options (XmlIds(..))
import Text.XML.Generator

mei :: Namespace
mei = namespace "" "http://www.music-encoding.org/ns/mei"

tc :: Namespace
tc = namespace "tc" "http://data.t-mus.org/ns/tabcode"

meiAttr :: MEIAttr -> Xml Attr
meiAttr (StringAttr name value) = xattr name value
meiAttr (IntAttr name value) = xattr name (pack $ show value)
meiAttr (PrefIntAttr name (prefix, value)) = xattr name (append prefix (pack (show value)))

meiAttrs :: MEIAttrs -> XmlIds -> Xml Attr
meiAttrs as WithXmlIds = xattrs $ map meiAttr as
meiAttrs as WithoutXmlIds = xattrs $ map meiAttr $ filter (not . isXmlId) as

isXmlId :: MEIAttr -> Bool
isXmlId (PrefIntAttr "xml:id" _) = True
isXmlId _ = False

meiXml :: Text -> MEIAttrs -> [MEI] -> XmlIds -> Xml Elem
meiXml n as cs xmlIds = xelemQ mei n $ (meiAttrs as xmlIds) <#> (xelems $ map (flip meiDoc xmlIds) cs)

meiXml_ :: Text -> MEIAttrs -> XmlIds -> Xml Elem
meiXml_ n as xmlIds = xelemQ mei n $ meiAttrs as xmlIds

tcXml :: Text -> MEIAttrs -> [MEI] -> XmlIds -> Xml Elem
tcXml n as cs xmlIds = xelemQ tc n $ (meiAttrs as xmlIds) <#> (xelems $ map (flip meiDoc xmlIds) cs)

tcXml_ :: Text -> MEIAttrs -> XmlIds -> Xml Elem
tcXml_ n as xmlIds = xelemQ tc n $ meiAttrs as xmlIds

meiDoc :: MEI -> XmlIds -> Xml Elem
meiDoc (MEI attrs []) xmlIds =
  meiXml_ "mei" attrs xmlIds
meiDoc (MEI attrs children) xmlIds =
  meiXml  "mei" attrs children xmlIds
meiDoc (MEIBarLine attrs []) xmlIds =
  meiXml_ "barLine" attrs xmlIds
meiDoc (MEIBarLine attrs children) xmlIds =
  meiXml  "barLine" attrs children xmlIds
meiDoc (MEIBassTuning attrs []) xmlIds =
  meiXml_ "bassTuning" attrs xmlIds
meiDoc (MEIBassTuning attrs children) xmlIds =
  meiXml  "bassTuning" attrs children xmlIds
meiDoc (MEIBeam attrs []) xmlIds =
  meiXml_ "beam" attrs xmlIds
meiDoc (MEIBeam attrs children) xmlIds =
  meiXml  "beam" attrs children xmlIds
meiDoc (MEIBody attrs []) xmlIds =
  meiXml_ "body" attrs xmlIds
meiDoc (MEIBody attrs children) xmlIds =
  meiXml  "body" attrs children xmlIds
meiDoc (MEIChord attrs []) xmlIds =
  meiXml_ "chord" attrs xmlIds
meiDoc (MEIChord attrs children) xmlIds =
  meiXml  "chord" attrs children xmlIds
meiDoc (MEICourse attrs []) xmlIds =
  meiXml_ "course" attrs xmlIds
meiDoc (MEICourse attrs children) xmlIds =
  meiXml  "course" attrs children xmlIds
meiDoc (MEICourseTuning attrs []) xmlIds =
  meiXml_ "courseTuning" attrs xmlIds
meiDoc (MEICourseTuning attrs children) xmlIds =
  meiXml  "courseTuning" attrs children xmlIds
meiDoc (MEIFermata attrs []) xmlIds =
  meiXml_ "fermata" attrs xmlIds
meiDoc (MEIFermata attrs children) xmlIds =
  meiXml  "fermata" attrs children xmlIds
meiDoc (MEIFingering attrs []) xmlIds =
  meiXml_ "fingering" attrs xmlIds
meiDoc (MEIFingering attrs children) xmlIds =
  meiXml  "fingering" attrs children xmlIds
meiDoc (MEIFretGlyph attrs []) xmlIds =
  meiXml_ "fretGlyph" attrs xmlIds
meiDoc (MEIFretGlyph attrs children) xmlIds =
  meiXml  "fretGlyph" attrs children xmlIds
meiDoc (MEIHead attrs []) xmlIds =
  meiXml_ "meiHead" attrs xmlIds
meiDoc (MEIHead attrs children) xmlIds =
  meiXml  "meiHead" attrs children xmlIds
meiDoc (MEIInstrConfig attrs []) xmlIds =
  meiXml_ "instrConfig" attrs xmlIds
meiDoc (MEIInstrConfig attrs children) xmlIds =
  meiXml  "instrConfig" attrs children xmlIds
meiDoc (MEIInstrDesc attrs []) xmlIds =
  meiXml_ "instrDesc" attrs xmlIds
meiDoc (MEIInstrDesc attrs children) xmlIds =
  meiXml  "instrDesc" attrs children xmlIds
meiDoc (MEIInstrName attrs []) xmlIds =
  meiXml_ "instrName" attrs xmlIds
meiDoc (MEIInstrName attrs children) xmlIds =
  meiXml  "instrName" attrs children xmlIds
meiDoc (MEILayer attrs []) xmlIds =
  meiXml_ "layer" attrs xmlIds
meiDoc (MEILayer attrs children) xmlIds =
  meiXml  "layer" attrs children xmlIds
meiDoc (MEIMDiv attrs []) xmlIds =
  meiXml_ "mdiv" attrs xmlIds
meiDoc (MEIMDiv attrs children) xmlIds =
  meiXml  "mdiv" attrs children xmlIds
meiDoc (MEIMeasure attrs []) xmlIds =
  meiXml_ "measure" attrs xmlIds
meiDoc (MEIMeasure attrs children) xmlIds =
  meiXml  "measure" attrs children xmlIds
meiDoc (MEIMensur attrs []) xmlIds =
  meiXml_ "mensur" attrs xmlIds
meiDoc (MEIMensur attrs children) xmlIds =
  meiXml  "mensur" attrs children xmlIds
meiDoc (MEIMeterSig attrs []) xmlIds =
  meiXml_ "meterSig" attrs xmlIds
meiDoc (MEIMeterSig attrs children) xmlIds =
  meiXml  "meterSig" attrs children xmlIds
meiDoc (MEIMusic attrs []) xmlIds =
  meiXml_ "music" attrs xmlIds
meiDoc (MEIMusic attrs children) xmlIds =
  meiXml  "music" attrs children xmlIds
meiDoc (MEINote attrs []) xmlIds =
  meiXml_ "note" attrs xmlIds
meiDoc (MEINote attrs children) xmlIds =
  meiXml  "note" attrs children xmlIds
meiDoc (TCOrnament attrs []) xmlIds =
  tcXml_  "ornament" attrs xmlIds
meiDoc (TCOrnament attrs children) xmlIds =
  tcXml   "ornament" attrs children xmlIds
meiDoc (MEIPageBreak attrs []) xmlIds =
  meiXml_ "pb" attrs xmlIds
meiDoc (MEIPageBreak attrs children) xmlIds =
  meiXml  "pb" attrs children xmlIds
meiDoc (MEIPart attrs []) xmlIds =
  meiXml_ "part" attrs xmlIds
meiDoc (MEIPart attrs children) xmlIds =
  meiXml  "part" attrs children xmlIds
meiDoc (MEIParts attrs []) xmlIds =
  meiXml_ "parts" attrs xmlIds
meiDoc (MEIParts attrs children) xmlIds =
  meiXml  "parts" attrs children xmlIds
meiDoc (MEIPerfMedium attrs []) xmlIds =
  meiXml_ "perfMedium" attrs xmlIds
meiDoc (MEIPerfMedium attrs children) xmlIds =
  meiXml  "perfMedium" attrs children xmlIds
meiDoc (MEIPerfRes attrs []) xmlIds =
  meiXml_ "perfRes" attrs xmlIds
meiDoc (MEIPerfRes attrs children) xmlIds =
  meiXml  "perfRes" attrs children xmlIds
meiDoc (MEIPerfResList attrs []) xmlIds =
  meiXml_ "perfResList" attrs xmlIds
meiDoc (MEIPerfResList attrs children) xmlIds =
  meiXml  "perfResList" attrs children xmlIds
meiDoc (MEIRest attrs []) xmlIds =
  meiXml_ "rest" attrs xmlIds
meiDoc (MEIRest attrs children) xmlIds =
  meiXml  "rest" attrs children xmlIds
meiDoc (MEIRhythmSign attrs []) xmlIds =
  meiXml_ "rhythmGlyph" attrs xmlIds
meiDoc (MEIRhythmSign attrs children) xmlIds =
  meiXml  "rhythmGlyph" attrs children xmlIds
meiDoc (MEISection attrs []) xmlIds =
  meiXml_ "section" attrs xmlIds
meiDoc (MEISection attrs children) xmlIds =
  meiXml  "section" attrs children xmlIds
meiDoc (MEIStaff attrs []) xmlIds =
  meiXml_ "staff" attrs xmlIds
meiDoc (MEIStaff attrs children) xmlIds =
  meiXml  "staff" attrs children xmlIds
meiDoc (MEIStaffDef attrs []) xmlIds =
  meiXml_ "staffDef" attrs xmlIds
meiDoc (MEIStaffDef attrs children) xmlIds =
  meiXml  "staffDef" attrs children xmlIds
meiDoc (MEIString attrs []) xmlIds =
  meiXml_ "string" attrs xmlIds
meiDoc (MEIString attrs children) xmlIds =
  meiXml  "string" attrs children xmlIds
meiDoc (MEISystemBreak attrs []) xmlIds =
  meiXml_ "sb" attrs xmlIds
meiDoc (MEISystemBreak attrs children) xmlIds =
  meiXml  "sb" attrs children xmlIds
meiDoc (MEITuplet attrs []) xmlIds =
  meiXml_ "tuplet" attrs xmlIds
meiDoc (MEITuplet attrs children) xmlIds =
  meiXml  "tuplet" attrs children xmlIds
meiDoc (MEIWork attrs []) xmlIds =
  meiXml_ "work" attrs xmlIds
meiDoc (MEIWork attrs children) xmlIds =
  meiXml  "work" attrs children xmlIds
meiDoc (MEIWorkDesc attrs []) xmlIds =
  meiXml_ "workDesc" attrs xmlIds
meiDoc (MEIWorkDesc attrs children) xmlIds =
  meiXml  "workDesc" attrs children xmlIds
meiDoc (XMLText txt) _ =
  xtext txt
meiDoc (XMLComment cmt) _ =
  xcomment $ unpack cmt
