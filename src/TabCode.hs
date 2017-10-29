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

module TabCode where

import Data.Set                          (size, fromList)
import Data.Vector                       (Vector)
import Text.ParserCombinators.Parsec.Pos (Line, Column)

data Duration
  = Fermata
  | Breve
  | Semibreve
  | Minim
  | Crotchet
  | Quaver
  | Semiquaver
  | Demisemiquaver
  | Hemidemisemiquaver
  | Semihemidemisemiquaver
  deriving (Eq, Show)

data Dot
  = Dot
  | NoDot
  deriving (Eq, Show)

data Beam a
  = BeamOpen a
  | BeamClose a
  deriving (Eq, Show)

beamDuration :: Int -> Duration
beamDuration 1 = Quaver
beamDuration 2 = Semiquaver
beamDuration 3 = Demisemiquaver
beamDuration 4 = Hemidemisemiquaver
beamDuration 5 = Semihemidemisemiquaver
beamDuration _ = error "Invalid beam count"

data Beat
  = Simple
  | Compound
  deriving (Eq, Show)

data RhythmSign = RhythmSign Duration Beat Dot (Maybe (Beam Duration)) deriving (Eq, Show)

-- FIXME Perhaps this data model should rather be a numeric fret
-- number and then any symbol from the source should be captured as
-- another part of the Note type
data Fret = A | B | C | D | E | F | G | H | I | J | K | L | M | N deriving (Eq, Show, Ord)

data Course
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Bass Int
  deriving (Eq, Show, Ord)

data Attachment
  = PosAboveLeft
  | PosAbove
  | PosAboveRight
  | PosLeft
  | PosRight
  | PosBelowLeft
  | PosBelow
  | PosBelowRight
  deriving (Eq, Show)

data Finger
  = FingerOne
  | FingerTwo
  | FingerThree
  | FingerFour
  | Thumb deriving (Eq, Show)

data Fingering
  = FingeringLeft Finger (Maybe Attachment)
  | FingeringRight Finger (Maybe Attachment)
  deriving (Eq, Show)

type OrnSubtype = Int
-- FIXME Work out the proper ornament names
data Ornament
  = OrnA (Maybe OrnSubtype) (Maybe Attachment)
  | OrnB (Maybe OrnSubtype) (Maybe Attachment)
  | OrnC (Maybe OrnSubtype) (Maybe Attachment)
  | OrnD (Maybe OrnSubtype) (Maybe Attachment)
  | OrnE (Maybe OrnSubtype) (Maybe Attachment)
  | OrnF (Maybe OrnSubtype) (Maybe Attachment)
  | OrnG (Maybe OrnSubtype) (Maybe Attachment)
  | OrnH (Maybe OrnSubtype) (Maybe Attachment)
  | OrnI (Maybe OrnSubtype) (Maybe Attachment)
  | OrnJ (Maybe OrnSubtype) (Maybe Attachment)
  | OrnK (Maybe OrnSubtype) (Maybe Attachment)
  | OrnL (Maybe OrnSubtype) (Maybe Attachment)
  | OrnM (Maybe OrnSubtype) (Maybe Attachment)
  deriving (Eq, Show)

data SepareePosition
  = SepareeLeft
  | SepareeRight
  | SepareeCentre
  deriving (Eq, Show)

data SepareeDirection
  = SepareeUp
  | SepareeDown
  deriving (Eq, Show)

data Articulation
  = Ensemble
  | Separee (Maybe SepareeDirection) (Maybe SepareePosition)
  deriving (Eq, Show)

data SlurDirection
  = SlurUp
  | SlurDown
  deriving (Eq, Show)

type ConnectingId = Int

data Connecting
  = Slur SlurDirection
  | StraightFrom ConnectingId (Maybe Attachment)
  | StraightTo ConnectingId (Maybe Attachment)
  | CurvedUpFrom ConnectingId (Maybe Attachment)
  | CurvedUpTo ConnectingId (Maybe Attachment)
  | CurvedDownFrom ConnectingId (Maybe Attachment)
  | CurvedDownTo ConnectingId (Maybe Attachment)
  deriving (Eq, Show)

data Note = Note Course Fret (Maybe Fingering, Maybe Fingering) (Maybe Ornament) (Maybe Articulation) (Maybe Connecting) deriving (Eq, Show)

duplicateCoursesInNotes :: [Note] -> Bool
duplicateCoursesInNotes ns =
  length cs > size (fromList cs)
  where cs = coursesFromNotes ns

duplicateCourses :: TabWord -> Bool
duplicateCourses (Chord _ _ _ ns _) = duplicateCoursesInNotes ns
duplicateCourses _ = False

data Counting
  = Counting
  | NotCounting
  deriving (Eq, Show)

data Dashed
  = Dashed
  | NotDashed
  deriving (Eq, Show)

data RepeatMark
  = RepeatLeft
  | RepeatRight
  | RepeatBoth
  deriving (Eq, Show)

data Repetition
  = Reprise
  | NthTime Int
  deriving (Eq, Show)

data Bar
  = SingleBar (Maybe RepeatMark) (Maybe Repetition) Dashed Counting
  | DoubleBar (Maybe RepeatMark) (Maybe Repetition) Dashed Counting
  deriving (Eq, Show)

data Mensuration
  = PerfectMajor
  | PerfectMinor
  | ImperfectMajor
  | ImperfectMinor
  | HalfPerfectMajor
  | HalfPerfectMinor
  | HalfImperfectMajor
  | HalfImperfectMinor
  | Beats Int
  deriving (Eq, Show)

data MeterSign
  = SingleMeterSign Mensuration
  | VerticalMeterSign Mensuration Mensuration
  | HorizontalMeterSign Mensuration Mensuration
  deriving (Eq, Show)

data Comment
  = Comment String
  deriving (Eq, Show)

data TabWord
  = Chord Line Column (Maybe RhythmSign) [Note] (Maybe Comment)
  | Rest Line Column RhythmSign (Maybe Comment)
  | BarLine Line Column Bar (Maybe Comment)
  | Meter Line Column MeterSign (Maybe Comment)
  | CommentWord Line Column Comment
  | SystemBreak Line Column (Maybe Comment)
  | PageBreak Line Column (Maybe Comment)
  | Invalid String Line Column String
  deriving (Eq, Show)

twPos :: TabWord -> (Line, Column)
twPos (Chord l c _ _ _)      = (l, c)
twPos (Rest l c _ _)         = (l, c)
twPos (BarLine l c _ _)      = (l, c)
twPos (Meter l c _ _)        = (l, c)
twPos (CommentWord l c _)    = (l, c)
twPos (SystemBreak l c _)    = (l, c)
twPos (PageBreak l c _)      = (l, c)
twPos (Invalid _ l c _)      = (l, c)

twLine :: TabWord -> Line
twLine = fst . twPos

twColumn :: TabWord -> Column
twColumn = snd . twPos

coursesFromNotes :: [Note] -> [Course]
coursesFromNotes ns = map (\(Note crs _ _ _ _ _) -> crs) ns

courses :: TabWord -> [Course]
courses (Chord _ _ _ ns _) = coursesFromNotes ns
courses _ = []

data Rule = Rule String String deriving (Eq, Show)

ruleLkup :: [Rule] -> String -> Maybe String
ruleLkup rls rl = lookup rl rlsMap
  where
    rlsMap = map (\(Rule r v) -> (r, v)) rls

notation :: [Rule] -> Maybe String
notation = (flip ruleLkup) "notation"

title :: [Rule] -> Maybe String
title = (flip ruleLkup) "title"

data TabCode = TabCode [Rule] (Vector TabWord)
