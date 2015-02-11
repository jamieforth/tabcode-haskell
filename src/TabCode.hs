-- TabCode - A parser for the Tabcode lute tablature language
--
-- Copyright (C) 2015 Richard Lewis, Goldsmiths' College
-- Author: Richard Lewis <richard.lewis@gold.ac.uk>

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module TabCode where

data Duration = Breve | Semibreve | Minim | Crotchet | Quaver | Semiquaver | Demisemiquaver | Hemidemisemiquaver | Semihemidemisemiquaver

data Dot = Dot | NoDot

data Beam a = BeamOpen a
            | BeamClose a

data Beat = Simple | Compound

data RhythmSign = RhythmSign (Beam Duration) Duration Beat Dot

data Fret = A | B | C | D | E | F | G | H
data Course = One | Two | Three | Four | Five | Six | Bass Int

data Attachment = AboveLeft | Above | AboveRight | Left | Right | BelowLeft | Below | BelowRight

data Hand = RH | LH
data Finger = FingerOne | FingerTWo | FingerThree | FingerFour | Thumb
data Fingering = Fingering (Maybe Hand) Finger (Maybe Attachment)
data Ornament = OrnA (Maybe Attachment)
              | OrnB (Maybe Attachment)
              | OrnC (Maybe Attachment)
              | OrnD (Maybe Attachment)
              | OrnE (Maybe Attachment)
              | OrnF (Maybe Attachment)
              | OrnG (Maybe Attachment)
              | OrnH (Maybe Attachment)
              | OrnI (Maybe Attachment)
              | OrnJ (Maybe Attachment)
              | OrnK (Maybe Attachment)
              | OrnL (Maybe Attachment)

data SepareePosition = SepareeLeft | SepareeRight | SepareeCentre
data SepareeDirection = SepareeUp | SepareeDown

data Articulation = EnsembleFrom Note
                  | EnsembleTo Note
                  | SepareeFrom Note SepareeDirection (Maybe SepareePosition)
                  | SepareeTo Note SepareeDirection (Maybe SepareePosition)

data SlurDirection = SlurUp | SlurDown
type ConnectingId = Int
data Connecting = SlurFrom Note SlurDirection
                | SlurTo Note SlurDirection
                | StraightFrom Note ConnectingId (Maybe Attachment)
                | StraightTo Note ConnectingId (Maybe Attachment)
                | CurvedFrom Note ConnectingId (Maybe Attachment)
                | CurvedTo Note ConnectingId (Maybe Attachment)

data Note = Note Course Fret (Maybe Fingering) (Maybe Ornament) (Maybe Articulation) (Maybe Connecting)

data Bar = SingleBar | DoubleBar | RepeatLBar | RepeatLRBar | DashedBar | NonCountingBar | NthTimeBar Int

data Tempus = Perfect | Imperfect | HalfPerfect | HalfImperfect | HalfHalfPerfect | HalfHalfImperfect |  Beats Int

data MeterSign = SingleMeterSign Tempus Dot
               | VerticalMeterSign Tempus Tempus Dot
               | HorizontalMeterSign Tempus Tempus Dot

data TabWord = Chord RhythmSign [Note]
             | Rest RhythmSign
             | BarLine Bar
             | Meter MeterSign
             | Comment String
             | SystemBreak
             | PageBreak
