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

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module TabCode.Parser ( parseTabcode
                      , parseTabcodeStdIn
                      , parseTabcodeFile ) where

import TabCode
import Text.Parsec (ParsecT)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import Prelude hiding (words)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

tablature :: GenParser Char st TabCode
tablature = do
  rls   <- option [] rules
  words <- tabword `endBy` spaces
  eof
  return $ TabCode rls words

rules :: GenParser Char st [Rule]
rules = do
  rls <- between (string "{<rules>") (string "</rules>}") $ do
    many1 $ (try $ rule "notation") <|> (try $ rule "title")
  spaces
  return rls

rule :: String -> GenParser Char st Rule
rule r = do
  spaces
  nt <- between (string $ "<" ++ r ++ ">") (string $ "</" ++ r ++ ">") $ many1 $ noneOf "<"
  spaces
  return $ Rule r nt

tabword :: GenParser Char st TabWord
tabword = (try rest) <|> (try barLine) <|> (try meter) <|> (try comment) <|> (try systemBreak) <|> (try pageBreak) <|> chord

chord :: GenParser Char st TabWord
chord = do
  rs <- option Nothing $ do { r <- rhythmSign; return $ Just r }
  ns <- uniqueNotes $ many1 note
  return $ Chord rs ns

uniqueNotes :: ParsecT s u m [Note] -> ParsecT s u m [Note]
uniqueNotes parser = do
  ns <- parser
  if duplicateCoursesInNotes ns
    then fail $ "Chord contains duplicate courses: " ++ (show $ coursesFromNotes ns)
    else return ns

rhythmSign :: GenParser Char st RhythmSign
rhythmSign = do
  dur  <- duration
  bt   <- beat
  dts  <- dots
  beam <- openBeam <|> closeBeam

  -- FIXME if beam, lookAhead for tabword with closing beam
  return $ RhythmSign dur bt dts beam

dots :: GenParser Char st Dot
dots = option NoDot (do { _ <- many1 (char '.'); return Dot })

beat :: GenParser Char st Beat
beat = option Simple (do { _ <- char '3'; return Compound })

beams :: Char -> (Duration -> Beam Duration) -> GenParser Char st (Maybe (Beam Duration))
beams c mkBeam = option Nothing $ do
  cs <- many1 (char c)
  return $ Just $ mkBeam (beamDuration (length cs))

openBeam :: GenParser Char st (Maybe (Beam Duration))
openBeam = beams '[' BeamOpen

closeBeam :: GenParser Char st (Maybe (Beam Duration))
closeBeam = beams ']' BeamClose

duration :: GenParser Char st Duration
duration = do
  d <- oneOf "ZYTSEQHWBF"
  return $ case d of
   'Z' -> Semihemidemisemiquaver
   'Y' -> Hemidemisemiquaver
   'T' -> Demisemiquaver
   'S' -> Semiquaver
   'E' -> Quaver
   'Q' -> Crotchet
   'H' -> Minim
   'W' -> Semibreve
   'B' -> Breve
   'F' -> Fermata
   _   -> error $ "Invalid duration " ++ (show d)

rest :: GenParser Char st TabWord
rest = do
  rs <- rhythmSign
  try $ many1 space
  return $ Rest rs

barLine :: GenParser Char st TabWord
barLine = do
  leftRpt  <- option False $ do { char ':'; return True }
  line     <- (try dbl) <|> (try sgl)
  rightRpt <- option False $ do { char ':'; return True }
  nonC     <- option Counting $ do { char '0'; return NotCounting }
  dash     <- option NotDashed $ do { char '='; return Dashed }
  rep      <- option Nothing addition

  if line == "|"
    then return $ BarLine $ SingleBar (combineRepeat leftRpt rightRpt) rep dash nonC
    else return $ BarLine $ DoubleBar (combineRepeat leftRpt rightRpt) rep dash nonC

  where
    sgl     = string "|"
    dbl     = string "||"
    addition = (try reprise) <|> (try nthTime)
    reprise = between (char '(') (char ')') $ do
      string "T=:\\R"
      return $ Just Reprise
    nthTime = between (char '(') (char ')') $ do
      string "T+:\\"
      time <- int
      return $ Just $ NthTime time
    combineRepeat True True  = Just RepeatBoth
    combineRepeat True False = Just RepeatLeft
    combineRepeat False True = Just RepeatRight
    combineRepeat _ _        = Nothing

meter :: GenParser Char st TabWord
meter = do
  char 'M'
  between (char '(') (char ')') $ do
    m1  <- do { m <- digit <|> mensurSign; return $ Just m }
    c1  <- cuts
    p1  <- prol
    arr <- option Nothing $ do { a <- char ':' <|> char ';'; return $ Just a }
    m2  <- option Nothing $ do { t <- digit <|> mensurSign; return $ Just t }
    c2  <- cuts
    p2  <- prol

    return $ mkMS arr m1 c1 p1 m2 c2 p2

  where
    mensurSign = char 'O' <|> char 'C' <|> char 'D'
    cuts       = option Nothing $ do { cs <- many1 $ char '/'; return $ Just cs }
    prol       = option Nothing $ do { ds <- char '.'; return $ Just ds }

    mkMS (Just arrangement) mensur1 cut1 prolation1 mensur2 cut2 prolation2
      | arrangement == ':' = Meter $ VerticalMeterSign (mkMensur mensur1 cut1 prolation1) (mkMensur mensur2 cut2 prolation2)
      | arrangement == ';' = Meter $ HorizontalMeterSign (mkMensur mensur1 cut1 prolation1) (mkMensur mensur2 cut2 prolation2)
    mkMS (Just _) _ _ _ _ _ _ = error "Invalid meter arrangement symbol"
    mkMS Nothing mensur1 cut1 prolation1 _ _ _
      = Meter $ SingleMeterSign (mkMensur mensur1 cut1 prolation1)

    mkMensur (Just 'O') Nothing      (Just '.') = PerfectMajor
    mkMensur (Just 'O') Nothing      Nothing    = PerfectMinor
    mkMensur (Just 'C') Nothing      (Just '.') = ImperfectMajor
    mkMensur (Just 'C') Nothing      Nothing    = ImperfectMinor
    mkMensur (Just 'O') (Just ['/']) (Just '.') = HalfPerfectMajor
    mkMensur (Just 'O') (Just ['/']) Nothing    = HalfPerfectMinor
    mkMensur (Just 'C') (Just ['/']) (Just '.') = HalfImperfectMajor
    mkMensur (Just 'C') (Just ['/']) Nothing    = HalfImperfectMinor
    mkMensur (Just 'D') Nothing      (Just '.') = HalfImperfectMajor
    mkMensur (Just 'D') Nothing      Nothing    = HalfImperfectMinor
    mkMensur (Just ms)  _            _          =
      Beats $ ((read [ms]) :: Int)
    mkMensur Nothing    _            _          = error "Invalid mensuration sign"

note :: GenParser Char st Note
note = trebNote <|> bassNote

trebNote :: GenParser Char st Note
trebNote = do
  f   <- fret
  c   <- course
  fng <- fingering
  orn <- ornament
  art <- articulation
  con <- connecting

  return $ Note c f fng orn art con

fret :: GenParser Char st Fret
fret = do
  f <- oneOf ['a'..'n']
  return $ case f of
    'a' -> A
    'b' -> B
    'c' -> C
    'd' -> D
    'e' -> E
    'f' -> F
    'g' -> G
    'h' -> H
    'i' -> I
    'j' -> J
    'k' -> K
    'l' -> L
    'm' -> M
    'n' -> N
    _   -> error $ "Invalid fret symbol: " ++ (show f)

course :: GenParser Char st Course
course = do
  c <- oneOf "123456"
  return $ case c of
    '1' -> One
    '2' -> Two
    '3' -> Three
    '4' -> Four
    '5' -> Five
    '6' -> Six
    _   -> error $ "Invalid course number: " ++ (show c)

bassNote :: GenParser Char st Note
bassNote = do
  char 'X'

  f   <- fret
  c   <- bassCourse
  fng <- fingering
  orn <- ornament
  art <- articulation
  con <- connecting

  return $ Note c f fng orn art con

bassCourse :: GenParser Char st Course
bassCourse = do
  c <- many (char '/')
  return $ Bass $ (length c) + 1

fingering :: GenParser Char st (Maybe Fingering)
fingering = option Nothing $ abbr <|> full
  where
    abbr = do
      f <- fingeringDots <|> rhThumb <|> rhFinger2
      return $ Just $ Fingering Nothing f Nothing
    full = do
      between (char '(') (char ')') $ do
        char 'F'
        h <- hand
        f <- finger <|> fingeringDots <|> rhThumb <|> rhFinger2
        a <- attachment

        return $ Just $ Fingering h f a

hand :: GenParser Char st (Maybe Hand)
hand = option Nothing (left <|> right)
  where
    left  = do { char 'l'; return $ Just LH }
    right = do { char 'r'; return $ Just RH }

finger :: GenParser Char st Finger
finger = do
  n <- oneOf "1234"
  return $ case n of
    '1' -> FingerOne
    '2' -> FingerTwo
    '3' -> FingerThree
    '4' -> FingerFour
    _   -> error $ "Invalid finger number: " ++ (show n)

fingeringDots :: GenParser Char st Finger
fingeringDots = do
  d <- many1 $ char '.'
  return $ case (length d) of
    1 -> FingerOne
    2 -> FingerTwo
    3 -> FingerThree
    4 -> FingerFour
    _ -> error $ "Invalid fingering dots: " ++ (show d)

rhThumb :: GenParser Char st Finger
rhThumb = char '!' >> (notFollowedBy $ char '!') >> return Thumb

rhFinger2 :: GenParser Char st Finger
rhFinger2 = string "!!" >> return FingerTwo

attachmentNoColon :: GenParser Char st (Maybe Attachment)
attachmentNoColon = option Nothing $ do
  pos <- oneOf "12345678"
  return $ Just $ case pos of
    '1' -> PosAboveLeft
    '2' -> PosAbove
    '3' -> PosAboveRight
    '4' -> PosLeft
    '5' -> PosRight
    '6' -> PosBelowLeft
    '7' -> PosBelow
    '8' -> PosBelowRight
    _   -> error $ "Invalid attachment position: " ++ (show pos)

attachment :: GenParser Char st (Maybe Attachment)
attachment = char ':' >> attachmentNoColon

--modifier :: GenParser Char st

ornament :: GenParser Char st (Maybe Ornament)
ornament = option Nothing $ do
  between (char '(') (char ')') $ do
    char 'O'
    t   <- oneOf "abcdefghijkl"
    s   <- option Nothing $ int >>= return . Just
    pos <- option Nothing $ attachment
    return $ Just $ case t of
      'a' -> OrnA s pos
      'b' -> OrnB s pos
      'c' -> OrnC s pos
      'd' -> OrnD s pos
      'e' -> OrnE s pos
      'f' -> OrnF s pos
      'g' -> OrnG s pos
      'h' -> OrnH s pos
      'i' -> OrnI s pos
      'j' -> OrnJ s pos
      'k' -> OrnK s pos
      'l' -> OrnL s pos
      _   -> error $ "Invalid ornament: " ++ (show t)

articulation :: GenParser Char st (Maybe Articulation)
articulation = option Nothing (ensemble <|> separee)

ensemble :: GenParser Char st (Maybe Articulation)
ensemble = option Nothing $ do
  between (char '(') (char ')') $ do
    char 'E'
    return $ Just Ensemble

separee :: GenParser Char st (Maybe Articulation)
separee = option Nothing $ do
  between (char '(') (char ')') $ do
    char 'S'
    dir <- direction
    pos <- position

    return $ Just $ Separee dir pos

    where
      direction = do
        d <- oneOf "ud"
        return $ case d of
          'u' -> SepareeUp
          'd' -> SepareeDown
          _   -> error $ "Invalid separee direction: " ++ (show d)
      position = option Nothing $ do
        char ':'
        p <- oneOf "lr"
        return $ Just $ case p of
          'l' -> SepareeLeft
          'r' -> SepareeRight
          _   -> error $ "Invalid separee position: " ++ (show p)

connecting :: GenParser Char st (Maybe Connecting)
connecting = option Nothing (slur <|> straight <|> curved)

slur :: GenParser Char st (Maybe Connecting)
slur = option Nothing $ do
  between (char '(') (char ')') $ do
    char 'C'
    dir <- direction

    return $ Just (Slur dir)

    where
      direction = option SlurDown $ do
        d <- oneOf "ud"
        return $ case d of
          'u' -> SlurUp
          'd' -> SlurDown
          _   -> error $ "Invalid slur direction: " ++ (show d)

straight :: GenParser Char st (Maybe Connecting)
straight = option Nothing $ do
  between (char '(') (char ')') $ do
    char 'C'
    cid <- int
    pos <- attachment
    if cid < 0
      then return $ Just (StraightFrom cid pos)
      else return $ Just (StraightTo cid pos)

curved :: GenParser Char st (Maybe Connecting)
curved = option Nothing $ do
  between (char '(') (char ')') $ do
    char 'C'
    cid <- int
    char ':'
    dir <- oneOf "ud"
    pos <- attachmentNoColon

    return $ mkCurved cid dir pos

    where
      mkCurved i 'u' p | i >= 0 = Just (CurvedUpFrom i p)
                       | i < 0  = Just (CurvedUpTo i p)
      mkCurved i 'd' p | i >= 0 = Just (CurvedDownFrom i p)
                       | i < 0  = Just (CurvedDownTo i p)
      mkCurved _  d  _ = error $ "Invalid curved connecting line direction: " ++ (show d)

comment :: GenParser Char st TabWord
comment = do
  c <- braces (many1 anyChar)
  return $ Comment c
  where
    braces = between (char '{') (char '}')

systemBreak :: GenParser Char st TabWord
systemBreak = string "{^}" >> return SystemBreak

pageBreak :: GenParser Char st TabWord
pageBreak = string "{>}{^}" >> return PageBreak

parseTabcode :: String -> Either ParseError TabCode
parseTabcode = parse tablature ""

parseTabcodeStdIn :: IO TabCode
parseTabcodeStdIn = getContents >>= (return . parseTabcode) >>= either reportErr return

parseTabcodeFile :: FilePath -> IO TabCode
parseTabcodeFile fileName = parseFromFile tablature fileName >>= either reportErr return

reportErr :: ParseError -> IO a
reportErr err = do
  hPutStrLn stderr $ "Error: " ++ show err
  exitFailure
