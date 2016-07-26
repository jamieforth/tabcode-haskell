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

import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Prelude hiding (words)
import           System.IO (hPutStrLn, stderr)
import           System.Exit (exitFailure)
import           TabCode
import           TabCode.Options
import           Text.Parsec (ParsecT,  getPosition)
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Number
import           Text.ParserCombinators.Parsec.Pos (SourcePos, sourceLine, sourceColumn, sourceName)

tablature :: ParseMode -> GenParser Char st TabCode
tablature mode = do
  rls   <- option [] rules
  words <- (tabword mode) `sepEndBy` (skipMany1 space)
  eof
  return $ TabCode rls (V.fromList words)

rules :: GenParser Char st [Rule]
rules = do
  -- FIXME We need to allow files to start with comments but also not
  -- suppress errors in incorrect <rules>
  rls <- option [] $ try $ between (string "{<rules>") (string "</rules>}") $ do
    many1 $ (try $ rule "notation") <|> (try $ rule "title") <|> (try $ rule "tuning_named")
      <|> (try $ rule "rhythm-font") <|> (try $ rule "pitch") <|> (try $ rule "bass_tuning")
      <|> (try $ rule "tuning") <|> (try $ rule "rhythm_noteheads")
  spaces
  return rls

rule :: String -> GenParser Char st Rule
rule r = do
  spaces
  nt <- between (string $ "<" ++ r ++ ">") (string $ "</" ++ r ++ ">") $ many1 $ noneOf "<"
  spaces
  return $ Rule r nt

tabword :: ParseMode -> GenParser Char st TabWord
tabword Strict     = (try rest) <|> (try barLine) <|> (try meter) <|> (try comment) <|> (try systemBreak) <|> (try pageBreak) <|> chord
tabword Permissive = (try rest) <|> (try barLine) <|> (try meter) <|> (try comment) <|> (try systemBreak) <|> (try pageBreak) <|> (try chord) <|> (try invalid)

endOfWord :: GenParser Char st ()
endOfWord = (lookAhead $ try (do { space; return () })) <|> eof--try (do { c <- try eof; unexpected (show c) } <|> return "")

invalid :: GenParser Char st TabWord
invalid = do
  pos <- getPosition
  wrd <- manyTill anyChar endOfWord
  if (length wrd) > 0
    then return $ Invalid (sourceName pos) (sourceLine pos) (sourceColumn pos) wrd
    else fail "FIXME Just consume any trailing whitespace"

chord :: GenParser Char st TabWord
chord = do
  pos <- getPosition
  rs <- option Nothing $ do { r <- rhythmSign; return $ Just r }
  ns <- uniqueNotes $ many1 note
  endOfWord
  return $ Chord (sourceLine pos) (sourceColumn pos) rs ns

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
  pos <- getPosition
  rs <- rhythmSign
  endOfWord
  return $ Rest (sourceLine pos) (sourceColumn pos) rs

barLine :: GenParser Char st TabWord
barLine = do
  pos <- getPosition
  leftRpt  <- option False $ do { char ':'; return True }
  line     <- (try dbl) <|> (try sgl)
  rightRpt <- option False $ do { char ':'; return True }
  nonC     <- option Counting $ do { char '0'; return NotCounting }
  dash     <- option NotDashed $ do { char '='; return Dashed }
  rep      <- option Nothing addition

  endOfWord
  if line == "|"
    then return $ BarLine (sourceLine pos) (sourceColumn pos) $ SingleBar (combineRepeat leftRpt rightRpt) rep dash nonC
    else return $ BarLine (sourceLine pos) (sourceColumn pos) $ DoubleBar (combineRepeat leftRpt rightRpt) rep dash nonC

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
  pos <- getPosition
  char 'M'
  char '('
  m1  <- do { m <- digit <|> mensurSign; return $ Just m }
  c1  <- cuts
  p1  <- prol
  arr <- option Nothing $ do { a <- char ':' <|> char ';'; return $ Just a }
  m2  <- option Nothing $ do { t <- digit <|> mensurSign; return $ Just t }
  c2  <- cuts
  p2  <- prol
  char ')'

  endOfWord
  return $ mkMS pos arr m1 c1 p1 m2 c2 p2

  where
    mensurSign = char 'O' <|> char 'C' <|> char 'D'
    cuts       = option Nothing $ do { cs <- many1 $ char '/'; return $ Just cs }
    prol       = option Nothing $ do { ds <- char '.'; return $ Just ds }

    mkMS pos (Just arrangement) mensur1 cut1 prolation1 mensur2 cut2 prolation2
      | arrangement == ':' = Meter (sourceLine pos) (sourceColumn pos) $ VerticalMeterSign (mkMensur mensur1 cut1 prolation1) (mkMensur mensur2 cut2 prolation2)
      | arrangement == ';' = Meter (sourceLine pos) (sourceColumn pos) $ HorizontalMeterSign (mkMensur mensur1 cut1 prolation1) (mkMensur mensur2 cut2 prolation2)
    mkMS _ (Just _) _ _ _ _ _ _ = error "Invalid meter arrangement symbol"
    mkMS pos Nothing mensur1 cut1 prolation1 _ _ _
      = Meter (sourceLine pos) (sourceColumn pos) $ SingleMeterSign (mkMensur mensur1 cut1 prolation1)

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
note = (try trebNote) <|> (try bassNote) <|> bassNoteOpenAbbr

unorderedPair :: GenParser Char st (Maybe a) -> GenParser Char st (Maybe a) -> GenParser Char st (Maybe a, Maybe a)
unorderedPair p q =
  p >>= \r -> case r of
                Just r' -> do { s <- q; return (r, s) }
                Nothing -> do { t <- q; u <- p; return (t, u) }

trebNote :: GenParser Char st Note
trebNote = do
  f    <- fret
  c    <- course
  fng  <- unorderedPair fingeringLeft fingeringRight
  orn  <- ornament
  art  <- articulation
  con  <- connecting

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

  f    <- fret
  c    <- bassCourse
  fng  <- unorderedPair fingeringLeft fingeringRight
  orn  <- ornament
  art  <- articulation
  con  <- connecting

  return $ Note c f fng orn art con

bassCourse :: GenParser Char st Course
bassCourse = do
  c <- many (char '/')
  return $ Bass $ (length c) + 1

bassNoteOpenAbbr :: GenParser Char st Note
bassNoteOpenAbbr = do
  char 'X'

  c    <- int
  fng  <- unorderedPair fingeringLeft fingeringRight
  orn  <- ornament
  art  <- articulation
  con  <- connecting

  return $ Note (Bass c) A fng orn art con

fingeringLeft :: GenParser Char st (Maybe Fingering)
fingeringLeft = option Nothing $ try $ do
  string "(F"
  optional $ char 'l'
  f <- finger
  a <- attachment
  char ')'
  return $ Just $ FingeringLeft f a

fingeringRight :: GenParser Char st (Maybe Fingering)
fingeringRight = option Nothing $ (try abbr) <|> (try full)
  where
    abbr = do
      f <- (try fingeringDots) <|> (try finger2Abbr) <|> (try rhThumb) <|> rhFinger2
      return $ Just $ FingeringRight f Nothing
    full = do
      string "(F"
      optional $ char 'r'
      f <- (try finger) <|> (try fingeringDots) <|> (try finger2Abbr) <|> (try rhThumb) <|> rhFinger2
      a <- attachment
      char ')'
      return $ Just $ FingeringRight f a

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
  if (length d) > 4
    then fail $ "Invalid fingering dots: " ++ (show d)
    else return $ case (length d) of
                    1 -> FingerOne
                    2 -> FingerTwo
                    3 -> FingerThree
                    4 -> FingerFour
                    _ -> error $ "Invalid fingering dots: " ++ (show d)

finger2Abbr :: GenParser Char st Finger
finger2Abbr = char ':' >> return FingerTwo

rhThumb :: GenParser Char st Finger
rhThumb = char '!' >> return Thumb

rhFinger2 :: GenParser Char st Finger
rhFinger2 = char '"' >> return FingerTwo

attachmentNoColon :: GenParser Char st Attachment
attachmentNoColon = do
  pos <- oneOf "12345678"
  return $ case pos of
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
attachment = option Nothing $ do
  char ':'
  a <- attachmentNoColon
  return $ Just a

ornament :: GenParser Char st (Maybe Ornament)
ornament = option Nothing $ (try abbr) <|> (try full)
  where
    abbr = do
      o <- oA1 <|> oB <|> oC1 <|> oC2 <|> oE <|> oF <|> oH
      return $ Just o

    oA1 = do { (try $ char ',')    ; return $ OrnA (Just 1) Nothing }
    oB  = do { (try $ string "(C)"); return $ OrnB Nothing Nothing }
    oC1 = do { (try $ char 'u')    ; return $ OrnC (Just 1) Nothing }
    oC2 = do { (try $ char '<')    ; return $ OrnC (Just 2) Nothing }
    oE  = do { (try $ char '#')    ; return $ OrnE Nothing Nothing }
    oF  = do { (try $ char 'x')    ; return $ OrnF Nothing Nothing }
    oH  = do { (try $ char '~')    ; return $ OrnH Nothing Nothing }

    full = do
      between (char '(') (char ')') $ do
        char 'O'
        t   <- oneOf "abcdefghijklm"
        s   <- optionMaybe int
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
          'm' -> OrnM s pos
          _   -> error $ "Invalid ornament: " ++ (show t)

articulation :: GenParser Char st (Maybe Articulation)
articulation = option Nothing $ separee <|> ensemble

ensemble :: GenParser Char st (Maybe Articulation)
ensemble = try $ do
  between (char '(') (char ')') $ do
    char 'E'
    return $ Just Ensemble

separee :: GenParser Char st (Maybe Articulation)
separee = try $ do
  between (char '(') (char ')') $ do
    char 'S'
    dir <- direction
    pos <- position

    return $ Just $ Separee dir pos

    where
      direction = option Nothing $ do
        d <- oneOf "ud"
        return $ Just $ case d of
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
    pos <- optionMaybe attachmentNoColon

    return $ mkCurved cid dir pos

    where
      mkCurved i 'u' p | i >= 0 = Just (CurvedUpFrom i p)
                       | i < 0  = Just (CurvedUpTo i p)
      mkCurved i 'd' p | i >= 0 = Just (CurvedDownFrom i p)
                       | i < 0  = Just (CurvedDownTo i p)
      mkCurved _  d  _ = error $ "Invalid curved connecting line direction: " ++ (show d)

comment :: GenParser Char st TabWord
comment = do
  pos <- getPosition
  char '{'
  notFollowedBy $ (try $ string "^}") <|> (try $ string ">}{^}")
  c <- manyTill anyChar (try $ char '}')
  endOfWord
  return $ Comment (sourceLine pos) (sourceColumn pos) c

systemBreak :: GenParser Char st TabWord
systemBreak = do
  pos <- getPosition
  string "{^}"
  endOfWord
  return $ SystemBreak (sourceLine pos) (sourceColumn pos)

pageBreak :: GenParser Char st TabWord
pageBreak = do
  pos <- getPosition
  string "{>}{^}"
  endOfWord
  return $ PageBreak (sourceLine pos) (sourceColumn pos)

parseTabcode :: TCOptions -> String -> Either ParseError TabCode
parseTabcode opts s = parse (tablature $ parseMode opts) "" s

parseTabcodeStdIn :: TCOptions -> IO TabCode
parseTabcodeStdIn opts = getContents >>= (return . (parseTabcode opts)) >>= either reportErr return

parseTabcodeFile :: TCOptions -> FilePath -> IO TabCode
parseTabcodeFile opts fileName = parseFromFile (tablature $ parseMode opts) fileName >>= either reportErr return

reportErr :: ParseError -> IO a
reportErr err = do
  hPutStrLn stderr $ "Error: " ++ show err
  exitFailure
