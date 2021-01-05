module Scrabble where

import Data.Monoid

-- Exercise 3
newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

scores :: [(Char,Int)]
scores =
  [
    ('A',1),('E',1),('I',1),('L',1),('N',1),('O',1),('R',1),('S',1),('T',1),('U',1),
    ('D',2),('G',2),
    ('B',3),('C',3),('M',3),('P',3),
    ('F',4),('H',4),('V',4),('W',4),('Y',4),
    ('K',5),
    ('J',8),('X',8),
    ('Q',10),('Z',10)
  ]

getScore :: Score -> String
getScore (Score s) = s

score :: Char -> Score
score c = maybe 0 Score $ lookup (toUpper c) scores

instance Monoid Score where
    mempty = Score 0
    mappend = (+)


scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

scoreString :: String -> Score
scoreString = mconcat . fmap score
