module Day07 (solveFrom) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Attoparsec.Text as A
import Helpers (quickParseT)
import Data.List (sort, sortOn, group)


data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | J | Q | K | A  deriving (Show, Eq, Ord)
newtype JCard = JCard Card deriving (Show, Eq)


data Hand = Hand {cards::[Card], bid::Int} deriving (Show)

data NormalHand = NormalHand HandType [Card] deriving (Eq, Ord)
data JokerHand = JokerHand HandType [JCard] deriving (Eq, Ord)

type HandType = [Int]

-- Boilerplate
solveFrom :: FilePath -> IO (String, String)
solveFrom = fmap solve . T.readFile

solve :: T.Text -> (String, String)
solve txt = (show $ part1 hands, show $ part2 hands)
    where hands = quickParseT handsP txt

-- Parsing
charToCard :: Char -> Card
charToCard card = case card of
                    '2' -> Two
                    '3' -> Three
                    '4' -> Four
                    '5' -> Five
                    '6' -> Six
                    '7' -> Seven
                    '8' -> Eight
                    '9' -> Nine
                    'A' -> A
                    'K' -> K
                    'J' -> J
                    'Q' -> Q
                    'T' -> Ten
                    _   -> error "Invalid card!"

cardChars :: [Char]
cardChars = "23456789AKJQT"

handsP :: Parser [Hand]
handsP = handP `sepBy` endOfLine

handP :: Parser Hand
handP = Hand <$> (count 5 cardP) <*> (space *> decimal)

cardP :: Parser Card
cardP = charToCard <$> satisfy (inClass cardChars)

-- Specifics
instance Ord JCard where
    compare (JCard c1) (JCard c2) = case (c1, c2) of
                    (J, J) -> EQ
                    (J, _) -> LT
                    (_, J) -> GT
                    (x, y) -> compare x y

toNormalHand :: Hand -> NormalHand
toNormalHand (Hand cards _) = NormalHand (getNormalType cards) cards

toJokerHand :: Hand -> JokerHand
toJokerHand (Hand cards _) = JokerHand (getJokerType cards) (JCard <$> cards)

getNormalType :: [Card] -> HandType
getNormalType = reverse . sort . map length . group . sort

getJokerType :: [Card] -> HandType
getJokerType cards = case getNormalType noJokers of
                        (x:xs) -> (x+numJokers):xs
                        [] -> [5]
    where noJokers = filter (/= J) cards
          numJokers = 5 - length noJokers


winnings :: Ord a => (Hand -> a) -> [Hand] -> Int
winnings f = sum . zipWith (*) [1..] . map bid . sortOn f

part1 :: [Hand] -> Int
part1 = winnings toNormalHand

part2 :: [Hand] -> Int
part2 = winnings toJokerHand
