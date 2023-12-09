module Day07 (solveFrom) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Attoparsec.Text as A
import Helpers (quickParseT)
import Data.List (sort, sortOn, sortBy, group)
import Data.Char
import Data.Ord (comparing, Down(Down))

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace  deriving (Show, Eq, Ord, Enum)
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
charToCard 'T' = Ten
charToCard 'A' = Ace
charToCard 'K' = King
charToCard 'J' = Jack
charToCard 'Q' = Queen
charToCard card 
    | isDigit card && card /= '1' = toEnum (digitToInt card - 2)
    | otherwise = error "Invalid card!"

cardChars :: [Char]
cardChars = "23456789AKJQT"

handsP :: Parser [Hand]
handsP = handP `sepBy` endOfLine

handP :: Parser Hand
handP = Hand <$> count 5 cardP <*> (space *> decimal)

cardP :: Parser Card
cardP = charToCard <$> satisfy (inClass cardChars)

-- Specifics
instance Ord JCard where
    compare (JCard c1) (JCard c2) = comparing fromJester c1 c2
        where fromJester Jack = -1
              fromJester x = fromEnum x

toNormalHand :: Hand -> NormalHand
toNormalHand (Hand cards _) = NormalHand (getNormalType cards) cards

toJokerHand :: Hand -> JokerHand
toJokerHand (Hand cards _) = JokerHand (getJokerType cards) (JCard <$> cards)

getNormalType :: [Card] -> HandType
getNormalType = sortBy (comparing Down) . map length . group . sort

getJokerType :: [Card] -> HandType
getJokerType cards = case getNormalType noJokers of
                        (x:xs) -> (x+numJokers):xs
                        [] -> [numJokers]
    where noJokers = filter (/= Jack) cards
          numJokers = 5 - length noJokers


winnings :: Ord a => (Hand -> a) -> [Hand] -> Int
winnings f = sum . zipWith (*) [1..] . map bid . sortOn f

part1 :: [Hand] -> Int
part1 = winnings toNormalHand

part2 :: [Hand] -> Int
part2 = winnings toJokerHand
