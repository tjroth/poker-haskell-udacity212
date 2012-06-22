import Data.List
import Control.Applicative


data HandRank = HandRank Integer Hand deriving (Eq, Show)

data CardSuit = Spade | Diamond | Club | Heart deriving (Show, Eq)

type CardRank =  Integer

type Card = (CardRank, CardSuit)

type Hand = [Card]

instance Ord CardSuit where
        compare _ _ = EQ

instance Ord HandRank where
	HandRank r1 h1 `compare` HandRank r2 h2 | r1 /= r2 = r1 `compare` r2		 
						| otherwise = arrangeHand h1 `compare` arrangeHand h2 


--------------------------------------------------------------------------
--Return a standard deck of cards without the jokers
standardDeck :: [Card]
standardDeck = (,) <$> [2..14] <*> [Spade, Diamond, Club, Heart]


--------------------------------------------------------------------------
--Main Function - takes a list of hands and returns the highest ranked hand
--poker :: [Hand] -> HandRank
poker hands = filter (== arrangeHand bestHand) arrangedHands -- maximum handRanks
        where
	 HandRank bestRank bestHand  = maximum $ map handRank hands
	 arrangedHands = map arrangeHand hands


-------------------------------------------------------------------------
--Takes a hand and returns a HandRank data type with the first paramater 
--the hand type rank and the second parameter as the hand
handRank :: Hand -> HandRank 
handRank hand 	| isStraight hand && isFlush hand  = HandRank 8 hand 
		| isKind 4 hand = HandRank 7 hand
		| isFullHouse hand = HandRank 6 hand
		| isFlush hand = HandRank 5 hand
		| isStraight hand = HandRank 4 hand
		| isKind 3 hand = HandRank 3 hand
		| isTwoPair hand = HandRank 2 hand
		| isKind 2 hand = HandRank 1 hand
		| otherwise = HandRank 0 hand

		
--------------------------------------------------------------------------
--The following functions determine if the given hand is of the specified
--hand type
isStraightFlush :: Hand -> Bool
isStraightFlush hand = isStraight hand && isFlush hand 

isStraight :: Hand -> Bool
isStraight hand = (maximum $ cardRanks hand) - (minimum $ cardRanks hand) == 4

isFlush :: Hand -> Bool
isFlush hand = (length . group $ map snd hand) == 1 

isFullHouse :: Hand -> Bool
isFullHouse hand = (isKind 3 hand) && (isKind 2 hand)

isKind :: Int -> Hand -> Bool
isKind num hand = (length $ filter (\x-> length x == num) $ group . cardRanks $ hand) > 0 

isTwoPair :: Hand -> Bool
isTwoPair hand = (length $ filter (\x-> length x == 2) $ group . cardRanks $ hand) == 2


-------------------------------------------------------------------------------
--Return list of card ranks for the hand.  Each rank is the number vallue  of
--the card, ie 1..
cardRanks :: Hand -> [CardRank]
cardRanks hand  | ranks hand == [14,5,4,3,2] = [5,4,3,2,1]
	  	| otherwise = ranks hand
	where
	 ranks = reverse . sort . map fst


-------------------------------------------------------------------------------------
--Arrange the hand so that hands of equal type can be compared ie a full house
--would have the three of a kind first and then the pair, high card hand would
--sort the hand from highest card to lowest
arrangeHand :: Hand -> [CardRank]
arrangeHand hand = concat $ map (\(r,h)-> h) $  reverse . sort $ map(\x-> (length x, x)) $ group . cardRanks $ hand




--Hands for testing
tk= [(9,Spade), (9,Diamond), (9,Club), (3,Club), (1,Club)]
fk = [(9,Spade), (9,Diamond), (9,Club), (9, Heart), (1,Club)]
sf = [(4,Spade), (5,Spade), (6,Spade), (7,Spade), (8,Spade)]
twok = [(4,Spade), (4,Diamond), (9, Heart), (7,Spade), (8,Spade)]
fh = [(4,Spade), (4,Diamond), (4, Heart), (3,Spade), (3, Diamond)]
hk1 = [(4,Spade), (11,Diamond), (1, Heart), (8,Spade), (5,Spade)]
hk2 = [(11,Spade), (8,Diamond), (2, Heart), (4,Spade), (5,Spade)]
tp = [(10,Spade), (10,Diamond), (1, Heart), (1,Spade), (5,Spade)]
fh2 = [(1,Spade), (10,Diamond), (1, Heart), (10,Spade), (10,Spade)]
tp2 = [(3,Spade), (10,Diamond), (3, Heart), (5,Spade), (10,Spade)]
st1 = [(3,Spade), (14,Diamond), (4, Heart), (5,Spade), (2,Spade)]
st2 = [(14,Spade), (10,Diamond), (11, Heart), (12,Spade), (13,Spade)]






