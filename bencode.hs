--Bencoding Library
--Alex Midlash
--2012

--atoms:
--  string	LEN:[char]
--  int		i[NUM]e
--
--containers:
--  list	l[BENC]e
--  dict	d[BENCSTR BENCVAL]e
import qualified Data.Map as Map

data BEVal 	=  BEInt Int | BEStr String | BEDict (Map.Map String BEVal)  | BEList [BEVal]
		deriving (Show)
			    
{-slurpBEValue :: String -> BEValue
slurpBEValue (x:xs) = BEString (x:' ':xs)-}

subString :: String -> Int -> Int -> String
subString hay start len = take len $ drop start hay

first (a,_) = a
second (_,b) = b
apply f a = f a
-- i1234e5hello
-- be functions should return a tuple. (VALUE, endposition)
slurpBEInt :: String -> Int -> (BEVal, Int)
slurpBEInt (_:xs) pos = 
	let str = takeWhile (/= 'e') (drop pos xs) 	--drop first char(i), read till e
	in (BEInt$read str, pos + length str + 2)
	
slurpBEStr :: String -> Int -> (BEVal, Int)
slurpBEStr str pos = 
	let 
	s = drop pos str
	a = takeWhile (/= ':') s
	b = 1 + length a
	c = subString s b (read a :: Int)
	in (BEStr$c, pos + b+read a) 

slurpBEList :: String ->  Int -> ([BEVal], Int) --expects pos to be beyond the opening l
slurpBEList str pos =
	let (barr,end) = slurpBEVal str pos 
	    (next,end2)= if str !! end == 'e' then ([],end+1) else slurpBEList str end
	in ((barr : next), end2)


--packBEDict :: [BEVal] -> [ (BEVal,BEVal) ]

packBEList :: ([BEVal], Int) -> (BEVal, Int)
packBEList (val, end) = (BEList val, end)

slurpBEVal :: String -> Int -> (BEVal, Int)
slurpBEVal str pos  
	| x `elem` ['0'..'9'] = (slurpBEStr str pos)
	| x == 'i' = (slurpBEInt str pos)
	| x == 'l' = packBEList$slurpBEList str (pos+1)
	| x == 'd' = packBEList$slurpBEList str (pos+1)
	where x = str !! pos


main = do
	s <- readFile "torrent.torrent"
	print$slurpBEVal s 0
