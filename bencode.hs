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
import Prelude hiding ((!!))
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.ByteString.Char8 as BS

data BEVal 	=  BEInt Int | BEStr BS.ByteString | BEDict (Map.Map BS.ByteString BEVal)  | BEList [BEVal]
		deriving (Show)
			    
(!!) = BS.index	--convenient alias

subString :: BS.ByteString -> Int -> Int -> BS.ByteString
subString hay start len = BS.take len $ BS.drop start hay

toBSInt :: BS.ByteString -> Int
toBSInt s = first$Maybe.fromJust$BS.readInt s

toBSList s = BS.groupBy (\a b -> True) s
first (a,_) = a
second (_,b) = b
apply f a = f a
-- i1234e5hello
-- be functions should return a tuple. (VALUE, endposition)
slurpBEInt :: BS.ByteString -> Int -> (BEVal, Int)
slurpBEInt x pos = 
	let str = BS.takeWhile (/= 'e') (BS.drop pos xs) 	--drop first char(i), read till e
	in (BEInt$toBSInt str, pos + BS.length str + 2)
	where xs = BS.drop 1 x
	
slurpBEStr :: BS.ByteString -> Int -> (BEVal, Int)
slurpBEStr str pos = 
	let 
	s = BS.drop pos str
	a = BS.takeWhile (/= (':' )) s
	b = 1 + BS.length a
	c = subString s b (toBSInt a)
	in (BEStr$c, pos + b+toBSInt a) 

slurpBEList :: BS.ByteString ->  Int -> ([BEVal], Int) --expects pos to be beyond the opening l
slurpBEList str pos =
	let (barr,end) = slurpBEVal str pos 
	    (next,end2)= if str !! end == 'e' then ([],end+1) else slurpBEList str end
	in ((barr : next), end2)


--packBEDict :: [BEVal] -> [ (BEVal,BEVal) ]

packBEList :: ([BEVal], Int) -> (BEVal, Int)
packBEList (val, end) = (BEList val, end)

slurpBEVal :: BS.ByteString -> Int -> (BEVal, Int)
slurpBEVal str pos  
	| x `elem` ['0'..'9'] = (slurpBEStr str pos)
	| x == 'i' = (slurpBEInt str pos)
	| x == 'l' = packBEList$slurpBEList str (pos+1)
	| x == 'd' = packBEList$slurpBEList str (pos+1)
	where x = str !! pos


main = do
	s <- BS.readFile "data/ubuntu.torrent"
	print$slurpBEVal s 0
