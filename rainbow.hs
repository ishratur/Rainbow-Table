import RainbowAssign
import qualified Data.Map as Map
import Data.Maybe

--Global Variable
pwLength, nLetters, width, height :: Int
filename :: FilePath
pwLength = 5
nLetters = 18
width = 60
height = 800

filename = "table.txt"  -- filename to store the table



toBase::Int->Int->[Int]
toBase n 0 = []
toBase n counter = (n `mod` nLetters) : toBase ( n `div` nLetters) (counter-1)
--Reverse the list
revBase :: Int->Int->[Int]
revBase n counter=reverse (toBase n counter)


--Converts the Int to a string using corresponding ASCII value
--map will apply toLetter function to every list element
pwReduce :: Hash -> Passwd
pwReduce x=map(toLetter)(revBase (fromEnum x) counter) 
 where counter=pwLength
 
--Building the table
rainbowTable' :: Int->[Passwd]->[(Hash,Passwd)]
rainbowTable' wid []= []
rainbowTable' wid (x:xs)= [((newHash wid x),x)] ++ rainbowTable' wid (xs)
   

rainbowTable :: Int -> [Passwd] -> Map.Map Hash Passwd
rainbowTable wid xs = Map.fromList (rainbowTable' wid (xs))

--construct a function that will do recursive calls on defined number

testRecursion:: Int->Passwd->Passwd
testRecursion 0 pas=pas
testRecursion a pas= pwReduce(pwHash pas) ++ testRecursion (a-1) (pwReduce(pwHash pas))

newHash::Int->Passwd->Hash
newHash wid pass=pwHash(drop((length(testRecursion wid pass))-pwLength)(testRecursion wid pass))

main :: IO ()
main = do
  generateTable
  res <- test2 10000
  print res



 -------------------------Reverse Hash----------------------------

searchPassword :: Map.Map Hash Passwd -> Int -> Hash ->Hash->Maybe Passwd

searchPassword table (-1) hashVal hash= Nothing
searchPassword table wid hashVal hash= case (Map.lookup hashVal table) of
 

 Nothing -> searchPassword table (wid-1) (pwHash (pwReduce hashVal)) hash  
 Just val -> checkHash widX hash val
 where widX=width



checkHash::Int->Hash->Passwd->Maybe Passwd
checkHash (-1) originalHash val= Nothing
checkHash wid originalHash val 
 |pwHash(val) == originalHash = Just val
 |otherwise= checkHash (wid-1) originalHash (pwReduce (pwHash val))


findPassword :: Map.Map Hash Passwd -> Int -> Hash ->Maybe Passwd
findPassword table wid hashVal = searchPassword table wid hashVal hashVal 

-----------------------------------------------------------------

generateTable :: IO ()
generateTable = do
  table <- buildTable rainbowTable nLetters pwLength width height
  writeTable table filename
test1 = do
  table <- readTable filename
  return (Map.lookup (1192807752) table)


test2 :: Int -> IO ([Passwd], Int)
test2 n = do
  table <- readTable filename
  pws <- randomPasswords nLetters pwLength n
  let hs = map pwHash pws
  let result = Data.Maybe.mapMaybe (findPassword table width) hs
  return (result, length result)

