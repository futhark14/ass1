-- DO NOT MODIFY THE FOLLOWING LINES

module Huffman(HuffmanTree, characterCounts, huffmanTree, codeTable, encode, compress, decompress) where

import Table
import PriorityQueue
import Test.HUnit

import Data.List
import Data.Maybe
{- a bit code (of a character or string) is represented by a list of Booleans
   INVARIANT:
     the bit code is a concatenation of (0 or more) valid code words for some Huffman tree
 -}
type BitCode = [Bool]

-- END OF DO NOT MODIFY ZONE
--------------------------------------------------------------------------------

-- Erdem Garip, erga4829, erdem.garip.4829@student.uu.se

{- characterCounts str
   RETURNS: A Table that maps each character that occurs in the str to the number of times the character occurs in the str
   EXAMPLE: 
   characterCounts "test"  ==  T [('e',1),('s',1),('t',2)]
   characTerCounts ""      ==  T []
 -}
characterCounts :: String -> Table Char Int
characterCounts list = characterCountsAux (mySort list) Table.empty


{-characterCountsAux str acc 
  RETURNS: The acc that maps each character that occurs in the str to the number of times the character occurs in the str
  PRE: str is sorted
  EXAMPLE:
  characterCounts "test" == characTerCountsAux "estt" Table.empty ==  T [('e',1),('s',1),('t',2)]
  characTerCounts ""     == characterCountsAux ""     Table.empty ==  T []
-}
characterCountsAux :: String -> Table Char Int -> Table Char Int
--variant length of str
characterCountsAux []         acc = acc
characterCountsAux str@(x:_) acc  = characterCountsAux (myTrim x str) (Table.insert acc x (myFilter x str))

{- mySort list
  RETURNS: sorted list
  EXAMPLE: mySort "test" == "estt"
-}
mySort :: String -> String
mySort = Data.List.sort

{- myFilter char list
  RETURNS: The frequency count of the Char x occuring in the String xs
  EXAMPLE: myFilter 't' "test" == 2
           myFilter 'a' "test" == 0
-}
myFilter :: Char -> String -> Int
myFilter x xs = length $ filter (==x) xs

{- myTrim x xs 
  RETURNS: a new list where elements x is removed from the list xs
  PRE: sorted list
  EXAMPLE: myTrim 'e' "eest" == "st"
  -}
myTrim :: Char -> String -> String
myTrim x xs = drop (myFilter x xs) xs


{- HuffmanTree
   RETURNS: A HuffmanTree which is a full binary tree with two possible cases: Leaf or Node
   Leaf (Char, Int) represents a char and the total number of its occurances
   Node Int HuffmanTree HuffmanTree representes the non-trivial case where
  
   PRE: Int is always positive.
   POST: A non-empty HuffmanTree. This was a design choice suggested by a teaching-assistent.
   INVARIANTS: A leaf with lower char occurance count can only exist in a higher depth compared
   all other leafs on lower depth.
   Int is the sum of the char's occurances in its sub-trees.
-}
data HuffmanTree = Leaf (Char, Int) 
                 | Node Int HuffmanTree HuffmanTree deriving (Show, Eq, Ord)



{- huffmanTree table
  Creates a HuffmanTree based on the table
  PRE:  table maps each key to a positive value
  RETURNS: a Huffman tree based on the character counts in table
  POST: A non-empty HuffmanTree, an empty string is mapped as in next example
  EXAMPLE: huffmanTree $ characterCounts ""       == Leaf (' ',0)
  EXAMPLE: huffmanTree $ characterCounts "t"      == Leaf ('t',1)
  EXAMPLE: huffmanTree $ characterCounts "test"   == Node 4 (Leaf ('t',2)) (Node 2 (Leaf ('e',1)) (Leaf ('s',1)))
 -}
huffmanTree :: Table Char Int -> HuffmanTree
huffmanTree table | emptyChecker $ tableToPQ $ table = Leaf (' ', 0)
huffmanTree table                                    = _PQtoSortedHT $ tableToPQ table

{- PriorityQueue HuffmanTree
   A helper function to handle the empty table(empty string)
   RETURNS: True if priorityQueue is empty, False otherwise  
-}
emptyChecker :: PriorityQueue HuffmanTree -> Bool
emptyChecker priorityQueue | PriorityQueue.is_empty priorityQueue = True
                           | otherwise                            = False


{- tableToPQ table
   Iterates over a table and creates a PriorityQueue of type HuffmanTree
   RETURNS: A PriorityQueue of type HuffmanTree based on table where each Leaf is the key-value pair
   EXAMPLE: tableToPQ $ characterCounts ""     == BinoHeap []
   EXAMPLE: tableToPQ $ characterCounts "test" == BinoHeap [Node 0 2 (Leaf ('t',2)) [],Node 1 1 (Leaf ('e',1)) [Node 0 1 (Leaf ('s',1)) []]]
-}
tableToPQ :: Table Char Int -> PriorityQueue HuffmanTree
tableToPQ table = Table.iterate table toPQfunction PriorityQueue.empty
  where
    toPQfunction = (\priorityQueue (key, value) -> PriorityQueue.insert priorityQueue (Leaf (key, value), value))


{- _PQtoSortedHT priorityQueue
  Converts the priorityQueue to a huffmanTree
  RETURNS: A huffmanTree based on the character counts in priorityQueue
  EXAMPLE: _PQtoSortedHT $ tableToPQ $ characterCounts "test" == Node 4 (Leaf ('t',2)) (Node 2 (Leaf ('e',1)) (Leaf ('s',1)))
-}
_PQtoSortedHT :: PriorityQueue HuffmanTree -> HuffmanTree
_PQtoSortedHT rawPQ | isProcessed = trivialPQ
                    | otherwise   = _PQtoSortedHT $ nonTrivialPQ
  where
-- variant: length of rawPQ
    isProcessed  = PriorityQueue.is_empty $ snd $ PriorityQueue.least rawPQ
    subPQ1       = PriorityQueue.least rawPQ
    subPQ2       = PriorityQueue.least $ snd $ (PriorityQueue.least rawPQ)
    trivialPQ    = fst $ fst $ PriorityQueue.least rawPQ
    nonTrivialPQ = PriorityQueue.insert (snd $ subPQ2)
                   (Node ((snd $ fst subPQ1) + (snd $ fst $ subPQ2)) (fst $ fst $ subPQ1) (fst $ fst subPQ2), --sum, tree, tree
                   (snd $ fst subPQ1) + (snd $ fst $ subPQ2)) --sum



{- codeTable huffmanTree
   Creates a table based on huffmanTree through the intermediary functions
   NOTE: tree variable is eta-reduced in the function calls
   RETURNS: a table that maps each character in huffmanTree to its Huffman code
  EXAMPLE: codeTable $ huffmanTree $ characterCounts ""     == T [(' ',[])]
  EXAMPLE: codeTable $ huffmanTree $ characterCounts "t"    == T [('t',[])]
  EXAMPLE: codeTable $ huffmanTree $ characterCounts "test" == T [('t',[False]),('e',[True,False]),('s',[True,True])]
 -}
codeTable :: HuffmanTree -> Table Char BitCode
codeTable = makeTable . makeTuples
  where
    makeTuples = _HTtoTuples []
    makeTable  = tuplesToTable Table.empty


{- _HTtoTuples bitCodeList huffmanTree
   Traverses through the huffmanTree and appends True or False depending on left or right branching
   RETURNS: A list of tuples where the accumulator bitCodeList denotes the BitCode path from huffmanTree's root to the Char
   PRE: bitCodeList is an empty list
   EXAMPLE: _HTtoTuples [] (huffmanTree $ characterCounts "test") == [('t',[False]),('e',[True,False]),('s',[True,True])]
-}
_HTtoTuples :: BitCode -> HuffmanTree -> [(Char, BitCode)]
-- variant: huffmanTree
_HTtoTuples bc (Leaf (char, _))            = [(char, bc)]
_HTtoTuples bc (Node _ leftTree rightTree) = _HTtoTuples (bc ++ [False]) leftTree ++ _HTtoTuples  (bc ++ [True]) rightTree


{- tuplesToTable accTable bitCodeList
   Adds elements to accTable until the bitCodeList is exhausted
   RETURNS: The bitCodeList converted to to a Table by the accumulator accTable
   PRE: accTable is empty
   EXAMPLE: tuplesToTable Table.empty ([('t',[False]),('e',[True,False]),('s',[True,True])]) == T [('t',[False]),('e',[True,False]),('s',[True,True])]
-}
tuplesToTable :: Table Char BitCode -> [(Char, BitCode)] -> Table Char BitCode
tuplesToTable accTable []     = accTable
tuplesToTable accTable (x:xs) = tuplesToTable updatedTable xs
  where 
    updatedTable = Table.insert accTable (fst x) (snd x)

{- encode huffmanTree string
   PRE: All characters in string appear in huffmanTree
   RETURNS: the concatenation of the characters of string encoded using the Huffman code table of huffmanTree.
   EXAMPLE: encode (huffmanTree $ characterCounts "xxx")   ("xxx") == []
   EXAMPLE: encode (huffmanTree $ characterCounts "")      ("")    == []
   EXAMPLE: encode (huffmanTree $ characterCounts "test") ("test") == [False,True,False,True,True,False]
 -}
encode :: HuffmanTree -> String -> BitCode
encode huffmanTree []      = []
encode huffmanTree (x:xs)  = processedHead ++ encode huffmanTree xs
  where 
    processedHead = encodeAux (codeTable huffmanTree) x

{-encodeAux table char
  Helper function that abstracts away the complexity of Maybe a -> a conversion from encode
  PRE: The char must be in table, otherwise the Exception: Maybe.fromJust: Nothing is invoked
  RETURNS: The BitCode of a char based on the table
  EXAMPLE: encodeAux  (codeTable $ (huffmanTree $ characterCounts "test")) 't' == [False]
-}
encodeAux :: Table Char BitCode -> Char -> BitCode
encodeAux huffmanCode char = Data.Maybe.fromJust $ Table.lookup huffmanCode char


{- compress s
  Performs function calls and creates a tuple out of them
   RETURNS: (a Huffman tree based on s, the Huffman coding of s under this tree)
   EXAMPLES: compress ""    == (Leaf (' ',0),[])
   EXAMPLE: compress "xxx"  == (Leaf ('x',3),[])
   EXAMPLE: compress "test" == (Node 4 (Leaf ('t',2)) (Node 2 (Leaf ('e',1)) (Leaf ('s',1))),[False,True,False,True,True,False])
 -}
compress :: String -> (HuffmanTree, BitCode)
compress string = (theTree, theEncoding)
  where
    theTree     = huffmanTree $ characterCounts string
    theEncoding = encode theTree string

{- decompress h bits
   Performs a function call to decompressAux that traverses the huffmanTree
   PRE:  bits is a concatenation of valid Huffman code words for h
   RETURNS: the decoding of bits under h
   EXAMPLE: decompress (Leaf ('x', 3)) [] == "xxx"
   EXAMPLE: decompress (Leaf (' ',0))  [] ==  ""
   EXAMPLE: decompress decompress (Node 4 (Leaf ('t',2)) (Node 2 (Leaf ('e',1)) (Leaf ('s',1)))) ([False,True,False,True,True,False]) == "text"
 -}
decompress :: HuffmanTree -> BitCode -> String
decompress huffmanTree []      = decompressAuxA huffmanTree  
decompress huffmanTree bitCode = decompressAuxC huffmanTree huffmanTree bitCode


{-decompressAuxA huffmanLeaf
  Appends k to the list until v reaches zero
  RETURNS: The decoding of the huffmanLeaf, as a String
  EXAMPLE: decompressAuxA (Leaf ('x', 3)) == "xxx"
  EXAMPLE: decompressAuxA (Leaf (' ',0))  ==  ""
-}
decompressAuxA :: HuffmanTree -> String
--variant: v
decompressAuxA (Leaf (k, 0)) = []
decompressAuxA (Leaf (k,v))  = k : decompressAuxA (Leaf (k, v-1))


{-decompressAuxC persistentTree huffmanTree bitCode
  Traverses the persistentTree and branches depending on bitCode in order to create a String
  RETURNS: The decoding of bitCode under persistentTree
  EXAMPLE: decompressAuxC (Node 4 (Leaf ('t',2)) (Node 2 (Leaf ('e',1)) (Leaf ('s',1)))) 
                          (Node 4 (Leaf ('t',2)) (Node 2 (Leaf ('e',1)) (Leaf ('s',1)))) 
                          ([False,True,False,True,True,False])  == "test"
  NOTE: huffmanTree is not included in RETURNS because its sole purpose is to be used for pattern matching
-}
decompressAuxC :: HuffmanTree -> HuffmanTree -> BitCode -> String
--variant: length of (x:xs)
decompressAuxC persistentTree (Leaf (k, v)) []                               = [k]
decompressAuxC persistentTree (Leaf (k, v)) (x:xs)                           = [k] ++ decompressAuxC persistentTree persistentTree (x:xs)
decompressAuxC persistentTree (Node _ leftTree rightTree) (x:xs) | x == True = decompressAuxC persistentTree rightTree xs
                                                                 | otherwise = decompressAuxC persistentTree leftTree xs




--------------------------------------------------------------------------------
-- Test Cases
-- You may add your own test cases here:
-- Follow the pattern and/or read about HUnit on the interwebs.
--------------------------------------------------------------------------------

-- characterCounts
test1 = TestCase $ assertEqual "characterCounts"
            (Just 7) (Table.lookup (characterCounts "this is an example of a huffman tree") ' ')

-- codeTable
-- while the precise code for ' ' may vary, its length (for the given example string) should always be 3 bits
test2 = TestCase $ assertEqual "codeTable"
            3 (maybe (-1) length (Table.lookup (codeTable (huffmanTree (characterCounts "this is an example of a huffman tree"))) ' '))

-- compress
-- while the precise code for the given example string may vary, its length should always be 135 bits
test3 = TestCase $ assertEqual "compress"
            135 (length (snd (compress "this is an example of a huffman tree")))

-- decompress
test4 =
    let s = "this is an example of a huffman tree"
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

test5 =
    let s = "xxx"
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

test6 =
    let s = ""
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

-- for running all the tests
runtests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6]
