{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- DO NOT MODIFY THE FOLLOWING LINE
module CompLing(wordCount, adjacentPairs, pairsCount, neighbours, mostCommonNeighbour) where


import PandP      -- provide sample text to play with (variable austin)
import Test.HUnit -- provides testing framework
-- DO NOT CHANGE THESE TYPES
type Sentence = [String]
type Document = [Sentence]
type WordTally = [(String, Int)]
type Pairs = [(String, String)]
type PairsTally = [((String, String), Int)]

-- DO NOT CHANGE THE TYPE SIGNATURES FOR THESE FUNCTIONS

wordCount :: Document -> WordTally
wordCount document = wordCountAux (concat document) []

wordCountAux :: [String] -> WordTally -> WordTally
wordCountAux [] acc = acc 
wordCountAux (x:xs) [] = wordCountAux xs [(x, 1)]
wordCountAux (x:xs) ((k, v):kvs) | wordCount_checker x ((k, v):kvs) == True = wordCountAux xs (wordCount_increase x ((k, v):kvs))
                                 | otherwise = wordCountAux xs ((x, 1):(k, v):kvs)

wordCount_checker :: String -> WordTally -> Bool
wordCount_checker word [] = False
wordCount_checker word ((k, v):kvs) | word == k = True
                                    | otherwise = wordCount_checker word kvs

wordCount_increase :: String -> WordTally -> WordTally
wordCount_increase word ((k, v):kvs) | word == k = ((k, v+1):kvs)
                                     | otherwise = wordCount_increase word kvs
adjacentPairs :: Document -> Pairs
adjacentPairs document = ap_aux document []

ap_aux :: Document -> Pairs -> Pairs
ap_aux [] pairs = pairs
ap_aux (d:doc) pairs = ap_aux doc (makePairs d pairs)

makePairs :: Sentence -> Pairs -> Pairs
makePairs [] pairs = pairs
makePairs [x] pairs = pairs
makePairs (x:y:sentence) pairs = makePairs (y:sentence)  (pairs ++ [(x,y)])


initialPairs :: Document -> Pairs
initialPairs [] = []
initialPairs (d:doc) | length d > 1 = [head (makePairs d [])] ++ (initialPairs doc)
                     | otherwise = initialPairs doc


finalPairs :: Document -> Pairs
finalPairs [] = []
finalPairs (d:doc) | length d > 1 = [last (makePairs d [])] ++ (finalPairs doc)
                   | otherwise = finalPairs doc





pairsCount :: Pairs -> PairsTally
pairsCount pairs = pairsCountAux pairs []


pairsCountAux :: Pairs -> PairsTally -> PairsTally
pairsCountAux [] pts = pts
pairsCountAux ((p1, p2):pairs) [] = pairsCountAux pairs  [((p1, p2), 1)]
pairsCountAux ((p1, p2):pairs) pts = pairsCountAux pairs (insertPairs (p1, p2) pts [])

insertPairs :: (String, String) -> PairsTally -> PairsTally -> PairsTally
insertPairs (p1, p2) [] acc = ((p1, p2), 1):acc
insertPairs (p1, p2) (((k1, k2), v):pts) acc
    | p1 == k1 || p1 == k2 = (((k1, k2), v+1):acc) ++ pts
    | otherwise = insertPairs (p1, p2) pts (((k1, k2), v):acc)


{-
pairsCountAux :: Pairs -> PairsTally -> PairsTally
pairsCountAux [] pairsTally = pairsTally
-- pairsCountAux ((p1, p2):pairs) [] = [((p1, p2), 1)]
--Fel, pairsTally minskar med rekursion i nedanstående pattern matching och hamnar här som kodar det till första elementet
pairsCountAux ((p1, p2):pairs) [] = pairsCountAux pairs [((p1, p2), 1)]
pairsCountAux ((p1, p2):pairs) (((k1, k2), v):pairsTally)
    | pairsCountChecker (p1, p2) (((k1, k2), v):pairsTally) == True = pairsCountAux pairs (pairsCount_increase (p1, p2) (((k1, k2), v):pairsTally))
    | otherwise = pairsCountAux pairs (((p1, p2), 1) : ((k1, k2), v) : pairsTally)

pairsCountChecker :: (String, String) -> PairsTally -> Bool
pairsCountChecker _ [] = False
pairsCountChecker (p1, p2) (((k1, k2), v):pairsTally) | (p1 == k1 && p2 == k2) || (p1 == k2 && p2 == k1) = True
                                                    | otherwise = pairsCountChecker (p1, p2) pairsTally

pairsCount_increase :: (String, String) -> PairsTally -> PairsTally
pairsCount_increase (p1, p2) [] = []
pairsCount_increase (p1, p2) (((k1, k2), v):pairsTally) | p1 == k1 && p2 == k2 = ((k1, k2), v+1):pairsTally
                                                        | p1 == k2 && p2 == k1 = ((k1, k2), v+1):pairsTally
                                                        | otherwise = pairsCount_increase (p1, p2) pairsTally
-}

--här har jag inte skapat en aux funktion som anropas med
--neighbours pairsTally word = neighboursAux pairsTally word [] där [] introducerar en artificiell och onödig variabel
neighbours :: PairsTally -> String -> WordTally
neighbours [] word = []
neighbours (((k1, k2), v):pairsTally) word = check ((k1, k2), v) word ++ neighbours pairsTally word
    where
        check ((k1, k2), v) word | k1 == word = [(k2, v)]
                                 | k2 == word = [(k1, v)]
                                 | otherwise = []



filterWord :: PairsTally -> String -> PairsTally
filterWord pairsTally word = filter (\pairsTally -> (fst (fst pairsTally))  == word || (snd (fst pairsTally))  == word) pairsTally

mcn_sort :: PairsTally -> String -> (String, Int) -> (String, Int)
mcn_sort [] _ (key, val) = (key, val)
mcn_sort (((k1, k2), v):pt) word (key, val) | k1 == word && v > val = mcn_sort pt word (k2, v)
                                            | k2 == word && v > val = mcn_sort pt word (k1, v)
                                            | otherwise = mcn_sort pt word (key, val)



mostCommonNeighbour :: PairsTally -> String -> Maybe String
mostCommonNeighbour pairsTally word
    | filterWord pairsTally word == [] = Nothing
    | otherwise = Just (fst (mcn_sort (myFilter) word ("test", 0)))
        where
            myFilter = filterWord pairsTally word



  
{-

mcn_aux :: PairsTally -> String -> [(String, Int, String)] -> Maybe String
mcn_aux [] word [] = Nothing
mcn_aux [] word [(p1, v, p2)] ""= Just p2
mcn_aux (((k1, k2), v):pairsTally) word []
    | word == k1 = mcn_aux pairsTally word [(k1, v, k2)]
    | word == k2 = mcn_aux pairsTally word [(k2, v, k1)]
    | otherwise = mcn_aux pairsTally word []
mcn_aux (((k1, k2), v):pairsTally) word [(p1, val, p2)]
    | word == k1 && v > val = mcn_aux pairsTally word [(k1, v, k2)]
    | word == k2 && v > val = mcn_aux pairsTally word [(k2, v, k1)]
    | otherwise = mcn_aux pairsTally word [(p1, val, p2)]
-}
-- Test Cases
-- feel free to add other test cases here. an independent set of
-- test cases will be used when grading your code

-- wordCount
test1 = TestCase $ assertEqual "wordCount []" [] (wordCount [])

test2 = TestCase $ assertBool "wordCount [[\"a\",\"b\"],[\"a\"]]" (elem ("a", 2) (wordCount [["a", "b"], ["a"]]))

-- adjacentPairs, initialPairs, finalPairs
test3 = TestCase $ assertEqual "adjacentPairs [[\"foo\"],[\"bar\"]]" [] (adjacentPairs [["foo"],["bar"]]) 

test3a = TestCase $ assertEqual "initialPairs" [("a","b")] (initialPairs [["a","b","a"],["c"]])
                      
test3b = TestCase $ assertEqual "finalPairs" [("b","a")] (finalPairs [["a","b","a"],["c"]])
                      

-- pairsCount
test4 = TestCase $ assertBool "pairsCount simple" 
            (elem (("a","b"), 2) (pairsCount [("a","b"),("c","d"),("a","b")]))
test5 = TestCase $ assertBool "pairsCount tricky" 
             (let x = pairsCount (adjacentPairs [["a","b","a"],["c"]]) in 
                      elem (("a","b"), 2) x || elem (("b","a"), 2) x)

-- neighbours
test6 = TestCase $ assertEqual "neighbours left" [("b",2)] 
                                                 (neighbours [(("a","b"),2),(("c","d"),1)] "a") 

test7 = TestCase $ assertEqual "neighbours left" [("a",2)]
                                                 (neighbours [(("a","b"),2),(("c","d"),1)] "b") 

-- mostCommonNeighbour
test8 = TestCase $ assertEqual "mostCommonNeighbour text \"the\"" (Just "fun") 
                                                                  (mostCommonNeighbour input "the") 
  where input = [(("the", "fun"),4),(("the","foot"),3),(("dog","power"),2)]

test9 = TestCase $ assertEqual "mostCommonNeighbour text \"spam\"" 
                      Nothing (mostCommonNeighbour input "spam")
  where input = [(("the", "fun"),4),(("the","foot"),3),(("dog","power"),2)]

-- testing the PandP.austin text
test10 = TestCase $ assertEqual "mostCommonNeighbour of \"bennet\"" 
            (Just "mr") (mostCommonNeighbour (pairsCount $ adjacentPairs $ austin) "bennet") 

-- for running all the tests (type "runtests" within ghci --- without the quotes)
runtests = runTestTT $ TestList [test1, test2, test3, test3a, test3b, test4, test5, test6, test7,test8,test9,test10]

--runtests = runTestTT $ TestList [test1, test2, test3, test3a, test3b]
