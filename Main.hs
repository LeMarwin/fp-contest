module Main where

import Data.List

inputMatrix :: [(String, [String])]
inputMatrix = [("Has bomburias",        ["Yes",      "Yes",        "No",       "Yes",    "No"]),
               ("Clepticons number",    ["1",       "1",         "0",         "3",     "5"]),
               ("Veloria's colour",     ["Red", "Orange", "Orange", "-",     "Blue"]),
               ("Has pumpel",           ["No",     "Yes",        "Yes",        "-",     "-"]),
               ("Pumpel size",          ["-",       "Big",   "Small", "-",     "-"]),
               ("Ability to crocot",    ["No",     "No",       "-",         "Yes",    "No"]),
               ("Ability to bulbotan",  ["No",     "Yes",        "-",         "Yes",    "No"]),
               ("Has ducs and tucs",    ["-",       "-",         "-",         "-",     "Yes"]),
               ("Lempel's colour",      ["Yellow",  "Yellow",    "Yellow",    "White", "White"]),
               ("Has pils trapcs",      ["Yes",      "Yes",        "Yes",        "Yes",    "Yes"])]

names::[String]
names = [ "Aurata setuniata",
          "Desyatilnyata lepayata",
          "Semipunctata Kokhata",
          "Populii Gryzhomelscii",
          "Gorticola Filoperieva"]

dataCheck::[(String,[(String)])]->[String]->Bool
dataCheck datum nominibus = not (a && b)
     where a = (length $ nub (map (\(_,e)-> length e) datum)) == 1
           b = (length nominibus) == length (snd $ head datum)

dataOrder::(String, [String])->(String, [String])->Ordering
dataOrder (_,a) (_, b) = if (n>m)
                         then LT
                         else if (m>n)
                              then GT
                              else EQ
                         where n = length $ nub a
                               m = length $ nub b

quest::[(String, [(Int,String)])]->IO()
quest [] = print "Not found, sry"
quest (q:qs) = do
     print $ fst q
     x <- getLine
     let check = (foldl' (||) False $ map (\e-> (snd e) `elem` [x,"-"]) $ snd q) || x == "-" 
     if(check) 
          then do
               let res = map fst $ filter (\e -> (snd e) `elem` [x,"-"]) $ snd q
               if (length res == 1) then print $ names!!(head res)
                    else quest $ leaveOnly res qs
          else print "No such answer, sry"

leaveOnly::[Int]->[(String, [(Int,String)])]->[(String, [(Int,String)])]
leaveOnly nums mas = map (\(a,b)-> (a, filter(\(i,_)-> i `elem` nums) b)) mas

main::IO()
main = if(dataCheck inputMatrix names)
          then print $ "Not enough data/names. Check inputs" ++ show (dataCheck inputMatrix names)
          else quest $ map (\(s,m)->(s, zip [0..(length m)] m)) $ sortBy (dataOrder) inputMatrix