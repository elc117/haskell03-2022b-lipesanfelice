add10toall :: [Int] -> [Int]
add10toall list = [x+10 | x <- list]

multN :: Int -> [Int] -> [Int]
multN x list = [x*element | element <- list]

multN' :: Int -> [Int] -> [Int]
multN' x list = map (*x) list

applyExpr :: [Int] -> [Int]
applyExpr list = [3*x+2 | x <- list]

applyExpr' :: [Int] -> [Int]
applyExpr' list = map (\list -> 3*list+2) list

addSuffix :: String -> [String] -> [String]
addSuffix word list = [x++word | x <- list]

selectgt5 :: [Int] -> [Int]
selectgt5 list = [x | x <- list, x > 5]

sumOdds :: [Int] -> Int
sumOdds [] = 0
sumOdds (x:list) = x + sumOdds [x | x <- list, mod x 2 /= 0]

impar :: Int -> Bool
impar x = mod x 2 /= 0

sumOdds' :: [Int] -> Int
sumOdds' [] = 0
sumOdds' (x:list) =  x + sumOdds'(filter impar list)

selectExpr :: [Int] -> [Int]
selectExpr list = [x | x <- list, mod x 2 == 0 && x < 50 && x > 20]

-- countShorts :: [String] -> Int
-- countShorts [] = 0
-- countShorts list = 

calcExpr :: [Float] -> [Float]
calcExpr list = filter (>10) [(x^2)/2 | x <- list]

muda :: Char -> Char
muda x = '-'

trSpaces :: String -> String
trSpaces word = [if x == ' ' then muda x else x | x <- word]

selectSnd :: [(Int,Int)] -> [Int]
selectSnd list = [y | (_,y) <- list]

-- dotProd :: [Int] -> [Int] -> Int
-- dotProd list1 list2 =  dotProd list1 list2

main = do
  putStrLn "Vamos testar..."
  
