-- Glen Rejeki Sitorus
-- 11S23024
-- Informatika

--Defenisi
data Tree t = Empty | Node t (Tree t) (Tree t) deriving (Show)

-- Konstruktor 
makeTree :: [a] -> Tree a
makeTree [] = Empty
makeTree (x:xs) = Node x (makeTree leftTail) (makeTree rightTail) 
                where
                  panjang = length xs
                  mid = panjang  `div` 2
                  (leftTail, rightTail) = splitAt (if panjang `mod` 2 == 0 then mid else mid+ 1)(xs)

-- Selektor 
akar :: Tree t -> t
akar Empty = error "Pohon kosong tidak memiliki akar"
akar (Node a _ _) = a


left :: Tree t -> Tree t 
left Empty = Empty 
left (Node _ l _ ) = l

right :: Tree t -> Tree t 
right Empty = Empty 
right (Node _ _ r ) = r 

-- Operator 
nbElmt :: Tree t -> Int 
nbElmt Empty  = 0 
nbElmt (Node _ l r ) = 1 + nbElmt l + nbElmt r

nbDaun :: Tree t -> Int 
nbDaun Empty = 0
nbDaun (Node _ Empty Empty) = 1  -- Jika node merupakan daun
nbDaun (Node _ l r ) = nbDaun l + nbDaun r 

nbEven :: Tree Int -> Int 
nbEven Empty = 0
nbEven (Node a l r ) 
    | even  a = 1 +nbEven l + nbEven r
    | otherwise = nbEven l + nbEven r 

nbOdd :: Tree Int -> Int 
nbOdd Empty = 0 
nbOdd (Node a l r ) 
    | odd a =  1 + nbOdd l + nbOdd r
    | otherwise = nbOdd  l + nbOdd r 

-- Predikat 
isTreeEmpty :: Tree a -> Bool 
isTreeEmpty Empty = True 
isTreeEmpty _ = False  -- Pohong /= kosong 

isOneElemt :: Tree t -> Bool
isOneElemt Empty = False 
isOneElemt (Node a l r ) =  nbElmt (Node a l r) == 1 

isUnerLeft :: Tree t -> Bool  
isUnerLeft Empty = False
isUnerLeft f 
    | nbElmt f == 2 = True 
    | otherwise = False 

isUnerRight :: Tree t -> Bool 
isUnerRight p = False

isBiner :: Tree t -> Bool 
isBiner Empty = False 
isBiner f 
    | nbElmt f ==1 = False 
    | nbElmt f==2 =False 
    | nbElmt f > 2 = True

isExitLeft :: Tree t -> Bool 
isExitLeft Empty = False 
isExitLeft (Node _ l _ ) = not (isTreeEmpty l)
    where isTreeEmpty Empty = True 
          isTreeEmpty _ = False

isExitRight :: Tree t -> Bool 
isExitRight Empty = False 
isExitRight (Node _ _ r ) = not (isTreeEmpty r)
            where isTreeEmpty Empty = True
                  isTreeEmpty _     = False 

isExistN :: Tree Int -> Int -> Bool 
isExistN Empty n = False 
isExistN (Node a l r ) n
        | a == n = True -- Kasus kalau nilai pada akar sama dengan n
        | otherwise = isExistN l n || isExistN r n -- Teknik rekursi untuk mencari n apakah sama dengan sub pohon kiri atau kanan 

isBalance :: Tree Int -> Bool 
isBalance (Node _ l r ) = nbElmt l == nbElmt r 

-- Fungsi lainnya
getListFromInput :: Int -> IO [Int]
getListFromInput 0 = return []
getListFromInput n = do
    x <- readLn :: IO Int
    xs <- getListFromInput (n - 1)
    return (x : xs)

showTree :: Show t => Tree t -> String
showTree tree = unlines (showTree' tree)

showTree' :: Show t => Tree t -> [String]
showTree' Empty = ["_"]
showTree' (Node a l r)
    | isTreeEmpty l && isTreeEmpty r = [show a]
    | otherwise = show a : zipWith (\l r -> l ++ " " ++ r)
            (showTree' l)
            (showTree' r)

        

main :: IO ()
main = do 
    totalList <- readLn :: IO Int
    list <- getListFromInput totalList

    let p = makeTree list
    let emptyTree = isTreeEmpty p

    if not emptyTree
     then do
        n <- readLn :: IO Int
        putStrLn $ "n = " ++ show n
        -- memanggil fungsi showTree(p)
        putStrLn "tree p : "
        putStrLn $ showTree p
        -- memanggil fungsi akar(p)
        putStrLn $ "akar(p) = " ++ show (akar p :: Int)
        -- memanggil fungsi left(p)
        putStrLn "left(p) ="
        putStrLn $ showTree (left p)
        -- memanggil fungsi right(p)
        putStrLn "right(p) ="
        putStrLn $ showTree (right p)
        -- memanggil fungsi nbElmt(p)
        putStrLn $ "nbElmt(p) = " ++ show (nbElmt p)
        -- memanggil fungsi nbDaun(p)
        putStrLn $ "nbDaun(p) = " ++ show (nbDaun p)
        -- memanggil fungsi nbEven(p)
        putStrLn $ "nbEven(p) = " ++ show (nbEven p)
        -- memanggil fungsi nbOdd(p)
        putStrLn $ "nbOdd(p) = " ++ show (nbOdd p)

        -- memanggil fungsi isTreeEmpty(p)
        putStrLn $ "isTreeEmpty(p) = " ++ show (isTreeEmpty p)
        -- memanggil fungsi isOneElemt(p)
        putStrLn $ "isOneElemt(p) = " ++ show (isOneElemt p)
        -- memanggil fungsi isUnerLeft(p)
        putStrLn $ "isUnerLeft(p) = " ++ show (isUnerLeft p)
        -- memanggil fungsi isUnerRight(p)
        putStrLn $ "isUnerRight(p) = " ++ show (isUnerRight p)
        -- memanggil fungsi isBiner(p)
        putStrLn $ "isBiner(p) = " ++ show (isBiner p)
        -- memanggil fungsi isExitLeft(p)
        putStrLn $ "isExitLeft(p) = " ++ show (isExitLeft p)
        -- memanggil fungsi isExitRight(p)
        putStrLn $ "isExitRight(p) = " ++ show (isExitRight p)
        -- memanggil fungsi isExistN(p, n)
        putStrLn $ "isExistN(p, n) = " ++ show (isExistN p n)
        -- memanggil fungsi isBalance(p)
        putStrLn $ "isBalance(p) = " ++ show (isBalance p)


        else putStrLn "Ini pohon kosong!"

