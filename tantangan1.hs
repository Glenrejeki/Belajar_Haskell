-- Glen Rejeki Sitorus
-- 11S23024
-- Informatika

-- konstruktor
konso :: Int -> [Int] -> [Int]
konso e l = e : l


konsb :: [Int] -> Int -> [Int]
konsb l e = reverseList (konso e (reverseList l))

-- Selektor
firstElmt :: [Int] -> Int
firstElmt [] = error "List kosong"
firstElmt (x:_) = x


tailList :: [Int] -> [Int]
tailList [] = []
tailList (_:l) = l

lastElmt :: [Int] -> Int
lastElmt l = firstElmt (reverseList l)


headList :: [Int] -> [Int]
headList l = reverseList (tailList (reverseList l))


my_length::[Int]->Int
my_length [] = 0
my_length (_:xs)= 1+my_length xs 

midList :: [Int] -> [Int]
midList [] = []
midList [x] = [x]
midList l
    | len `mod` 2 /= 0 = [elmtKeN l (midIndex + 1)]
    | otherwise = [elmtKeN l midIndex, elmtKeN l (midIndex + 1)]
    where len = nbElmt l
          midIndex = len `div` 2



-- Operator
nbElmt::[Int]->Int  
nbElmt [] = 0
nbElmt (_:list)= 1 + nbElmt list

konkat::[Int]->[Int]->[Int] -- penggabungan 2 list
konkat [] l2 = l2
konkat (x:l1)l2= x:konkat l1 l2

reverseList::[Int]->[Int]
reverseList [] = []
reverseList(x:xs) = reverse (x:xs)

maxList::[Int]->Int
maxList [x] = x 
maxList (x:xs) 
        |x>maxTail = x 
        |otherwise = maxTail
        where maxTail = maxList xs

elmtKeN::[Int]->Int -> Int
elmtKeN list n 
    |n<1 = error "harus lebih besar dari 0"
    |null list = error "Kosong"
    |length list < n = error "List gk mencukupi"
    |otherwise = list !! (n-1)


sumList::[Int]-> Int 
sumList [] = 0
sumList (x:xs) = x +sumList xs

averageList :: [Int] -> Double
averageList [] = 0
averageList l = fromIntegral (sumList l) / fromIntegral (my_length l)

--Predikat
isEmpty::[Int]->Bool
isEmpty l = null l

isOneElemt :: [Int] -> Bool
isOneElemt l = nbElmt l == 1

isEqual::[Int]->[Int]->Bool
isEqual l1 l2 = l1==l2

isMember::[Int]->Int->Bool
isMember [] _ = False 
isMember  (y:ys) x
        |x==y = True
        |otherwise = isMember ys x

isFirst::[Int]->Int->Bool
isFirst [] _ = False
isFirst (y:_) x = x == y


islast::[Int]->Int->Bool
islast [] _ =False 
islast  ys x = x == last ys

isNbElemt::[Int]->[Int]->Bool
isNbElemt l1 l2 = (nbElmt l1) == (nbElmt l2)

isReverse::[Int]->[Int]->Bool
isReverse l1 l2 = l2 == (reverseList l1)

isXElmtKeN :: [Int] -> Int -> Int -> Bool
isXElmtKeN _ _ n 
    | n <= 0 = False
isXElmtKeN (x:xs) e n 
    | n == 1 = x == e
    | otherwise = isXElmtKeN xs e (n - 1)
isXElmtKeN [] _ _ = False

-- Fungsi lainnya
getListFromInput :: Int -> IO [Int]
getListFromInput 0 = return []
getListFromInput n = do
    x <- readLn :: IO Int
    xs <- getListFromInput (n - 1)
    return (x : xs)

main::IO()
main = do 
    totalList1 <- readLn :: IO Int
    l1 <- getListFromInput totalList1
    totalList2 <- readLn :: IO Int
    l2 <- getListFromInput totalList2
    e <- readLn :: IO Int
    n1 <- readLn :: IO Int
    n2 <- readLn :: IO Int
    x1 <- readLn :: IO Int
    x2 <- readLn :: IO Int

    -- menggunakan konso dan konsb
    let l1konso = konso e l1
        l1konsb = konsb l1 e
    let l2konso = konso e l2
        l2konsb = konsb l2 e

    putStrLn $ "e = " ++ show e
    putStrLn $ "l1 = " ++ show l1
    putStrLn $ "l1 (konso) = " ++ show l1konso
    putStrLn $ "l1 (konsb) = " ++ show l1konsb
    putStrLn $ "l2 = " ++ show l2
    putStrLn $ "l2 (konso) = " ++ show l2konso
    putStrLn $ "l2 (konsb) = " ++ show l2konsb
    putStrLn $ "n1 = " ++ show n1
    putStrLn $ "n2 = " ++ show n2
    putStrLn $ "x1 = " ++ show x1
    putStrLn $ "x2 = " ++ show x2
    -- memanggil fungsi firstElmt(l)
    putStrLn $ "firstElmt(l1) = " ++ show (firstElmt l1)
    putStrLn $ "firstElmt(l2) = " ++ show (firstElmt l2)
    -- memanggil fungsi tailList(l)
    putStrLn $ "tailList(l1) = " ++ show (tailList l1)
    putStrLn $ "tailList(l2) = " ++ show (tailList l2)
    -- memanggil fungsi lastElmt(l)
    putStrLn $ "lastElmt(l1) = " ++ show (lastElmt l1)
    putStrLn $ "lastElmt(l2) = " ++ show (lastElmt l2)
    -- memanggil fungsi headList(l)
    putStrLn $ "headList(l1) = " ++ show (headList l1)
    putStrLn $ "headList(l2) = " ++ show (headList l2)
    -- memanggil fungsi midList(l)
    putStrLn $ "midList(l1) = " ++ show (midList l1)
    putStrLn $ "midList(l2) = " ++ show (midList l2)
    -- memanggil fungsi nbElmt(l)
    putStrLn $ "nbElmt(l1) = " ++ show (nbElmt l1)
    putStrLn $ "nbElmt(l2) = " ++ show (nbElmt l2)
    -- memanggil fungsi konkat(l1, l2)
    putStrLn $ "konkat(l1, l2) = " ++ show (konkat l1 l2)
    -- memanggil fungsi maxList(l)
    putStrLn $ "maxList(l1) = " ++ show (maxList l1)
    putStrLn $ "maxList(l2) = " ++ show (maxList l2)

    -- memanggil fungsi elmtKeN(l, n)
    putStrLn $ "elmtKeN(l1, n1) = " ++ show (elmtKeN l1 n1)
    putStrLn $ "elmtKeN(l2, n2) = " ++ show (elmtKeN l2 n2)
    -- memanggil fungsi sumList(l)
    putStrLn $ "sumList(l1) = " ++ show (sumList l1)
    putStrLn $ "sumList(l2) = " ++ show (sumList l2)
    -- memanggil fungsi averageList(l)
    putStrLn $ "averageList(l1) = " ++ show (averageList l1)
    putStrLn $ "averageList(l2) = " ++ show (averageList l2)
    -- memanggil fungsi isEqual(l1, l2)
    putStrLn $ "isEqual(l1, l2) = " ++ show (isEqual l1 l2)
    -- memanggil fungsi isEqual(l1, l2)
    putStrLn $ "isEqual(l1, l2) = " ++ show (isEqual l1 l2)
    -- memanggil fungsi isMember(l, x)
    putStrLn $ "isMember(l1, x1) = " ++ show (isMember l1 x1)
    putStrLn $ "isMember(l2, x2) = " ++ show (isMember l2 x2)
    -- memanggil fungsi isFirst(l, x)
    putStrLn $ "isFirst(l1, x1) = " ++ show (isFirst l1 x1)
    putStrLn $ "isFirst(l2, x2) = " ++ show (isFirst l2 x2)
    -- memanggil fungsi islast(l, x)
    putStrLn $ "islast(l1, x1) = " ++ show (islast l1 x1)
    putStrLn $ "islast(l2, x2) = " ++ show (islast l2 x2)
    -- memanggil fungsi isNbElemt(l1, l2)
    putStrLn $ "isNbElemt(l1, l2) = " ++ show (isNbElemt l1 l2)
    -- memanggil fungsi isReverse(l1, l2)
    putStrLn $ "isReverse(l1, l2) = " ++ show (isReverse l1 l2)
    -- isXElmtKeN(l, x, n)
    putStrLn $ "isXElmtKeN(l1, x1, n1) = " ++ show (isXElmtKeN l1 x1 n1)
    putStrLn $ "isXElmtKeN(l2, x2, n2) = " ++ show (isXElmtKeN l2 x2 n2)
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --
    --