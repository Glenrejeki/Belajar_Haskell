{--
    nama : Glen Rejeki Sitorus
    NIM : 11S23024
    prodi : Informatika
--}



data Date=Date {dd::Int, mm::Int, yy::Int} deriving Show
-- Selektor
day::Date -> Int    
day date = dd date  

month::Date -> Int
month date = mm date

year:: Date -> Int
year date = yy date

-- Konstruktor
makeDate :: Int -> Int -> Int -> Date
makeDate dd mm yy = Date{dd=dd, mm=mm, yy=yy}

-- Fungsi untuk mendapatkan jumlah hari dalam sebuah bulan 
daysInMonth :: Int -> Int -> Int
daysInMonth month year
    | month `elem` [1, 3, 5, 7, 8, 10, 12] = 31
    | month `elem` [4, 6, 9, 11]           = 30
    | month == 2                           = if isKabisat year then 29 else 28
    | otherwise                            = 0
-- Operator
nextDay :: Date -> Date
nextDay (Date dd mm yy)
    | dd < daysInMonth mm yy = Date (dd + 1) mm yy -- Tanggal berikutnya di dalam bulan yang sama
    | mm < 12                = Date 1 (mm + 1) yy  -- Tanggal berikutnya di bulan berikutnya dalam tahun yang sama
    | otherwise              = Date 1 1 (yy + 1)   -- Tanggal berikutnya di bulan Januari tahun berikutnya

yesterday :: Date -> Date
yesterday (Date d m y)
    | d > 1     = makeDate (d - 1) m y
    | m > 1     = makeDate (daysInMonth (m - 1) y) (m - 1) y
    | otherwise = makeDate 31 12 (y - 1)

addDay :: Date -> Int -> Date
addDay date n = iterate nextDay date !! n
  where
    nextDay :: Date -> Date
    nextDay (Date d m y)
      | d < daysInMonth m y = Date (d + 1) m y
      | m < 12              = Date 1 (m + 1) y
      | otherwise           = Date 1 1 (y + 1)

subDay :: Date -> Int -> Date
subDay date n = if n == 0 then date 
                else if year date <0 then Date {dd = 1, mm=1 ,yy = 1} 
                else subDay (yesterday date ) (n-1)

nextMonth :: Date -> Date
nextMonth (Date d m y)
    | m < 12    = makeDate d (m + 1) y
    | otherwise = makeDate d 1 (y + 1)

prevMonth :: Date -> Date
prevMonth (Date d m y)
    | m > 1     = makeDate d (m - 1) y
    | otherwise = makeDate d 12 (y - 1)

addMonth :: Date -> Int -> Date
addMonth (Date d m y) n = iterate nextMonth (Date d m y) !! n
    where
        nextMonth :: Date -> Date
        nextMonth (Date d m y)
            | m < 12    = makeDate d (m + 1) y
            | otherwise = makeDate d 1 (y + 1)

subMonth :: Date -> Int -> Date
subMonth date n = if n == 0 then date 
                    else if year date <0 then Date {dd= 1, mm=1, yy=1}
                    else subMonth (prevMonth date) (n-1)

addYear :: Date -> Int -> Date
addYear (Date d m y) n = Date d m (y + n)

subYear :: Date -> Int -> Date 
subYear date n = if n == 0 then date
                        else if year date < n then Date {dd = 1, mm=1, yy = 1 }
                        else Date {dd = day date, mm = month date, yy = year date - n }

-- -- Predikat
isEqD :: Date -> Date -> Bool
isEqD (Date d1 m1 y1) (Date d2 m2 y2) = d1 == d2 && m1 == m2 && y1 == y2 

isNotEqD :: Date -> Date -> Bool
isNotEqD (Date d1 m1 y1) (Date d2 m2 y2) = d1 /= d2 ||m1 /= m2 || y1 /= y2

isBefore :: Date -> Date -> Bool
isBefore date1 date2 = isEqD (yesterday date2) (date1)

isNext :: Date -> Date -> Bool 
isNext date1 date2 = isEqD (nextDay date2) (date1)


-- -- Fungsi untuk mengecek apakah tanggal d1 sebelum d2 sebanyak n hari
isBeforeN :: Date -> Date -> Int -> Bool
isBeforeN date1 date2 n= isEqD (subDay  date2 n) (date1)

isNextN :: Date -> Date -> Int -> Bool
isNextN date1 date2 n =isEqD (addDay date2 n) (date1)


-- Fungsi Lain
isKabisat :: Int -> Bool
isKabisat yy
    | yy `mod` 400 == 0 = True
    | yy `mod` 4 == 0 && yy `mod` 100 /= 0 = True
    | otherwise = False



main::IO ()
main = do 
        dd1 <- readLn :: IO Int
        mm1 <- readLn :: IO Int
        yy1 <- readLn :: IO Int 
        dd2 <- readLn :: IO Int
        mm2 <- readLn :: IO Int 
        yy2 <- readLn :: IO Int
        n <- readLn :: IO Int

        let d1 = makeDate dd1 mm1 yy1 
            d2 = makeDate dd2 mm2 yy2
        
        putStrLn $ "d1 = " ++show d1
        putStrLn $ "d2 = " ++show d2
        putStrLn $ "n = " ++ show n
        
        putStrLn $ "nextDay(d1) = " ++ show (nextDay d1)
        putStrLn $ "nextDay(d2) = " ++ show (nextDay d2)


        putStrLn $ "yesterday(d1) = "++show (yesterday d1)
        putStrLn $ "yesterday(d2) = "++show (yesterday d2)
        
        putStrLn $ "addDay(d1, n) = " ++ show (addDay d1 n)
        putStrLn $ "addDay(d2, n) = " ++ show (addDay d2 n)

        putStrLn $ "subDay(d1, n) = "++show(subDay d1 n)
        putStrLn $ "subDay(d2, n) = "++show (subDay d2 n)

        putStrLn $ "nextMonth(d1) = "++show (nextMonth d1)
        putStrLn $ "nextMonth(d2) = "++show (nextMonth d2)

        putStrLn $ "prevMonth(d1) = "++show (prevMonth d1)
        putStrLn $ "prevMonth(d2) = "++show (prevMonth d2)

        putStrLn $ "addMonth(d1, n) = " ++ show (addMonth d1 n)
        putStrLn $ "addMonth(d2, n) = " ++ show (addMonth d2 n)


        putStrLn $ "subMonth(d1, n) = " ++ show (subMonth d1 n)
        putStrLn $ "subMonth(d2, n) = " ++ show (subMonth d2 n)

        putStrLn $ "addYear(d1, n) = "++show (addYear d1 n)
        putStrLn $ "addYear(d2, n) = "++show (addYear d2 n)

        putStrLn $ "subYear(d1, n) = "++show (subYear d1 n)
        putStrLn $ "subYear(d2, n) = "++show (subYear d2 n)

        putStrLn $ "isEqD(d1, d2) = "++show (isEqD d1 d2)

        putStrLn $ "isNotEqD(d1, d2) = "++show(isNotEqD d1 d2)

        putStrLn $ "isBefore(d1, d2) = "++show(isBefore d1 d2)

        putStrLn $ "isNext(d1, d2) = " ++ show (isNext d1 d2)

        putStrLn $ "isBeforeN(d1, d2, n) = "++show(isBeforeN d1 d2 n)

        putStrLn $ "isNextN(d1, d2, n) = "++show(isNextN d1 d2 n)
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