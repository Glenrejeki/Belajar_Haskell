-- fungsi untuk memeriksa apakah suatu bilangan itu genap atau ganjil
-- fungsi ini menerima 1 bilangan bulat dan mengembalikan data boolean 
isEven :: Int -> Bool 
isEven x = x `mod` 2==0

-- FUNGSI numToday
numToday :: Int->String 
numToday n = case n of
    1 -> "Senin"
    2 -> " Selasa"
    3 -> "Rabu"
    4 -> "Kamis"
    5 -> "Jumat"
    6 -> "Sabtu"
    7 -> "Minggu"
    _ -> "Tidak valid"

    --fungsi ini menerima 1 string bilangan bulat dan mengembalikan data Char
charAt :: String -> Int -> Char
charAt str position -- 2 parameter (String, Int)
    | position >= 0 && position < length str = str !! position
    | otherwise = error "Posisi tidak valid"

    -- fungsi untuk menghitung rata-rata pada daftar bilangan bulat 
    -- menerima daftar bilangan bulat dan mengembalikan data Double
average::[Int] -> Double
average listInt 
    | length listInt == 0 = 0.0  -- mengatasi kasus daftar kosong untuk menghindari pembagian nol
    | otherwise = fromIntegral(sum listInt)/ fromIntegral (length listInt)

    --fungsi untuk menghitung varians pada daftar bilangan bulat 
variance :: [Int] -> Double
variance listInt = sumSquareDiff / fromIntegral (length listInt)
        where
            mean = average listInt
            squareDiff = [(fromIntegral x- mean)^2 | x <- listInt]
            sumSquareDiff = sum squareDiff

    -- fungsi untuk menghitung standar deviasi 
stdv:: [Int] -> Double
stdv listInt
    |length listInt <= 1 = 0.0  -- mengatasi kasus daftar kosong atau daftar dengan sastu elemen 
    | otherwise = sqrt (variance listInt)


main :: IO ()
main = do 
    -- memanggil fungsi even(genap)
    putStrLn $ "Apakah 1 bilangan genap ?" ++ show (isEven 1)
    putStrLn $ "Apakah 20 bilangan genap ?" ++ show (isEven 20 )

    -- funsi numToday
    putStrLn $ " Hari ke-3:"++ numToday 3
    putStrLn $ " Hari ke-8:"++ numToday 8

    --fungsi charAt
    -- catatan : index selalu dimulai dari 0
    putStrLn $ "Index ke-4 dari string Abdullah : " ++ show (charAt "Abdullah" 4)

    --fungsi average
    putStrLn $ " Nilai rata-rata dari list [2,4,4,4,5,7,9] :"++ show (average [2,4,4,4,5,7,9])

    -- fungsi varians 
    putStrLn $ " Nila variasnsi dari list [2,4,4,4,5,7,9]: " ++ show (variance [2,4,4,4,5,5,7,9])

    -- fungsi standar deviasi 
    putStrLn $ "Nilai standar deviasi dari list [2,4,4,4 .5,7,9]:"++ show (stdv[2,4,4,4,5,7,9])