-- min2(x, y) mengambil nilai terkecil dari 2 bilangan integer
min2 :: Int -> Int -> Int
min2 x y = if x < y
            then x
            else y
-- TODO: realisasi Haskell disini
-- min3(x, y, z) mengambil nilai terkecil dari 3 bilangan integer
min3 :: Int -> Int -> Int -> Int
min3 x y z = min2 (min2 x y)z
-- TODO: realisasi Haskell disini
-- min4(v, x, y, z) mengambil nilai terkecil dari 4 bilangan integer
min4 :: Int -> Int -> Int -> Int -> Int
min4 v x y z = min3 v x y
-- TODO: realisasi Haskell disini
main :: IO ()
main = do
    v <- readLn :: IO Int
    x <- readLn :: IO Int
    y <- readLn :: IO Int
    z <- readLn :: IO Int
-- memanggil fungsi min4
    print (min4 v x y z)