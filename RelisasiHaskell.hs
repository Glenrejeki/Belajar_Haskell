-- isPositif(x) memeriksa apakah sebuah bilangan integer itu positif atau tidak
isPositif:: Int->Bool
isPositif x = x>=0


-- isValid(x) memeriksa apakah sebuah bilangan integer(x) valid atau tidak 
    -- valid jika bilangan x berada pada rentang nilai 0 sampai 100, bilangan lainnya dianggap tidak valid 
isValid :: Int->Bool
isValid x=x>=0 && x<= 100

-- c(x) menghasilkan pangkat dua dari x 
c :: Int -> Int 
c x = x*x

-- cd (x,y) menghasilkan pangkat dua dari hasil pengurangan x dengan y 
cd::Int-> Int -> Int
cd x y = c (x-y)

-- ls (x1,y1,x2,y2) menghasilkan akar dari penjumlahan pangkat dua dari penguranan y1 dan pangkat dua dari hasil pengurangan x2 dengan x1
ls :: Int -> Int -> Int -> Int -> Double
ls x1  y1 x2 y2 =
  let cd1 = cd y2 y1
      cd2 = cd x2 x1
      z = cd1 + cd2
  in sqrt (fromIntegral z)


main::IO()
main = do
    -- memanggil fungsi isPositif
    putStrLn $ "Apakah -10 positif ?"++ show (isPositif(-10))
    putStrLn $ "Apakah -1 positif ?"++ show (isPositif(-1))
    putStrLn $ "Apakah 0 positif ?"++ show (isPositif(0))
    putStrLn $ "Apakah 1 positif ?"++ show (isPositif(1))
    putStrLn $ "Apakah 10 positif ?"++ show (isPositif(10))


    -- memanggil fungsi valid 
    putStrLn $ "Apakah -1 bilangan valid ?"++ show (isValid(-1))
    putStrLn $ "Apakah 0 bilangan valid ?"++ show (isValid(0))
    putStrLn $ "Apakah 1 bilangan valid ?"++ show (isValid(1))
    putStrLn $ "Apakah 99 bilangan valid ?"++ show (isValid(99))
    putStrLn $ "Apakah 100 bilangan valid ?"++ show (isValid(100))
    putStrLn $ "Apakah 101 bilangan valid ?"++ show (isValid(101))

    -- memanggil fungsi isValid 
    putStrLn $ " ls (1,1,1,1)="++ show (ls 1 1 1 1)
    putStrLn $ "ls (2,20,3,40)"++ show(ls 2 20 3 40)
    putStrLn $ "ls (70,5,30,4)"++ show(ls 70 5 30 4)
    putStrLn $ "ls (60,7,7,60)"++ show(ls 60 7 7 60)
    putStrLn $ "ls (9,80,80,9)"++ show(ls 9 80 80 9)