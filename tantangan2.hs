-- wujudZatAir(x) melakukan pengecekan wujud zat dari air, berdasarkan suhu air x, dengan x (bilangan real) yang merupakan masukan ke fungsi
wujudZatAir :: Double -> String
wujudZatAir x 
        |x > 0 = "Gas"
        |x < 0 = "PADAT"
        |x == 0 ="CAIR"
-- TODO: realisasi Haskell disini
main :: IO ()
main = do
    x <- readLn :: IO Double
-- memanggil fungsi wujUdZatAir
    print (wujudZatAir x)