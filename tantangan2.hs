-- detikToFormatJam(x) adalah fungsi untuk mengubah bilangan integer (x)sebagai detik, menjadi string jam dengan format ”j:m:d”.
detikToFormatJam :: Int -> String
detikToFormatJam x = 
    let j = x `div`3600
        s = x  `mod`3600
        m = s `div`60
        d = s `mod`60
    in 
        show j++":"++show m ++":"++show d 

-- TODO: realisasi Haskell disini
main :: IO()
main = do
    x <- readLn :: IO Int

-- memanggil fungsi detikToFormatJam
    print (detikToFormatJam x)
