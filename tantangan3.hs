-- grade(x) menentukan bilangan grade yang diperoleh oleh seorang mahasiswa untuk nilai ujian x (bilangan real).
grade :: Double -> String
grade x 
    |x>= 80 = "A"
    |x>=72 && x<80 = "AB"
    |x>=65&&x<72 = "B"
    |x>=57&&x<65 = "BC"
    |x>=50&&x<57 = "C"
    |x>=35&&x<50 = "D"
    |x < 35 = "E"

-- TODO: realisasi Haskell disini
main :: IO ()
main = do
    x <- readLn :: IO Double
-- memanggil fungsi grade
    print (grade x)
