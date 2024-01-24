-- isJamValid(j, m, d) adalah fungi untuk melakukan pemeriksaan jika 3 bilangan integer j, m, d menyusun format jam yang valid.
-- Definisi jam yang valid adalah jika elemen jam (j) bernilai antara 0 sampai 23,
-- elemen menit (m) bernilai antara 0 sampai 59,
-- dan elemen detik (d) bernilai anatar 0 dan 59.
isJamValid :: Int -> Int -> Int -> Bool
isJamValid  j m d = (j > 0 && j <= 23)&&(m >= 0 && m <=59)&&(d>= 0 && d <= 59)
-- TODO: realisasi Haskell disini
main :: IO()
main = do
    j <- readLn :: IO Int
    m <- readLn :: IO Int
    d <- readLn :: IO Int
-- memanggil fungsi isJamValid
    print (isJamValid j m d)
