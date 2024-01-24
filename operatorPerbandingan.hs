main :: IO()
main = do

    let nilaiInt :: Int
        nilaiInt = 20
    let nilaiInt2 :: Int
        nilaiInt2 = 20

    -- mengecek kesamaan data
    let hasil = nilaiInt == nilaiInt2
    print hasil 
    
    -- mengecek ketidaksamaan data
    let hasil = nilaiInt/=nilaiInt2
    print hasil

    -- mengecek apakah data lebih besar 
    let hasil = nilaiInt > nilaiInt2
    print hasil

    -- mengecek apakah data lebih besar sama dengan 
    let hasil = nilaiInt>=nilaiInt2
    print hasil

    -- mengecek apakah data lebih kecil
    let hasil = nilaiInt < nilaiInt2
    print hasil

    -- mengecek apakah data lebih kecil sama dengan 
    let hasil = nilaiInt <= nilaiInt2
    print hasil