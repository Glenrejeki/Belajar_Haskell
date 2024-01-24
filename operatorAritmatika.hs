main:: IO()
main = do
    -- variabel
    let nilaiInt ::Int -- nilai bilangan bulat 
        nilaiInt =20
    let nilaiInt2:: Int 
        nilaiInt2=11
    let nilaiDouble::Double -- untuk nilai desimal
        nilaiDouble=20.0
    let nilaiDouble2 :: Double
        nilaiDouble2=11.00
    
    -- melakukan penjumlahan
    let hasilPenjumlahan = nilaiInt + nilaiInt2
    let hasilPenjumlahan2= nilaiDouble + nilaiDouble2
    -- menampilkan 
    print hasilPenjumlahan
    print hasilPenjumlahan2

    -- melakukan pengurangan 
    let hasilPengurangan = nilaiInt - nilaiInt2
    let hasilPenguragan2 = nilaiDouble-nilaiDouble2
    print hasilPengurangan
    print hasilPenguragan2

    -- melakukan perkalian 
    let hasilperkalian = nilaiInt*nilaiInt2
    let hasilperkalian2 = nilaiDouble*nilaiDouble2
    print hasilperkalian
    print hasilperkalian2

    -- melakukan pembagian 
    let hasilPembagian =  fromIntegral nilaiInt / fromIntegral nilaiInt2 -- perlu tambahkan fromIntegral jika dia tidak desimal jika ingin pembagian 
    let hasilPembagian2=   nilaiDouble/  nilaiDouble2
    print hasilPembagian
    print hasilPembagian2

    --mengambil hasil sisa bagi 
    let hasilSisaBagi = nilaiInt `mod` nilaiInt2
    let hasilSisaBagi2= floor(nilaiDouble)`mod`floor(nilaiDouble2) -- harus pakai floor jika ingin mengambil hasil sisa bagi dari angka desimal dan harus dalam kurung si variabelnya
    print hasilSisaBagi
    print hasilSisaBagi2


    -- melakukan pemangkatan
    let hasilPangkat = nilaiInt^nilaiInt2
    let hasilPangkat2 = nilaiDouble2**nilaiDouble2
    print hasilPangkat -- hasilnya 204800000000000
    print hasilPangkat2 -- kenapa hasilnya beda = 2.85311670611e11

    -- mengambil hasil akar 
    let hasilAkar = sqrt (fromIntegral nilaiInt)
    let hasilAkar2 = sqrt nilaiDouble2
    print hasilAkar
    print hasilAkar2