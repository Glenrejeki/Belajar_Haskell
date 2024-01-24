main :: IO ()
main = do 
    -- 1) Data number 
    -- mendeklarasikan bilangan bulat 
    let dataInt =42
    -- mendeklarasikan bilangan rill
    let dataDouble =3.14
    -- menampilkan bilangan rill
    print dataInt 
    print dataDouble

    -- 2) Data character
    -- menampilkan karakter
    let dataChar = 'A'
    -- menampilkan karakter
    print dataChar

    -- 3) Data string 
    -- menampilkan string
    let dataStr= "Belajar bahasa pemprograman Haskel bersama"
    let dataStr2= "Abdullah Ubaid"
    -- menampilkan string
    putStrLn dataStr
    putStrLn dataStr2

    -- 4) Data Boolean
    let dataBool = True
    print dataBool

    -- 5) Data list
    -- menampilkan daftar
    let myTuple = (3,'-',5.0,False)
    -- menampilkan tuple
    print myTuple
    