-- Nama = Glen Rejeki Sitorus
-- NIM = 11S23024
-- Prodi = Informatika
--
--
--
--                                 

data Pecahan = Pecahan {a::Int, b::Int} deriving Show

pemb::Pecahan ->Int
pemb pecahan = a pecahan

penny::Pecahan -> Int
penny pecahan = b pecahan

makeP:: Int->Int->Pecahan
makeP a b = Pecahan{a=a, b=b}

addP::Pecahan -> Pecahan->Pecahan
addP p1 p2 = makeP (pemb p1 *penny p2  + pemb p2 *penny p1 ) (penny p1 *penny p2)

subP::Pecahan -> Pecahan ->Pecahan
subP p1 p2 = makeP (pemb p1 * penny p2 - pemb p2 * penny p1) (penny p1 * penny p2)

mulP::Pecahan -> Pecahan ->Pecahan
mulP p1 p2 = makeP (pemb p1 * pemb p2) (penny p1 * penny p2)

divP:: Pecahan -> Pecahan -> Pecahan 
divP p1 p2 = makeP (pemb p1 * penny p2) (penny p1 * pemb p2 )

isEqP::Pecahan -> Pecahan -> Bool
isEqP p1 p2 = pemb p1 * penny p2 == penny p1 * pemb p2

isLtP::Pecahan -> Pecahan -> Bool
isLtP p1 p2  = pemb p1 * penny p2 < penny p1 * pemb p2


main::IO()
main = do 
    pem1<-readLn::IO Int
    pen1<- readLn :: IO Int
    pem2 <- readLn :: IO Int
    pen2 <- readLn :: IO Int

    let p1 = makeP pem1 pen1
        p2 = makeP pem2 pen2
    
    putStrLn $ "p1 = " ++ show p1
    putStrLn $ "p2 = " ++ show p2 

    putStrLn $ "addP(p1, p2) = " ++ show (addP p1 p2)

    putStrLn $ "subP(p1, p2) = "++show (subP p1 p2)

    putStrLn $ "mulP(p1, p2) = "++show (mulP p1 p2)
  
    putStrLn $ "divP(p1, p2) = "++show (divP p1 p2)

    putStrLn $ "isEqP(p1, p2) = "++show (isEqP p1 p2)

    putStrLn $ "isLtP(p1, p2) = "++show (isLtP p1 p2)
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--