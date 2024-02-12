data Point = Point {x:: Int, y::Int} deriving Show 

absis :: Point -> Int 
absis point = x point 

ordinat ::Point -> Int 
ordinat point = y point 

makePoint:: Int -> Int -> Point 
makePoint x y = Point {x = x, y=y }

isOrigin::Point -> Bool 
isOrigin p = absis p == 0 && ordinat p== 0 

kuadran :: Point -> String 
kuadran p 
    |absis p > 0 && ordinat p > 0 = "1"
    |absis p < 0 && ordinat p >0 = "2"
    |absis p <0 && ordinat p<0 = "3"
    |absis p >0 && ordinat p<0 = "4"
    |otherwise = "Lainnya"

main::IO()
main = do 
    let p1 = makePoint 2 (-2)
    let p2 = makePoint 2 2
    let p3 = makePoint 0 0 
    let p4 = makePoint (-2) (-2)
    let p5 = makePoint (-2) 2


    -- menentukan apakah x dan y = (0,0)
    putStrLn (show (isOrigin p1))
    putStrLn ( show (isOrigin p2))
    putStrLn ( show (isOrigin p3))
    putStrLn (show (isOrigin p4))
    putStrLn (show(isOrigin p5))
    
    -- menentukan kuadran brp dari masing2 x dan y 
    putStrLn (kuadran p1)
    putStrLn (kuadran p2)
    putStrLn (kuadran p3)
    putStrLn (kuadran p4 )
    putStrLn (kuadran p5)