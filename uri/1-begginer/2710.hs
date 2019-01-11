# This solution to 2710 produces a TLE
data T = U Int Int Int Int Int | A Int Int deriving Show

parseU :: [String] -> T
parseU l =
    let x = map (\x -> read x :: Int ) l
    in U (x !! 0) (x !! 1) (x !! 2) (x !! 3) (x !! 4)

parseA :: [String] -> T
parseA l =
    let x = map (\x -> read x :: Int ) l
    in A (x !! 0) (x !! 1)


parseLine :: [String] -> T
parseLine ("U":xs) = parseU xs
parseLine ("A":xs) = parseA xs

valueof :: [T] -> T -> Int
valueof t (A p q) = sum [ v | (U x y a b v) <- t, p >= x, p <= a, q >= y, q <= b]

foldStep :: ([T], [Int]) -> T -> ([T], [Int])
foldStep (t, out) (U x y z w v)  = ((U x y z w v):t, out)
foldStep (t, out) (A x y)  = (t, (valueof t (A x y)):out)


solution input =
    let (h:t) = lines input
        x = map parseLine (map words t)
        (s, out) = foldl foldStep ([], []) x
    in unlines $ map show $ reverse out

main = interact solution
