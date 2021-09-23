module PCalc where

import Control.Monad
import Data.Monoid
import Data.Ratio
import Numeric.RootFinding
import System.IO

infix 7 <.>

data RootFinder
    = RootFinder
        { nameOfTheRoot :: String
        , leftBoundOfTheRoot :: Double
        , initialGuessOfTheRoot :: Double
        , rightBoundOfTheRoot :: Double
        , theFunctionWithZeroValueAtTheRoot :: Double -> Double
        }
    deriving ()

at :: [a] -> Int -> a
list `at` index
    | index >= 1
    = case drop (index - 1) list of
        [] -> error errmsg
        result : _ -> result
    | otherwise
    = error errmsg
    where
        
        errmsg :: String
        errmsg = "In `at': index-is-out-of-range, index=" ++ shows index (", list-length=" ++ shows (length list) ".")

toDouble :: Integral a => a -> Double
toDouble = fromInteger . toInteger

isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = list == reverse list

count :: Eq a => [a] -> a -> Int
count = flip (curry (length . uncurry (filter . (==))))

(<.>) :: Num a => [a] -> [a] -> a
list1 <.> list2
    | n1 == n2 = sum (zipWith (*) list1 list2)
    | otherwise = error ("In `(<.>)': different-lengthes, left=" ++ shows n1 (", right=" ++ shows n2 "."))
    where

        n1 :: Int
        n1 = length list1

        n2 :: Int
        n2 = length list2

fractionalpart :: RealFrac a => a -> a
fractionalpart x = x - fromInteger (floor x)

runRootFinder :: RootFinder -> Double
runRootFinder (RootFinder root_name left_bound initial_guess right_bound function_with_zero_value_at_the_root)
    = case newtonRaphson param (left_bound, initial_guess, right_bound) (yy' function_with_zero_value_at_the_root) of
        NotBracketed -> error ("In `runRootFinder': not-bracketed, root.name=" ++ shows root_name ".")
        SearchFailed -> error ("In `runRootFinder': search-failed, root.name=" ++ shows root_name ".")
        Root the_root -> the_root
    where

        param :: NewtonParam
        param = NewtonParam
            { newtonMaxIter = 10^3
            , newtonTol = AbsTol 1.0e-6
            }

        yy' :: (Double -> Double) -> Double -> (Double, Double)
        yy' f x = (y, y') where

            h :: Double
            h = 1.0e-3

            y :: Double
            y = f x

            y' :: Double
            y' = (f (x + h) - f (x - h)) / (2.0 * h)

slope :: Double -> Ratio Int
slope m
    | e <= replicate (length e) 0 = 1
    | e >= [0] ++ replicate (length e - 1) 1 = 0
    | not (1 `elem` u)
    = if [ e `at` i | i <- [length u + 2 .. 2 * length u + 2] ] < [1] ++ u
        then (length u + 1) % (length u + 2)
        else (length u) % (length u + 1)
    | not (0 `elem` u)
    = if [ e `at` i | i <- [length u + 2 .. 2 * length u + 2] ] < [0] ++ u
        then 1 % (length u + 1)
        else 1 % (length u + 2)
    | is1stCase = (length u - count u 1 + 1) % (length u + 2)
    | is2ndCase = (length qPal - count qPal 1 + 1) % (length qPal + 2)
    | is3rdCase = (length pPal - count pPal 1 + 1) % (length pPal + 2)
    | is4thCase = (length u - count u 1 + 1) % (length u + 2)
    where

        e :: [Int]
        e = [ digits `at` i | i <- [2 .. nodigits] ] where

            nodigits :: Int
            nodigits = 100

            px :: Double -> Double
            px x = (1.0 + sqrt (m / (m - 1.0))) * x

            t :: Double -> Double
            t = fractionalpart . px

            digits :: [Int]
            digits = map (floor . px) (iterate t 1.0)

        u :: [Int]
        u = palcalc (error "In `u': pal-binding-is-null.") [e `at` 2] where

            paldef :: [Int] -> ([Int] -> [Int])
            paldef l = foldr go const [1 .. length l] [] where

                go :: Int -> ([Int] -> [Int] -> [Int]) -> [Int] -> ([Int] -> [Int])
                go i kont palpref = maybe (kont palpref) (pure . join kont) (foldr changePal Nothing [1 .. length l0]) where

                    l0 :: [Int]
                    l0 = palpref ++ [l `at` i]

                    changePal :: Int -> (Maybe [Int] -> Maybe [Int])
                    changePal n = case splitAt (n - 1) l0 of
                        (longPalPrefix, longPal) -> if isPalindrome longPal then const (Just (l0 ++ reverse longPalPrefix)) else id

            palcalc :: [Int] -> [Int] -> [Int]
            palcalc pal le
                | pal' == [ e `at` i | i <- [2 .. length pal' + 1] ] && not (2 `elem` pal') = palcalc pal' le'
                | otherwise = paldef (take (length le' - 2) le') pal'
                where

                    pal' :: [Int]
                    pal' = paldef le pal

                    le' :: [Int]
                    le' = le ++ [e `at` (length pal' + 2)]

        modi :: Int
        modi = head (modis ++ [denominator q]) where

            q :: Ratio Int
            q = (count u 1 + 1) % (length u + 2)

            modis :: [Int]
            modis = filter (\i -> (i * (denominator q - numerator q)) `mod` (denominator q) == 1) [1 .. denominator q]

        pPal :: [Int]
        pPal = [ e `at` i | i <- [2 .. modi - 1] ]

        qPal :: [Int]
        qPal = [ e `at` i | i <- [2 .. length u - modi + 1] ]

        is1stCase :: Bool
        is1stCase = or
            [ and
                [ [ e `at` i | i <- [length u + 2 .. length u + 3] ] == [0, 1]
                , u ++ [0, 1] < [ e `at` i | i <- [length u + 4 .. 2 * length u + 5] ]
                ]
            , and
                [ [ e `at` i | i <- [length u + 2 .. length u + 3] ] == [1, 0]
                , [ e `at` i | i <- [length u + 4 .. 2 * length u + 5] ] < u ++ [1, 0]
                ]
            ]

        is2ndCase :: Bool
        is2ndCase = or
            [ [ e `at` i | i <- [length u + 2 .. length u + 3] ] == [0, 0]
            , and 
                [ [ e `at` i | i <- [length u + 2 .. length u + 3] ] == [0, 1]
                , [ e `at` i | i <- [length u + 4 .. 2 * length u + 5] ] < u ++ [0, 1]
                ]
            ]

        is3rdCase :: Bool
        is3rdCase = or
            [ [1, 1] <= [ e `at` i | i <- [length u + 2 .. length u + 3] ]
            , and
                [ [ e `at` i | i <- [length u + 2 .. length u + 3] ] == [1, 0]
                , u ++ [1, 0] < [ e `at` i | i <- [length u + 4 .. 2 * length u + 5] ]
                ]
            ]

        is4thCase :: Bool
        is4thCase = otherwise

p :: Double -> Double
p m = if m < mudm then ans1 else ans2 where

    slopem :: Ratio Int
    slopem = slope m

    a :: Int
    a = numerator slopem

    b :: Int
    b = denominator slopem

    central :: [Int]
    central = [ ceiling (toDouble a / toDouble b * toDouble (n + 1)) - ceiling (toDouble a / toDouble b * toDouble n) | n <- [1 .. b - 2] ]

    centralInv :: [Int]
    centralInv = [ ceiling (toDouble (b - a) / toDouble b * toDouble (n + 1)) - ceiling (toDouble (b - a) / toDouble b * toDouble n) | n <- [1 .. b - 2] ]

    beta :: Double
    beta = runRootFinder betaFinder where
        
        betaFinder :: RootFinder
        betaFinder = RootFinder
            { nameOfTheRoot = "beta"
            , leftBoundOfTheRoot = 1.0
            , initialGuessOfTheRoot = 2.5
            , rightBoundOfTheRoot = 3.0
            , theFunctionWithZeroValueAtTheRoot = (\x -> x^b - 2.0 * x^(b - 1) - [ toDouble c | c <- centralInv ] <.> [ x^(b - 2 - k) | k <- [1 .. b - 2] ])
            }

    mudm :: Double
    mudm = (beta^b * (beta - 1.0)^2) / (beta^(b + 2) - 2.0 * beta^(b + 1) + 1.0)

    ans1 :: Double
    ans1 = runRootFinder ans1Finder where
        
        ans1Finder :: RootFinder
        ans1Finder = RootFinder 
            { nameOfTheRoot = "ans1"
            , leftBoundOfTheRoot = 1.0
            , initialGuessOfTheRoot = 2.5
            , rightBoundOfTheRoot = 3.0
            , theFunctionWithZeroValueAtTheRoot = (\x -> (m - 1.0) * x^b - m * x^(b - 1) - [ (m - 1.0) * toDouble c + 1.0 | c <- central ] <.> [ x^(b - 1 - k) | k <- [1 .. b - 2] ] - m)
            }

    ans2 :: Double
    ans2 = runRootFinder ans2Finder where
        
        ans2Finder :: RootFinder
        ans2Finder = RootFinder 
            { nameOfTheRoot = "ans2"
            , leftBoundOfTheRoot = 1.0
            , initialGuessOfTheRoot = 2.5
            , rightBoundOfTheRoot = 3.0
            , theFunctionWithZeroValueAtTheRoot = (\x -> x^b - [ (m - 1.0) * toDouble c | c <- centralInv ] <.> [ x^(b - k) | k <- [1 .. b - 2] ] - m)
            }

showsp :: Double -> ShowS
showsp m = shows (fromInteger (round (p m * 10.0^5)) / 10.0^5)

test :: IO ()
test = go where

    concatShowS :: [ShowS] -> ShowS
    concatShowS = appEndo . mconcat . map Endo

    runTest :: Int -> (Double, Double) -> ShowS
    runTest i (m, pm) = concatShowS
        [ showString "#" . shows i . showChar '\n'
        , showString "Mathematica> p(" . shows m . showString ") = " . shows pm . showChar '\n'
        , showString "Haskell>     p(" . shows m . showString ") = " . showsp m . showChar '\n'
        ]

    testresults :: [ShowS]
    testresults = zipWith runTest [1 ..]
        [ ( 6.0, 2.06341)
        , ( 7.0, 2.02647)
        , ( 2.5, 2.27977)
        , ( 2.7, 2.23872)
        , ( 5.0, 2.11674)
        , (10.0, 2.04794) 
        ]

    go :: IO ()
    go = do
        let putShowS s = putStrLn (s "")
        mapM putShowS testresults
        return ()

{- test:
#1
Mathematica> p(6.0) = 2.06341
Haskell>     p(6.0) = 2.06341

#2
Mathematica> p(7.0) = 2.02647
Haskell>     p(7.0) = 2.02647

#3
Mathematica> p(2.5) = 2.27977
Haskell>     p(2.5) = 2.27977

#4
Mathematica> p(2.7) = 2.23872
Haskell>     p(2.7) = 2.23872

#5
Mathematica> p(5.0) = 2.11674
Haskell>     p(5.0) = 2.11674

#6
Mathematica> p(10.0) = 2.04794
Haskell>     p(10.0) = 2.04794

-}
