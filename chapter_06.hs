{-# OPTIONS -Wall #-}

main :: IO ()
main = do
    let cnt = 10000 :: Int
    let res1 = show $ trapIntegrate cnt (** 3.0) 0.0 1.0
    putStrLn $ "Integral from 0 -> 1 for the function x^3 is: " ++ res1
    let res2 = show $ trapIntegrate cnt (** 3.0) 0.0 1e-6
    putStrLn $ "Integral from 0 -> 1e-6 for the function x^3 is: " ++ res2
    let fn x = exp $ -x ** 2
    let res3 = show $ trapIntegrate cnt fn 0 1
    putStrLn $ "Integral from 0 -> 1 for the function e^(-x^2) is: " ++ res3

trapIntegrate :: Int                -- # of trapezoids
              -> (Double -> Double) -- input function
              -> Double             -- lower limit
              -> Double             -- upper limit
              -> Double             -- result
trapIntegrate n f a b =
    let dx = (b - a) / fromIntegral n
    in  0.5 * dx * sum [f x + f (dx + x) | x <- [a, a+dx .. b]]
