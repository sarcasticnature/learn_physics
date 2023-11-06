{-# OPTIONS -Wall #-}

main :: IO ()
main = do
    let cnt = 10000 :: Int

    putStrLn "trapIntegrate:"
    let res1 = show $ trapIntegrate cnt (** 3.0) 0.0 1.0
    putStrLn $ "Integral from 0 -> 1 for the function x^3 is: " ++ res1
    let res2 = show $ trapIntegrate cnt (** 3.0) 0.0 1e-6
    putStrLn $ "Integral from 0 -> 1e-6 for the function x^3 is: " ++ res2
    let fn x = exp $ -x ** 2
    let res3 = show $ trapIntegrate cnt fn 0 1
    putStrLn $ "Integral from 0 -> 1 for the function e^(-x^2) is: " ++ res3

    putStrLn "trapIntegrate':"
    let res4 = show $ trapIntegrate' cnt (** 3.0) 0.0 1.0
    putStrLn $ "Integral from 0 -> 1 for the function x^3 is: " ++ res4
    let res5 = show $ trapIntegrate' cnt (** 3.0) 0.0 1e-6
    putStrLn $ "Integral from 0 -> 1e-6 for the function x^3 is: " ++ res5
    let res6 = show $ trapIntegrate' cnt fn 0 1
    putStrLn $ "Integral from 0 -> 1 for the function e^(-x^2) is: " ++ res6

trapIntegrate :: Int                -- # of trapezoids
              -> (Double -> Double) -- input function
              -> Double             -- lower limit
              -> Double             -- upper limit
              -> Double             -- result
trapIntegrate n f a b =
    let dx = (b - a) / fromIntegral n
    in  0.5 * dx * sum [f x + f (dx + x) | x <- [a, a+dx .. b]]

trapIntegrate' :: Int                -- # of trapezoids
               -> (Double -> Double) -- input function
               -> Double             -- lower limit
               -> Double             -- upper limit
               -> Double             -- result
trapIntegrate' n f a b =
    let dx = (b - a) / fromIntegral n
        ys = [f x | x <- [a, a+dx .. b]]
        ys' = zip ys $ tail ys
    in 0.5 * dx * sum [y + y' | (y, y') <- ys']
