{-
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143?
-}
module Problem3 where

import Data.List (nub,union,sort)

m_prime :: Integer -> Bool
m_prime p = not $ any (\x -> (mod p x) == 0) (takeWhile (<= m_flrt p) primes)

m_flrt = floor . sqrt . fromIntegral

primes = 2 : (filter m_prime [3,5..])

m_divsBy n k = (mod n k) == 0

primefactors :: Integer -> [Integer]
primefactors n = p_h n (takeWhile (<= (div n 2)) primes) []
    where p_h 1 _ acc = acc
          p_h _ [] acc = acc
          p_h n (x:xs) acc = if m_divsBy n x then p_h (div n x) xs (x:acc) else p_h n xs acc

pfacc n = cc pf n 1
    where pf = primefactors n
          cc [] _ _ = []
          cc (x:xs) n k | rem n (x^(k+1)) == 0 = cc (x:xs) n (k+1)
                        | otherwise            = (x,k) : cc xs n 1

divs :: Integer -> Integer
divs n = foldl (*) 1 $ map ((+1) . snd) ll
    where ll = pfacc n
