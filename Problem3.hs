{-
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143?
-}
import Data.List (nub,union)

m_prime :: Integer -> Bool
m_prime p = not $ any (\x -> (mod p x) == 0) (takeWhile (<= m_flrt p) primes)

m_flrt = floor . sqrt . fromIntegral

primes = 2 : (filter m_prime [3,5..])

m_divsBy n k = (mod n k) == 0

primefactors :: Integer -> [Integer]
primefactors n = nub $ p_h n (takeWhile (<= (div n 2)) primes) []
    where p_h 1 _ acc = acc
          p_h _ [] acc = acc
          p_h n (x:xs) acc = if m_divsBy n x then p_h (div n x) xs (x:acc) else p_h n xs acc
