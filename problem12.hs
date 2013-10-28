import Problem3 (divs)

triangles = [ div (n*(2+n-1)) 2 | n <- [1..]]

sol n = head $ dropWhile ((< n ) . fst) trifacs

trifacs = zip (map divs triangles) triangles

main = print $ sol 500
