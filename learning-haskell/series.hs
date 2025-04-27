factorial x = product [1..x]

inverse x = 1/x

toInt :: Float -> Int
toInt x = round x

altHarmonicTerm x = (inverse x) * (-1)^(toInt (x+1))


harmonicSeries x = sum ( map inverse [1..x] ) -- the harmonic series

altHarmonicSeries x = sum ( map altHarmonicTerm [1..x]) -- the alternating harmonic series

eSeries x = sum (map inverse (map factorial [0..x])) -- an approximation of e