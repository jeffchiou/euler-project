-- comprehension with guards
ans1 = sum [ x | x <- [0..999], factors <- [3,5], (mod x factors) == 0]

-- guarded equation
check35 x | x `mod` 3 == 0 = x
          | x `mod` 5 == 0 = x
          | otherwise = 0
ans2 = sum (map check35 [0..999])
