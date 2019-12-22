-- Phi, one of the characteristic roots of the recurrence relation (x^2-x-1=0)
phi = (1+sqrt(5))/2
-- From R. Knott's FibFormula page. Note - not accurate for small n near a fibonacci number
fibIndex n = floor ( ((logBase 10 n) + (logBase 10 5)/2) / (logBase 10 phi) )
-- Alternative to Binet Formula (Theorem 8.1, Koshy 2018). Inaccurate after 75 or so.
fib i = floor( phi^i / sqrt(5) + 0.5 )
-- Dijkstra Method
fibD 0 = 0
fibD 1 = 1
fibD i  | isEven = ( 2 * (fibD (i1-1)) + (fibD i1) ) * (fibD i1)
        | otherwise = (fibD (i1-1)) ^2 + (fibD i1)^2
    where 
        isEven = i `mod` 2 == 0
        i1 = if isEven then (i `div` 2) else ((i `div` 2) + 1)

-- Gibonnaci Summation, (Eq. 36.3, Koshy 2019), R. Knott's FibFormulae Page (Identity 25, B&Q 2003)
fibEvenSum i = ((fib (i+2)) - 1) `div` 2