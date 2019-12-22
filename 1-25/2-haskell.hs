-- There are several ways to do this problem. First, the naive way
-- 1) Basic Fibonacci written for clarity
fibClear :: Int -> Integer
fibClear 1 = 1
fibClear 2 = 2
fibClear x = fibClear (x-1) + fibClear (x-2)

fibSlow = [fibClear x | x <- [1..]]

-- 2) Quick, concise method that adds the two tails together
fibConcise = 1 : 2 : zipWith (+) fibConcise (tail fibConcise)

fibEvenSumFromList fibMethod = sum (filter even (takeWhile (<= 4000000) fibMethod) )

slowAnswer = fibEvenSumFromList fibSlow
conciseAnswer = fibEvenSumFromList fibConcise

-- Here are some ways that seem rarely used.
-- From here on I switch the indexing: F0 = 0 and F1 = 1
-- 3) Fast and accurate Dijkstra method
fibD 0 = 0
fibD 1 = 1
fibD i  | isEven = ( 2 * (fibD (i1-1)) + (fibD i1) ) * (fibD i1)
        | otherwise = (fibD (i1-1)) ^2 + (fibD i1)^2
    where 
        isEven = i `mod` 2 == 0
        i1 = if isEven then (i `div` 2) else ((i `div` 2) + 1)

-- 4) Use formulas
-- Phi, one of the characteristic roots of the recurrence relation (x^2-x-1=0)
phi = ( 1 + sqrt(5) ) / 2

-- From R. Knott's FibFormula page. Note - not accurate for small n near a fibonacci number
fibIndex n = floor ( ((logBase 10 n) + (logBase 10 5)/2) / (logBase 10 phi) )

-- Alternative to Binet Formula (Theorem 8.1, Koshy 2018). Inaccurate after 75 or so.
fib i = floor( phi^i / sqrt(5) + 0.5 )

-- Derived from Gibonnaci summation (Eq. 36.3, Koshy 2019), also R. Knott's FibFormulae Page (Identity 25, B&Q 2003)
fibEvenSum fibMethod i = ((fibMethod (i+2)) - 1) `div` 2

formulaAnswer = fibEvenSum fib (fibIndex 4000000)
dijkstraAnswer = fibEvenSum fibD (fibIndex 4000000) 