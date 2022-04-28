module SimpleFunctions where

firstOrEmpty :: [String] -> String
firstOrEmpty lst = if not (null lst) then head lst else "empty"

(+++) :: [a] -> [a] -> [a]
lst1 +++ lst2 = if null lst1
                then lst2
                else (head lst1) : (tail lst1) +++ (lst2)
                
reverse2 :: [a] -> [a]
reverse2 lst = if null lst
               then []
               else reverse2 (tail lst) +++ [head lst]
               
maxmin xs :: [Int] -> (Int, Int)
maxmin xs = let h = head xs
            in if null (tail xs)
               then (h, h)
               else ( if h > t_max then h else t_max
                    , if h < t_min then h else t_min)
                    where t = maxmin (tail xs)
                          t_max = fst t
                          t_min = snd t

