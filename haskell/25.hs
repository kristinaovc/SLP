--Задача 25.Реализовать алгоритм быстрой сортировки.
quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser = filter (< p) xs
        greater = filter (>= p) xs
main = print (quicksort [1, 7, 4, 2])
-- [1,2,4,7]