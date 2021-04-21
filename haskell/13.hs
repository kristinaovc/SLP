--Задача 13.Определите функцию, которая, чередуя элементы списков (a b...) и (1 2...),
--образует новый список (a 1 b 2 ...).

interChange [] [] = []
interChange [] [a] = [a]
interChange [a] [] = [a]
interChange (x:xs) (y:ys) =  x : y : interChange xs ys
main = print (interChange [1, 2, 3,8,9] [4, 5 , 6, 7])
-- [1,4,2,5,3,6,8,7,9]