--Задача 15.Определите функцию (ПЕРВЫЙ-СОВПАДАЮЩИЙ х у), которая возвращает первый 
--элемент, входящий в оба списка х и у, в противном случае NIL.
contains x [] = False
contains x (y:ys) = 
    if (x == y) then True
    else contains x ys

firstMatching (x:xs) y = 
    if contains x y then x
    else firstMatching xs y
    
main = print (firstMatching [1, 2, 6] [4, 5 , 6])
-- 6