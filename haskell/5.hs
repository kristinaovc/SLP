--Задача 5.Определите функцию, упаковывающую последовательные дубликаты списка
--в подсписки вида (M N), где N - элемент списка, M - количество повторе-
--ний. Например, [’a’, ’a’, ’a’, ’a’, ’b’, ’c’, ’c’, ’a’, ’a’, ’d’, ’e’,
--’e’, ’e’, ’e’] должен бытб переведен в [(4, ’a’), (1, ’b’), (2, ’c’),
--(2, ’a’), (1, ’d’), (4, ’e’)].
-- Сколько раз элемент входит в список
howMany :: (Eq a) => [a] -> a -> Int
howMany [] _ = 0
howMany (x:xs) n = if (x == n) then 1 + howMany xs n
                   else howMany xs n
-- для каждого элемента первого списк
-- считаем, сколько раз он входит во второй список
-- возвращаем список пар
occu :: (Eq a) => [a] -> [a] -> [(a,Int)]
occu [] _ = []
occu (x:xs) y  = (x , (howMany y x)) : occu xs y

occurencies :: (Eq a) => [a] -> [(a, Int)]
occurencies x = occu x x
main = print (occurencies ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'])
-- [('a',6),('a',6),('a',6),('a',6),('b',1),('c',2),('c',2),('a',6),('a',6),('d',1),('e',4),('e',4),('e',4),('e',4)]