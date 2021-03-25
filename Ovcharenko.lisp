;вместо car писать (lambda (x) (car x))  ???

(print "Задача 2")

;Определите функцию, возвращающую последний элемент списка

(defun last-el (lst)
    ((lambda (f r)
    (cond
        ((null lst) nil)
        ((null r) f)
        (t (last-el r))
        ))
        (car lst)
        (cdr lst)
        )
    )

(print ( last-el '( )))
(print ( last-el '(1)))
(print ( last-el '(1 2 3 4)))

(print "Задача 5")

;Определите функцию, которая увеличивает элементы исходного списка на единицу.

(defun plus-one (lst)
    (cond
        ((null lst) nil)
        (t (cons (1+ (car lst)) (plus-one (cdr lst))))
        )
    )

(print ( plus-one '( )))
(print ( plus-one '(1)))
(print ( plus-one '(1 2 3 4)))

(print "Задача 7")

;Определите функцию, удаляющую из исходного списка элементы с четными номерами.

(defun delete-even-number (lst)
    (cond
        ((null lst) nil)
        (t (cons (car lst) (delete-even-number (cddr lst))))
        )
    )

(print ( delete-even-number '( )))
(print ( delete-even-number '(1)))
(print ( delete-even-number '(1 2 3 4)))

(print "Задача 13")

;Определите функцию, удаляющую в исходном списке все повторные вхождения элементов.

(defun compare (x lst)
(( lambda (f r)
    (cond
        ((null lst) nil)
        ((eq x f) (compare x r))
        (t (cons f (compare x r)))
        ))
        (car lst)
        (cdr lst)
    )
    )

(defun delete-equals (lst)
((lambda (f)
    (cond
        ((null lst) nil)
        (t (cons f (delete-equals (compare f (cdr lst)))))
        ))
        (car lst)
        )
    )

(print ( delete-equals '( )))
(print ( delete-equals '(1 1)))
(print ( delete-equals '(1 1 2 3 3 4 1)))

(print "Задача 18")

;Определите предикат, проверяющий, является ли аргумент одноуровневым списком.

(defun one-level-list (lst)
    (cond 
        ((null lst) t)
        ((atom lst) nil) 
        ((atom (car lst)) (one-level-list (cdr lst)))
        (t nil)
        )
    )

(print ( one-level-list '( )))
(print ( one-level-list '(1 2 3 4 5)))
(print ( one-level-list '(1 (2 (3) 3) 4 1)))        

(print "Задача 31")

;Определите функцию (ПЕРВЫЙ-СОВПАДАЮЩИЙ х у), которая возвращает первый элемент, входящий в оба списка х и у, в противном случае NIL.

(defun member1 (a x)
    (cond
        ((null x) nil)
        ((eq a (car x)) T)
        (t (member1 a (cdr x)) )
        )
    )

(defun первый-совпадающий (x y)
((lambda (f)
    (cond 
        ((null x) nil)
        ((null y) nil)
        ((member1 f y) f)
        (t (первый-совпадающий (cdr x) y))
        ))
        (car x)
        )
    )

(print ( первый-совпадающий '() '()))
(print ( первый-совпадающий '(1 2 3 4 5) '(1 6 7 8)))
(print ( первый-совпадающий '(1 2 3 4 5) '(6 7 8 5)))

(print "Задача 37")

;Определите функцию ПЕРЕСЕЧЕНИЕ, формирующую пересечение двух множеств, т.е. множество из их общих элементов.

(defun member1 (a x)
    (cond
        ((null x) nil)
        ((eq a (car x)) T)
        (t (member1 a (cdr x)) )
        ) 
    )

(defun пересечение (x y)
((lambda (f r)
    (cond
        ((null x) nil)
        ((member1 f y) (cons f (пересечение r y)))
        (t (пересечение r y))
        ))
        (car x)
        (cdr x)
        )
    )

(print ( пересечение '() '()))
(print ( пересечение '(1) '(2)))
(print ( пересечение '(1 2 3 4 5) '(6 1 7 8 5)))

(print "Задача 40")

;Определите функцию РАЗНОСТЬ, формирующую разность двух множеств, т.е. удаляющую из первого множества все общие со вторым множеством элементы.

(defun member1 (a x)
    (cond
        ((null x) nil)
        ((eq a (car x)) T)
        (t (member1 a (cdr x)) )
        ) 
    )

(defun разность (x y)
((lambda (f r)
    (cond
        ((null x) nil)
        ((member1 f y) (разность r y))
        (t (cons f (разность r y)))
        ))
        (car x)
        (cdr x)
        )
    )

(print ( разность '() '()))
(print ( разность '(1 2 3) '(4 5 6)))
(print ( разность '(1 2 3 4 5 6 9) '(6 1 7 8 5)))

(print "Задача 43")

;Определите функцию, подсчитывающую количество всех вершин данного дерева заданной высоты.

(defun количество-вершин ( tree )
    (cond 
        ((null tree) 0)
        ((equal (cdr tree) (list nil nil)) 1)
        (t ( + ( количество-вершин (cadr tree)) ( количество-вершин (caddr tree))))
        )
    )
   
(print (количество-вершин '( )))
(print (количество-вершин '(1 nil nil)))
(print (количество-вершин '(5 ( 3 (1 nil nil ) ( 4 nil nil )) ( 7 ( 6 nil nil) ( 13 ( 11 nil nil) ( 15 nil nil ))))))

(print "Задача 48")

;Функция GET возвращает в качестве результата NIL в том случае, если у символа нет данного свойства, либо если значением этого свойства является NIL. Следовательно, функцией GET нельзя проверить, есть ли некоторое свойство в списке свойств. Напишите предикат (ИМЕЕТ-СВОЙСТВО символ свойство), который проверяет, обладает ли символ данным свойством

(defun member1 (a x)
    (cond
        ((null x) nil)
        ((eq a (car x)) T)
        (t (member1 a (cdr x)) )
        ) 
    )

(setf (get 'духи 'фирма) 'Шанель)
(setf (get 'духи 'название) 'Мадмуазель)
(setf (get 'духи 'производитель) 'Франция)
(setf (get 'духи 'аромат) 'цветочный)

(print (symbol-plist 'духи))

(defun имеет-свойство ( символ свойство) 
    (if (member1 свойство (symbol-plist символ))  T))
    
(print (имеет-свойство 'духи 'автор ))
(print (имеет-свойство 'духи 'название ))
