;~-- Вариант 10

;;; Задача 5: Определите функцию, которая увеличивает элементы исходного списка на единицу.

(defun inc(li)
	(cond 
		( (null li) nil )
		( t (cons (+(car li) 1) (inc(cdr li))) )
	) 
)

;~- Test cases

;	Input: (inc '(1 2 3 4))
;	Output: (2 3 4 5)

;	Input: (inc '(-3 -4 -5 -6))
;	Output: (-2 -3 -4 -5)

;	Input: (inc '())
;	Output: NIL

;	Input: (inc '(-3 -4 -5 -6))
;	Output: (-2 -3 -4 -5)

;	Input: (inc '(0))
;	Output: (1)
;;; Задача 24: Определите функции, осуществляющие преобразования между видами (a b c) и (((a) b) c).

(defun left-gluing (li a)
	(cond
		((null li) a)
		(t (left-gluing (cdr li) (cons a (list(car li)))))
	)
)

(defun result (li)
	(left-gluing (cdr li) (list(car li)))
)

;~- Test cases

;	Input: (result '())
;	Output: (NIL)

;	Input: (result '(a b c))
;	Output: (((A) B) C)

;	Input: (result '(1 a 2 b 3 (c 5)))
;	Output: ((((((1) A) 2) B) 3) (C 5))

;;;; 31) Определите функцию(ПЕРВЫЙ-СОВПАДАЮЩИЙ х у), которая возвращает первыйэлемент, входящий в оба спискахиу, в противном случае
NIL


(defun first-not-null (arr fun)
	(cond
		((null arr) Nil)
		(T
			(let
				((ans (funcall fun (car arr))))
				(if (null ans)
					(first-not-null (cdr arr) fun)
					ans
				)
			)
		)
	)
)

(defun first-match (arr1 arr2)
	(first-not-null arr1
		(lambda (x)
			(first-not-null arr2
				(lambda (y)
					(if (= x y)
						x
						Nil
					)
				)
			)
		)
	)
)

(defun output (func &rest args)
	(format t "With args ~a received ~a~%" args (apply func args))
)



(output 'first-match '(5 6 7 8) '(9 7 3)) 	; 7
(output 'first-match '(6 7 8) '(3 4 5)) 	; Nil
(output 'first-match '(1 2 3) '(7)) ; Nil

;;;; 41) Реализовать генератор деревьев, чтобы выдаваемые им деревья имели коли-
чество вершин, точно соответствующее числу, указанному в его первом аргу-
менте.

(defun generate-tree (n)
	(if (= n 0)
		Nil
		(list
			(generate-tree (ceiling (/ (- n 1) 2)))
			(list n)
			(generate-tree (floor (/ (- n 1) 2)))
		)
	)
)

(defun output (func &rest args)
	(format t "With args ~a received ~a~%" args (apply func args))
)

(output 'generate-tree 0) ; Nil
(output 'generate-tree 1) ; (NIL (1) NIL)
(output 'generate-tree 2) ; ((NIL (1) NIL) (2) NIL)
(output 'generate-tree 3) ; ((NIL (1) NIL) (3) (NIL (1) NIL))
(output 'generate-tree 4) ; (((NIL (1) NIL) (2) NIL) (4) (NIL (1) NIL))
