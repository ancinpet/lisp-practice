;homemade arithmetic 2

(defun podil (a b)
	(if (eq b 0)
		nil
		(if (< a b)
			0
			(+ 1 (podil (- a b) b))))) 
			
(defun modulo (a b)
	(if (eq b 0)
		nil
		(if (< a b)
			a
			(modulo(- a b) b))))

(defun spoj (a b)
	(if (eq a ())
		b
		(cons (car a) (spoj (cdr a) b))))

(defun otoc (a)
	(if (eq a ())
		a
		(spoj (otoc (cdr a)) (list (car a)))))
		
(defun fact(n)
	(if (eq n 0)
		1
		(* n (fact (- n 1)))))

;binary pow
(defun bmocnina (n k)
	(if (eq k 0)
		1
		(if (evenp k)
			(* (bmocnina n (/ k 2)) (bmocnina n (/ k 2)))
			(* n (bmocnina n (/ (- k 1) 2)) (bmocnina n (/ (- k 1) 2))))))

;more efficient binary pow
(defun ebmocnina (n k)
	(if (eq k 0)
		1
		(if (evenp k)
			(let ((x (ebmocnina n (/ k 2)))) (* x x))
			(let ((x (ebmocnina n (/ (- k 1) 2)))) (* n x x)))))
			
;more efficient binary pow 2
(defun ebmocnina2 (n k)
	(if (eq k 0)
		1
		(if (evenp k)
			((lambda (x) (* x x)) (ebmocnina2 n (/ k 2)))
			((lambda (x) (* n x x)) (ebmocnina2 n (/ (- k 1) 2))))))

;factorial with accumulator
(defun fac2 (n)
	(factAcc (n 1)))

(defun factAcc (n acc)
	(if (eq n 0)
		acc
		(factAcc (- n 1) (* n acc))))
		
;fibonacci with accumulator
(defun fib2 (n)
	(fibAcc n 1 0))
	
(defun fibAcc (n acc ac)
	(if (eq n 0)
		ac
		(fibAcc (- n 1) (+ acc ac) acc)))
	
;multiply lists
(defun pronasob (a b)
	(if (and (eq a ()) (eq b ()))
		()
		(if (eq a ())
			(cons nil (pronasob a (cdr b)))
			(if (eq b ())
				(cons nil (pronasob (cdr a) b))
				(cons (* (car a) (car b)) (pronasob (cdr a) (cdr b)))))))