;multiply lists more efficiently
(defun pronasob (a b)
	(cond ((and (eq a ()) (eq b ())) 
		  ())
		((eq a ())
		  (cons nil (pronasob a (cdr b))))
		((eq b ())
		  (cons nil (pronasob (cdr a) b)))
		((cons (* (car a) (car b)) (pronasob (cdr a) (cdr b))))))
		
		
(defun findMax-helper (a m)
	(if (eq a ())
		m
		(if (> (car a) m)
			(findMax-helper (cdr a) (car a))
			(findMax-helper (cdr a) m))))
			
(defun findMax (a)
	(findMax-helper a (car a)))
	
(defun findMaxPos-helper (a m maxpos pos)
	(if (eq a ())
		(cons m maxpos)
		(if (> (car a) m)
			(findMaxPos-helper (cdr a) (car a)  pos (+ pos 1))
			(findMaxPos-helper (cdr a) m maxpos (+ pos 1)))))
			
(defun findMaxPos (a)
	(findMaxPos-helper a (car a) 0 1))
	
(defun findMaxPosAll-helper (a m maxpos pos)
	(if (eq a ())
		(cons maxpos m)
		(if (> (car a) m)
			(findMaxPosAll-helper (cdr a) (car a)  (list pos) (+ pos 1))
			(if (eq (car a) m)
				(findMaxPosAll-helper (cdr a) m (cons pos maxpos) (+ pos 1))
				(findMaxPosAll-helper (cdr a) m maxpos (+ pos 1))))))
			
(defun findMaxPosAll (a)
	(if (eq a ())
		nil
		(findMaxPosAll-helper (cdr a) (car a) (list 0) 1)))

;comparator	
(defun testfuncall ()
	(funcall '+ 1 2 3 4 5))
	
(defun insert (i a &optional (comp '<))
	(if (eq a ())
		(list i)
		(if (funcall comp i (car a))
			(cons i a)
			(cons (car a) (insert i (cdr a) comp)))))
	
	
(defun insert-sort (a &optional (comp '<))
	(if (eq a ())
		a
		(insert (car a) (insert-sort (cdr a) comp) comp)))
		
;trees
(defparameter *strom*
	'(5
	  (3
           (1 nil nil)
	   (4 nil nil))
	  (10
	   (7 nil nil)
	   (12 nil nil))))
	   
(defun jePrazdny (koren)
	(eq koren ()))
	
(defun jeList (uzel)
	(and (eq (levy uzel) nil) (eq (pravy uzel) nil)))
	
(defun hodnotaUzlu (uzel)
	(car uzel))
	
(defun levy (uzel)
	(cadr uzel))
	
(defun pravy (uzel)
	(caddr uzel))
	
(defun inOrder (koren)
	(if (jePrazdny koren)
		()
		(append 
			(inOrder (levy koren)) 
			(list (hodnotaUzlu koren)) 
			(inOrder (pravy koren)))))
	
(defun preOrder (koren)
	(if (jePrazdny koren)
		()
		(append 
			(list (hodnotaUzlu koren)) 
			(preOrder (levy koren)) 
			(preOrder (pravy koren)))))
	
(defun postOrder (koren)
	(if (jePrazdny koren)
		()
		(append 
			(postOrder (levy koren)) 
			(postOrder (pravy koren))
			(list (hodnotaUzlu koren)))))
			
(defun vyska (koren)
	(if (jePrazdny koren)
		0
		(+ 1 (max (vyska (levy koren)) (vyska (pravy koren))))))
				

(defun delka (a)
 (if (eq a ())
  0
  ( + 1 (delka (cdr a)))))
		
;matrix
(defparameter *matice*
	'((1 2 3) (4 5 6) (7 8 9) (1 2 3)))
	
(defun rozmer (m)
	(cons (delka m) (delka (car m))))
	
(defun nty (a i &optional (p 0))
	(if (eq a ())
		nil
		(if (eq p i)
			(car a)
			(nty (cdr a) i (+ p 1)))))
			
(defun radek (m i)
	(nty m i))
	
(defun sloupec (m i)
	(mapcar (lambda (r) (nty r i)) m))