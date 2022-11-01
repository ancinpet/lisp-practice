;homemade arithmetic

(defun soucet (x y)
 (if  (eq y 0)
   x
   (+ 1 (soucet x (- y 1))) ))

(defun rozdil (x y)
 (if  (eq y 0)
   x
   (- (rozdil x (- y 1)) 1) ))

(defun ctverec_helper (x y)
 (if (eq y 0)
  0
  (soucet x (ctverec_helper x (- y 1)))))

(defun ctverec (x)
 (ctverec_helper x x))

(defun soucin (x y)
 (ctverec_helper x y))

(defun mocnina (n k)
 (if (eq k 0)
  1
  (soucin n (mocnina n (- k 1)))))

(defun fib (n)
 (if (eq n 1)
  1
  (if (eq n 2)
   1
   (+ (fib (- n 1)) (fib (- n 2))))))

(defun delka (a)
 (if (eq a ())
  0
  ( + 1 (delka (cdr a)))))

(defun pocet_atomu (a)
 (if (eq a ())
  0
  (if (atom (car a))
   ( + 1 (pocet_atomu (cdr a)))
   ( + (pocet_atomu (car a)) (pocet_atomu (cdr a))))))
