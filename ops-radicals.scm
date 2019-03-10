;; a radical is a pair consisting of a coefficient, and radicand.
;; ie (* coefficient (sqrt radicand))

;; add-radicals: radical radical -> radical(or number)
;; this procedure adds two radicals.
;; given: (add-radicals (* 1 (sqrt 16)) (* 3 (sqrt 27)))
;; expect: (+ 4 (* 9 (sqrt 3)))
(define (add-radicals e1 e2)
  (let ((radical1 (simplify e1))
	(radical2 (simplify e2)))
    (cond ((and (product? radical1)
		(product? radical2))
	   (if (same-radicand? radical1 radical2)
	       (make-product (+ (coefficient radical1)
				(coefficient radical2))
			     (make-sqrt (radicand radical1)))
	       (make-addition radical1 radical2)))
	  ((and (product? radical1)
		(number? radical2))
	   (make-addition radical1 radical2))
	  ((and (number? radical1)
		(product? radical2))
	   (make-addition radical1 radical2))
	  ((and (number? radical1)
		(number? radical2))
	   (+ radical1 radical2))
	  (else (error "Invalid operands -- ADD-RADICALS")))))

;; simplify: radical -> radical(or number)
;; given a radical, it triggers `iter`. in turn, `iter` simplifies the radical.
;; given: (* 1 (sqrt 16)); expect: 4
;; given: (* 1 (sqrt 27)); expect: (* 3 (sqrt 3))
;; given: (* 1 (sqrt 7)); expect: (* 1 (sqrt 7))
(define (simplify x)
  (iter 2 (coefficient x) (radicand x)))

(define (iter a b max)
  (cond ((>= a max) (make-product (* 1 b) (make-sqrt max))) ; first case, (* 1 (sqrt 7))
	((perfect-square? max) (* b (sqrt max))) ; sec case, (* 1 (sqrt 16))
	((= (remainder max a) 0)
	 (let ((n (/ max a)))
	   (if (perfect-square? n)
	       (make-product (* b (sqrt n))
			     (make-sqrt a))
	       (iter (+ a 1) b max))))
	(else (iter (+ a 1) b max))))

;;----
;; helpers

;; number -> bool
(define (perfect-square? n)
  (define (iter a b max)
    (cond ((>= a max) #f)
	  ((= (* a b) max) #t)
	  (else (iter (+ a 1) (+ b 1) max))))
  (iter 1 1 n))

;; expression expression -> expression
(define (make-addition e1 e2)
  (list '+ e1 e2))

;;----
;; representation of radicals
(define (make-product a b) (list '* a b))
(define (product? exp) (and (pair? exp) (eq? (car exp) '*)))
(define (make-sqrt radicand)
  (list 'sqrt radicand))
(define (radicand radical)
  (cadr (caddr radical)))
(define (coefficient radical)
  (cadr radical))
