(define (memq item x)
(cond ((null? x) false)
((eq? item (car x)) x)
(else (memq item (cdr x)))))

(define (append list1 list2)
(if (null? list1)
list2
(cons (car list1) (append (cdr list1) list2))))

(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(caadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))

(define (equal? a b)
(cond  ((null? a) (null? b))
	   ((null? b) (null? a))
	   (else

			(let ( (x (car a)) 
		   		(y (car b))
		   		(lastA (cdr a))
		   		(lastB (cdr b))
		   		(isX (pair? (car a)))
		   		(isY (pair? (car b)))
		 	) 
			(cond ((AND isX isY) (AND (equal? x y)  (equal? lastA lastB) ))
				  ((OR isX isY) false)
				  (else (AND (eq? x y) (equal? lastA lastB)))
		    )

		    )
	)
)
)

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
	(and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))
(define (addend s) (car s))

 (define (multiplicand p) 
   (if (null? (cdddr p)) 
       (caddr p) 
       (cond ((sum? (cdr p)) () )
       		make-product (multiplier (cdr p)) (multiplicand (cdr p)))
       )
   ) 
  
 (define (augend s) 
   (if (null? (cdddr s)) 
       (caddr s) 
       (make-sum (addend (cdr s)) (augend (cdr s))))) 
 				
(define (product? x) (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) (car p))


(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
(cond ((=number? a1 0) a2)
((=number? a2 0) a1)
((and (number? a1) (number? a2))
(+ a1 a2))
(else (list a1 '+ a2))))

(define (make-product m1 m2)
(cond ((or (=number? m1 0) (=number? m2 0)) 0)
((=number? m1 1) m2)
((=number? m2 1) m1)
((and (number? m1) (number? m2)) (* m1 m2))
(else (list m1 '* m2))))

(define (make-substr a1 a2)
(cond ((=number? a1 0) a2)
((=number? a2 0) a1)
((and (number? a1) (number? a2))
(- a1 a2))
(else (list '- a1 a2))))


(define (exponentiation? x)(and (pair? x) (eq? (car x) '**)))
(define (base x)(cadr x))
(define (exponent x)(caddr x))
(define (make-exponentiation m1 m2)
	(cond ((=number? m2 1) m1)
		  ((=number? m2 0) 1)
		  ((and (number? m1) (number? m2)) (expt m1 m2))
		(else (list '** m1 m2)))
	)



(define (deriv exp var)
	(cond ((number? exp) 0)
		  ((variable? exp) (if (same-variable? exp var) 1 0))
		  ((sum? exp) (make-sum (deriv (addend exp) var)
							(deriv (augend exp) var)))
		  ((product? exp)
					  (make-sum
							(make-product (multiplier exp) (deriv (multiplicand exp) var))
							(make-product (deriv (multiplier exp) var)(multiplicand exp))))
		  ((exponentiation? exp) (make-product (exponent exp) (make-exponentiation (base exp) (make-substr (exponent exp) 1))))
		  (else (error "unknown expression type: DERIV" exp))
		  )
	)
