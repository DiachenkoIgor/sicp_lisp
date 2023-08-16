(define (element-of-set? x set)
(cond ((null? set) false)
((eq? x (car set)) true)
(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
(if (element-of-set? x set)
set
(cons x set)))


(define (get-size set)
	(if (null? set)
		0
		(+ 1 (get-size (cdr set)) )
))


(define (count-pairs x)

	(define (count-pairs-inner x nodes)
		(if (not (pair? x))
			nodes
			(begin (set! nodes (count-pairs-inner (car x) nodes))
				(set! nodes (count-pairs-inner (cdr x) nodes))
				(adjoin-set x nodes)
			))
		)

(count-pairs-inner x '())
)

(define third (cons 'a 'b)) 
 (define second (cons third third)) 
 (define first (cons second second)) 


(get-size (count-pairs first))

(define (cycle-reference? arg)
	(define (cycle-reference-inner arg elems)
		(cond ((null? arg) #t)
			  ((NOT(pair? arg)) #t)
			  ((element-of-set? arg elems) #f)
			  (else (AND (cycle-reference-inner (car arg) (adjoin-set arg elems))  
			  			 (cycle-reference-inner (cdr arg) (adjoin-set arg elems))))
			)
)
	(NOT (cycle-reference-inner arg '()))
	)

(cycle-reference? third)


(define (floyd-cycle x)
	(define (step-two arg)
		(cond ((null? (cdr arg)) nil)
			  (else (cddr arg)))
	)
	(let( (hare x)
		  (tortoise x)
		  (isMet #f)
		)
		(define (floyd-cycle-inner)
			(display hare)
			(newline)
			(begin 
				(set! hare (step-two hare))
				(set! tortoise (cdr tortoise))
				(cond ((null? hare) nil)
					  ((AND (eq? hare tortoise) isMet) hare)
					  ((eq? hare tortoise) (begin (set! isMet #t) (floyd-cycle-inner)))
					  (else (floyd-cycle-inner)))
			)
		)
		(floyd-cycle-inner)
	)
)





