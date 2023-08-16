(define (last-pair list)
	(if (null? (cdr list))
		list
		(last-pair (cdr list))
	)
)

(last-pair (list 1 2 3 4 5 6))

(define test-list (list 1 2 3 4 5 6))


(define (reverse par)
	(define (inner-reverse list1 result)
		(if (null? list1) 
			result
			(inner-reverse (cdr list1) (cons (car list1) result))
		)
		
	)
	(inner-reverse par (list))
)

(reverse (list 1 2 3 4 5 6))


#| (define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
(cond ((= amount 0) 1)
((or (< amount 0) (= kinds-of-coins 0)) 0)
(else (+ (cc amount
(- kinds-of-coins 1))
(cc (- amount
(first-denomination
kinds-of-coins))
kinds-of-coins)))))
 |#
#| (define (first-denomination kinds-of-coins)
(cond ((= kinds-of-coins 1) 1)
((= kinds-of-coins 2) 5)
((= kinds-of-coins 3) 10)
((= kinds-of-coins 4) 25)
((= kinds-of-coins 5) 50))) |#

(define (cc amount kinds-of-coins)
(cond ((= amount 0) 1)
((or (< amount 0) (null? kinds-of-coins)) 0)
(else (+ (cc amount
(cdr kinds-of-coins))
(cc (- amount
(car kinds-of-coins))
kinds-of-coins)))))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(cc 100 us-coins)

(define (same-parity x . args)
	(define (inner-parity isParity? list1)
			(if (null? list1)
				list1
				(if (isParity? (car list1))
					(cons(car list1) (inner-parity isParity? (cdr list1)))
					(inner-parity isParity? (cdr list1))
				 )
				)
		)
	(inner-parity (lambda (y)(= (remainder y 2) (remainder x 2)))  args)
	)

(same-parity 2 2 3 4 5 6 7 8)


(define (mapN func items)
	(if (null? items)
			'()
			(cons (func (car items)) (mapN func (cdr items)))
		)
	)

(mapN (lambda (x) (* x 10)) (list 1 2 3 4 5))

(define (for-eachm func args)
		(cond ((null? args) #t)
			  (else ((func (car args)) (for-each func (cdr args))))
	)
)

(for-eachm (lambda (x)
(newline)
(display x))
(list 57 321 88))
