(define (create-accumulator initial)
	(lambda (y) (begin (set! initial (+ initial y)) initial))
)

(define accumulator (create-accumulator 10))

(accumulator 5)

(accumulator 10)

(accumulator 1)

(define (make-monitored f)
	(define (inner count)
			(lambda (y)(cond ((eq? y 'how-many-calls?) count) 
								((eq? y 'reset-count) (begin (set! count 0)))
								(else (begin (set! count (+ count 1)) (f y)))
					)
			)
	)
	(inner 0)
)

(define (make-account balance pass)
	(define (make-account-inner attempt)
		(define (wrong-pass)
			(display attempt)
			(let ((att (- attempt 1)))
				(if ( < att 1) (begin (set! attempt att) (call-cops))
					(begin (set! attempt att) (error "Incorrect password"))
					)
				)
			)
	(define (withdraw amount)
				(if (>= balance amount)
					(begin (set! balance (- balance amount)) balance)
					"Insufficient funds"))
	(define (deposit amount)
				(set! balance (+ balance amount))
				balance)
	(define (dispatch param m)
			(if (NOT(eq? param pass))
				(wrong-pass)
				(cond ((eq? m 'withdraw) withdraw)
						((eq? m 'deposit) deposit)
						(else (error "Unknown request: MAKE-ACCOUNT" m)))))

dispatch)
	(make-account-inner 3)
	)

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)


(define rand (let ((x random-init))
(lambda ()
(set! x (rand-update x))
x)))

(define (monte-carlo trials experiment)
(define (iter trials-remaining trials-passed)
(cond ((= trials-remaining 0)
(/ trials-passed trials))
((experiment)
(iter (- trials-remaining 1)
(+ trials-passed 1)))
(else (iter (- trials-remaining 1)
trials-passed))))
(iter trials 0))

(define (random-in-range low high)
(let ((range (- high low)))
(+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2 trials)

	(define (point-integrator )
		(lambda () 
			(P (random-in-range x1 x2) (random-in-range y1 y2))
		)
	)


	(define (estimate-integral-inner)
		(let ((height (abs (- y1 y2))) 
		  (width (abs (- x1 x2))))
			(* (* height width) (monte-carlo trials (point-integrator)))
		)
	)
	(estimate-integral-inner)
)

(define (circle-predicate xS yS r)
	(lambda (x y)(>= (* r r) (+ (expt (- x xS) 2) (expt (- y yS) 2)))
	)
)

(estimate-integral (circle-predicate 2 4 3) 2 8 4 10 10000)


(define rand
	(let ((x random-init))
		(lambda (symbol .. args)
				(if (eq? symbol 'generate)
					 (begin (set! x (rand-update x)) x)
					 (begin (set! x (car args)) (set! x (rand-update x)) x)
				)
)))

(define rand (let ((x random-init))
(lambda ()
(set! x (rand-update x))
x)))


(define (make-joint acc pass new-pass)
	(lambda (passP opP)
			(if (eq? passP new-pass)
				(acc pass opP)
				(error "Wrong pass" passP)
				)
		)
)

(define paul-acc (make-joint acc 'secret-password 'rosebud))

((paul-acc 'rosebud 'withdraw) 40)


(define (order-proc)
	(let ((prev-num -1))
		(lambda (x)
				(cond ((AND (= 0 prev-num) (= x 1))
							(begin (set! prev-num x) 0))
						((AND (= 1 prev-num) (= x 0))
							 (begin (set! prev-num x) 1))
						(else (begin (set! prev-num x) 0))
			)
		)
		)
	)

(define f (order-proc))

(+ (f 0) (f 1))




(define (last-pair x)
(if (null? (cdr x)) x (last-pair (cdr x))))


(define (make-cycle x)
(set-cdr! (last-pair x) x)
x)


(define z (make-cycle (list 'a 'b 'c)))

(last-pair z)