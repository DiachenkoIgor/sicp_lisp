
(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

(stream-car (stream-cdr(stream-cdr(stream-cdr fibs))))

(define (scale-stream stream factor)
(stream-map (lambda (x) (* x factor))
stream))

(define (stream-ref s n)
	(if (= n 0)
		(stream-car s)
		(stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc s)
	(if (stream-null? s)
		the-empty-stream
		(cons-stream (proc (stream-car s))
			(stream-map proc (stream-cdr s)))))
(define (stream-for-each proc s)
	(if (stream-null? s)
		'done
		(begin (proc (stream-car s))
			(stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
	(stream-for-each display-line s))
(define (display-line x) (newline) (display x))

(define (stream-enumerate-interval low high)
	(if (> low high)
		the-empty-stream
		(cons-stream low
			(stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
(cond ((stream-null? stream) the-empty-stream)
((pred (stream-car stream))
(cons-stream (stream-car stream)
(stream-filter
pred
(stream-cdr stream))))
(else (stream-filter pred (stream-cdr stream)))))

(define (force delayed-object) (delayed-object))

(define (memo-proc proc)
(let ((already-run? false) (result false))
(lambda ()
(if (not already-run?)
(begin (set! result (proc))
(set! already-run? true)
result)
result))))

(define (stream-map proc . argstreams)
	(if ( stream-null? (car argstreams))
		the-empty-stream
	(cons-stream
		(apply proc (map stream-car argstreams))
		(apply stream-map
			(cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2) (stream-map + s1 s2))

(define (mul-stream a b)
	(stream-map * a b)
	)

(define ones (cons-stream 1 ones))

(define negative-ones (cons-stream -1 ones))

(stream-ref ones 8)

 (define integers
(cons-stream 1 (add-streams ones integers)))

(define (partial-sums s)
	(cons-stream (stream-car s) (add-streams s (partial-sums (stream-cdr s)))))

(define test (partial-sums integers))

(stream-cdr test)

(stream-car(stream-cdr(stream-cdr test)))

(define (merge s1 s2)
	(cond ((stream-null? s1) s2)
		  ((stream-null? s2) s1)
		  (else
			(let ((s1car (stream-car s1))
				 (s2car (stream-car s2)))
				(cond ((< s1car s2car)
						(cons-stream s1car
							(merge (stream-cdr s1) s2)))
					  ((> s1car s2car)
						(cons-stream s2car
							(merge s1 (stream-cdr s2))))
					(else
						(cons-stream s1car
							(merge (stream-cdr s1)
								(stream-cdr s2))))
					)
			)
		)
	)
)


(define S (cons-stream 1 (merge
(merge (scale-stream S 2) (scale-stream S 3)) (scale-stream 5 S))))

 (define revert-integers
	(stream-map / ones integers))

(define (integrate-series series)
	(stream-map * revert-integers series)
	)

(define cosine-series (cons-stream 1 (stream-map * sine-series negative-ones)))
(define sine-series (cons-stream 0 cosine-series))

(define (one-value-stream s)
	(cons-stream s (one-value-stream s))
	)

(define (stream-map-tolerance prev tolerance proc . argstreams)
	(if ( stream-null? (car argstreams))
		the-empty-stream
		(if (> tolerance (abs (- prev (car argstreams))))
			(one-value-stream (car argstreams))
			(cons-stream
				(apply proc (map stream-car argstreams))
				(apply stream-map
					(cons proc (map stream-cdr argstreams))))))
	)

(define (interleave s1 s2)
(if (stream-null? s1)
s2
(cons-stream (stream-car s1)
(interleave s2 (stream-cdr s1)))))

(define (pairs s t)
	(cons-stream
		(list (stream-car s) (stream-car t))
		(interleave
			(interleave
				(stream-map (lambda (x) (list (stream-car s) x))(stream-cdr t))
				(stream-map (lambda (x) (list x (stream-car t)))(stream-cdr s))
				)
	(pairs (stream-cdr s) (stream-cdr t)))
	)
)

(define (print-stream s n)
	(if (NOT(= n 0))
		(begin 
			(display (stream-car s))
			(newline)
			(print-stream (stream-cdr s) (- n 1))
			)
		)
	)

(define (integers-find n integers)
	(if (= (stream-car integers) n)
		integers
		(integers-find n (stream-cdr integers))
	)
)

(set-cdr! (cdr (list 1 2)) (list 3))



(define (triplets s t u)
	(define (triplets-convert streams)
		(cons-stream
			(stream-car (stream-car streams))
			(interleave
				(stream-cdr (stream-car streams))
				(triplets-convert (stream-cdr streams))
				)
		)
	)
	(triplets-convert
			(stream-map 
				(lambda (x)
					(stream-map 
						(lambda (val)(begin (set-cdr! (cdr x) (list val)) x))
			   			(integers-find (cadr x) u))
					)
				(pairs s t)
			)
	)
)


(define (merge-weighted s1 s2 weight)
	(cond ((stream-null? s1) s2)
		  ((stream-null? s2) s1)
		  (else
			(let ((s1car (stream-car s1))
				 (s2car (stream-car s2)))
				(cond (( >= 1 (weight s1car s2car))
						(cons-stream s1car
							(merge-weighted (stream-cdr s1) s2 weight)))
					  (( <= -1 (weight s1car s2car))
						(cons-stream s2car
							(merge-weighted s1 (stream-cdr s2) weight)))
					(else
						(cons-stream s1car
							(merge-weighted (stream-cdr s1)
								(stream-cdr s2) weight)))
					)
			)
		)
	)
)

(define (weighted-pairs s t weight)
	(cons-stream
		(list (stream-car s) (stream-car t))
		(merge-weighted
			(stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
			(weighted-pairs (stream-cdr s) (stream-cdr t) weight)
			weight
			)
	)
)

(define pairs-stream (weighted-pairs integers integers	(lambda (pair1 pair2)(- (+ (car pair1)(cadr pair1)) (+ (car pair2)(cadr pair2))))
		))

(define (pair-cube-sum pair)
	(+ (expt (car pair) 3) (expt (cadr pair) 3))
)
(define (pair-square-sum pair)
	(+ (expt (car pair) 2) (expt (cadr pair) 2))
)

(define ordered-pairs-stream
	 (weighted-pairs integers integers 
	 	(lambda (pair1 pair2)
	 		(- (pair-square-sum pair1)
	 		   (pair-square-sum pair2)
			)
		)
	)
)

(define (ramanujan-numbers pairs)
	(let
		((s1 (pair-cube-sum (stream-car pairs)))
		(s2 (pair-cube-sum (stream-car(stream-cdr pairs)))))
		(if (= s1 s2)
			(cons-stream s1 (ramanujan-numbers (stream-cdr pairs)))
			(ramanujan-numbers (stream-cdr pairs))
			)
		)
)


(define (triplets-numbers pairs)
	(let
		((s1 (pair-square-sum (stream-car pairs)))
		(s2 (pair-square-sum (stream-car(stream-cdr pairs))))
		(s3 (pair-square-sum (stream-car(stream-cdr(stream-cdr pairs))))))
		(if (= s1 s2 s3)
			(cons-stream 
				(list s1 (stream-car pairs) (stream-car(stream-cdr pairs)) (stream-car(stream-cdr(stream-cdr pairs))))
				 (triplets-numbers (stream-cdr pairs)))
			(triplets-numbers (stream-cdr pairs))
			)
		)
)


(define (integral integrand initial-value dt)
	(define int
		(cons-stream initial-value
			(add-streams (scale-stream integrand dt) int)
		)
	)
int)

(define (RT R C dt)
	(define (circuit-calculate i v)
		(+ v (* R i) (/ 1 C) dt)
	)
		
	(lambda (seq v)
		(stream-map seq (lambda (val) (circuit-calculate val v)))
		)
)

(define (integral delayed-integrand initial-value dt)
	(newline)(display delayed-integrand)
(define int
(cons-stream
initial-value
(let ((integrand (delayed-integrand)))
	(newline)(display "test")
(add-streams (scale-stream integrand dt) int))))
int)

(define (solve f y0 dt)
(define y (integral (delay dy) y0 dt))
(define dy (stream-map f y))
y)

(define (solve-2nd f dt y0 dy0 )
	(define y
		(integral (delay dy) y0 dt)
		)
	(define dy
		(integral (delay (stream-map f y dy)) dy0 dt)
		)
)

(define (RLC R L C dt)
	(define (circuit iO vO)
		(define v
			(integral (delay dvc) vO dt)
		)
		(define i
			(integral (delay diL) iO dt)
		)
		(define dvc
			(scale-stream i (/ -1 C))
		)
		(define di-one
			(scale-stream i (/ (- 0 R) L))
		)
		(define di-two
			(scale-stream v (/ 1 L))
		)
		(define diL
			(add-streams di-one di-two)
		)
		(define (result-stream v i)
			(newline)(display "result-stream")
			(cons-stream 
				(list (stream-car v) (stream-car i))
				(result-stream (stream-cdr v) (stream-cdr i))
			)
		)
		(result-stream v i)
	)

	(lambda (iO vO)
		(circuit iO vO)
	)
)

 (define (rand-update x) 
         (remainder (+ (* 13 x) 5) 24)) 
 (define random-init (random-update (expt 2 32)))


(define (random input initial)
	(define (stream-generator start)
		(define 
			tmp (cons-stream 
				(rand-update start) (stream-map rand-update tmp))
		)
		tmp
	)
	(define (stream-executor input randoms)
		(let ((operation (car (stream-car input))))
			(if (eq? operation 'generate)
					(cons-stream 
							(stream-car randoms)
							(stream-executor (stream-cdr input)(stream-cdr randoms)))
					(stream-executor 
							(stream-cdr input)
							(stream-generator (cadr (stream-car input))))
			)			
		)
	)
	(newline)
	(stream-executor input (stream-generator initial))
)

