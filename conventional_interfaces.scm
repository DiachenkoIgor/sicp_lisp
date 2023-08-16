(define (accumulate op initial sequence)
(if (null? sequence)
initial
(op (car sequence)
(accumulate op initial (cdr sequence)))))

(define (filter predicate sequence)
(cond ((null? sequence) nil)
((predicate (car sequence))
(cons (car sequence)
(filter predicate (cdr sequence))))
(else (filter predicate (cdr sequence)))))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
(cond ((> (square test-divisor) n) n)
((divides? test-divisor n) test-divisor)
(else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
(define (append seq1 seq2)
	(accumulate cons seq2 seq1))

(define (prime? n)
(= n (smallest-divisor n)))

(define (length sequence)
	(accumulate (lambda (x y) (+ y 1)) 0 sequence))

(define (reverse par)
	(define (inner-reverse list1 result)
		(if (null? list1) 
			result
			(inner-reverse (cdr list1) (cons (car list1) result))
		)
		
	)
	(inner-reverse par (list))
)

(map (lambda (x) (* 2 x)) (list 1 2 3 4 5))

(append (list 1 2 3) (list 4 5 6))

(length (list 1 2 3))

(define nil '())

(define (horner-eval x coefficient-sequence)
(accumulate
	(lambda (this-coeff higher-terms)
		(if (= this-coeff (car coefficient-sequence)) (+ higher-terms this-coeff) (* (+ higher-terms this-coeff) x)))
	0
	coefficient-sequence)
)

(define (accumulate-n op init seqs)
(if (null? (car seqs))
			nil
			(cons (accumulate op init (map (lambda (x)(car x)) seqs))
					(accumulate-n op init (map (lambda (x)(cdr x)) seqs) ))))


(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6)
(list 7 8 9) (list 10 11 12)))

(define (matrix-*-vector m v)
(map (lambda (x)(accumulate + 0 (map * x v)) ) m))

(matrix-*-vector (list (list 2 4 0) (list -2 1 3) (list -1 0 1)) (list 1 2 -1))

(define (transpose mat)
(accumulate-n
cons nil mat))

(transpose (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

(define (matrix_matrix m n)
(let ((cols (transpose n)))
(map
	(lambda (x)(map (lambda (y) (accumulate + 0 (map * x  y))) cols) ) m)

))

(matrix_matrix (list (list 2 -3 1) (list 5 4 -2))  (list (list -7 5) (list 2 -1) (list 4 3)))


(define (fold-left op initial sequence)
(define (iter result rest)
(if (null? rest)
result
(iter (op result (car rest))
(cdr rest))))
(iter initial sequence))

(accumulate / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))

(accumulate list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))

(define (reverse-t sequence)
(fold-left (lambda (x y)
					 	(cons y x))
					 nil sequence))

(reverse-t (list 1 2 3 4))

(define (enumerate-interval low high)
(if (> low high)
nil
(cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
(accumulate append nil (map proc seq)))

(define (unique-pairs n)
	(flatmap (lambda (x) (map (lambda (y)(list x y)) (enumerate-interval 1 (- x 1))))
		(enumerate-interval 1 n)
	)
)

(define (make-pair-sum pair)
(list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum? pair)
(prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
	(map make-pair-sum
	(filter prime-sum? (unique-pairs 6)
	))
)

(unique-pairs 6)


(define (unique-set n quantity)
	(if (= quantity 1)  (enumerate-interval 1 n) 
		(flatmap (lambda (x) (map (lambda (y)(if (pair? y) (cons x y) (list x y) )) (unique-set (- x 1) (- quantity 1))) )
			(enumerate-interval 1 n)
		)
	)
)

(define (unique-triplets n s)
		(filter (lambda (x)(= s (+ (car x) (cadr x) (caddr x))))  (unique-set n 3))
	)

(unique-triplets 6 11)

(define (adjoin-position new-row k rest-of-queens) 
	(append rest-of-queens (list new-row))
	)

(define (sub-list k args)
	(if (= k 0) '() (cons (car args) (sub-list (- k 1)(cdr args)))))

(define (last-pair list)
	(if (null? (cdr list))
		list
		(last-pair (cdr list))
	)
)

(define (safe? k positions)
	(let (
		  (pos (- k 1))
		  (last-elem (car (last-pair positions)) )
		)
		(accumulate 
			(lambda (y z)(AND (not(= (caddr y) last-elem))  (not (= (car y) last-elem)) (not(= (cadr y) last-elem)) z))
		 	#t 
		 	(map (lambda (x y)(list (- x (- k y) ) (+ x (- k y)) x)) (sub-list pos positions) (enumerate-interval 1 pos)))
		)
	)

(define (queens board-size)
	(define (queen-cols k)
		(if (= k 0)
			(list '())
			(filter
				(lambda (positions) (safe? k positions))
				(flatmap
					(lambda (rest-of-queens)
						(map (lambda (new-row)
								(adjoin-position new-row k rest-of-queens))
						(enumerate-interval 1 board-size)))
		(queen-cols (- k 1))))))
(queen-cols board-size))







