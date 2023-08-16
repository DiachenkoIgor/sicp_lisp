(define (count-leaves x)
(cond ((null? x) 0)
((not (pair? x)) 1)
(else (+ (count-leaves (car x))
(count-leaves (cdr x))))))

(define (deep-reverse par)
	(define (inner-reverse list1 result)
		(cond ((null? list1) result) 
			  ((pair? (car list1)) (inner-reverse (cdr list1) (cons (inner-reverse (car list1) '()) result)))
			  (else (inner-reverse (cdr list1) (cons (car list1) result)))
			)	
	)
	(inner-reverse par (list))
)

(define x (list (list 1 2) (list 3 4)))

(deep-reverse x)


(define (fringe list1)
	(define (fringe-inner items result)
			(cond ((null? items) result) 
			  ((pair? (car items)) (fringe-inner (car items) (fringe-inner (cdr items) result))  )
			  (else (cons (car items) (fringe-inner (cdr items) result)))
			)
	)
	(fringe-inner list1 '())
)

(fringe (list x x))


(define (make-mobile left right)
(list left right))

(define (make-branch length structure)
(list length structure))

(define left-branch car)
(define right-branch cadr)
(define branch-length car)
(define branch-structure cadr)

(define test-mobile (make-mobile (make-branch 1 (make-mobile (make-branch 2 2) (make-branch 2 2)))
				 (make-branch 1 4) ))

(define (total-weight tree)
	(cond ((null? tree) 0) 
		  ( (not (pair? tree)) tree)
		  (else (+ (total-weight (branch-structure (left-branch tree)))
		  		   (total-weight (branch-structure (right-branch tree))))
		  	)

		)
	)

(define (is-balanced tree)
	(let ( (left (branch-structure (left-branch tree)))
		(right (branch-structure (right-branch tree)))
		(left-length (branch-length (left-branch tree)))
		(right-length (branch-length (right-branch tree)))
		(is-left (pair? (branch-structure (left-branch tree)))) 
			(is-right (pair? (branch-structure (right-branch tree))))
		) 
	(cond
			((and is-left is-right)(and (is-balanced left) (is-balanced right)
										(= ( * (total-weight left) left-length)
										   ( * (total-weight right) right-length)
											))
			)
			((AND is-left (not is-right)) (AND 
					(is-balanced left)
					(= ( * (total-weight left) left-length)
					   ( * (branch-structure (right-branch tree)) right-length)
											)

				))
			((AND is-right (not is-left)) (AND 
					(is-balanced right)
					(= ( * (total-weight right) right-length)
					   ( * (branch-structure (left-branch tree)) left-length)
											)

				))
			(else (= ( * (branch-structure (left-branch tree)) left-length)
					 ( * (branch-structure (right-branch tree)) right-length)
											)
				)
			)
		)
	)

(define (scale-tree tree factor)
	(cond ((null? tree) tree)
		  ((pair? (car tree)) (cons (scale-tree (car tree) factor) (scale-tree (cdr tree) factor)))
		  (else (cons (factor (car tree)) (scale-tree (cdr tree) factor))) 
		)
	)

(define (scale-tree-map tree factor)
	(map (lambda (sub-tree) (

					if (pair? sub-tree)
						(scale-tree-map sub-tree factor) 
						(factor sub-tree)
						)) 
	tree)
	)

(define (subsets s)
	(if (null? s)
		(list '())
		(let ((rest (subsets (cdr s))))
		(append rest (map
			(lambda (item)(cons (car s) item)) rest)))))

(subsets (list 1 2 3))


(define (accumulate op initial sequence)
(if (null? sequence)
initial
(op (car sequence)
(accumulate op initial (cdr sequence)))))

(define (map p sequence)
	(accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
	(accumulate cons seq2 seq1))

(define (length sequence)
	(accumulate (lambda (x y) (+ y 1)) 0 sequence))


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










