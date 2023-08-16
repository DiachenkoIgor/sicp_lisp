

(define (element-of-set? x set)
(cond ((null? set) false)
((equal? x (car set)) true)
(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
(if (element-of-set? x set)
set
(cons x set)))

(define (intersection-set set1 set2)
(cond ((or (null? set1) (null? set2)) '())
((element-of-set? (car set1) set2)
(cons (car set1) (intersection-set (cdr set1) set2)))
(else (intersection-set (cdr set1) set2))))

(define (union-sets set1 set2)
	(cond
		((null? set1) set2)
		((element-of-set? (car set1) set2) (union-sets (cdr set1) set2))
		(else (cons (car set1) (union-sets (cdr set1) set2)))
	)
)

(union-sets (list 1 3 5 6) (list 1 2 4 6))


(define (adjoin-set-ordered x set)
	(cond 
		((null? set) x)
		((= x (car set)) set)
		((< x (car set)) (cons x set))
		((> x (car set)) (cons (car set) (adjoin-set-ordered x (cdr set)))) 
	)
)

(adjoin-set-ordered 4 (list 3 5 7 8))

(define (union-set-ordered set1 set2)
	(cond 
		((null? set1) set2)
		((null? set2) set1)
		((= (car set2) (car set1)) (cons (car set1) (union-set-ordered (cdr set1) (cdr set2))))
		((< (car set1) (car set2)) (cons (car set1) (union-set-ordered (cdr set1) set2)))
		((> (car set1) (car set2)) (cons (car set2) (union-set-ordered set1 (cdr set2)))) 
	)
)

(define (append list1 list2)
(if (null? list1)
list2
(cons (car list1) (append (cdr list1) list2))))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
(list entry left right))

(define (list->tree elements)
(car (partial-tree elements (length elements))))
(define (partial-tree elts n)
(if (= n 0)
(cons '() elts)
(let ((left-size (quotient (- n 1) 2)))
(let ((left-result
(partial-tree elts left-size)))
(let ((left-tree (car left-result))
(non-left-elts (cdr left-result))
(right-size (- n (+ left-size 1))))
(let ((this-entry (car non-left-elts))
(right-result
(partial-tree
(cdr non-left-elts)
right-size)))
(let ((right-tree (car right-result))
(remaining-elts
(cdr right-result)))
(cons (make-tree this-entry
left-tree
right-tree)
remaining-elts))))))))

(define (ordered-tree-to-list tree)
	(if (null? tree) '()
		(let ( (right (ordered-tree-to-list (right-branch tree)))
			   (left (ordered-tree-to-list (left-branch tree)))
			   (val (entry tree))
		 	 )
		(cond ((AND (null? left) (null? right)) val )
			  ((null? left) (cons val right))
			  ((pair? left) (append left (cons val right)))
			  (else (cons left (cons val (cons right '()) )))
			)

		)
	)
)

(list->tree (list ()))

(list->tree	(union-set-ordered 
		(ordered-tree-to-list (list->tree (list 0 1 3 5 7 9 11)))
		(list 2 4 6 8 10)
	)
)

(define (key entity) (car entity))
(define (value entity) (cadr entity))

(define (ordered-tree-lokup givenK tree)
	(cond ((null? tree) false )
		   ((= givenK (key (car tree))) (value (car tree)))
		   ((< givenK (key (car tree))) ordered-tree-lokup givenK (right-branch tree))
			(else (ordered-tree-lokup givenK (left-branch tree)))
	)
)




