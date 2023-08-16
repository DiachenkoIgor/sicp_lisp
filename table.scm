(define (assoc-inner key records same-key?)
		(cond   ((null? records) false)
				((null? key) false)
				((same-key? key (caar records)) (car records))
				(else (assoc key (cdr records) same-key?))
		)
)

(define (assoc key records) 
	(assoc-inner key records equal?)
)

(define (assoc key records same-key?) 
	(assoc-inner key records same-key?)
)


(define (make-table same-key?)
	(let ((local-table (list '*table*)))
			(define (lookup-inner table keys)
				(cond ((null? table) false)
					  ((AND (null? keys) (NOT (null? table))) (cdr table))
					  ((null? keys) false)
					  (else	(let ((subtable (assoc (car keys) (cdr table) same-key?)))
						(if subtable
							(lookup-inner subtable (cdr keys))
							false)
						))
				)
			)
			(define (lookup keys)
				(lookup-inner local-table keys)
			)
			(define (insert-inner! table keys value)
					(cond ((null? keys) 'ok)
					  (else	(let ((subtable (assoc (car keys) (cdr table) same-key?)))

						(if subtable
							(insert-inner! subtable (cdr keys) value)
							(if (null? (cdr keys))

								(set-cdr! table (cons (list (car keys) value)
								(cdr table)))

								(begin (set-cdr! table (cons (list (car keys))
								(cdr table)))
								(insert-inner! (cadr table) (cdr keys) value))
							)
						)
						)
					  )
				)
			)

			(define (insert! keys value)
				(insert-inner! local-table keys value)
				)

		(define (dispatch m)
			(cond   ((eq? m 'lookup-proc) lookup)
					((eq? m 'insert-proc!) insert!)
					(else (error "Unknown operation: TABLE" m))))
	dispatch)
)

(define table (make-table eq?))

((table 'insert-proc!) (list 10 5 8) 'test)
((table 'insert-proc!) (list 1 7 12) 'test2)
((table 'insert-proc!) (list 4 0 15) 'test3)

((table 'lookup-proc) (list 4 0 15))



(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
(list entry left right))

(define (entry-key entry) (car entry))

(define (entry-value entry) (cdr entry))


(define (ordered-tree-lokup givenK tree)
	(cond ((null? tree) false )
		   ((= givenK (entry-key (car tree))) (entry-value (car tree)))
		   ((< givenK (entry-key (car tree))) ordered-tree-lokup givenK (right-branch tree))
			(else (ordered-tree-lokup givenK (left-branch tree)))
	)
)


(define (make-table-tree)
	(let ((local-table '()))
		(define stub -1)
		(define (create-branch! top key value)
			(set-car! top (make-tree (cons key value) stub stub))
			)
		(define (insert-inner! table key value)
			(let ((right (right-branch table))
				 (right-ref (cddr table))
				 (left (left-branch table))
				 (left-ref (cdr table))
				 (entry (entry table))
				 (entryK (entry-key(entry table)))
				 (entryV (entry-value (entry table))))
					(cond ((eq? entryK key) (set-cdr! entry value))
						  ((< entryK key) (if (eq? left stub)
						  		(create-branch! left-ref key value)
						  		(insert-inner! left key value)
						  	))
						  (else (if (eq? right stub)
						  		(create-branch! right-ref key value)
						  		(insert-inner! right key value)
						  	))
					)
			)
		)

		(define (insert! key value) 
			(if (eq? local-table '())
				(set! local-table (make-tree (cons key value) stub stub))
				(insert-inner! local-table key value)
				)
			)

		(define (ordered-tree-lokup givenK tree)
			(cond ((OR (null? tree) (eq? tree stub)) false )
		   		((= givenK (entry-key (car tree))) (entry-value (car tree)))
		   		((< givenK (entry-key (car tree))) (ordered-tree-lokup givenK (right-branch tree)))
				(else (ordered-tree-lokup givenK (left-branch tree)))
			)
		)

		(define (lookup key)
			(ordered-tree-lokup key local-table)
			)

		(define (dispatch m)
			(cond   ((eq? m 'lookup-proc) lookup)
					((eq? m 'insert-proc!) insert!)
					(else (error "Unknown operation: TABLE" m))))
	dispatch)
)

(define tree-table (make-table-tree))

((tree-table 'insert-proc!) 5 10)

((tree-table 'insert-proc!) 3 111)
((tree-table 'insert-proc!) 50 13)

((tree-table 'lookup-proc) 50)

((tree-table 'lookup-proc) 2)

((tree-table 'lookup-proc) 5)




