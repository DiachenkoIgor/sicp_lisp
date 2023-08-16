
(define (make-queue)
	(let ((front-ptr (cons '() '()))
		  (rear-ptr (cons '() '())))
	
	(define (set-front-ptr! item)
			(set-car! front-ptr item))
	(define (set-rear-ptr! item)
			(set-car! rear-ptr item))

	(define (empty-queue?)
			(null? (car front-ptr)))

	(define (front-queue)
		(if (empty-queue?)
			(error "FRONT called with an empty queue" queue)
			(car front-ptr)))

	(define (pop-queue!)
		(let ((result nil))
			(if (empty-queue?)
				result
				(begin! 
					(set! result (car front-ptr))
					(delete-queue!)
					(result)
					)
			)
		)
	)



	(define (insert-queue! item)
		(let ((new-pair (cons item '())))
			(cond ((empty-queue?)
				(set-front-ptr! new-pair)
				(set-rear-ptr! new-pair))
				(else 
					(set-cdr! (car rear-ptr) new-pair)
					(set-rear-ptr! new-pair)))))

	(define (delete-queue!)
		(cond ((empty-queue?)
					(error "DELETE! called with an empty queue"))
					(else (set-front-ptr! (cdr (car front-ptr)))
			)
		)
	)

	(define (print-queue)
		(define (inner q)
			(cond ((null? q) (display ")") #t)
				(else (begin (display (car q))(display " ") (inner (cdr q))))
			)
		)
	(newline)
	(display "( ")
	(inner (car front-ptr))
)

	(define (dispatch m item)
		(cond   ((eq? m 'empty) (empty-queue?))
				((eq? m 'insert) (insert-queue! item))
				((eq? m 'delete) (delete-queue!))
				((eq? m 'pop) (pop-queue!))
				((eq? m 'print) (print-queue))
				(else (error "Unknown error!!"))
		)
	)
	dispatch)
	)

(define (make-deque) (cons '() '()))

(define (empty-deque? deque)
	(AND (null? (car deque)) (null? (cdr deque)))
)

(define (front-deque deque)
	(if (null? (car deque))
		(car deque)
		(caar deque)
	)
)

(define (rear-deque deque)
	(if (null? (cdr deque))
		(cdr deque)
		(cadr deque)
	)
)

(define (front-insert-deque! deque item)
	(let ((new-pair (cons item '()))
		  (front (car deque))
		  (rear (cdr deque))
		 )
		(cond ((empty-deque? deque)(begin (set-car! deque new-pair)
									 (set-cdr! deque new-pair) deque))
				((eq? front rear)
					(begin (set-cdr! new-pair rear) (set-car! deque new-pair) (set-cdr! rear new-pair) deque))
				(else (begin (set-cdr! new-pair front) (set-car! deque new-pair) deque))

		)
	)
)

(define (rear-insert-deque! deque item)
	(let ((new-pair (cons item '()))
		  (front (car deque))
		  (rear (cdr deque))
		 )
		(cond ((empty-deque? deque)(begin (set-car! deque new-pair)
									 (set-cdr! deque new-pair) deque))
				(else (begin (set-cdr! new-pair rear) (set-cdr! deque new-pair) deque))

		)
	)
)

(define (front-delete-deque! deque)
	(cond ((null? (car deque)) (car deque))
		  (else (begin (set-cdr! (cdar deque) '())
		  				(set-car! deque (cdar deque))))
	)
)

(define (rear-delete-deque! deque)
	(let ((pair (cdr deque))
				  (front (car deque))
		  (rear (cdr deque))
		) 
		(cond ((null? (cdr deque)) (cdr deque))
			((null? (cdr deque)))
			  ((eq? front rear)
				(begin (set-cdr! front '()) (set-cdr! deque '()) (car pair)))
		  	(else (begin (set-cdr! deque (cdr pair))
		  				(set-cdr! pair '()) (car pair)))
		)
	)
)

(define dq (make-deque))

(front-insert-deque! dq 'a)
(front-insert-deque! dq 'b)
(rear-insert-deque! dq 'c)
(rear-insert-deque! dq 'd)

(rear-delete-deque! dq)
(rear-delete-deque! dq)
(rear-delete-deque! dq)
(rear-delete-deque! dq)


(rear-delete-deque! dq)
(rear-delete-deque! dq)









