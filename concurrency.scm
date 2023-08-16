
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
			nil
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
				((eq? m 'front) (front-queue))
				((eq? m 'print) (print-queue))
				(else (error "Unknown error!!"))
		)
	)
	dispatch)
	)

#| concurrency |#

(define (test-and-set! cell)
	(if (car cell) true (begin (set-car! cell true) false)))

(define (make-mutex)
	(let ((cell (list false)))
		(define (the-mutex m)
			(cond ((eq? m 'acquire)
						(if (test-and-set! cell) (the-mutex 'acquire)))
; retry
				  ((eq? m 'release) (clear! cell))
			)
		)
		the-mutex)
)
(define (clear! cell) (set-car! cell false))

(define (make-serializer)
	(let ((mutex (make-mutex)))
		(lambda (p)
			(define (serialized-p . args)
				(mutex 'acquire)
				(let ((val (apply p args)))
					(mutex 'release)
					val)
				)
			serialized-p)
		)
	)


(define (make-semaphore n)
	(define (get-mutex queue)
		(if (null? (queue 'front))
			(get-mutex queue) 
			(queue 'pop)
			)
	)
	(define (queue-of-mutexes queue n)
		(if (NOT (= n 0)) 
			(begin! (queue 'insert (make-mutex)) (queue-of-mutexes queue (- n 1)))
		)
	)
	(let ((mutexes (queue-of-mutexes n)))
		(define (semaphore f)
			(lambda (. args)
				(let ((mutex (get-mutex mutexes)))
					(mutex 'acquire)
					(let ((val (apply f args)))
						(mutex 'release)
						(queue 'insert mutex)
						val)
				)
					
			)
		)
		semaphore
	)
)

(define (make-semaphore-atomic n)
	(define (list-cells n)
		(if (= n 0)
			'() 
			(cons false (list-cells (- n 1)))
		)
	)
	(define (get-cell cells n)
		(if (null? cells)
			nil
			(if (= n 0) cells (get-cell  cells (- n 1)))
		)
	)

	(define (test-and-set! cell)
		(if (car cell) true (begin (set-car! cell true) false)))

	(define (block-cell cell)
		(if (test-and-set! cell) (block-cell cell))
		)
	
	(let ((cells (list-cells n))
		(pointer 0))
		(define (semaphore f)
			(lambda (. args)
				(let ((cell (get-cell pointer)))
					(block-cell cell)
					(set! pointer (+ pointer 1))
					(let ((val (apply f args)))
						(set-car! cell false)
						(set! pointer (- pointer 1))
						val)
				)
					
			)
		)
		semaphore
	)
)

(define acc-id ((lambda ()
	(let ((id 0))
	 		(define (inc) (begin (set! id (+ id 1)) id))
	 		inc
	 	)
	))
)

(define (make-account-and-serializer balance)
	(define (withdraw amount)
		(if (>= balance amount)
			(begin (set! balance (- balance amount)) balance)
			"Insufficient funds"))

(define (deposit amount)
	(set! balance (+ balance amount)) balance)

(let ((balance-serializer (make-serializer))(id (acc-id)))
	(define (dispatch m)
		(cond   ((eq? m 'withdraw) withdraw)
				((eq? m 'deposit) deposit)
				((eq? m 'id) id)
				((eq? m 'balance) balance)
				((eq? m 'serializer) balance-serializer)
				(else (error "Unknown request: MAKE-ACCOUNT" m))))
	dispatch)
)



(define (serialized-exchange account1 account2)
	(let ((serializer1 
		(if (< (account1 'id) (account2 'id)) 
			(account1 'serializer)
			(account2 'serializer)
		))
		 (serializer2 
		 		(if (> (account1 'id) (account2 'id)) 
			(account1 'serializer)
			(account2 'serializer)
		)))
	((serializer1 
		(serializer2 exchange))
				account1
				account2)))


