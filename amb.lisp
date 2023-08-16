(define (amb-between data)  
    (require (NOT (null? data)))  
    (amb (car data) (amb-between (cdr data)))
(define (an-interger-between low high)  
    (require (<= low high))  
    (amb low (an-interger-between (+ low 1) high)))

(define (append list1 list2)
(if (null? list1)
list2
(cons (car list1) (append (cdr list1) list2))))

(define (element-of-set? x set)
(cond ((null? set) false)
((eq? x (car set)) true)
(else (element-of-set? x (cdr set)))))

(define (an-interger-between-excepts low high except)  
    (require (<= low high))
    (if (element-of-set? low except)
    	(an-interger-between (+ low 1) high)
    	(amb low (an-interger-between (+ low 1) high))
    	)
    )

(define (an-integer-starting-from x)
	(amb x (an-integer-starting-from ( + x 1)))
)

(define (a-pythagorean-triple-infinite low)
	(let ((k (an-integer-starting-from low)))
		(let ((j (an-integer-between low k)))
			(let ((i (an-integer-between low j)))
				(require (= (+ (* i i) (* j j)) (* k k)))
				(list i j k)
			)
		)
	)
)

(define (distinct? items)
(cond ((null? items) true)
((null? (cdr items)) true)
((member (car items) (cdr items)) false)
(else (distinct? (cdr items)))))

(define (multiple-dwelling)
	(let ((baker (amb 1 2 3 4)) (cooper (amb 1 2 3 4 5))
		 (fletcher (amb 1 2 3 4 5)) (miller (amb 1 2 3 4 5))(smith (amb 1 2 3 4 5)))
			(require (distinct? (list baker cooper fletcher miller smith)))
			(require (not (= baker 5)))
			(require (not (= cooper 1)))
			(require (not (= fletcher 5)))
			(require (not (= fletcher 1)))
			(require (> miller cooper))
			(require (not (= (abs (- smith fletcher)) 1)))
			(require (not (= (abs (- fletcher cooper)) 1)))
			(list (list 'baker baker)
					(list 'cooper cooper)
					(list 'fletcher fletcher)
					(list 'miller miller)
					(list 'smith smith))))

(define (multiple-dwelling-optimized)
	(let ((fletcher (amb 2 3 4)))
		(let ((cooper (an-interger-between-excepts 2 5 (list (- fletcher 1) fletcher (+ fletcher 1))))
				(smith (an-interger-between-excepts 1 5 (list (- fletcher 1) fletcher (+ fletcher 1))))
				(miller (an-interger-between (+ cooper 1) 5))
				(baker (amb 1 2 3 4)))
				(require (distinct? (list baker cooper fletcher miller smith)))
				(list (list 'baker baker)
					(list 'cooper cooper)
					(list 'fletcher fletcher)
					(list 'miller miller)
					(list 'smith smith))
			)))

(define (multiple-dwelling)
	(let ((Moore (list 'Lorna (amb 'Mary)))
		(Barnacle (list 'Gabrielle (amb 'Melissa))) 
		 (Hall (list 'Rosalind (amb 'Gabrielle 'Lorna 'Rosalind)))
		 (Downing (list 'Melissa (amb 'Gabrielle 'Lorna 'Rosalind)))
		 (Parker (list 'Mary (amb 'Lorna 'Rosalind))))
			(require (distinct? (map cadr (list Barnacle Moore Hall Downing Parker))))
			(require (if (eq? (cadr Hall) 'Gabrielle)
							(eq? (cadr Parker) 'Rosalind)
							(eq? (cadr Parker) 'Melissa)
						))
			(list (list 'baker baker)
					(list 'cooper cooper)
					(list 'fletcher fletcher)
					(list 'miller miller)
					(list 'smith smith))))

(define (queens board-size)
	(define (queen-cols k)
		(if (= k 0)
			(list empty-board)
			(filter
				(lambda (positions) (safe? k positions))
				(flatmap
						(lambda (rest-of-queens)
							((lambda (new-row)(adjoin-position new-row k rest-of-queens))
									(an-interger-between 1 board-size))
						(queen-cols (- k 1))
				)
			)
		)
	)
(queen-cols board-size))

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))
(define conjuctions '(prep and but or yet))

(define (parse-word word-list)
	(require (not (null? *unparsed*)))
	(require (memq (car *unparsed*) (cdr word-list)))
	(let ((found-word (car *unparsed*)))
		(set! *unparsed* (cdr *unparsed*))
		(list (car word-list) found-word)))

(define *unparsed* '())
(define (parse input)
(set! *unparsed* input)
(let ((sent (parse-sentence)))
(require (null? *unparsed*)) sent))

(define (parse-prepositional-phrase)
(list 'prep-phrase
(parse-word prepositions)
(parse-noun-phrase)))

(define (parse-sentence)
(list 'sentence (parse-noun-phrase) (parse-verb-phrase)))

(define (parse-verb-phrase)
(define (maybe-extend verb-phrase)
(amb verb-phrase (maybe-extend
(list 'verb-phrase
verb-phrase
(parse-prepositional-phrase)))))
(maybe-extend (parse-word verbs)))

(define (parse-simple-noun-phrase)
(list 'simple-noun-phrase
(parse-word articles)
(parse-word nouns)))
(define (parse-noun-phrase)
(define (maybe-extend noun-phrase)
(amb noun-phrase
(maybe-extend
(list 'noun-phrase
noun-phrase
(parse-prepositional-phrase)))))
(maybe-extend (parse-simple-noun-phrase)))

(define (parse-conjuction-phrase)
(list
(parse-word conjuctions)
(complex-sentence)))

(define (complex-sentence)
	(define (maybe-extend sentence)
		(amb sentence
				(maybe-extend (append 
								(list sentence)
								(parse-conjuction-phrase)))
		)
	)
	(maybe-extend (parse-sentence))
)

(define (analyze-sentence)
	(let ((result (complex-sentence)))
		(if (eq? (car result) 'sentence)
			result
			(cons 'complex-sentence result)
			)
		)
)

(define (parse-word word-list)
	(require (not (null? *unparsed*)))
	(let ((found-word (car *unparsed*)))
		(set! *unparsed* (cdr *unparsed*))
		(list (car word-list) (amb-between word-list))))