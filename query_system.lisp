(define (stream-for-each proc s)
(if (stream-null? s)
'done
(begin (proc (stream-car s))
(stream-for-each proc (stream-cdr s)))))

(define (display-stream stream)
	(cond ((null? stream)(newline))
			(else (begin (display (stream-car stream)) (newline) (display-stream (stream-cdr stream))))

		)
	)

(define (stream-filter pred stream)
(cond ((stream-null? stream) the-empty-stream)
((pred (stream-car stream))
(cons-stream (stream-car stream)
(stream-filter
pred
(stream-cdr stream))))
(else (stream-filter pred (stream-cdr stream)))))

(define (display-stream s)
(stream-for-each display-line s))
(define (display-line x) (newline) (display x))

(define THE-ASSERTIONS the-empty-stream)

(define *op-table* (make-hash-table))

(define (put op type proc)
  (hash-table/put! *op-table* (list op type) proc))

(define (get op type)
  (hash-table/get *op-table* (list op type) #f))

(define THE-ASSERTIONS the-empty-stream)
(define (fetch-assertions pattern frame)
(if (use-index? pattern)
(get-indexed-assertions pattern)
(get-all-assertions)))
(define (get-all-assertions) THE-ASSERTIONS)
(define (get-indexed-assertions pattern)
(get-stream (index-key-of pattern) 'assertion-stream))

(define THE-RULES the-empty-stream)

(define (fetch-rules pattern frame)
(if (use-index? pattern)
(get-indexed-rules pattern)
(get-all-rules)))

(define (get-all-rules) THE-RULES)
(define (get-indexed-rules pattern)
(stream-append
(get-stream (index-key-of pattern) 'rule-stream)
(get-stream '? 'rule-stream)))

(define (get-stream key1 key2)
(let ((s (get key1 key2)))
(if s s the-empty-stream)))

(define (add-rule-or-assertion! assertion)
(if (rule? assertion)
(add-rule! assertion)
(add-assertion! assertion)))
(define (add-assertion! assertion)
(store-assertion-in-index assertion)
(let ((old-assertions THE-ASSERTIONS))
(set! THE-ASSERTIONS
(cons-stream assertion old-assertions))
'ok))
(define (add-rule! rule)
(store-rule-in-index rule)
(let ((old-rules THE-RULES))
(set! THE-RULES (cons-stream rule old-rules))
'ok))

(define (store-assertion-in-index assertion)
(if (indexable? assertion)
(let ((key (index-key-of assertion)))
(let ((current-assertion-stream
(get-stream key 'assertion-stream)))
(put key
'assertion-stream
(cons-stream
assertion
current-assertion-stream))))))
(define (store-rule-in-index rule)
(let ((pattern (conclusion rule)))
(if (indexable? pattern)
(let ((key (index-key-of pattern)))
(let ((current-rule-stream
(get-stream key 'rule-stream)))
(put key
'rule-stream
(cons-stream rule
current-rule-stream)))))))

(define (indexable? pat)
(or (constant-symbol? (car pat))
(var? (car pat))))

(define (index-key-of pat)
(let ((key (car pat)))
(if (var? key) '? key)))

(define (singleton-stream x)
(cons-stream x the-empty-stream))

(define (type exp)
(if (pair? exp)
(car exp)
(error "Unknown expression TYPE" exp)))
(define (contents exp)
(if (pair? exp)
(cdr exp)
(error "Unknown expression CONTENTS" exp)))

(define (query-syntax-process exp)
(map-over-symbols expand-question-mark exp))
(define (map-over-symbols proc exp)
(cond ((pair? exp)
(cons (map-over-symbols proc (car exp))
(map-over-symbols proc (cdr exp))))
((symbol? exp) (proc exp))
(else exp)))
(define (expand-question-mark symbol)
(let ((chars (symbol->string symbol)))
(if (string=? (substring chars 0 1) "?")
(list '?
(string->symbol
(substring chars 1 (string-length chars))))
symbol)))

(define rule-counter 0)

(define (tagged-list? exp tag)
(if (pair? exp)
(eq? (car exp) tag)
false))

(define (make-binding variable value)
(cons variable value))
(define (binding-variable binding) (car binding))
(define (binding-value binding) (cdr binding))
(define (binding-in-frame variable frame)
(assoc variable frame))
(define (extend variable value frame)
(cons (make-binding variable value) frame))

(define (extend-frames variable value frame frame2)
	(cons 
		(cons (make-binding variable value) frame)
		(cons (make-binding variable value) frame2)
		)
)

(define (contract-question-mark variable)
(string->symbol
(string-append "?"
(if (number? (cadr variable))
(string-append (symbol->string (caddr variable))
"-"
(number->string (cadr variable)))
(symbol->string (cadr variable))))))

(define (new-rule-application-id)
(set! rule-counter (+ 1 rule-counter))
rule-counter)
(define (make-new-variable var rule-application-id)
(cons '? (cons rule-application-id (cdr var))))

(define (var? exp) (tagged-list? exp '?))
(define (constant-symbol? exp) (symbol? exp))

(define (rule? statement)
(tagged-list? statement 'rule))
(define (conclusion rule) (cadr rule))
(define (rule-body rule)
(if (null? (cddr rule)) '(always-true) (caddr rule)))

(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))
(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))
(define (negated-query exps) (car exps))
(define (unique-query exps) (car exps))
(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

(define (assertion-to-be-added? exp)
(eq? (type exp) 'assert!))
(define (add-assertion-body exp) (car (contents exp)))

(define (stream-flatmap proc s)
(flatten-stream (stream-map proc s)))
(define (flatten-stream stream)
(if (stream-null? stream)
	the-empty-stream
(interleave-delayed
(stream-car stream)
(delay (flatten-stream (stream-cdr stream))))))

(define (stream-append-delayed s1 delayed-s2)
(if (stream-null? s1)
(force delayed-s2)
(cons-stream
(stream-car s1)
(stream-append-delayed
(stream-cdr s1)
delayed-s2))))
(define (interleave-delayed s1 delayed-s2)
(if (stream-null? s1)
(force delayed-s2)
(cons-stream
(stream-car s1)
(interleave-delayed
(force delayed-s2)
(delay (stream-cdr s1))))))

(define (use-index? pat) (constant-symbol? (car pat)))

(define (depends-on? exp var frame)
	(define (tree-walk e)
				(cond ((var? e) (if (equal? var e)
									true
									(let ((b (binding-in-frame e frame)))
										(if b
											(tree-walk (binding-value b))
											false))))
				((pair? e) (or (tree-walk (car e)) (tree-walk (cdr e))))
				(else false)))
	(tree-walk exp))

(define (extend-if-possible var val frame)
(let ((binding (binding-in-frame var frame)))
(cond (binding
(unify-match (binding-value binding) val frame))
; ***
((var? val)
(let ((binding (binding-in-frame val frame)))
(if binding
(unify-match
var (binding-value binding) frame)
(extend var val frame))))
((depends-on? val var frame)
; ***
'failed)
(else (extend var val frame)))))


(define (unify-match p1 p2 frame)
	(cond   ((eq? frame 'failed) 'failed)
			((equal? p1 p2) frame)
			((var? p1) (extend-if-possible p1 p2 frame))
			((var? p2) (extend-if-possible p2 p1 frame))
			((and (pair? p1) (pair? p2))
				(unify-match (cdr p1)
							(cdr p2)
							(unify-match (car p1) (car p2) frame)))
			(else 'failed)))

(define (extend-if-possible-frames var val frame frame2)
	(let ((binding (binding-in-frame var frame)))
		(cond (binding (unify-match-frames (binding-value binding) val frame frame2))
		((var? val) (let ((binding (binding-in-frame val frame2)))
						(if binding
							(unify-match-frames var (binding-value binding) frame2 frame)
							(extend-frames var val frame))))
		((depends-on? val var frame2) 'failed)
		(else (extend-frames var val frame)))))

(define (unify-match-frames p1 p2 frame1 frame2)
	(cond   ((or (eq? frame1 'failed)(eq? frame2 'failed)) (cons 'failed 'failed))
			((equal? p1 p2) (cons frame1 frame2))
			((var? p1) (extend-if-possible-frames p1 p2 frame1 frame2))
			((var? p2) (extend-if-possible-frames p2 p1 frame2 frame1))
			((and (pair? p1) (pair? p2))
				(let ((res (unify-match-frames (car p1) (car p2) frame1 frame2)))
					(unify-match-frames (cdr p1)
							(cdr p2)
							(car res)(cdr res)))
					)
			(else (cons 'failed 'failed))))

(define (append-frames frame1 frame2)
	(define (inner-iter frame)
		(cond ((null? frame) frame2)
				((binding-in-frame (caar frame) frame2) (inner-iter (cdr frame)))
				(else (cons (car frame) (inner-iter (cdr frame))))
			)
		)
	(inner-iter frame1)
	)

(define (merge-frames-if-possible frame1 frame2)
	(let ((frames (cons frame1 frame2)))
		(define (iter data)
			(cond 	((eq? (car frames) 'failed) 'failed)
					((null? data) (append-frames (car frames)(cdr frames)))
					((binding-in-frame (caar data) frame2)
						(let ((binding (binding-in-frame (caar data) frame2))
								(result-frame (unify-match-frames (cdar data) (cdr (binding-in-frame (caar data) frame2)) (car frames)(cdr frames)))
							)
							(set! frames result-frame)
							(iter (cdr data))
							))
					(else (iter (cdr data)))
		)
	)
	(iter frame1)
)
	)

(define (rename-variables-in rule)
(let ((rule-application-id (new-rule-application-id)))
(define (tree-walk exp)
(cond ((var? exp)
(make-new-variable exp rule-application-id))
((pair? exp)
(cons (tree-walk (car exp))
(tree-walk (cdr exp))))
(else exp)))
(tree-walk rule)))

(define (apply-a-rule rule query-pattern query-frame)
	(let ((clean-rule (rename-variables-in rule)))
		(let ((unify-result (unify-match query-pattern (conclusion clean-rule) query-frame)))
			(if (eq? unify-result 'failed)
				the-empty-stream
				(qeval (rule-body clean-rule)
				(singleton-stream unify-result))))))

(define (apply-rules pattern frame)
(stream-flatmap (lambda (rule)
(apply-a-rule rule pattern frame))
(fetch-rules pattern frame)))

(define (extend-if-consistent var dat frame)
(let ((binding (binding-in-frame var frame)))
(if binding
(pattern-match (binding-value binding) dat frame)
(extend var dat frame))))

(define (pattern-match pat dat frame)
	(cond ((eq? frame 'failed) 'failed)
			((equal? pat dat) frame)
			((var? pat) (extend-if-consistent pat dat frame))
			((and (pair? pat) (pair? dat))
				(pattern-match
					(cdr pat)
					(cdr dat)
					(pattern-match (car pat) (car dat) frame)))
			(else 'failed)))

(define (check-an-assertion assertion query-pat query-frame)
(let ((match-result
(pattern-match query-pat assertion query-frame)))
(if (eq? match-result 'failed)
the-empty-stream
(singleton-stream match-result))))

(define (find-assertions pattern frame)
(stream-flatmap
(lambda (datum) (check-an-assertion datum pattern frame))
(fetch-assertions pattern frame)))

(define (always-true ignore frame-stream) frame-stream)
(put 'always-true 'qeval always-true)

(define (execute exp)
(apply (eval (predicate exp) user-initial-environment)
(args exp)))

(define (lisp-value call frame-stream)
	(stream-flatmap
		(lambda (frame)
			(if (execute
					(instantiate
					call
					frame
					(lambda (v f)
						(error "Unknown pat var: LISP-VALUE" v))))
				(singleton-stream frame)
				the-empty-stream))
		frame-stream))

(define (check-negate-query-execution exp frame)
	(define (tree-walk e)
				(cond ((var? e) 
								(let ((b (binding-in-frame e frame)))
									(if b
										true
										false)))
				((pair? e) (AND (tree-walk (car e)) (tree-walk (cdr e))))
				(else true)))
	(tree-walk exp)
)

(define (create-promise operands)
		(lambda (frame)
			(if (check-negate-query-execution (negated-query operands) frame)
					(if (stream-null? (qeval (negated-query operands) (singleton-stream frame)))
						(singleton-stream frame)
						the-empty-stream)
				(cons-stream false (singleton-stream frame))
				)
			)
	)

(define (execute-promise frame)
	(define (remove-first-promise frame-data)
		(cond 
			((null? frame-data) the-empty-stream)
			((eq? 'promise (caar frame-data)) (cdr frame-data))
			(else (cons (car frame-data) (remove-first-promise (cdr frame-data))))
			)
		)

	(define (execute-promise-inside promise)
		(let ((result (promise frame)))
			(cond ((eq? result the-empty-stream) result)
					((eq? (stream-car result) false) result)
					(else (singleton-stream (remove-first-promise (stream-car result))))
				)
			)
		)
	(define (inner-iter data)
		(cond
			((null? data) (singleton-stream frame))
			((eq? data the-empty-stream) (singleton-stream frame))
			((eq? (car (car data)) 'promise)
					(let ((result (execute-promise-inside (cdr (car data)))))
						(cond
							((eq? result the-empty-stream) the-empty-stream)
							((eq? (stream-car result) false) (singleton-stream frame))
							(else (begin (inner-iter (stream-car result))))
							)
						)
				)
			
			(else (inner-iter (cdr data)))
		)
	)
	(inner-iter frame)

)

(define (check-promises-frames frames)
	(stream-flatmap
		(lambda (frame)
			(execute-promise frame)
			)
	frames)
)

(define (extend-with-predicate promise frame)
	(cons (cons 'promise promise) frame))

(define (negate operands frame-stream)
(stream-flatmap
		(lambda (frame)
			(if (check-negate-query-execution (negated-query operands) frame)
				(if (stream-null? (qeval (negated-query operands) (singleton-stream frame)))
					(singleton-stream frame)
					the-empty-stream)
				(singleton-stream (extend-with-predicate (create-promise operands) frame))
			)
		)
		frame-stream)
)


#| (define (negate operands frame-stream)
	(stream-flatmap
		(lambda (frame)
			(if (stream-null? (qeval (negated-query operands) (singleton-stream frame)))
				(singleton-stream frame)
				the-empty-stream))
		frame-stream)
) |#

(define (check-size stream-data limit)
	(define (inner-stream-data data position)
		(cond ((stream-null? data) position)
				((= position limit) position)
				(else (inner-stream-data (stream-cdr data) (+ position 1)))
			)
		)
	(inner-stream-data stream-data 0)
	)


(define (uniquely-asserted operands frame-stream)
	(stream-flatmap
		(lambda (frame)
				(let ((result-stream (qeval (unique-query operands) (singleton-stream frame))))
					(if (= 1 (check-size result-stream 2))
						result-stream
						the-empty-stream)
					)
			)
		frame-stream)
	)

(define (merge-frame-streams stream1 stream2)

	(cond ((eq? stream1 the-empty-stream) stream2)
		((eq? stream2 the-empty-stream) stream1)
		(else 	(stream-filter (lambda (val) (NOT (eq? val the-empty-stream)))
					(stream-flatmap
							(lambda (frame)
								(stream-flatmap
									(lambda (frame2)
										(let ((res (merge-frames-if-possible frame frame2)))
											(if (eq? res 'failed)
												the-empty-stream
												(singleton-stream res))
										)
									)
								stream2)
							)
					stream1)
				)	
		)
	)
)


(define (conjoin conjuncts frame-stream)
	(define (inner-conjoin conjuncts result-stream)
		(if (empty-conjunction? conjuncts)
			result-stream
			(inner-conjoin (rest-conjuncts conjuncts)
				(merge-frame-streams (qeval (first-conjunct conjuncts) frame-stream) result-stream)))
		)
	(inner-conjoin conjuncts the-empty-stream)
)

(define (disjoin disjuncts frame-stream)
(if (empty-disjunction? disjuncts)
	the-empty-stream
	(interleave-delayed
			(qeval (first-disjunct disjuncts) frame-stream)
			(delay (disjoin (rest-disjuncts disjuncts) frame-stream)))))


(define (simple-query query-pattern frame-stream)
	(stream-flatmap (lambda (frame)
						(stream-append-delayed (find-assertions query-pattern frame)
												(delay (apply-rules query-pattern frame))))
					frame-stream)
)

(define (qeval query frame-stream)
	(let ((qproc (get (type query) 'qeval)))
		#| (check-promises-frames frame-stream) |#
		(if qproc
			(check-promises-frames (qproc (contents query) frame-stream))
			(simple-query query frame-stream))))

(define (instantiate exp frame unbound-var-handler)
	(define (copy exp)
		(cond   ((var? exp)
					(let ((binding (binding-in-frame exp frame)))
						(if binding
							(copy (binding-value binding))
							(unbound-var-handler exp frame))
					))
				((pair? exp) (cons (copy (car exp)) (copy (cdr exp))))
				(else exp)))
	(copy exp)
)

(put 'and 'qeval conjoin)
(put 'or 'qeval disjoin)
(put 'not 'qeval negate)
(put 'lisp-value 'qeval lisp-value)
(put 'unique 'qeval uniquely-asserted)

(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")

(define (prompt-for-input string)
(newline) (newline) (display string) (newline))

(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (job (Bitdiddle Ben) (computer wizard)))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (salary (Bitdiddle Ben) 60000))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (job (Hacker Alyssa P) (computer programmer)))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (salary (Hacker Alyssa P) 40000))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (address (Fect Cy D) (Cambridge (Ames Street) 3)))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (job (Fect Cy D) (computer programmer)))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (salary (Fect Cy D) 35000))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (supervisor (Fect Cy D) (Bitdiddle Ben)))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (address (Tweakit Lem E) (Boston (Bay State Road) 22)))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (job (Tweakit Lem E) (computer technician)))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (salary (Tweakit Lem E) 25000))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (supervisor (Tweakit Lem E) (Bitdiddle Ben)))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80)))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (job (Reasoner Louis) (computer programmer trainee)))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (salary (Reasoner Louis) 30000))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (supervisor (Reasoner Louis) (Hacker Alyssa P)))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (supervisor (Bitdiddle Ben) (Warbucks Oliver)))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (address (Warbucks Oliver) (Swellesley (Top Heap Road))))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (job (Warbucks Oliver) (administration big wheel)))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (salary (Warbucks Oliver) 150000))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (address (Scrooge Eben) (Weston (Shady Lane) 10)))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (job (Scrooge Eben) (accounting chief accountant)))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (salary (Scrooge Eben) 75000))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (supervisor (Scrooge Eben) (Warbucks Oliver)))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (address (Cratchet Robert) (Allston (N Harvard Street) 16)))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (job (Cratchet Robert) (accounting scrivener)))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (salary (Cratchet Robert) 18000))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (supervisor (Cratchet Robert) (Scrooge Eben)))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (address (Aull DeWitt) (Slumerville (Onion Square) 5)))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (job (Aull DeWitt) (administration secretary)))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (salary (Aull DeWitt) 25000))")))))
(add-rule-or-assertion! (add-assertion-body (query-syntax-process (read (string->input-port "(assert! (supervisor (Aull DeWitt) (Warbucks Oliver)))")))))


(define (query-driver-loop)
	(prompt-for-input input-prompt)
	(let ((q (query-syntax-process (read))))
		(cond ((assertion-to-be-added? q)
			(add-rule-or-assertion! (add-assertion-body q))
			(newline)
			(display "Assertion added to data base.")
			(query-driver-loop))
			(else
				(newline)
				(display output-prompt)
				(display-stream	
					(stream-map
						(lambda (frame) (instantiate
										 q
										frame
										(lambda (v f)
											(contract-question-mark v))))
						(qeval q (singleton-stream '()))))
(query-driver-loop)))))

(query-driver-loop)
