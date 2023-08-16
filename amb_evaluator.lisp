(define (true? x)(not (eq? x false)))
(define (false? x) (eq? x false))

(define unassigned "*unassigned*")

(define (make-procedure parameters body env)

(list 'procedure parameters body env))
(define (compound-procedure? p)
(tagged-list? p 'procedure))
(define (procedure-parameters p)
	(cadr p)
	)
(define (procedure-parameters-clean p)
	(map (lambda (x) (if (symbol? x) x (car x))) (cadr p))
	)
(define (procedure-body p)
 (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
 (list (map cons variables values))
)
(define (frame-bindings frame) (car frame))
(define (add-binding-to-frame! var val frame)
(set-car! frame (cons (cons var val) (car frame))))

(define (extend-environment vars vals base-env)
(if (= (length vars) (length vals))
(cons (make-frame vars vals) base-env)
(if (< (length vars) (length vals))
(error "Too many arguments supplied" vars vals)
(error "Too few arguments supplied" vars vals))))

(define (find-in-frame var frame)
		(define (scan bindings)
				(cond ((null? bindings) '())
					((eq? var (caar bindings)) (car bindings))
					(else (scan (cdr bindings)))))
		(scan (frame-bindings frame))
)

(define (find-in-all-env var env)
	(define (env-loop env)
		(if (eq? env the-empty-environment)
			(error "Unbound variable" var)
			(let ((frame (first-frame env)))
				(let ((value (find-in-frame var frame)))
					(if (null? value)
						(env-loop (enclosing-environment env))
						value
						)
					)
				
			)
		)
	)
	(env-loop env)
)

(define (lookup-variable-value var env)
	(let ((binding (find-in-all-env var env)))
		(cond ((null? binding) (error "Unbound variable" var))
				((eq? unassigned (cdr binding)) (error "unassigned var" var))
				(else (cdr binding))
			)
		)
)

(define (set-variable-value! var val env)
	(let ((binding (find-in-all-env var env)))
		(if (null? binding)
			(error "Unbound variable" var)
			(set-cdr! binding val)
			)
		)
)

(define (define-variable! var val env)
	(let ((binding (find-in-frame var (first-frame env))))
		(if (null? binding)
			(add-binding-to-frame! var val (first-frame env))
			(set-car! (cdr binding) val)
			)
		)
)

(define (unbound-variable! var env)
	(define (rebuild-frame vars)
		(cond ((null? vars) '())
			((eq? var (caar vars)) (rebuild-frame (cdr vars)))
			(else (cons (car vars) (rebuild-frame (cdr vars))))
		)
	)
	(let ((frame(first-frame env)))
		(set-car! frame (rebuild-frame (car frame)))
		)
)

(define (self-evaluating? exp)
	(cond ((number? exp) true)
		  ((string? exp) true)
			(else false)))

(define (variable? exp)
 (symbol? exp))

(define (tagged-list? exp tag)
	(if (pair? exp)
		(eq? (car exp) tag)
		false))

(define (tailed-list? exp tag)
	(if (pair? exp)
		(eq? (cadr exp) tag)
		false))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp)
 	(if (pair? (cadr exp))
 		(generate-lazy-list (cadr exp))
 		(cadr exp)
 		)
 )

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))
(define (unbound? exp) (tagged-list? exp 'make-unbound!))
(define (unbound-symbol exp) (cadr exp))

(define (definition-variable exp)
	(if (symbol? (cadr exp))
		(cadr exp)
		(caadr exp)))

(define (definition-value exp)
	(if (symbol? (cadr exp))
		(caddr exp)
		(make-lambda (cdadr exp)
					(cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
	(cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
(if (not (null? (cdddr exp)))
	(cadddr exp)
	'false))

(define (make-if predicate consequent alternative)
	(list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
(cond ((null? seq) seq)
((last-exp? seq) (first-exp seq))
(else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
	(eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp env) (expand-clauses (cond-clauses exp) env))

(define (cond-recipient-clause? clause)
	(eq? (car (cond-actions clause)) '=>))

(define (cond-recipient-func exp)
	(caddr exp))

(define (cond-recipient-predicate exp env)
	(lambda ()
		(if (eq? (eval (car exp) env) 'false)
				false
				true
			)
	)
)

(define (cond-recipient-predicate-true exp env)
	(lambda()
		(apply (cond-recipient-func exp) (eval (car exp) env))
	)
)


(define (expand-clauses clauses env)
	(if (null? clauses)
		'false
		(let ((first (car clauses))
			(rest (cdr clauses)))
			(if (cond-else-clause? first)
				(if (null? rest)
					(sequence->exp (cond-actions first))
					(error "ELSE clause isn't last: COND->IF" clauses))
				(if (cond-recipient-clause? first)
						(make-if  (cond-recipient-predicate first env)
											(cond-recipient-predicate-true first env)
											(expand-clauses rest env))
						(make-if (cond-predicate first)
									(sequence->exp (cond-actions first))
											(expand-clauses rest env))
				)
			)
		)
	)
)

(define (eval-if exp env)
	(if (true? (actual-value (if-predicate exp) env))
			(eval (if-consequent exp) env)
			(eval (if-alternative exp) env)))

(define (left-to-right val1 val2)
	(cons val1 val2)
	)

(define (invert vars)
	(define (invert-inner args val)
		(if(null? args)
			val
			(invert-inner (cdr args) (cons (car args) val))
			)
		)
	(invert-inner vars nil)
	)

(define (list-of-values exps env)
	(if (no-operands? exps)
		'()
		(cons (eval (first-operand exps) env)
			(list-of-values (rest-operands exps) env))))

(define (eval-sequence exps env)
	(cond ((last-exp? exps)
				(eval (first-exp exps) env))
		  (else
				(eval (first-exp exps) env)
					(eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
	(set-variable-value! (assignment-variable exp)
						 (eval (assignment-value exp) env)
						  env)
	'ok)

(define (eval-definition exp env)
	(define-variable! (definition-variable exp)
					  (eval (definition-value exp) env)
					  env)
	'ok)
 (define (and? exp) (tagged-list? exp 'and)) 
 (define (and-predicates exp) (cdr exp)) 
 (define (first-predicate seq) (car seq)) 
 (define (rest-predicates seq) (cdr seq)) 
 (define (no-predicate? seq) (null? seq)) 
 (define (eval-and-predicates exps env) 
     (cond ((no-predicates? exps) true) 
           ((not (true? (eval (first-predicate exps)))) false) 
           (else (eval-and-predicate (rest-predicates exps) env)))) 
  
 (define (or? exp) (tagged-list? exp 'or)) 
 (define (or-predicates exp) (cdr exp)) 
 (define (eval-or-predicates exps env) 
     (cond ((no-predicates? exps) false) 
           ((true? (eval (first-predicate exps))) true) 
           (else (eval-or-predicate (rest-predicates exps) env)))) 
  
 ; derived expressions 
 (define (and->if exp) 
     (expand-and-predicates (and-predicates exp))) 
 (define (expand-and-predicates predicates) 
     (if (no-predicates? predicates) 
         'true 
         (make-if (first-predicate predicates) 
                  (expand-predicates (rest-predicates predicates)) 
                  'false))) 
  
 (define (or->if exp) 
     (expand-or-predicates (or-predicates exp))) 
 (define (expand-or-predicates predicates) 
     (if (no-predicate? predicates) 
         'false 
         (make-if (first-predicate predicates) 
                 'true 
                 (expand-predicates (rest-predicates predicates)))))

(define (let? exp) (tagged-list? exp 'let))

(define (let-vars expr) (map car (cadr expr)))
(define (let-inits expr) (map cdr (cadr expr)))
(define (let-body expr) (cddr expr))


(define (append list1 list2)
(if (null? list1)
list2
(cons (car list1) (append (cdr list1) list2))))

(define (let->combination exp)
	(if (let-named? exp)
		(sequence-exp (let-named-define exp) (let-named-call exp))
		(cons (make-lambda (let-vars exp) (let-body exp)) 
         	(let-inits exp))
		)
)

(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-vars exp) (cdr exp))
(define (let*-body exp) (cddr exp))

(define (let*->nested-lets exp)
	(define (inner-transform vars)
		(if (null? exp)
			(let*-body exp)
			(list 'let (car vars) (inner-transform (cdr vars)))
		)
	)
	(inner-transform (let*-vars exp))
)

(define (letrec? exp) (tagged-list? exp 'letrec))

(define (letrec->nested-let exp)
	(define (get-defines body)
		(if (null? body)
			'()
			(let ((variable (caar body)) (val (cadar body))) 
				 (cons (list 'define variable val) (get-defines (cdr body)))
				)
			)
		)
	(scan-out-defines (append (get-defines (cadr exp)) (cddr exp)))
)

(define (let-named? exp) (AND (tagged-list? exp 'let) (symbol? (cadr exp))))

(define (let-named-var exp)(cadr exp))
(define (let-named-bindings exp)(caddr exp))
(define (let-named-body exp)(cadddr exp))

(define (let-named-params exp)
	(map car (let-named-bindings exp)))

(define (let-named-params-values exp)
	(map cadr (let-named-bindings exp)))

(define (let-named-define exp)
	(list 'define 
				(list (let-named-var exp) (let-named-params exp)) (let-named-body exp))
	)
(define (let-named-call exp)
	(cons (make-lambda (let-named-params exp) (let-named-body exp)) (let-named-params-values exp))
	)

(define (let-name-create var bindings body)
	(list 'let var bindings body)
	)

(define (for? exp)(tagged-list? exp 'for))
(define(for-from exp)(cadr exp))
(define(for-to exp)(caddr exp))
(define(for-body exp)(cadddr exp))

(define (for-function-body body)
	(make-if (list '= (list '+ 1 'current) 'from)
		body
		(make-begin body (list 'for (list '+ 1 'current) 'to body))
	 )
)

(define (for-function-bindings exp)
	(list (cons 'current (for-from exp)) (cons 'to (for-to exp)))
)

(define (eval-for exp)
	(let-name-create 'for (for-function-bindings exp) (for-function-body (for-body exp)))
)

(define (scan-out-defines procedure-body)
	(define (get-defines body)
		(cond ((null? body) '())
			((definition? (car body)) (cons (car body) (get-defines (cdr body))))
			(else (get-defines (cdr body))) 
			)
		)
	(define (without-defines body)
		(cond ((null? body) '())
			((NOT (definition? (car body))) (cons (car body) (without-defines (cdr body))))
			(else (without-defines (cdr body))) 
			)
		)

	(define (get-variable-from-define define)
		(if (pair? (cadr define))
			(caadr define)
			(cadr define)
			)
		)

	(define (create-variables defines)
			(if (null? defines)
				'()
				(cons (cons (get-variable-from-define (car defines)) unassigned) (create-variables (cdr defines)))
				)
	)
	(define (get-set-from-define define)
		(if (pair? (cadr define))
			(caadr define)
			(cadr define)
			)
		)
	(define (create-sets defines)
			(if (null? defines)
				'()
				(cons 
					(list 'set! 
						(get-variable-from-define (car defines))
						 (definition-value (car defines)))
						  (create-sets (cdr defines)))
				)
	)
	(if (null? (get-defines procedure-body))
		procedure-body
		(let ((variables (create-variables (get-defines procedure-body)))
			(sets (create-sets (get-defines procedure-body))))
				(append
					(append
						(list 'let variables) sets) (without-defines procedure-body))
		)
	)
)

(define (actual-value exp env)
(force-it (eval exp env)))

(define (evaluated-thunk? obj)
(tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk)
(cadr evaluated-thunk))

(define (force-it obj)
	(cond 
		((thunk? obj)(actual-value (thunk-exp obj) (thunk-env obj)))
		((thunk-memo? obj) 
			(let ((result (actual-value (thunk-exp obj) (thunk-env obj))))
				(set-car! obj 'evaluated-thunk)
				(set-car! (cdr obj) result)
				(set-cdr! (cdr obj)'())
				result))
		((evaluated-thunk? obj) (thunk-value obj))
		(else obj)))

(define (delay-it exp env)
(list 'thunk exp env))
(define (delay-it-memo exp env)
(list 'thunk-memo exp env))
(define (thunk-memo? obj)
(tagged-list? obj 'thunk-memo))
(define (thunk? obj)
(tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr
thunk))
(define (thunk-env thunk) (caddr thunk))

(define (generate-lazy-list quoted)
	(if (null? quoted)
		'()
		(lambda (m) (m (car quoted) (generate-lazy-list (cdr quoted))))
		)
	)

(define (cons-result? exp)
	(tagged-list? exp 'cons-result))

(define (cons? exp)
	(tagged-list? exp 'cons))
(define (eval-cons exp env)
	(list 'cons-result (lambda (m) (m (eval (cadr exp) env) (eval (caddr exp) env)))))

(define (car? exp)
	(tagged-list? exp 'car))

(define (eval-car exp env)
	(if (cons-result? exp)
			((cadr exp) (lambda (p q) p))
			((cadr(actual-value (cadr exp) env)) (lambda (p q) p))
		)
	)

(define (random-list-shuffle data)
	(define (pick-element items)
		(cond ((null? items) '())
				((null? (cdr items)) (car items))
				((= 0 (remainder (random (length items)) (+ (integer-floor (length items) 3)11))) (car items))
				(else (pick-element (cdr items)))
			)
		)
	(define (remove-element-from-list items elem)
				(cond ((null? items) '())
				((eq? (car items) elem) (remove-element-from-list (cdr items) elem))
				(else (cons (car items) (remove-element-from-list (cdr items) elem)))
			)
		)

	(define (reshuffle items)
		(cond ((null? items) '())
			(else
				(let ((element (pick-element items)))
					(cons element (reshuffle (remove-element-from-list items element)))
					)
				)
		)
	)
	(reshuffle data)
)

(define (analyze-ramb exp)
	(let ((cprocs (random-list-shuffle (map analyze (amb-choices exp)))))
		(lambda (env succeed fail)
			(define (try-next choices)
				(if (null? choices)
					(fail)
					((car choices) env succeed
						(lambda () (try-next (cdr choices))))
				)
			)
			(try-next cprocs)
		)
	)
)

(define (analyze-amb exp)
	(let ((cprocs (map analyze (amb-choices exp))))
		(lambda (env succeed fail)
			(define (try-next choices)
				(if (null? choices)
					(fail)
					((car choices) env succeed
						(lambda () (try-next (cdr choices))))
				)
			)
			(try-next cprocs)
		)
	)
)


(define (cdr? exp)
	(tagged-list? exp 'cdr))
(define (eval-cdr exp env)
	(if (cons-result? exp)
			((cadr exp) (lambda (p q) q))
			((cadr (actual-value (cadr exp) env)) (lambda (p q) q))
		)
	)
(define (analyze-self-evaluating exp)
(lambda (env succeed fail)
(succeed exp fail)))

(define (analyze-quoted exp)
(let ((qval (text-of-quotation exp)))
(lambda (env succeed fail)
(succeed qval fail))))
(define (analyze-variable exp)
(lambda (env succeed fail)
(succeed (lookup-variable-value exp env) fail)))

(define (analyze-assignment exp)
	(let ((var (assignment-variable exp))
		(vproc (analyze (assignment-value exp))))
		(lambda (env succeed fail)
					(vproc env
						(lambda (val fail2)
; *1*
							(let ((old-value(lookup-variable-value var env)))
								(set-variable-value! var val env)
								(succeed 'ok
									(lambda ()
; *2*
										(set-variable-value! var old-value env)
										(fail2)
									)
								)
							)
						)
						fail)
		)
	)
)

(define (require? exp)
(tagged-list? exp 'require))
(define (require-predicate exp)
(cadr exp))

(define (analyze-require exp)
	(let ((pproc (analyze (require-predicate exp))))
		(lambda (env succeed fail)
			(pproc env
				(lambda (pred-value fail2)
						(if (not (true? pred-value))
							(fail2)
							(succeed 'ok fail2)
						)
				)
			fail)
		)
	)
)

(define (permanent-assignment? exp) (tagged-list? exp 'permanent-set!))

(define (analyze-permanent-assignment exp)
	(let ((var (assignment-variable exp))
		(vproc (analyze (assignment-value exp))))
		(lambda (env succeed fail)
					(vproc env
						(lambda (val fail2)
								(set-variable-value! var val env)
								(succeed 'ok fail2)
						)
						fail)
		)
	)
)

(define (if-fail? exp) (tagged-list? exp 'if-fail))
(define (if-fail-success exp) (cadr exp))
(define (if-fail-default exp) (caddr exp))

(define (analyze-if-fail exp)
		(lambda (env succeed fail)
			((analyze (if-fail-success exp))
					env succeed
					(lambda ()
							((analyze (if-fail-success exp)) env succeed fail)
						)
					)
			)
)
		

(define (analyze-definition exp)
	(let ((var (definition-variable exp))
(vproc (analyze (definition-value exp))))
(lambda (env succeed fail)
(vproc env
(lambda (val fail2)
(define-variable! var val env)
(succeed 'ok fail2))
fail))))

(define (analyze-if exp)
(let ((pproc (analyze (if-predicate exp)))
(cproc (analyze (if-consequent exp)))
(aproc (analyze (if-alternative exp))))
(lambda (env succeed fail)
(pproc env
;; success continuation for evaluating the predicate
;; to obtain pred-value
(lambda (pred-value fail2)
(if (true? pred-value)
(cproc env succeed fail2)
(aproc env succeed fail2)))
;; failure continuation for evaluating the predicate
fail))))

(define (analyze-let exp)
	(lambda (env)
		(
			(analyze (make-lambda (let-vars exp) (let-body exp)) (map analyze (let-inits exp)))
			 env)
		)
)

(define (analyze-lambda exp)
(let ((vars (lambda-parameters exp))
(bproc (analyze-sequence (lambda-body exp))))
(lambda (env succeed fail)
(succeed (make-procedure vars bproc env) fail))))

(define (analyze-sequence exps)
(define (sequentially a b)
(lambda (env succeed fail)
(a env
;; success continuation for calling a
(lambda (a-value fail2)
(b env succeed fail2))
;; failure continuation for calling a
fail)))
(define (loop first-proc rest-procs)
(if (null? rest-procs)
first-proc
(loop (sequentially first-proc
(car rest-procs))
(cdr rest-procs))))
(let ((procs (map analyze exps)))
(if (null? procs)
(error "Empty sequence: ANALYZE"))
(loop (car procs) (cdr procs))))

(define (analyze-application exp)
(let ((fproc (analyze (operator exp)))
(aprocs (map analyze (operands exp))))
(lambda (env succeed fail)
(fproc env
(lambda (proc fail2)
(get-args aprocs env
	(lambda (args fail3)
(execute-application
proc args succeed fail3))
fail2))
fail))))

(define (get-args aprocs env succeed fail)
(if (null? aprocs)
(succeed '() fail)
((car aprocs)
env
;; success continuation for this aproc
(lambda (arg fail2)
(get-args
(cdr aprocs)
env
;; success continuation for
;; recursive call to get-args
(lambda (args fail3)
(succeed (cons arg args) fail3))
fail2))
fail)))

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (ramb? exp) (tagged-list? exp 'ramb))

(define (ambeval exp env succeed fail)
((analyze exp) env succeed fail))

(define 
	(eval exp env)
		((analyze exp) env)
	)

(define (analyze exp)
	(cond ((self-evaluating? exp) (analyze-self-evaluating exp))
		((quoted? exp) (analyze-quoted exp))
		((amb? exp) (analyze-amb exp))
		((ramb? exp) (analyze-ramb exp))
		((variable? exp) (analyze-variable exp))
		((assignment? exp) (analyze-assignment exp))
		((permanent-assignment? exp) (analyze-permanent-assignment exp))
		((require? exp) (analyze-require exp))
		((definition? exp) (analyze-definition exp))
		((if? exp) (analyze-if exp))
		((lambda? exp) (analyze-lambda exp))
		((begin? exp) (analyze-sequence (begin-actions exp)))
		((cond? exp) (analyze (cond->if exp)))
		((application? exp) (analyze-application exp))
		(else (error "Unknown expression type: ANALYZE" exp))))

(define (execute-application proc args succeed fail)
(cond ((primitive-procedure? proc)
(succeed (apply-primitive-procedure proc args)
fail))
((compound-procedure? proc)
((procedure-body proc)
(extend-environment
(procedure-parameters proc)
args
(procedure-environment proc))
succeed
fail))
(else
(error "Unknown procedure type: EXECUTE-APPLICATION"
proc))))

(define (primitive-procedure? proc)
(tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
	(list #| (list 'car car)
		(list 'cdr cdr)
		(list 'cons cons) |#
		(list 'null? null?)
		(list '+ +)
		(list '= =)
		(list '- -)
		(list '/ /)
		(list '* *)
		(list 'newline newline)
		(list 'display display)
))

(define (primitive-procedure-names)
(map car primitive-procedures))

(define (primitive-procedure-objects)
(map (lambda (proc) (list 'primitive (cadr proc)))
primitive-procedures))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
(apply-in-underlying-scheme
(primitive-implementation proc) args))

(define (setup-environment)
(let ((initial-env
(extend-environment (primitive-procedure-names)
(primitive-procedure-objects)
the-empty-environment)))
(define-variable! 'true true initial-env)
(define-variable! 'false false initial-env)
initial-env))
(define the-global-environment (setup-environment))

(define (list-of-arg-values exps env)
(if (no-operands? exps)
'()
(cons (actual-value (first-operand exps)
env)
(list-of-arg-values (rest-operands exps)
env))))

(define (list-of-delayed-args exps env)
(if (no-operands? exps)
'()
(cons (delay-it (first-operand exps)
env)
(list-of-delayed-args (rest-operands exps)
env))))

(define (lazy? exp) (tailed-list? exp 'lazy))
(define (lazy-memo? exp) (tailed-list? exp 'lazy-memo))


(define (list-of-args params exps env)
	(if (no-operands? exps)
		'()
		(cond 
			((lazy? (car params))
					(cons 
						(delay-it (first-operand exps)env)
						(list-of-args (cdr params) (cdr exps) env)))
			((lazy-memo? (car params))
					(cons 
						(delay-it-memo (first-operand exps)env)
						(list-of-args (cdr params) (cdr exps) env)))
			((symbol? (car params))
					(cons 
						(eval (first-operand exps) env)
						(list-of-args (cdr params) (cdr exps) env)))
				)
			)
	)
 
(define (apply procedure arguments env)
	(cond 	((primitive-procedure? procedure)
				(apply-primitive-procedure
				 procedure
				 (list-of-arg-values arguments env)))
			((compound-procedure? procedure)
				(eval-sequence
					(procedure-body procedure)
					(extend-environment
						(procedure-parameters-clean procedure)
						(list-of-args
							(procedure-parameters procedure)
							arguments
							env)
						#| (list-of-delayed-args arguments env) |#
						(procedure-environment procedure))))
			(else
				(error "Unknown procedure type: APPLY" procedure))))

(define (prompt-for-input string)
(newline) (newline) (display string) (newline))
(define (announce-output string)
(newline) (display string) (newline))


(define (user-print object)
(if (compound-procedure? object)
(display (list 'compound-procedure
(procedure-parameters object)
(procedure-body object)
'<procedure-env>))
(display object)))

(define input-prompt
";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define the-global-environment (setup-environment))

(define (driver-loop)
	(define (internal-loop try-again)
		(prompt-for-input input-prompt)
		(let ((input (read)))
			(if (eq? input 'try-again)
				(try-again)
				(begin (newline) (display ";;; Starting a new problem ")
						(ambeval input the-global-environment
							;; ambeval success
							(lambda (val next-alternative) (announce-output output-prompt)
									(user-print val)
									(internal-loop next-alternative)
							)
							;; ambeval failure
							(lambda ()
								(announce-output ";;; There are no more values of")
								(user-print input)
								(driver-loop)
							)
						)
					)
			)
		)
	)
	(internal-loop
		(lambda () (newline) (display ";;; There is no current problem") (driver-loop))
	)
)


(driver-loop)