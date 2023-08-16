(define (true? x)(not (eq? x false)))
(define (false? x) (eq? x false))

(define (empty-arglist arglist) (eq? arglist '()))

(define (append list1 list2)
(if (null? list1)
list2
(cons (car list1) (append (cdr list1) list2))))

(define (append-triple list1 list2 list3)
  (append list1 (append list2 list3)))


(define (append-compiler list1 list2)
(if (null? list1)
list2
(cons (car list1) (append (cdr list1) list2))))

(define (empty-arglist) '())
(define (adjoin-arg arg arglist) (append arglist (list arg)))

(define (no-more-exps? seq) (null? seq))

(define (start-eceval)
(set! the-global-environment (setup-environment))
(set-register-contents! eceval 'flag false)
(start eceval))

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
(create-error-obj (string-append "Too many arguments supplied: " (string vars) (string vals)))
(create-error-obj (string-append "Too few arguments supplied: " (string vars) (string vals))))))

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
      '()
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

(define (create-error-obj errorValue)
    (cons 'err101 errorValue)
  )

(define (is-error-return errorValue)
  (AND (NOT (null? errorValue)) (pair? errorValue) (eq? 'err101 (car errorValue)) (NOT (null? (cdr errorValue))))
  )

(define (get-error-text errorValue)
    (cdr errorValue)
  )

(define (lookup-variable-value var env)
  (let ((binding (find-in-all-env var env)))
    (cond ((null? binding) (create-error-obj (string-append "Unbound variable: " (string var))))
        ((eq? unassigned (cdr binding)) (create-error-obj (string-append "unassigned var: " (string var))) )
        (else (cdr binding))
      )
    )
)

(define (set-variable-value! var val env)
  (let ((binding (find-in-all-env var env)))
    (if (null? binding)
      (create-error-obj (string-append "Unbound variable: " (string var)))
      (begin (set-cdr! binding val) val)
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
(define (text-of-quotation exp env)
  (if (pair? (cadr expr))
    (generate-lazy-list (cadr exp) env)
    (cadr expr)
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

(define (generate-lazy-list quoted env)
  (if (null? quoted)
    '()
    (lambda (m) (m (car quoted) (generate-lazy-list (cdr quoted) env)))
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


(define (cdr? exp)
  (tagged-list? exp 'cdr))
(define (eval-cdr exp env)
  (if (cons-result? exp)
      ((cadr exp) (lambda (p q) q))
      ((cadr (actual-value (cadr exp) env)) (lambda (p q) q))
    )
  )

 (define (eval exp env)
  #| (newline)(display "eval - ")(display exp)(newline) |#
  (cond
      ((self-evaluating? exp) exp)
      ((AND? exp) (eval-and exp env))
      ((OR? exp) (eval-or exp env))
      ((if? exp) (eval-if exp env))
      ((let? exp) (eval (let->combination exp) env))
      ((let*? exp) (eval (let*->nested-lets exp) env))
      ((letrec? exp) (eval (letrec->nested-let exp) env))
      ((for? exp) (eval (eval-for exp) env))
      ((cons? exp) (eval-cons exp env))
      ((car? exp) (eval-car exp env))
      ((cdr? exp) (eval-cdr exp env))
      ((variable? exp) (lookup-variable-value exp env))
      ((quoted? exp) (text-of-quotation exp env))
      ((assignment? exp) (eval-assignment exp env))
      ((definition? exp) (eval-definition exp env))
      ((unbound? exp) (unbound-variable! (unbound-symbol exp) env))
      ((lambda? exp) (make-procedure  (lambda-parameters exp)
                      (lambda-body exp)
                      env))
      ((begin? exp)
        (eval-sequence (begin-actions exp) env))
      ((cond? exp) (eval (cond->if exp env) env))
      ((application? exp)
        (apply (actual-value (operator exp) env)
            (operands exp)env))
      (else
        (error "Unknown expression type: EVAL" exp))))

(define (analyze-self-evaluating exp)
(lambda (env) exp))

(define (analyze-quoted exp)
(let ((qval (text-of-quotation exp)))
(lambda (env) qval)))

(define (analyze-variable exp)
(lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
(let ((var (assignment-variable exp))
(vproc (analyze (assignment-value exp))))
(lambda (env)
(set-variable-value! var (vproc env) env)
'ok)))

(define (analyze-definition exp)
(let ((var (definition-variable exp))
(vproc (analyze (definition-value exp))))
(lambda (env)
(define-variable! var (vproc env) env)
'ok)))

(define (analyze-if exp)
(let ((pproc (analyze (if-predicate exp)))
(cproc (analyze (if-consequent exp)))
(aproc (analyze (if-alternative exp))))
(lambda (env) (if (true? (pproc env))
(cproc env)
(aproc env)))))

(define (unless? exp) (tagged-list? exp 'unless))
(define (unless-condition exp) (cadr exp))
(define (unless-usual exp) (caddr exp))
(define (unless-exceptional exp) (cadddr exp))


(define (analyze-unless exp)
  (let ((condition (analyze (unless-condition exp)))
    (usual (analyze (unless-usual exp)))
    (exceptional (analyze (unless-exceptional exp))))
    
    (lambda (env)(if (true? (condition env)) (exceptional env) (usual env))))
)


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
(lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
(define (sequentially proc1 proc2)
(lambda (env) (proc1 env) (proc2 env)))
(define (loop first-proc rest-procs)
(if (null? rest-procs)
first-proc
(loop (sequentially first-proc (car rest-procs))
(cdr rest-procs))))
(let ((procs (map analyze exps)))
(if (null? procs) (error "Empty sequence: ANALYZE"))
(loop (car procs) (cdr procs))))

(define (analyze-application exp)
(let ((fproc (analyze (operator exp)))
(aprocs (map analyze (operands exp))))
(lambda (env)
(execute-application
(fproc env)
(map (lambda (aproc) (aproc env))
aprocs)))))

#| (define 
  (eval exp env)
    ((analyze exp) env)
  )

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
    ((quoted? exp) (analyze-quoted exp))
    ((variable? exp) (analyze-variable exp))
    ((assignment? exp) (analyze-assignment exp))
    ((definition? exp) (analyze-definition exp))
    ((if? exp) (analyze-if exp))
    ((unless? exp) (analyze-unless exp))
    ((lambda? exp) (analyze-lambda exp))
    ((begin? exp) (analyze-sequence (begin-actions exp)))
    ((cond? exp) (analyze (cond->if exp)))
    ((application? exp) (analyze-application exp))
    (else (error "Unknown expression type: ANALYZE" exp)))) |#

(define (execute-application proc args)
(cond ((primitive-procedure? proc)
(apply-primitive-procedure proc args))
((compound-procedure? proc)
((procedure-body proc)
(extend-environment
(procedure-parameters proc)
args
(procedure-environment proc))))
(else
(error "Unknown procedure type: EXECUTE-APPLICATION"
proc))))

(define (primitive-procedure? proc)
(tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define apply-in-underlying-scheme apply)

(define (car-primitive pair)
  (if (pair? pair)
    (car pair)
    (create-error-obj  "Car primitive error 'argument is not null'")
    )
  )
(define (cdr-primitive pair)
  (if (pair? pair)
    (cdr pair)
    (create-error-obj  "Cdr primitive error 'argument is not null'")
    )
  )

(define (all-numbers data)
    (cond ((null? data) #t)
        ((number? (car data))  (all-numbers (cdr data)))
        (else #f)
      )
)
(define (contains-zero? data)
    (cond ((null? data) #f)
        ((=  (car data) 0)  #t)
        (else (contains-zero? (cdr data)))
      )
)
(define (apply-integer-function name func arguments)
    (if (all-numbers arguments)
      (apply-in-underlying-scheme func arguments)
      (create-error-obj  (string-append name " function primitive error 'Not all arguments are numbers'"))
    )
  )
(define (plus-primitive . arguments)
    (apply-integer-function "plus" + arguments)
)

(define (minus-primitive . arguments)
  (apply-integer-function "minus" - arguments)
  )
(define (greater-primitive . arguments)
  (apply-integer-function "greater" > arguments)
  )
(define (less-primitive . arguments)
  (apply-integer-function "less" < arguments)
  )
(define (equal-primitive . arguments)
  (apply-integer-function "equal" = arguments)
  )
(define (multiply-primitive . arguments)
  (apply-integer-function "multiply" * arguments)
  )
(define (division-primitive . arguments)
  (cond ((NOT (all-numbers arguments))  (create-error-obj  "division primitive error 'Not all arguments are numbers'"))
        ((contains-zero? (cdr arguments)) (create-error-obj  "division primitive error 'division by zero'"))
        (else (apply-in-underlying-scheme / arguments ))

    )
  )

(define primitive-procedures
  (list  (list 'car car-primitive)
    (list 'cdr cdr-primitive)
    (list 'cons cons) 
    (list 'null? null?)
    (list '+ plus-primitive)
    (list '> greater-primitive)
    (list '< less-primitive)
    (list '= equal-primitive)
    (list '- minus-primitive)
    (list '/ division-primitive)
    (list '* multiply-primitive)
    (list 'newline newline)
    (list 'display display)
))

(define (primitive-procedure-names)
(map car primitive-procedures))

(define (primitive-procedure-objects)
(map (lambda (proc) (list 'primitive (cadr proc)))
primitive-procedures))


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
  (cond   ((primitive-procedure? procedure)
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

(define (print-list data)
  (define (print-list-inner l deep)
    (cond   ((null? l) (display ")"))
        ((NOT (pair? l)) (display l)(display ")"))
        ((= deep 0) (display ". . .)"))
        ((cons? l) (print-list-inner (actual-value l the-global-environment) deep))
        (else (begin
            (display (eval-car l the-global-environment))
            (display " ") 
            (print-list-inner (eval-cdr l the-global-environment) ( - deep 1))))
      )
    )
  (display "(")
  (print-list-inner data 10)
  )

(define (user-print object)
(cond ((compound-procedure? object)
(display (list 'compound-procedure
(procedure-parameters object)
(procedure-body object)
'<procedure-env>)))
((compiled-procedure? object)
(display '<compiled-procedure>))
(else (display object))))


#| --------------------------------------------------------------------------- |#

(define (tagged-list? exp tag)
(if (pair? exp)
(eq? (car exp) tag)
false))

(define (make-machine ops controller-text)
(let ((machine (make-new-machine)))
((machine 'install-operations) ops)
((machine 'install-instruction-sequence)
(assemble controller-text machine))
machine))

(define (make-register name)
  (let ((contents '*unassigned*)
      (trac #f)
    )

    (define (dispatch message)
        (cond ((eq? message 'get) contents)
          ((eq? message 'set-trace) (lambda(val) (if val (set! trac #t) (set! trac #f))))
            ((eq? message 'set)
              (lambda (value) 
                (if trac
                  (begin (newline)(display "reg: ")(display name)(display "; old value - ")(display contents)
                    (display "; new value - ") (display value)(newline))
                  )
                (set! contents value)
                ))
            (else
              (error "Unknown request: REGISTER" message))))
  dispatch)
)

(define (set-register-trace reg-name machine)
  (((get-register machine reg-name) 'set-trace) #t)
  )

(define (get-contents register) (register 'get))
(define (set-contents! register value)
((register 'set) value))

(define (make-stack)
  (let ((s '())
    (number-pushes 0)
    (max-depth 0)
    (current-depth 0))
    (define (push reg x)
      (let ((stack-reg (assoc reg s)))
        (if  stack-reg
          (set-cdr! stack-reg (cons x (cdr stack-reg)))
          (set! s (cons (cons reg (cons x '())) s))
          )
        (set! number-pushes (+ 1 number-pushes))
        (set! current-depth (+ 1 current-depth))
        (set! max-depth (max current-depth max-depth))
      )
    )
    (define (pop reg)
      (if (null? s)
        (error "Empty stack: POP")
        (let ((stack-reg (assoc reg s))
          (val '()))
          (if stack-reg
            (begin (set! val (cadr stack-reg)) (set-cdr! stack-reg (cddr stack-reg)) 
                (set! current-depth (- current-depth 1)) val)
            (error "Empty stack for register: POP")
            )
        )
      )
    )
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
          (newline)
          (display (list 'total-pushes '= number-pushes 'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
        ((eq? message 'pop) pop)
        ((eq? message 'initialize) (initialize))
        ((eq? message 'print-statistics) (print-statistics))
        (else (error "Unknown request: STACK" message))))
    dispatch)
)

(define (pop stack reg)
 ((stack 'pop) reg))
(define (push stack reg value) ((stack 'push) reg value))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
    (flag (make-register 'flag))
    (stack (make-stack))
    (ins-count 0)
    (the-instruction-sequence '())
    (distinct-instructions '())
    (entry-points-registers '())
    (stack-registers '())
    (register-sources '())
    (inst-labels '())
    (trace-on #f)
    (breakpoint '())
    (breakpoint-step -2)
    (breakpoint-position -1)
    )
      
      (let ((the-ops (list (list 'initialize-stack (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics (lambda () (stack 'print-statistics)))))
        (register-table (list (list 'pc pc) (list 'flag flag))))

        (define (allocate-register name)
          (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table (cons (list name (make-register name)) register-table))
            )
          'register-allocated)

(define (add-instruction-distinct pair)
          (define (recursive-add sequence)
            (if(null? sequence)
              (cons pair '())
              (if (> (cdar sequence) (cdr pair))
                (cons pair sequence)
                (cons (car sequence) (recursive-add (cdr sequence)))
                )
              )
            )
          (if (NOT (assoc (car pair) distinct-instructions))
            (set! distinct-instructions (recursive-add distinct-instructions))
            )
          )

        (define (add-entry-point-register reg)
          (cond ((null? entry-points-registers) (set! entry-points-registers (cons reg '())))
              ((NOT (memq reg entry-points-registers)) 
                (set! entry-points-registers (cons reg entry-points-registers)))
            )
          )

        (define (add-register-source reg source)
          (define (add-source data)
            (cond ((null? data) (cons source '()))
                ((eq? (car data) source) data)
                (else (cons (car data) (add-source (cdr data))))
              )
            )
          (cond ((null? register-sources) (set! register-sources (list (list reg source))))
            ((assoc reg register-sources) (set-cdr! (assoc reg register-sources)
             (cdr (add-source (assoc reg register-sources))) ))
            (else
              (set! register-sources (cons (list reg source) register-sources)))
            )
          )

        (define (add-stack-register reg)
          (cond ((null? stack-registers) (set! stack-registers (cons reg '())))
              ((NOT (memq reg stack-registers)) 
                (set! stack-registers (cons reg stack-registers)))
            )
          )

        (define (lookup-register name)
          (let ((val (assoc name register-table)))
            (if val
              (cadr val)
              #f)))
        (define (inc-count)
          (set! ins-count (+ ins-count 1))
        )
        (define (inc-breakpoint)
          (if (> breakpoint-position 0)
            (set! breakpoint-position (+ breakpoint-position 1))
            )
        )
        (define (reset-count)
          (set! ins-count 0)
        )
        (define (trace inst)
          (if trace-on
            (begin (newline)(display (get-inst-label (car inst))) (display " - ")(display (car inst))(newline))
            )
          )

        (define (add-inst-label inst label)
          (cond ((null? inst-labels) (set! inst-labels (list (cons inst label))))
            ((NOT (assoc inst inst-labels))(set! inst-labels (cons (cons inst label) inst-labels)))
          )
        )
        (define (get-inst-label inst)
          (if (assoc inst inst-labels)
            (cdr (assoc inst inst-labels))
            "NOT"
            )
        )

        (define (set-breakpoint label n)
          (set! breakpoint label)
          (set! breakpoint-step n)
          )


        (define (cancel-breakpoint label)
          (set! breakpoint '())
          (set! breakpoint-step -2)
          (set! breakpoint-position -1)
          )

        (define (check-breakpoint label)
          (if (eq? label breakpoint)
            (set! breakpoint-position 1)
            )
        )
        (define (make-breakpoint)
          (set! breakpoint-position -1)
          'breakpoint
        )

        (define (execute)
          (let ((insts (get-contents pc)))
            (cond   ((null? insts) 'done)

                ((eq? breakpoint-position breakpoint-step)
                    (make-breakpoint))

                (else (begin (inc-count) (inc-breakpoint) (trace (car insts)) ((instruction-execution-proc (car insts)))(execute)))
              )))


(define (dispatch message)
          (cond ((eq? message 'start)
              (set-contents! pc the-instruction-sequence) (execute))
              ((eq? message 'proceed)
              (execute))
              ((eq? message 'check-breakpoint)
              check-breakpoint)
              ((eq? message 'install-instruction-sequence)
              (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register)
                allocate-register)
              ((eq? message 'add-register-source)
                add-register-source)
              ((eq? message 'get-count)
                ins-count)
              ((eq? message 'set-breakpoint)
                set-breakpoint)
              ((eq? message 'cancel-breakpoint)
                cancel-breakpoint)
              ((eq? message 'trace-on)
                (set! trace-on #t))
              ((eq? message 'trace-off)
                (set! trace-on #f))
              ((eq? message 'add-inst-label)
                add-inst-label)
              ((eq? message 'get-inst-label)
                get-inst-label)
              ((eq? message 'reset-count)
                (reset-count))
              ((eq? message 'add-instruction-distinct)
                add-instruction-distinct)
              ((eq? message 'add-entry-point-register)
                add-entry-point-register)
              ((eq? message 'add-stack-register )
                add-stack-register )
              ((eq? message 'get-instructions-distinct)
                (map car distinct-instructions))
              ((eq? message 'get-register-source)
                register-sources)
              ((eq? message 'get-entry-point-register)
                entry-points-registers)
              ((eq? message 'get-stack-registers)
                stack-registers)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
                (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request: MACHINE" message))))
  dispatch))
)

(define (set-breakpoint machine label n)
  ((machine 'set-breakpoint) label n)
  )

(define (cancel-breakpoint machine label)
  ((machine 'cancel-breakpoint) label)
  )

(define (proceed-machine machine)
  (machine 'proceed)
  )

(define (get-register machine reg-name)
  (let ((reg ((machine 'get-register) reg-name)))
    (if reg
      reg
      (begin ((machine 'allocate-register) reg-name) ((machine 'get-register) reg-name))
      )

    )
)

(define (start machine) (machine 'start))

(define (stack-statistic machine)
  ((cadr (assoc 'print-stack-statistics (machine 'operations))))
  )

(define (get-register-contents machine register-name)
(get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
(set-contents! (get-register machine register-name)
value)
'done)

(define (add-register-source machine reg source)
((machine 'add-register-source) reg source))

(define (add-instruction-distinct machine pair)
((machine 'add-instruction-distinct) pair))

(define (add-entry-point-register machine reg)
((machine 'add-entry-point-register) reg))

(define (add-stack-registers machine reg)
((machine 'add-stack-register) reg))

(define (get-instruction-distinct machine)
(machine 'get-instruction-distinct))

(define (assemble controller-text machine)
(extract-labels
controller-text
(lambda (insts labels)
(update-insts! insts labels machine)
insts)))

(define (check-duplicate-label labels label-name)
(let ((val (assoc label-name labels)))
(if val
(error "DUPLICATE label: ASSEMBLE"
label-name)
false
)))


(define (extract-labels text receive)
  (if (null? text)
    (receive '() '())
    (extract-labels
      (cdr text)
      (lambda (insts labels)
        (let ((next-inst (car text)))
          (if (symbol? next-inst)
            (let ((res (check-duplicate-label labels next-inst))
                (new-inst (cons (create-breakpoint-inst next-inst) insts))
              )
                 (receive
                  new-inst
                  (cons (make-label-entry next-inst new-inst) labels))) 

            (receive (cons 
              (make-instruction next-inst) insts) labels)
          )
        )
      )
    )
  )
)

(define (create-breakpoint-inst label)
  (cons (list 'breakpoint label) '())
  )

(define (add-inst-label machine labels)
  (for-each
    (lambda (inst)
      (if (NOT (null? (cdr inst))) 
      ((machine 'add-inst-label) (caadr inst) (car inst))
      ))
    labels)
  )

(define (update-insts! insts labels machine)
(let ((pc (get-register machine 'pc))
   (flag (get-register machine 'flag))
   (stack (machine 'stack))
   (ops (machine 'operations)))
    (add-inst-label machine labels)
    (for-each
      (lambda (inst) (set-instruction-execution-proc! inst
                (make-execution-procedure (instruction-text inst)
                  labels machine pc flag stack ops)))
    insts)))

(define (make-instruction text) (cons text '()))
(define (instruction-text inst) (car inst))
(define (instruction-execution-proc inst) (cdr inst))
(define (set-instruction-execution-proc! inst proc)
(set-cdr! inst proc))

(define (make-label-entry label-name insts)
(cons label-name insts))

(define (lookup-label labels label-name)
(let ((val (assoc label-name labels)))
(if val
(cdr val)
(error "Undefined label: ASSEMBLE"
label-name))))

(define (make-execution-procedure
inst labels machine pc flag stack ops)
(cond ((eq? (car inst) 'assign)
(begin (add-instruction-distinct machine (cons 'assign 1))(make-assign inst machine labels ops pc)))
((eq? (car inst) 'test)
(begin (add-instruction-distinct machine (cons 'test 2))(make-test inst machine labels ops flag pc)))
((eq? (car inst) 'branch)
(begin (add-instruction-distinct machine (cons 'branch 3))(make-branch inst machine labels flag pc)))
((eq? (car inst) 'goto)
(begin (add-instruction-distinct machine (cons 'goto 4))(make-goto inst machine labels pc)))
((eq? (car inst) 'save)
(begin (add-instruction-distinct machine (cons 'save 5))(make-save inst machine stack pc)))
((eq? (car inst) 'restore)
(begin (add-instruction-distinct machine (cons 'restore 6))(make-restore inst machine stack pc)))
((eq? (car inst) 'perform)
(begin (add-instruction-distinct machine (cons 'perform 7))(make-perform inst machine labels ops pc)))
((eq? (car inst) 'inc) 
(begin (add-instruction-distinct machine (cons 'inc 8)) (make-inc inst machine labels ops pc)))
((eq? (car inst) 'breakpoint) 
    (make-breakpoint inst machine labels ops pc))
(else
(error "Unknown instruction type: ASSEMBLE"
inst))))

(define (make-breakpoint inst machine labels operations pc)
  (lambda () 
    ((machine 'check-breakpoint) (cadr inst))
    (advance-pc pc)
  )
  )

(define (make-inc inst machine labels operations pc)
(let ((target (get-register machine (inc-reg-name inst)))
    (value (get-contents (get-register machine (inc-reg-name inst)))))
    
    (lambda () (set-contents! target (+ value 1))
(advance-pc pc))))

(define (inc-reg-name inc-instruction)
(cadr inc-instruction))

(define (make-assign inst machine labels operations pc)
  (let ((target (get-register machine (assign-reg-name inst)))
    (value-exp (assign-value-exp inst)))
    (add-register-source machine (assign-reg-name inst) (assign-value-exp inst))
      (let ((value-proc
          (if (operation-exp? value-exp)
            (make-operation-exp value-exp machine labels operations)
            (make-primitive-exp (car value-exp) machine labels))))
(lambda ()
; execution procedure for assign
(set-contents! target (value-proc))
(advance-pc pc)))))

(define (assign-reg-name assign-instruction)
(cadr assign-instruction))
(define (assign-value-exp assign-instruction)
(cddr assign-instruction))

(define (advance-pc pc)
(set-contents! pc (cdr (get-contents pc))))

(define (make-test inst machine labels operations flag pc)
(let ((condition (test-condition inst)))
(if (operation-exp? condition)
(let ((condition-proc
(make-operation-exp
condition machine labels operations)))
(lambda ()
(set-contents! flag (condition-proc))
(advance-pc pc)))
(error "Bad TEST instruction: ASSEMBLE" inst))))
(define (test-condition test-instruction)
(cdr test-instruction))

(define (make-branch inst machine labels flag pc)
(let ((dest (branch-dest inst)))
(if (label-exp? dest)
(let ((insts
(lookup-label
labels
(label-exp-label dest))))
(lambda ()
(if (get-contents flag)
(set-contents! pc insts)
(advance-pc pc))))
(error "Bad BRANCH instruction: ASSEMBLE" inst))))
(define (branch-dest branch-instruction)
(cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
        (let ((insts (lookup-label labels
          (label-exp-label dest))))
          (lambda () (set-contents! pc insts))))
      ((register-exp? dest)
        (let ((reg (get-register machine (register-exp-reg dest))))
          (add-entry-point-register machine (register-exp-reg dest))
          (lambda () (set-contents! pc (get-contents reg)))))
      (else (error "Bad GOTO instruction: ASSEMBLE" inst)))))

(define (goto-dest goto-instruction)
(cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (add-stack-registers machine (stack-inst-reg-name inst))
      (lambda () (push stack (stack-inst-reg-name inst) (get-contents reg))
            (advance-pc pc)))

)

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (add-stack-registers machine (stack-inst-reg-name inst))
      (lambda () (set-contents! reg (pop stack (stack-inst-reg-name inst)))
      (advance-pc pc)))
)

(define (stack-inst-reg-name stack-instruction)
(cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
(let ((action (perform-action inst)))
(if (operation-exp? action)
(let ((action-proc
(make-operation-exp
action machine labels operations)))
(lambda () (action-proc) (advance-pc pc)))
(error "Bad PERFORM instruction: ASSEMBLE" inst))))
(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
      (let ((c (constant-exp-value exp)))
        (lambda () c)))
      ((label-exp? exp)
        (let ((insts (lookup-label labels (label-exp-label exp))))
        (lambda () insts)))
((register-exp? exp)
(let ((r (get-register machine (register-exp-reg exp))))
(lambda () (get-contents r))))
(else (error "Unknown expression type: ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
    (aprocs (map (lambda (e)
          (make-primitive-exp e machine labels))
          (map (lambda (t) 
              (if (label-exp? exp)
                (error "Cannot use label for OP!" exp)
                t))
            (operation-exp-operands exp)))))
    (lambda ()
      (apply-in-underlying-scheme op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
(and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
(cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
(cdr operation-exp))

(define (lookup-prim symbol operations)
(let ((val (assoc symbol operations)))
(if val
(cadr val)
(error "Unknown operation: ASSEMBLE"
symbol))))

#| -------------------------------------------- Compiler ----------------------------------------------- |#
(define (get-element data pos)
  (if (< pos 1)
    (car data)
    (get-element (cdr data) (- pos 1))
    )
  )

(define (lexical-address-operation lexical-address env operation)
  (let ((frame (car lexical-address))
      (position (cdr lexical-address)))
    (let ((variable (get-element (get-element env frame) position)))
        (operation variable)
      )
    )
  )

(define (lexical-address-lookup lexical-address env)
  (lexical-address-operation
   lexical-address
   env
    (lambda (variable)
        (if (eq? unassigned (cdar variable))
        (error "*unassigned* variable: " variable)
        (cdar variable)
        )
      )
  )
)

(define (lexical-address-set! lexical-address env value)
  (lexical-address-operation
   lexical-address
   env
    (lambda (variable)
        (if (OR (null? (cdr variable)) (NOT (pair? (cdr variable))))
        (error "Null or not variable: " variable)
        (set-cdr! (cdr variable) value)
        )
      )
  )
)

(define (find-variable var compile-env)

  (define (find-variable-in-frame var vars count)
    (cond ((eq? vars '()) -1)
      ((eq? var (car vars)) count)
      (else (find-variable-in-frame var (cdr vars) (+ count 1)))

      )
    )

  (define (find-variable-inner var frames count)
    (if (eq? '() frames)
      'not-found
      (let ((result (find-variable-in-frame var (car frames) 0))) 
        (if (< result 0)
          (find-variable-inner var (cdr frames) (+ count 1))
          (cons count result)
          )
        )
      )
    )
  (find-variable-inner var compile-env 0)
  )

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
      ((string? exp) true)
      (else false)))

(define (last-exp? seq) (null? (cdr seq)))


(define (quoted? exp) (tagged-list? exp 'quote))

(define (variable? exp)
 (symbol? exp))

 (define (assignment? exp) (tagged-list? exp 'set!))

 (define (assignment-value exp) (caddr exp))
 (define (assignment-variable exp) (cadr exp))

 (define (definition? exp) (tagged-list? exp 'define))

 (define (if? exp) (tagged-list? exp 'if))

 (define (lambda? exp) (tagged-list? exp 'lambda))

 (define (begin? exp) (tagged-list? exp 'begin))

 (define (cond? exp) (tagged-list? exp 'cond))

(define (plus? exp) (tagged-list? exp '+))
(define (minus? exp) (tagged-list? exp '-))
(define (equal-c? exp) (tagged-list? exp '=))
(define (mult? exp) (tagged-list? exp '*))

 (define (application? exp) (pair? exp))

 (define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))

 (define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)
          (cddr exp))))

 (define (operator exp) (car exp))
(define (operands exp) (cdr exp))

 (define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

 (define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (scan-defines exp)
  (define (get-defines body)
    (cond ((null? body) '())
      ((definition? (car body)) 
          (cons (cons (definition-variable (car body)) (definition-value (car body)))
             (get-defines (cdr body))))
      (else (get-defines (cdr body))) 
      )
    )
  (get-defines exp)
)

(define (filter-defines exp)
    (cond ((null? exp) '())
      ((definition? (car exp)) (filter-defines (cdr exp)))
      (else (cons (car exp) (filter-defines (cdr exp)))) 
      )
)

(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
(if (not (null? (cdddr exp)))
  (cadddr exp)
  'false))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))



; target -- which speciﬁes the register in which the compiled code is to return the value of the expression
; linkage -- descriptor, which describes how the code resulting from the compilation of the expression should proceed when it has ﬁnished its execution
  ; • continue at the next instruction in sequence (this is speciﬁed by the linkage descriptor next),
  ; • return from the procedure being compiled (this is speciﬁed by the linkage descriptor return)
      ; If the linkage is return, we don’t need to set up continue - With this implementation of the return linkage, the compiler generates
      ; tail-recursive code
  ; • jump to a named entry point (this is speciﬁed by using the designated label as the linkage descriptor).

(define (compile exp target linkage compile-env)
  (cond ((self-evaluating? exp)
        (compile-self-evaluating exp target linkage))
    ((quoted? exp) 
      (compile-quoted exp target linkage))
    ((variable? exp)
        (compile-variable exp target linkage compile-env))
    ((assignment? exp)
        (compile-assignment exp target linkage compile-env))
    ((definition? exp)
        (compile-definition exp target linkage compile-env))
    ((if? exp) 
        (compile-if exp target linkage compile-env))
    ((lambda? exp) 
        (compile-lambda exp target linkage compile-env))
    ((begin? exp)
        (compile-sequence (begin-actions exp) target linkage compile-env))
    ((cond? exp)
        (compile (cond->if exp) target linkage))
    ((plus? exp)
        (compile-plus exp target linkage compile-env))
    ((equal-c? exp)
        (compile-equal exp target linkage compile-env))
    ((minus? exp)
        (compile-minus exp target linkage compile-env))
    ((mult? exp)
        (compile-mult exp target linkage compile-env))
    ((application? exp)
        (compile-application exp target linkage compile-env))
    (else (error "Unknown expression type: COMPILE" exp))))

(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define all-regs '(env proc val argl continue))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
        (make-instruction-sequence 
                  '(continue)
                  '()
                  '((goto (reg continue)))))
      ((eq? linkage 'next) (empty-instruction-sequence))
      (else
        (make-instruction-sequence 
              '()
              '()
              `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue) instruction-sequence (compile-linkage linkage)))

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
      (make-instruction-sequence '() (list target)
            `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
      (make-instruction-sequence '() (list target)
            `((assign ,target (const ,(text-of-quotation exp)))))))


(define (compile-variable exp target linkage compile-env)
  (let ((lexical (find-variable exp compile-env))) 
    (if (eq? 'not-found lexical)

        (end-with-linkage linkage
          (make-instruction-sequence '(env) (list target)
            `((assign ,target
            (op lookup-variable-value)
            (const ,exp)
            (reg env)))))

        (end-with-linkage linkage
          (make-instruction-sequence '(env) (list target)
            `((assign ,target
            (op lexical-address-lookup)
            (const ,lexical)
            (reg env)))))
      )

    )
  )

#| The recursive compilation has target val and linkage next so that the code will put
its result into val and continue with the code that is appended after it |#

(define (compile-assignment exp target linkage compile-env)
  (let ((var (assignment-variable exp))
    (lexical (find-variable exp compile-env)) 
    (get-value-code
        (compile (assignment-value exp) 'val 'next compile-env)))

    (if (eq? 'not-found lexical)

      (end-with-linkage linkage
        (preserving '(env) 
          get-value-code
          (make-instruction-sequence '(env val) (list target)
              `((perform (op set-variable-value!) (const ,var) (reg val) (reg env))
              (assign ,target (const ok))))))

      (end-with-linkage linkage
        (preserving '(env) 
          get-value-code
          (make-instruction-sequence '(env val) (list target)
              `((perform (op lexical-address-set!) (const ,lexical) (reg env) (reg val))
              (assign ,target (const ok))))))

      )
    )
)


(define (compile-definition exp target linkage compile-env)
  (let ((var (definition-variable exp))
      (get-value-code
        (compile (definition-value exp) 'val 'next compile-env)))
  (end-with-linkage linkage (preserving '(env)
      get-value-code
      (make-instruction-sequence '(env val) (list target)
        `((perform (op define-variable!) (const ,var) (reg val) (reg env))
          (assign ,target (const ok))))))))


; We can’t just use the labels true-branch, false-branch, and after-if
(define label-counter 0)
  (define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)
(define (make-label name)
  (string->symbol
    (string-append (symbol->string name)
          (number->string (new-label-number)))))

; The only slight complication is in how the linkage for the true branch should be
; handled. If the linkage for the conditional is return or a label, then the
; true and false branches will both use this same linkage. If the linkage
; is next, the true branch ends with a jump around the code for the false
; branch to the label at the end of the conditional.

(define (compile-if exp target linkage compile-env)
  (let ((t-branch (make-label 'true-branch))
    (f-branch (make-label 'false-branch))
    (after-if (make-label 'after-if)))

    (let ((consequent-linkage (if (eq? linkage 'next) after-if linkage)))
      
      (let ((p-code (compile (if-predicate exp)
          'val 'next compile-env))
        (c-code 
          (compile (if-consequent exp) target
          consequent-linkage compile-env))
        (a-code
          (compile (if-alternative exp) target linkage compile-env)))

        (preserving '(env continue) p-code
            (append-instruction-sequences
                (make-instruction-sequence '(val) '() `((test (op false?) (reg val))
                    (branch (label ,f-branch))))
                (parallel-instruction-sequences
                    (append-instruction-sequences t-branch c-code)
                    (append-instruction-sequences f-branch a-code))
                after-if))))))


(define (compile-sequence seq target linkage compile-env)
  (if (last-exp? seq)
    (compile (first-exp seq) target linkage compile-env)
    (preserving
      '(env continue)
      (compile (first-exp seq) target 'next compile-env)
      (compile-sequence (rest-exps seq) target linkage compile-env))))

(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))

(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))

(define (compiled-procedure-entry c-proc) (cadr c-proc))

(define (compiled-procedure-env c-proc) (caddr c-proc))

; When we compile the lambda expression, we also generate the code for
; the procedure body. Although the body won’t be executed at the time
; of procedure construction, it is convenient to insert it into the object
; code right after the code for the lambda. But if the linkage is next,
; we will need to skip around the code for the procedure body by using a
; linkage that jumps to a label that is inserted aer the body

(define (compile-lambda exp target linkage compile-env)
  (let ((proc-entry (make-label 'entry))
      (after-lambda (make-label 'after-lambda)))
    
    (let ((lambda-linkage (if (eq? linkage 'next) after-lambda linkage)))
      
      (append-instruction-sequences
        (tack-on-instruction-sequence
            (end-with-linkage lambda-linkage
              (make-instruction-sequence '(env) (list target)
                    `((assign ,target (op make-compiled-procedure) (label ,proc-entry) (reg env)))))
              (compile-lambda-body exp proc-entry compile-env))
        after-lambda))))

(define (compile-time-environment-add compile-env vars)
  (if (eq? '() vars)
    compile-env
    (cons vars compile-env)
    )
  )

(define (compile-sequence-defs seq target linkage compile-env defs-vars)
  (let ((result (map 
          (lambda (data)
              (list 'set! (car data) (cdr data)))
          defs-vars)))
    (compile-sequence (append result (filter-defines seq)) target linkage compile-env)
    )
  )

; This code begins with a label for the entry point. Next come instructions
;  that will cause the run-time evaluation environment to switch
; to the correct environment for evaluating the procedure body

(define (compile-lambda-body exp proc-entry compile-env)
  (let ((formals (lambda-parameters exp))
      (defs (scan-defines (lambda-body exp))))

      (if (eq? '() defs)

        (append-instruction-sequences
          (make-instruction-sequence '(env proc argl) '(env)
            `(,proc-entry
               (assign env (op compiled-procedure-env) (reg proc))
              (assign env (op extend-environment) (const ,formals) (reg argl) (reg env))))
          (compile-sequence (lambda-body exp) 'val 'return (compile-time-environment-add compile-env formals)))


        (let ((defs-vars (map car defs))
            (defs-holds (map (lambda (param) '*unassigned) defs)))

              (append-instruction-sequences
                (make-instruction-sequence '(env proc argl) '(env)
                  `(,proc-entry
                      (assign env (op compiled-procedure-env) (reg proc))
                      (assign env (op extend-environment) (const ,formals) (reg argl) (reg env))
                      (assign env (op extend-environment) (const ,defs-vars) (reg ,defs-holds) (reg env))
                      ))
                (compile-sequence-defs (lambda-body exp) 'val 'return (compile-time-environment-add compile-env formals) defs))
          )

        )
    )
)

; ⟨compilation of operator, target proc, linkage next⟩
; ⟨evaluate operands and construct argument list in argl⟩
; ⟨compilation of procedure call with given target and linkage⟩

; the code that performs the procedure call compile-procedure-call


(define (compile-application exp target linkage compile-env)
  (let ((proc-code (compile (operator exp) 'proc 'next compile-env))
      (operand-codes
        (map (lambda (operand) (compile operand 'val 'next compile-env))
           (operands exp))))

    (preserving '(env continue) proc-code
        (preserving '(proc continue) (construct-arglist operand-codes)
              (compile-procedure-call target linkage)))))


(define (compile-plus exp target linkage compile-env)
  (compile-primitive-operation '+ exp target linkage compile-env)
  )
(define (compile-minus exp target linkage compile-env)
  (compile-primitive-operation '- exp target linkage compile-env)
  )
(define (compile-equal exp target linkage compile-env)
  (compile-primitive-operation '= exp target linkage compile-env)
  )
(define (compile-mult exp target linkage compile-env)
  (compile-primitive-operation '* exp target linkage compile-env)
  )


(define (compile-primitive-operation op exp target linkage compile-env)
  (if (eq? (find-variable op compile-env) 'not-found)

    (let ((arguments (spread-arguments-multiple op (operands exp) compile-env)))
      (preserving 
        '(env continue)
        arguments
        (end-with-linkage
          linkage
           (make-instruction-sequence '(arg1 arg2) '(val arg1 arg2) `((assign ,target (op ,op) (reg arg1) (reg arg2)))))
        )
      )

    (compile-application exp target linkage compile-env)

    )
  )

(define (spread-arguments-multiple op operands-list compile-env)
  (define (rest-spread operands)
    (if (null? (cdr operands))
      (append-instruction-sequences
        (make-instruction-sequence '(arg1 arg2) '(val) `((assign val (op ,op) (reg arg1) (reg arg2))))
        (append-instruction-sequences
          (make-instruction-sequence '(val) '(arg1) `((assign arg1 (reg val))))
              (append-instruction-sequences 
                  (make-instruction-sequence '(arg1) '() `((save arg1)))
                    (append-instruction-sequences 
                      (compile (car operands) 'arg2 'next compile-env)
                      (make-instruction-sequence '(arg1) '() `((restore arg1)))
                        )
                      )))
      (append-instruction-sequences
        (preserving '(arg1) 
          (append-instruction-sequences
            (make-instruction-sequence '(arg1 arg2) '(val) `((assign val (op ,op) (reg arg2) (reg arg2))))
            (make-instruction-sequence '(val) '(arg1) `((assign arg1 (reg val))))
            )
          (compile (car operands) 'arg1 'next compile-env)
          )
        (rest-spread (cdr operands))
      )
      )

    )

  (define (first-spread operands)
    (cond ((null? operands)
        (append-instruction-sequences 
            (make-instruction-sequence '() '(arg1) '((assign arg1 (const ()))))
            (make-instruction-sequence '() '(arg2) '((assign arg2 (const ())))))
        )

        ((null? (cddr operands))
          (preserving '(arg1) (compile (car operands) 'arg1 'next compile-env)
            (compile (cadr operands) 'arg2 'next compile-env)))

        (else (append-instruction-sequences
                  (append-instruction-sequences
                    (compile (car operands) 'arg1 'next compile-env) 
                    (append-instruction-sequences 
                      (make-instruction-sequence '(arg1) '() `((save arg1)))
                      (append-instruction-sequences 
                        (compile (cadr operands) 'arg2 'next compile-env)
                        (make-instruction-sequence '(arg1) '() `((restore arg1)))
                        )
                      )
                    )
                  (rest-spread (cddr operands))
            )
        )

      )
    )

  (first-spread operands-list)
)

(define (spread-arguments operands compile-env)
  (if (null? operands)
      (append-instruction-sequences 
        (make-instruction-sequence '() '(arg1) '((assign arg1 (const ()))))
        (make-instruction-sequence '() '(arg2) '((assign arg2 (const ()))))
      )

      (let   ((first-arg (compile (car operands) 'arg1 'next compile-env))
          (second-arg (compile (cadr operands) 'arg2 'next compile-env)))
        
          (preserving '(arg1) first-arg second-arg))
  )
)



; implementation of "left to right" operand evaluation
(define (construct-arglist-left-to-right operand-codes)
  (let ((operand-codes operand-codes))
    (if (null? operand-codes)
      (make-instruction-sequence '() '(argl) '((assign argl (const ()))))
      (let 
        ((code-to-get-first-arg 
          (append-instruction-sequences 
            (car operand-codes)
            (make-instruction-sequence '(val) '(argl) '((assign argl (op list) (reg val)))))))

        (if (null? (cdr operand-codes))
          code-to-get-first-arg
          (preserving '(env)
              code-to-get-first-arg
              (code-to-get-rest-args-left-to-right
                (cdr operand-codes))))))))

(define (code-to-get-rest-args-left-to-right operand-codes)
  (let ((code-for-next-arg
       (preserving
        '(argl) 
        (car operand-codes)
        (make-instruction-sequence '(val argl) '(argl)
      '((assign argl (op adjoin-arg) (reg val) (reg argl)))))))

    (if (null? (cdr operand-codes))
      code-for-next-arg
      (preserving '(env) 
        code-for-next-arg
        (code-to-get-rest-args (cdr operand-codes))))))

; Since we cons the arguments onto argl in sequence, we must
; start with the last argument and end with the ﬁrst

; argl and env must be preserved around each operand evaluation
; The construct-arglist procedure
; takes as arguments the code that evaluates the individual operands

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
      (make-instruction-sequence '() '(argl) '((assign argl (const ()))))
      (let 
        ((code-to-get-last-arg 
          (append-instruction-sequences 
            (car operand-codes)
            (make-instruction-sequence '(val) '(argl) '((assign argl (op list) (reg val)))))))

        (if (null? (cdr operand-codes))
          code-to-get-last-arg
          (preserving '(env)
              code-to-get-last-arg
              (code-to-get-rest-args
              (cdr operand-codes))))))))


(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
       (preserving
        '(argl) 
        (car operand-codes)
        (make-instruction-sequence '(val argl) '(argl)
      '((assign argl (op cons) (reg val) (reg argl)))))))

    (if (null? (cdr operand-codes))
      code-for-next-arg
      (preserving '(env) 
        code-for-next-arg
        (code-to-get-rest-args (cdr operand-codes))))))


; (test (op primitive-procedure?) (reg proc))
;   (branch (label primitive-branch))
; compiled-branch
;   ⟨code to apply compiled procedure with given target
;   and appropriate linkage⟩
; primitive-branch
;   (assign ⟨target⟩ (op apply-primitive-procedure) (reg proc) (reg argl))
;   ⟨linkage⟩
; after-call

; Observe that the compiled branch must skip around the primitive branch.
; erefore, if the linkage for the original procedure call was next, the
; compound branch must use a linkage that jumps to a label that is in-
; serted aer the primitive branch.


(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
      (compiled-branch (make-label 'compiled-branch))
      (after-call (make-label 'after-call)))

      (let ((compiled-linkage (if (eq? linkage 'next) after-call linkage)))
        
        (append-instruction-sequences 
          (make-instruction-sequence 
            '(proc) '() 
            `((test (op primitive-procedure?) (reg proc))
             (branch (label ,primitive-branch))))
          (parallel-instruction-sequences
            (append-instruction-sequences
             compiled-branch
            (compile-proc-appl target compiled-linkage))
            (append-instruction-sequences
              primitive-branch
              (end-with-linkage linkage
                (make-instruction-sequence '(proc argl)
                  (list target)
                  `((assign ,target (op apply-primitive-procedure) (reg proc) (reg argl)))))))
          after-call))))


; (assign continue (label proc-return))
; (assign val (op compiled-procedure-entry) (reg proc)) -- get procedure label
; (goto (reg val))
;proc-return
; (assign ⟨target⟩ (reg val))
; (goto (label ⟨linkage⟩))

; compile-proc-appl generates the above procedure-application code
; by considering four cases, depending on whether the target for the call
; is val and whether the linkage is return.

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
      (make-instruction-sequence '(proc) all-regs
        `((assign continue (label ,linkage))
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val)))))
    ((and (not (eq? target 'val))
        (not (eq? linkage 'return)))
        (let ((proc-return (make-label 'proc-return)))
            (make-instruction-sequence '(proc) all-regs
              `((assign continue (label ,proc-return))
               (assign val (op compiled-procedure-entry) (reg proc))
               (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
    ((and (eq? target 'val) (eq? linkage 'return))
        (make-instruction-sequence
          '(proc continue)
          all-regs
          '((assign val (op compiled-procedure-entry)(reg proc))
          (goto (reg val)))))
    ((and (not (eq? target 'val))
        (eq? linkage 'return))
          (error "return linkage, target not val: COMPILE" target))))


(define (registers-needed s)
(if (symbol? s) '() (car s)))
(define (registers-modified s)
(if (symbol? s) '() (cadr s)))
(define (statements s)
(if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
(memq reg (registers-needed seq)))
(define (modifies-register? seq reg)
(memq reg (registers-modified seq)))

(define (list-union s1 s2)
(cond ((null? s1) s2)
    ((memq (car s1) s2) (list-union (cdr s1) s2))
    (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
      ((memq (car s1) s2) (list-difference (cdr s1) s2))
      (else (cons (car s1) (list-difference (cdr s1) s2)))))


; This takes two instruction sequences seq1 and seq2 and returns the 
; instruction sequence whose statements are the statements of seq1 fol-
; lowed by the statements of seq2, whose modiﬁed registers are those
; registers that are modiﬁed by either seq1 or seq2, and whose needed
; registers are the registers needed by seq1 together with those registers
; needed by seq2 that are not modiﬁed by seq1.

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
      (list-union
        (registers-needed seq1)
        (list-difference (registers-needed seq2)
                (registers-modified seq1)))
      (list-union (registers-modified seq1)
            (registers-modified seq2))
      (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
      (empty-instruction-sequence)
      (append-2-sequences 
        (car seqs)
        (append-seq-list (cdr seqs)))
    )
  )

(append-seq-list seqs))


; It returns an instruction sequence
; whose statements are the statements of seq1 followed by the statements
; of seq2, with appropriate save and restore instructions around seq1
; to protect the registers in regs that are modiﬁed by seq1 but needed by
; seq2.
; The following procedure implements this strategy recursively, walking
; down the list of registers to be preserved

(define (preserving regs seq1 seq2)
  (if (null? regs)
    (append-instruction-sequences seq1 seq2)
    (let ((first-reg (car regs)))
      (if (and (needs-register? seq2 first-reg) (modifies-register? seq1 first-reg))
        (preserving (cdr regs)
          (make-instruction-sequence
            (list-union (list first-reg)
                  (registers-needed seq1))
          (list-difference (registers-modified seq1)
                  (list first-reg))
          (append-triple  `((save ,first-reg))
               (statements seq1)
              `((restore ,first-reg))))
          seq2)

        (preserving (cdr regs) seq1 seq2)))))

; Because the procedure body is not “in line” to be executed as part of the
; combined sequence, its register use has no impact on the register use
; of the sequence in which it is embedded. We thus ignore the procedure
; body’s sets of needed and modiﬁed registers when we tack it onto the
; other sequence.

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
    (registers-needed seq)
    (registers-modified seq)
    (append (statements seq) (statements body-seq))))

; The two branches will never be executed sequentially;
; for any particular evaluation of the test, one branch or the other will be
; entered. Because of this, the registers needed by the second branch are
; still needed by the combined sequence

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
    (list-union (registers-needed seq1)
          (registers-needed seq2))
    (list-union (registers-modified seq1)
          (registers-modified seq2))
    (append (statements seq1) (statements seq2))))




#| -------------------------------------------- Compiler ----------------------------------------------- |#





(define the-global-environment (setup-environment))
(define (get-global-environment) the-global-environment)

(define (last-operand? ops) (null? (cdr ops)))
(define (first-exp seq) (car seq))

(define (text-of-quotation-without-env exp) (cadr exp))
(define (last-exp? seq) (null? (cdr seq)))

(define (make-compiled-procedure entry env)
(list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
(tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))

(define eceval-operations
  (list (list 'self-evaluating? self-evaluating?)
   (list 'prompt-for-input prompt-for-input)
   (list 'announce-output announce-output)
   (list 'user-print user-print)
   (list 'variable? variable?)
   (list 'lexical-address-lookup lexical-address-lookup)
   (list 'make-compiled-procedure make-compiled-procedure)
   (list 'compiled-procedure? compiled-procedure?)
   (list 'compiled-procedure-entry compiled-procedure-entry)
   (list 'compiled-procedure-env compiled-procedure-env)
   (list 'last-exp? last-exp?)
   (list 'cons cons)
   (list 'print-stack-statistics (lambda () (stack-statistic eceval)))
   (list 'quoted? quoted?)
   (list 'assignment? assignment?)
   (list 'definition? definition?)
   (list 'last-operand? last-operand?)
   (list 'delay-it delay-it)
   (list 'if? if?)
   (list 'first-exp first-exp)
   (list 'get-global-environment get-global-environment)
   (list 'cond? cond?)
   (list 'lambda? lambda?)
   (list 'let? let?)
   (list 'and? and?)
   (list 'or? or?)
   (list 'read read)
   (list 'begin? begin?)
   (list 'application? application?)
   (list 'thunk? thunk?)
   (list 'thunk-env thunk-env)
   (list 'thunk-exp thunk-exp)
   (list 'lookup-variable-value lookup-variable-value)
   (list 'get-error-text get-error-text)
   (list 'is-error-return? is-error-return)
   (list 'text-of-quotation text-of-quotation-without-env)
   (list 'rest-operands rest-operands)
   (list 'no-operands? no-operands?)
   (list 'first-operand first-operand)
   (list 'true? true?)
   (list 'let->combination let->combination)
   (list 'eq? eq?)
   (list 'false? false?)
   (list '= =)
   (list '+ +)
   (list '- -)
   (list '* *)
   (list 'list list)
   (list 'lambda-parameters lambda-parameters)
   (list 'lambda-body lambda-body)
   (list 'make-procedure make-procedure)
   (list 'operator operator)
   (list 'operands operands)
   (list 'empty-arglist empty-arglist)
   (list 'compound-procedure? compound-procedure?)
   (list 'adjoin-arg adjoin-arg)
   (list 'primitive-procedure? primitive-procedure?)
   (list 'apply-primitive-procedure apply-primitive-procedure)
   (list 'procedure-parameters procedure-parameters)
   (list 'procedure-environment procedure-environment)
   (list 'extend-environment extend-environment)
   (list 'procedure-body procedure-body)
   (list 'begin-actions begin-actions)
   (list 'no-more-exps? no-more-exps?)
   (list 'rest-exps rest-exps)
   (list 'if-predicate if-predicate)
   (list 'if-alternative if-alternative)
   (list 'if-consequent if-consequent)
   (list 'assignment-variable assignment-variable)
   (list 'assignment-value assignment-value)
   (list 'set-variable-value! set-variable-value!)
   (list 'definition-variable definition-variable)
   (list 'definition-value definition-value)
   (list 'define-variable! define-variable!)
   ))

(define eceval
  (make-machine
        eceval-operations
      '(
   (branch (label external-entry))
read-eval-print-loop
  (perform (op initialize-stack))
  (perform
  (op prompt-for-input) (const ";;EC-Eval input:"))
  (assign exp (op read))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (label eval-dispatch))
print-result
  (perform (op print-stack-statistics))
  (perform (op announce-output) (const ";;EC-Eval value:"))
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))

unknown-expression-type
  (assign val (const unknown-expression-type-error))
  (goto (label signal-error))
unknown-procedure-type
  (restore continue)
  (assign val (const unknown-procedure-type-error))
  (goto (label signal-error))
signal-error
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))


eval-dispatch
  (test (op self-evaluating?) (reg exp))
  (branch (label ev-self-eval))
  (test (op variable?) (reg exp))
  (branch (label ev-variable))
  (test (op quoted?) (reg exp))
  (branch (label ev-quoted))
  (test (op assignment?) (reg exp))
  (branch (label ev-assignment))
  (test (op definition?) (reg exp))
  (branch (label ev-definition))
  (test (op if?) (reg exp))
  (branch (label ev-if))
  (test (op cond?) (reg exp))
  (branch (label ev-cond))
  (test (op lambda?) (reg exp))
  (branch (label ev-lambda))
  (test (op let?) (reg exp)) 
  (branch (label ev-let))
  (test (op and?) (reg exp)) 
  (branch (label ev-and))
  (test (op or?) (reg exp)) 
  (branch (label ev-or)) 
  (test (op begin?) (reg exp))
  (branch (label ev-begin))
  (test (op application?) (reg exp))
  (branch (label ev-application))
  (goto (label unknown-expression-type))

is-error-return
 (assign val (op get-error-text) (reg val))
 (goto (label signal-error))

ev-self-eval
  (assign val (reg exp))
  (goto (reg continue))
ev-variable
  (assign val (op lookup-variable-value) (reg exp) (reg env))
  (test (op is-error-return?) (reg val))
  (branch (label is-error-return))
  (goto (reg continue))
ev-quoted
  (assign val (op text-of-quotation) (reg exp))
  (goto (reg continue))

ev-cond
  (save continue)
  (save unev)
  (save exp)
  (assign unev (op rest-operands) (reg exp))

ev-cond-loop
  (test (op no-operands?) (reg unev))
  (branch (label ev-cond-after-sequence))
  (assign exp (op first-operand) (reg unev))
  (assign exp (op first-operand) (reg exp))
  (save unev)
  (test (op eq?) (reg exp) (const else))
  (branch (label ev-cond-execute))
  (assign continue (label ev-cond-loop-val))
  (goto (label eval-dispatch))

ev-cond-loop-val
  (test (op true?) (reg val))
  (goto (label ev-cond-execute))
  (restore unev)
  (assign unev (op rest-operands) (reg unev))
  (goto (label ev-cond-loop))

ev-cond-execute
  (restore unev)
  (assign unev (op rest-operands) (reg unev))
  (assign continue (label ev-cond-after-sequence))
  (goto (label ev-sequence))

ev-cond-after-sequence
  (restore continue)
  (restore unev)
  (restore exp)
  (goto (reg continue))

ev-let
  (save continue)
  (save env)
  (assign exp (op let->combination) (reg exp))
  (assign continue (label ev-let-lambda))
  (goto (label eval-dispatch))

ev-let-lambda
  (assign exp (reg val))
  (assign continue (label ev-let-end))
  (goto (label ev-application))

ev-let-end
  (restore continue)
  (restore env)
  (goto (reg continue))

ev-or
  (save continue)
  (save env)
  (assign unev (op rest-operands) (reg exp))
  (goto (label ev-or-loop))

ev-or-loop
  (save unev)
  (assign exp (op first-operand) (reg unev))
  (assign continue (label ev-or-loop-val))
  (goto (label eval-dispatch))

ev-or-loop-val
  (restore unev)
  (assign unev (op rest-operands) (reg unev))
  (test (op true?) (reg val))
  (branch (label ev-or-loop-end))
  (test (op no-operands?) (reg unev))
  (branch (label ev-or-loop-end))
  (goto (label ev-or-loop))

ev-or-loop-end
  (restore continue)
  (restore env)
  (goto (reg continue))

ev-and
  (save continue)
  (save env)
  (assign unev (op rest-operands) (reg exp))
  (assign val (const true))
  (goto (label ev-and-loop))

ev-and-loop
  (save unev)
  (save val)
  (assign exp (op first-operand) (reg unev))
  (assign continue (label ev-and-loop-val))
  (goto (label eval-dispatch))

ev-and-loop-val
  (restore unev)
  (assign argl (reg val))
  (restore val)
  (assign val (op eq?) (reg argl) (reg val))
  (assign unev (op rest-operands) (reg unev))
  (test (op no-operands?) (reg unev))
  (branch (label ev-and-loop-end))
  (test (op false?) (reg val))
  (branch (label ev-and-loop-end))
  (goto (label ev-and-loop))

ev-and-loop-end
  (restore continue)
  (restore env)
  (goto (reg continue))

ev-lambda
  (assign unev (op lambda-parameters) (reg exp))
  (assign exp (op lambda-body) (reg exp))
  (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
  (goto (reg continue))

ev-application
  (save continue)
  (save env)
  (assign unev (op operands) (reg exp))
  (save unev)
  (assign exp (op operator) (reg exp))
  (assign continue (label ev-appl-did-operator))
  (goto (label eval-dispatch))

ev-appl-did-operator
  (restore unev)
  (restore env)
  (assign argl (op empty-arglist))
  (assign proc (reg val))
  (test (op no-operands?) (reg unev))
  (branch (label apply-dispatch))
  (save proc)
ev-appl-operand-loop
  (save argl)
  (assign exp (op first-operand) (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (label ev-appl-last-arg))
  (save env)
  (save unev)
  (assign continue (label ev-appl-accumulate-arg))
  (goto (label eval-dispatch))

ev-appl-accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (assign unev (op rest-operands) (reg unev))
  (goto (label ev-appl-operand-loop))

ev-appl-last-arg
  (assign continue (label ev-appl-accum-last-arg))
  (goto (label eval-dispatch))
ev-appl-accum-last-arg
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (restore proc)
  (goto (label apply-dispatch))

apply-dispatch
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-apply))
  (test (op compound-procedure?) (reg proc))
  (branch (label compound-apply))
  (test (op compiled-procedure?) (reg proc))
  (branch (label compiled-apply))
  (goto (label unknown-procedure-type))

  compiled-apply
  (restore continue)
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))

primitive-apply
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (restore continue)
  (goto (reg continue))

compound-apply
  (assign unev (op procedure-parameters) (reg proc))
  (assign env (op procedure-environment) (reg proc))
  (assign env (op extend-environment)
  (reg unev) (reg argl) (reg env))
  (assign val (reg env))
  (test (op is-error-return?) (reg val))
  (branch (label is-error-return))
  (assign unev (op procedure-body) (reg proc))
  (goto (label ev-sequence))

ev-begin
  (assign unev (op begin-actions) (reg exp))
  (save continue)
  (goto (label ev-sequence))

ev-sequence
  (assign exp (op first-exp) (reg unev))
  (test (op last-exp?) (reg unev))
  (branch (label ev-sequence-last-exp))
  (save unev)
  (save env)
  (assign continue (label ev-sequence-continue))
  (goto (label eval-dispatch))
ev-sequence-continue
  (restore env)
  (restore unev)
  (assign unev (op rest-exps) (reg unev))
  (goto (label ev-sequence))
ev-sequence-last-exp
  (restore continue)
  (goto (label eval-dispatch))

ev-if
  (save exp)
  (save env)
  (save continue)
  (assign continue (label ev-if-decide))
  (assign exp (op if-predicate) (reg exp))
  (goto (label eval-dispatch))
ev-if-decide
  (restore continue)
  (restore env)
  (restore exp)
  (test (op true?) (reg val))
  (branch (label ev-if-consequent))
ev-if-alternative
  (assign exp (op if-alternative) (reg exp))
  (goto (label eval-dispatch))
ev-if-consequent
  (assign exp (op if-consequent) (reg exp))
  (goto (label eval-dispatch))

ev-assignment
  (assign unev (op assignment-variable) (reg exp))
  (save unev)
  (assign exp (op assignment-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-assignment-1))
  (goto (label eval-dispatch))
ev-assignment-1
  (restore continue)
  (restore env)
  (restore unev)
  (assign val (op set-variable-value!) (reg unev) (reg val) (reg env))
  (test (op is-error-return?) (reg val))
  (branch (label is-error-return))
  (assign val (const ok))
  (goto (reg continue))

ev-definition
  (assign unev (op definition-variable) (reg exp))
  (save unev)
  ; save variable for later
  (assign exp (op definition-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-definition-1))
  (goto (label eval-dispatch))
  ; evaluate the deﬁnition value
ev-definition-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform (op define-variable!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))
external-entry
  (perform (op initialize-stack))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (reg val)))))

(define (compile-and-go expression)
(let ((instructions
(assemble
(statements
(compile expression 'val 'return '()))
eceval)))
(newline)(display "compile-and-go - ")(display instructions)(newline)
(set! the-global-environment (setup-environment))
(set-register-contents! eceval 'val instructions)
(set-register-contents! eceval 'flag true)
(start eceval)))



#| (start eceval) |#

(compile-and-go
'(define (factorial n)
(if (= n 1)
1
(* (factorial (- n 1)) n))))

