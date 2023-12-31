read-eval-print-loop
	(perform (op initialize-stack))
	(perform
	(op prompt-for-input) (const ";;EC-Eval input:"))
	(assign exp (op read))
	(assign env (op get-global-environment))
	(assign continue (label print-result))
	(goto (label eval-dispatch))
print-result
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

actual-value
	(save continue)
	(assign continue (label actual-value-after-eval))
	(goto (label eval-dispatch))

actual-value-after-eval
	(assign continue (label actual-value-after-force))
	(assign exp (reg val))
	(goto (label force-it))

actual-value-after-force
	(restore continue)
	(goto (reg continue))

force-it
	(save continue)
	(test (op thunk?) (reg exp))
	(branch (label force-it-thunk))
	(restore continue)
	(assign val (reg exp))
	(goto (reg continue))

force-it-thunk
	(assign env (op thunk-env) (reg exp))
	(assign exp (op thunk-exp) (reg exp))
	(assign continue (label force-it-thunk-after))
	(goto (label actual-value))

force-it-thunk-after
	(restore continue)
	(goto (reg continue))

ev-self-eval
	(assign val (reg exp))
	(goto (reg continue))
ev-variable
	(assign val (op lookup-variable-value) (reg exp) (reg env))
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
	(save exp)
	(save unev)
	(assign exp (op operator) (reg exp))
	(assign continue (label ev-application-after-operator-force))
	(goto (label actual-value))


ev-application-after-operator-force
	(restore continue)
	(restore env)
	(restore exp)
	(restore unev)
	(assign unev (op operands) (reg exp))
	(save unev)
	(goto (label ev-appl-did-operator))

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
	(test (op compound-procedure?) (reg proc))
	(branch (label ev-appl-operand-loop-delay))
	(assign continue (label ev-appl-accumulate-arg))
	(goto (label actual-value))

ev-appl-operand-loop-delay
	(assign val (op delay-it) (reg exp) (reg env))
	(goto (label ev-appl-accumulate-arg))

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
	(goto (label unknown-procedure-type))

primitive-apply
	(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
	(restore continue)
	(goto (reg continue))

compound-apply
	(assign unev (op procedure-parameters) (reg proc))
	(assign env (op procedure-environment) (reg proc))
	(assign env (op extend-environment)
	(reg unev) (reg argl) (reg env))
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
	(goto (label actual-value))
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
	(perform (op set-variable-value!) (reg unev) (reg val) (reg env))
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
