;;; -*- lexical-binding: t -*-

;; sicp scheme evaluator (first version) ported to emacs lisp

(setq sicp/apply-in-underlying-elisp #'apply)

(defun sicp/eval (exp env)
  (cond ((sicp/self-evaluatingp exp) exp)
        ((sicp/variablep exp) (sicp/lookup-variable-value exp env))
        ((sicp/quotedp exp) (sicp/text-of-quotation exp))
        ((sicp/assignmentp exp) (sicp/eval-assignment exp env))
        ((sicp/definitionp exp) (sicp/eval-definition exp env))
        ((sicp/ifp exp) (sicp/eval-if exp env))
        ((sicp/lambdap exp)
         (sicp/make-procedure (sicp/lambda-parameters exp)
                              (sicp/lambda-body exp)
                              env))
        ((sicp/beginp exp)
         (sicp/eval-sequence (sicp/begin-actions exp) env))
        ((sicp/condp exp) (sicp/eval (sicp/cond->if exp) env))
        ((sicp/letp exp) (sicp/eval (sicp/let->lambda exp) env))
        ((sicp/applicationp exp)
         (sicp/apply (sicp/eval (sicp/operator exp) env)
                     (sicp/list-of-values (sicp/operands exp) env)))
        (t (error "Unknown expression type -- EVAL" exp))))

(defun sicp/apply (procedure arguments)
  (cond ((sicp/primitive-procedurep procedure)
         (sicp/apply-primitive-procedure procedure arguments))
        ((sicp/compound-procedurep procedure)
         (sicp/eval-sequence
          (sicp/procedure-body procedure)
          (sicp/extend-environment
           (sicp/procedure-parameters procedure)
           arguments
           (sicp/procedure-environment procedure))))
        (t
         (error
          "Unknown procedure type -- APPLY" procedure))))

(defun sicp/list-of-values (exps env)
  (if (sicp/no-operandsp exps)
      '()
    (cons (sicp/eval (sicp/first-operand exps) env)
          (sicp/list-of-values (sicp/rest-operands exps) env))))

(defun sicp/eval-if (exp env)
  (if (sicp/truep (sicp/eval (sicp/if-predicate exp) env))
      (sicp/eval (sicp/if-consequent exp) env)
      (sicp/eval (sicp/if-alternative exp) env)))

(defun sicp/eval-sequence (exps env)
  (cond ((sicp/last-expp exps) (sicp/eval (sicp/first-exp exps) env))
        (t (sicp/eval (sicp/first-exp exps) env)
              (sicp/eval-sequence (sicp/rest-exps exps) env))))

(defun sicp/eval-assignment (exp env)
  (sicp/set-variable-value (sicp/assignment-variable exp)
                            (sicp/eval (sicp/assignment-value exp) env)
                            env)
  'ok)

(defun sicp/pairp (exp)
  (not (atom exp)))

(defun sicp/eval-if (exp env)
  (if (sicp/truep (sicp/eval (sicp/if-predicate exp) env))
      (sicp/eval (sicp/if-consequent exp) env)
      (sicp/eval (sicp/if-alternative exp) env)))

(defun sicp/eval-sequence (exps env)
  (cond ((sicp/last-expp exps) (sicp/eval (sicp/first-exp exps) env))
        (t (sicp/eval (sicp/first-exp exps) env)
              (sicp/eval-sequence (sicp/rest-exps exps) env))))

(defun sicp/eval-assignment (exp env)
  (sicp/set-variable-value (sicp/assignment-variable exp)
                            (sicp/eval (sicp/assignment-value exp) env)
                            env)
  'ok)

(defun sicp/eval-definition (exp env)
  (sicp/define-variable (sicp/definition-variable exp)
                        (sicp/eval (sicp/definition-value exp) env)
                        env)
  'ok)

(defun sicp/self-evaluatingp (exp)
  (cond ((numberp exp) t)
        ((stringp exp) t)
        (t nil)))

(defun sicp/variablep (exp) (symbolp exp))

(defun sicp/quotedp (exp)
  (sicp/tagged-listp exp 'quote))

(defun sicp/text-of-quotation (exp) (cadr exp))

(defun sicp/tagged-listp (exp tag)
  (if (sicp/pairp exp)
      (eq (car exp) tag)
    nil))

(defun sicp/assignmentp (exp)
  (sicp/tagged-listp exp 'set!))

(defun sicp/assignment-variable (exp) (cadr exp))

(defun sicp/assignment-value (exp) (caddr exp))

(defun sicp/definitionp (exp)
  (sicp/tagged-listp exp 'define))

(defun sicp/definition-variable (exp)
  (if (symbolp (cadr exp))
      (cadr exp)
    (caadr exp)))

(defun sicp/definition-value (exp)
  (if (symbolp (cadr exp))
      (caddr exp)
    (sicp/make-lambda (cdadr exp)   ; formal parameters
                      (cddr exp)))) ; body

(defun sicp/lambdap (exp) (sicp/tagged-listp exp 'lambda))

(defun sicp/lambda-parameters (exp) (cadr exp))

(defun sicp/lambda-body (exp) (cddr exp))

(defun sicp/make-lambda (parameters body)
  (cons 'lambda (cons parameters body)))

(defun sicp/ifp (exp) (sicp/tagged-listp exp 'if))

(defun sicp/if-predicate (exp) (cadr exp))

(defun sicp/if-consequent (exp) (caddr exp))

(defun sicp/if-alternative (exp)
  (if (not (null (cdddr exp)))
      (cadddr exp)
      'false))

(defun sicp/make-if (predicate consequent alternative)
  (list 'if predicate consequent alternative))

(defun sicp/beginp (exp) (sicp/tagged-listp exp 'begin))

(defun sicp/begin-actions (exp) (cdr exp))

(defun sicp/last-expp (seq) (null (cdr seq)))

(defun sicp/first-exp (seq) (car seq))

(defun sicp/rest-exps (seq) (cdr seq))

(defun sicp/sequence->exp (seq)
  (cond ((null seq) seq)
        ((sicp/last-expp seq) (sicp/first-exp seq))
        (t (sicp/make-begin seq))))

(defun sicp/make-begin (seq) (cons 'begin seq))

(defun sicp/applicationp (exp) (sicp/pairp exp))

(defun sicp/operator (exp) (car exp))

(defun sicp/operands (exp) (cdr exp))

(defun sicp/no-operandsp (ops) (null ops))

(defun sicp/first-operand (ops) (car ops))

(defun sicp/rest-operands (ops) (cdr ops))

(defun sicp/condp (exp) (sicp/tagged-listp exp 'cond))

(defun sicp/cond-clauses (exp) (cdr exp))

(defun sicp/cond-else-clausep (clause)
  (eq (sicp/cond-predicate clause) 'else))

(defun sicp/cond-predicate (clause) (car clause))

(defun sicp/cond-actions (clause) (cdr clause))

(defun sicp/cond->if (exp)
  (sicp/expand-clauses (sicp/cond-clauses exp)))

(defun sicp/expand-clauses (clauses)
  (if (null clauses)
      'false                          ; no `else' clause
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (sicp/cond-else-clausep first)
          (if (null rest)
              (sicp/sequence->exp (sicp/cond-actions first))
            (error "ELSE clause isn't last -- COND->IF"
                   clauses))
        (sicp/make-if (sicp/cond-predicate first)
                      (sicp/sequence->exp (sicp/cond-actions first))
                      (sicp/expand-clauses rest))))))

(defun sicp/letp (exp) (sicp/tagged-listp exp 'let))

(defun sicp/let-vars (exp)
  (map #'car (cadr exp)))

(defun sicp/let-exps (exp)
  (map #'cadr (cadr exp)))

(defun sicp/let-body (exp)
  (cddr exp))

(defun sicp/let->lambda (exp)
  (cons (cons 'lambda
              (cons (sicp/let-vars exp)
                    (sicp/let-body exp)))
        (sicp/let-exps exp)))

(defun sicp/truep (x)
  (not (eq x nil)))

(defun sicp/falsep (x)
  (eq x nil))

(defun sicp/make-procedure (parameters body env)
  (list 'procedure parameters body env))

(defun sicp/compound-procedurep (p)
  (sicp/tagged-listp p 'procedure))

(defun sicp/procedure-parameters (p) (cadr p))

(defun sicp/procedure-body (p) (caddr p))

(defun sicp/procedure-environment (p) (cadddr p))

(defun sicp/enclosing-environment (env) (cdr env))

(defun sicp/first-frame (env) (car env))

(setq sicp/the-empty-environment '())

(defun sicp/make-frame (variables values)
  (cons variables values))

(defun sicp/frame-variables (frame) (car frame))

(defun sicp/frame-values (frame) (cdr frame))

(defun sicp/add-binding-to-frame (var val frame)
  (setcar frame (cons var (car frame)))
  (setcdr frame (cons val (cdr frame))))

(defun sicp/extend-environment (vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (sicp/make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(defun sicp/lookup-variable-value (var env)
  (cl-labels ((sicp/env-loop (env)
                (if (eq env sicp/the-empty-environment)
                    (error "Unbound variable" var)
                  (let ((frame (sicp/first-frame env)))
                    (sicp/scan (sicp/frame-variables frame)
                               (sicp/frame-values frame)))))
              (sicp/scan (vars vals)
                (cond ((null vars)
                       (sicp/env-loop (sicp/enclosing-environment env)))
                      ((eq var (car vars))
                       (car vals))
                      (t (sicp/scan (cdr vars) (cdr vals))))))
    (sicp/env-loop env)))

(defun sicp/set-variable-value (var val env)
  (cl-labels ((sicp/env-loop (env) ;; define in the global scope twice? should I use setq?
                (if (eq env sicp/the-empty-environment)
                    (error "Unbound variable -- SET" var)
                  (let ((frame (sicp/first-frame env)))
                    (sicp/scan (sicp/frame-variables frame)
                               (sicp/frame-values frame)))))
              (sicp/scan (vars vals)
                (cond ((null vars)
                       (sicp/env-loop (sicp/enclosing-environment env)))
                      ((eq var (car vars))
                       (setcar vals val))
                      (t (sicp/scan (cdr vars) (cdr vals))))))
    (sicp/env-loop env)))

(defun sicp/define-variable (var val env)
  (let ((frame (sicp/first-frame env)))
    (cl-labels ((sicp/scan (vars vals)
                  (cond ((null vars)
                         (sicp/add-binding-to-frame var val frame))
                        ((eq var (car vars))
                         (setcar vals val))
                        (t (sicp/scan (cdr vars) (cdr vals))))))
      (sicp/scan (sicp/frame-variables frame)
                 (sicp/frame-values frame)))))

(setq sicp/primitive-procedures
      (list (list 'car #'car)
            (list 'cdr #'cdr)
            (list 'cons #'cons)
            (list 'null? #'null)
            (list '+ #'+)
            (list '- #'-)
            (list '* #'*)
            (list '/ #'/)))

(defun map (op items)
  (if (null items)
      nil
    (cons (funcall op (car items)) (map op (cdr items)))))

(defun sicp/primitive-procedure-names ()
  (map #'car sicp/primitive-procedures))

(defun sicp/primitive-procedure-objects ()
  (map #'(lambda (proc) (list 'primitive (cadr proc)))
       sicp/primitive-procedures))

(defun setup-environment ()
  (let ((initial-env
         (sicp/extend-environment (sicp/primitive-procedure-names)
                                  (sicp/primitive-procedure-objects)
                                  sicp/the-empty-environment)))
    (sicp/define-variable 'true t initial-env)
    (sicp/define-variable 'false nil initial-env)
    initial-env))

(defun sicp/primitive-procedurep (proc)
  (sicp/tagged-listp proc 'primitive))

(defun sicp/primitive-implementation (proc) (cadr proc))

(defun sicp/apply-primitive-procedure (proc args)
  (funcall sicp/apply-in-underlying-elisp
           (sicp/primitive-implementation proc) args))

(setq sicp/the-global-environment (setup-environment))

;;;; USAGE
;; To evaluate a single expression in the shell:
;; $ emacs --script evaluator.el --eval="(print (sicp/eval '(+ 1 1) sicp/the-global-environment) #'external-debugging-output)"
;;
;; For a repl in the shell:
;; $ emacs --load evaluator.el --batch --eval "(while t (print (prin1-to-string (sicp/eval (read) sicp/the-global-environment))))"
;;
;; (cf. https://www.reddit.com/r/emacs/comments/pm33pe/emacs_as_repl/
;;
;;
;; For a repl in Emacs, evaluate the evalutor code above and then the
;; following. (Results will be printed in the *Messages* buffer.)
;;
;; (defun sicp/driver-loop ()
;;   (let ((input (read)))
;;     (let ((output (sicp/eval input sicp/the-global-environment)))
;;       (sicp/user-print output)))
;;   (sicp/driver-loop))
;;
;; (defun sicp/user-print (object)
;;   (if (sicp/compound-procedurep object)
;;       (message (prin1-to-string (list 'compound-procedure
;;                                       (procedure-parameters object)
;;                                       (procedure-body object)
;;                                       '<procedure-env>)))
;;     (message (prin1-to-string object))))
;;
;; (sicp/driver-loop)
