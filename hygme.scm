;; Hygme. A simple language with a hygienic macro expander.

;; Abs -> (fn var body)
;; Var -> <symbol>
;; Const -> <string> | <symbol> | <number> | <boolean> ...
;; Cond -> (if <condition> <consequent> <alternate>)
;; Assm -> (set! var value)
;; App -> (<expr> <expr> ...)

(define *toplevel-env* 
  `((cons . ,cons)
    (car . ,car)
    (cdr . ,cdr)
    (list . ,list)
    (not . ,not)
    (+ . ,+)
    (- . ,-)
    (* . ,*)
    (/ . ,/)))

(define *toplevel-macenv* '())

(define (coretok? x)
  (memq x '(fn if quote set! macro)))

(define (mactok? x)
  (assq x *toplevel-macenv*))

(define (lookup-env var . envs)
  (let loop ((envs envs))
    (cond
     ((null? envs) '())
     ((assq var (car envs)) => (lambda (x) x))
     (else
      (loop (cdr envs))))))

(define selfev? symbol?)

(define (hygme-closure? e)
  (and (pair? e)
       (pair? (car e))
       (eq? (caar e) 'HYGME-CLOSURE)
       (= (length e) 3)))

(define (close-over abs env)
  (list (list 'HYGME-CLOSURE) abs env))

(define (cloenv e)
  (if (hygme-closure? e)
      (caddr e)
      (error "not a closure")))

(define (clocode e)
  (if (hygme-closure? e)
      (cadr e)
      (error "not a closure")))

(define (const? e)
  (or (number? e) 
      (string? e)
      (boolean? e)
      (hygme-closure? e)
      (procedure? e)))

(define (quoted? e)
  (and (pair? e)
       (eq? (car e) 'quote)
       (pair? (cdr e))))

(define (if? e)
  (and (pair? e)
       (eq? (car e) 'if)
       (let ((l (length e)))
         (and (> l 2) (< l 5)))))

(define (assm? e)
  (and (pair? e)
       (eq? (car e) 'set!)
       (symbol? (cadr e))
       (= (length e) 3)))

(define (mac? e)
  (and (pair? e)
       (eq? (car e) 'macro)
       (pair? (cdr e))
       (or (null? (cadr e)) (pair? (cadr e)))
       (pair? (cddr e))))

(define (abs? e)
  (and (pair? e)
       (eq? (car e) 'fn)
       (pair? (cdr e))
       (or (symbol? (cadr e)) (null? (cadr e)) (tsvar? (cadr e)))
       (pair? (cddr e))))

(define (absvar e)
;;  (display "absvar: ")
;;  (display e)
;;  (newline)
  (if (abs? e)
      (cadr e)
      (error "not an abstraction")))

(define (absbody e)
;;  (display "absbody: ")
;;  (display e)
;;  (newline)

  (if (abs? e)
      (caddr e)
      (error "not an abstraction")))

(define (macvars e)
  (if (mac? e)
      (cadr e)
      (error "not an macro")))

(define (macbody e)
  (if (mac? e)
      (caddr e)
      (error "not an macro")))

(define (ifcond e)
  (if (if? e)
      (cadr e)
      (error "not an if expression")))

(define (ifcons e)
  (if (if? e)
      (caddr e)
      (error "not an if expression")))

(define (ifalt e)
  (if (and (if? e)
           (pair? (cdddr e)))
      (cadddr e)
      '()))

(define (asstarget e)
  (if (assm? e)
      (cadr e)
      (error "not an assignment")))

(define (assval e)
  (if (assm? e)
      (caddr e)
      (error "not an assignment")))


(define (ev-quote expr env)
  (cadr expr))

(define (ev-if expr env)
  (if (hygme-eval (ifcond expr) env)
      (hygme-eval (ifcons expr) env)
      (if (pair? (ifalt expr))
          (hygme-eval (ifalt expr) env)
          'undefined)))

(define (ev-assignment expr env)
;;  (display "eval assignment")
  (let ((val (hygme-eval (assval expr) env))
        (binding (lookup-env (asstarget expr) env)))
    (if (pair? binding)
        (set-cdr! binding val)
        (let ((tlbinding (lookup-env (asstarget expr) *toplevel-env*)))
          (if (null? tlbinding)
              (set! *toplevel-env* (cons (cons (asstarget expr) val)
                                         *toplevel-env*))
              (set-cdr! tlbinding val))))))

;;;; apply one argument at a time to returned procedure
(define (ev-application expr env)
  (let ((proc (hygme-eval (car expr) env)))
    (if (or (procedure? proc) (hygme-closure? proc))
        (begin 
          (apply-all proc
                     (map (lambda (x) (hygme-eval x env)) (cdr expr))))
        (error "invalid application"))))

(define (ev-abstraction expr env)
  (close-over expr env))

;;; defines a macro in the *toplevel-macro* namespace
;; rewrites %id as (*stamp0* id)
(define (ev-macro expr env)
  (define (is-%? x)
    (and (char=? (string-ref (symbol->string x) 0) #\%)
         (> (string-length (symbol->string x)) 1)))
  (define (rewrite-% e)
    (cond
     ((and (symbol? e) (is-%? e))
      (*stamp0* (string->symbol (substring (symbol->string e) 1))))
     ((pair? e) (map rewrite-% e))
     (else e)))

  (let ((expr (rewrite-% expr)))
    ;; TODO
    expr))


(define (apply-one fn val)
  (if (hygme-closure? fn) 
      (let ((var (absvar (clocode fn))))
        (hygme-eval (absbody (clocode fn)) (cons (cons var val)
                                                 (cloenv fn))))
      (error "don't know how to apply this")))

(define (apply-all clo vals)
  (let loop ((res clo)
             (vals vals))
    (if (null? vals)
        res
        (if (procedure? clo)
            (apply clo vals)
            (loop (apply-one res (car vals)) (cdr vals))))))

(define (hygme-expand expr)
  (U (A (E (T expr *stamp0*) 1))))

(define (hygme-eval expr env)
;;  (display "evaluating!")
;;  (display expr)
;;  (newline)
;;  (set! expr (hygme-expand expr))
;;  (display "after expanding: ")
;;  (display expr)
;;  (newline)
  (cond
   ((selfev? expr) 
    (let ((binding (lookup-env expr env *toplevel-env*)))
;;      (display "binding: ")
;;      (display binding)
;;      (newline)
      (if (null? binding)
          (error "variable not found")
          (cdr binding))))
   ((const? expr) expr)
   ((quoted? expr) (ev-quote expr env))
   ((if? expr) (ev-if expr env))
   ((assm? expr) (ev-assignment expr env))
   ((mac? expr) (ev-macro expr env))
   ((abs? expr) (ev-abstraction expr env))
   ((pair? expr) (ev-application expr env))
   (else
    (error "invalid expression"))))

(define (hygme-repl)
  (let loop ()
    (display "hygme> ")
    (let ((r (read)))
      (if (eq? r '(unquote quit))
          'exit
          (begin
            (display (heval r))
            (newline)
            (loop))))))

(define (make-stamp v t)
  (list (list 'TSVAR) v t))

(define (S t)
  (let ((cache '()))
    (lambda (v)
      (cond
       ((assq v cache) => cdr)
       (else
        (let ((n (make-stamp v t)))
          (set! cache (cons (cons v n) cache))
          n))))))

(define *stamp0* (S 0))

(define (tsvar? v)
  (and (pair? v)
       (pair? (car v))
       (eq? 'TSVAR (caar v))
       (= (length v) 3)
       (symbol? (cadr v))
       (number? (caddr v))))

(define (atomic-non-var? v)
  (or
   (const? v)
   (tsvar? v)
   (mactok? v)
   (coretok? v)))

(define (atomic-not-stamped? v)
  (or
   (const? v)
   (and (symbol? v)
        (not (tsvar? v)))
   (mactok? v)
   (coretok? v)))

(define (macro? e)
  (and
   (pair? e)
   (symbol? (car e))
   (assq (car e) *toplevel-macenv*)))

(define (E s j)
;;  (display (macro? s))
;;  (newline) (newline)
  (cond
   ((const? s) s)
   ((tsvar? s) s)
   ((quoted? s) s)
   ((coretok? s) s)
   ((macro? s) => 
    (lambda (macbind)
;;      (display "rexpanding!\n\n")
      (E (T (apply-all (cdr macbind) (cdr s)) (S j)) (+ 1 j))))
   ((abs? s)
    `(fn ,(absvar s)
         ,(E (T (absbody s) (S j)) j)))
   (else
    (begin 
      (map (lambda (x) (E x j)) s)))))

(define (T e transform)
  (cond
   ((atomic-non-var? e) e)
   ((symbol? e) (transform e))
   (else (map (lambda (t) (T t transform)) e))))

(define (U e)
;;  (display "U of:")
;;  (display e)
;;  (newline)
  (cond
   ((atomic-not-stamped? e) e)
   ((tsvar? e) (cadr e))
   (else
    (map U e))))

(define (A e)
  (cond
   ((symbol? e) e)
   ((atomic-non-var? e) e)
   ((quoted? e) e)
   ((abs? e) 
    (let ((v (gensym (symbol->string (U (absvar e))))))
      `(fn ,v ,(A (*/* v (absvar e) (absbody e))))))
   (else (map A e))))

(define (*/* v w t)
  (cond
   ((tsvar? t) (if (eq? t w) v t))
   ((atomic-not-stamped? t) t)
   ((quoted? t) t)
   ((abs? t) 
    (if (eq? w (absvar t))
        `(fn ,w ,(absbody t))
        `(fn ,(absvar t)
             ,(*/* v w (absbody t)))))
   (else
    (map (lambda (t)
           (*/* v w t))
         t))))

(set! *toplevel-env* (cons (cons 'S0 *stamp0*) *toplevel-env*))

(set! *toplevel-macenv* 
      `((unless . ,(close-over 
                    '(fn test 
                         (fn body 
                             (list 'if (list 'not test)
                                   body)))
                    *toplevel-env*))
        (let . ,(close-over 
                 '(fn var 
                      (fn val 
                          (fn body
                              (list (list 'fn var body) val))))
                 *toplevel-env*))
        (or . ,(close-over 
               '(fn a
                    (fn b
                        (list 'let 'v a
                              (list 'if 'v 'v b))))
               *toplevel-env*))
        (and . ,(close-over 
               '(fn a
                    (fn b
                        (list 'if a b #f)))
               *toplevel-env*))))

(define (heval xp) 
  (hygme-eval (hygme-expand xp) *toplevel-env*))




;; (fn cond
;;     (fn body
;;         (let it %x
;;           (list 'let it cond
;;                 (list 'if it
;;                       (body it))))))



;; (fn cond2128 
;;     (fn body2129 
;;         ((fn it2130 (list (quote let) it2130 cond2128 (list (quote if) it2130 (body2129 it2130)))) %x)))



;; -3 -1 1 3

;; 0 2 4 6


;; 0 -3 -> -3
;; 2 -3 -> -1
;; 4 -3 -> 1
;; 6 -3 -> 3


;; ;        (aif . ,(close-over 
;; ;                 `(fn cnd
;; ;                      (fn cns
;; ;                          (fn alt
;; ;                              (let it ,(*stamp0* it)
;; ;                                (list 'let it cnd
;; ;                                      (list 'if cnd )))
;; ;                              (list 
;; ;                               'let
;; ;                               ((TSVAR) 'it 0)
;; ;                               cnd
;; ;                               (list 'if cnd cns alt)))))
;; ;                 *toplevel-env*))

 


;; ;(aif cnd cns alt) =>
;; ;   (let %it cnd
;; ;     (if %it
;; ;         cns
;; ;         alt))



;; ;; (hygme-expand '(unless foo bar) *toplevel-macenv*)


