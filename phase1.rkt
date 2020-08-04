#lang racket

(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)
(provide (all-defined-out))



; lexer
(define-lex-abbrevs
  (str (re-: "\"" (re-* (re-~ "\"")) "\""))
  (id-chars (re-or (char-range "A" "Z") (char-range "_" "_") (char-range "a" "z")))
  (id (re-+ id-chars))
  (posnumber (re-or (re-+ (char-range #\0 #\9)) (re-: (re-+ (char-range #\0 #\9)) #\. (re-+ (char-range #\0 #\9)))))            
  )

(define-tokens tokens1 (ID POSNUM STR))
(define-empty-tokens tokens2 (== != = - + / * WHILE DO END IF THEN ELSE ENDIF FUNC NULL FALSE TRUE SEMI COMMA < > PO PC KO KC LO LC RETURN EOF))

(define mylexer
  (lexer
   ("==" (token-==))
   ("!=" (token-!=))
   ("=" (token-=))
   ("-" (token--))
   ("+" (token-+))
   ("/" (token-/))
   ("*" (token-*))
   ("while" (token-WHILE))
   ("do" (token-DO))
   ("end" (token-END))
   ("if" (token-IF))
   ("then" (token-THEN))
   ("else" (token-ELSE))
   ("endif" (token-ENDIF))
   ("func" (token-FUNC))
   ("null" (token-NULL))
   ("false" (token-FALSE))
   ("true" (token-TRUE))
   (";" (token-SEMI))
   ("," (token-COMMA))
   ("<" (token-<))
   (">" (token->))
   ("(" (token-PO))
   (")" (token-PC))
   ("{" (token-KO))
   ("}" (token-KC))
   ("[" (token-LO))
   ("]" (token-LC))
   ("return" (token-RETURN))
   ((eof) (token-EOF))
   (id (token-ID lexeme))
   (str (token-STR lexeme))
   (posnumber (token-POSNUM (string->number lexeme)))
   (whitespace (mylexer input-port))
   ))

; parser
(define-struct command (ucmds))
(define-struct ucmd (ucmd))
(define-struct ucmd-while (exp command))
(define-struct ucmd-if (exp command1 command2))
(define-struct ucmd-return (exp))
(define-struct ucmd-assign (var exp))
(define-struct exp-uneq (op exp1 exp2))
(define-struct exp-== (exp1 exp2))
(define-struct exp-!= (exp1 exp2))
(define-struct exp-operation (op exp1 exp2))
(define-struct exp-neg (exp))
(define-struct exp-listmem (exp listmem))
(define-struct exp-var (var))
(define-struct exp-null ())
(define-struct ucmd-assign-func (var body))
(define-struct ucmd-assign-call (var call))
(define-struct ucmd-call (name vars))
(define-struct ucmd-func (vars body))
(define-struct thung (exp env))
(define-struct exp-func (body vars))

(define myparser
  (parser
   (start commandg)
   (end EOF)
   (tokens tokens1 tokens2)
   (error void)
   (grammar
    (commandg
     ((ucmdg) (make-command (list $1)))
     ((commandg SEMI ucmdg) (make-command (append (command-ucmds $1) (list $3))))
     )
    (ucmdg
     ((ucmdg-while) (make-ucmd $1))
     ((ucmdg-if) (make-ucmd $1))
     ((ucmdg-assign) (make-ucmd $1))
     ((ucmdg-ret) (make-ucmd $1))
     )
    (ucmdg-while
     ((WHILE expg DO commandg END) (make-ucmd-while $2 $4))
     )
    (ucmdg-if
     ((IF expg THEN commandg ELSE commandg ENDIF) (make-ucmd-if $2 $4 $6))
     )
    (ucmdg-ret
     ((RETURN expg) (make-ucmd-return $2))
     )
    (ucmdg-assign
     ((ID = expg) (make-ucmd-assign $1 $3))
     ((ID = funcg) (make-ucmd-assign-func $1 $3))
     ((ID = callg) (make-ucmd-assign-call $1 $3))
     )
     (expg
      ((aexpg) $1)
      ((aexpg > aexpg) (make-exp-uneq > $1 $3))
      ((aexpg < aexpg) (make-exp-uneq < $1 $3))
      ((aexpg == aexpg) (make-exp-== $1 $3))
      ((aexpg != aexpg) (make-exp-!= $1 $3))
      )
     (aexpg
      ((bexpg) $1)
      ((bexpg - aexpg) (make-exp-operation - $1 $3))
      ((bexpg + aexpg) (make-exp-operation + $1 $3))
      )
     (cexpg
      ((- cexpg) (make-exp-neg $2))
      ((PO expg PC) $2)
      ((POSNUM) $1)
      ((NULL) exp-null)
      ((ID) (make-exp-var $1))
      ((TRUE) (make-exp-var #t))
      ((FALSE)(make-exp-var #f))
      ((STR) (substring $1 1 (- (string-length $1) 1)))
      ((listg) $1)
      ((ID listmemg) (make-exp-listmem (make-exp-var $1) $2))
      )
     (bexpg
      ((cexpg) $1)
      ((cexpg * bexpg) (make-exp-operation * $1 $3))
      ((cexpg / bexpg) (make-exp-operation / $1 $3))
      )
     (listg
      ((LO listvg LC) $2)
      ((LO LC) '())
      )
     (listvg
      ((expg) (list $1))
      ((expg COMMA listvg) (cons $1 $3))
      )
     (listmemg
      ((LO expg LC) (list $2))
      ((LO expg LC listmemg) (cons $2 $4))
      )
     (funcg
      ((FUNC PO varsg PC KO commandg KC) (make-ucmd-func $3 $6))
      )
     (varsg
      ((ID) (list $1))
      ((ID COMMA varsg) (cons $1 $3))
      )
     (callg
       ((ID PO argsg PC) (make-ucmd-call $1 $3))
      )
      (argsg
       ((expg) (list $1))
       ((expg COMMA argsg) (cons $1 $3))
      )
    )
   ))
(define (thung-value th)
  (if (list? th) (map thung-value th) (if (thung? th) (eval-exp (thung-exp th) (thung-env th)) th)))

(define (eval-all ezp env)
  (displayln "AAAA")
  (cond
    [(null? ezp) '()]
    [(list? ezp) (cons (eval-exp (car ezp) env) (eval-all (cdr ezp) env))]
    [else (eval-exp ezp env)]
    ))

; library
(define (pow a b)
  (cond
    [(equal? b 1) a]
    [else (* a (pow a (- b 1)))])
  )
(define (make_list a b)
  (if (> 1 a) '() (cons b (make_list (- a 1) b))))
(define (reverse a)
  (if (empty? a) '() (append (reverse (cdr a)) (list (car a)))))
(define (reverse_all l)
  (cond
    [(null? l) empty]
    [(list? l) (append (reverse_all (cdr l)) (cons (reverse_all (car l)) '()))]
    [else l]
    ))
(define (set a index val)
  (cond [(empty? a) (error "list index bigger than list size")]
        [(equal? index 0) (cons val (cdr a))]
        [else (cons (car a) (set (cdr a) (- index 1) val))]))
(define (merge a b)
  (cond [(null? a) b]
        [(null? b) a]
        [(< (car a) (car b)) (cons (car a) (merge (cdr a) b))]
        [else (cons (car b) (merge (cdr b) a))]))
(define (get_last l n)
  (if (equal? n (length l)) l (get_last (cdr l) n)))
(define (get_first l n)
  (if (equal? n (length l)) l (reverse (get_last (reverse l) n)))
  )
(define (get_res l n)
  (if (equal? n 0) l (get_res (cdr l) (- n 1))))
(define (merge_sort a)
  (if (< (length a) 2) a (merge (merge_sort (get_first a (floor (/ (length a) 2)))) (merge_sort (get_res a (floor (/ (length a) 2))))))
)
; env


(define (empty-env) (lambda(search-var) (error "variable not found." search-var)))
(define (extend-env saved-var saved-val env) 
	(lambda (search-var)
		(if (equal? search-var saved-var) saved-val (apply-env env search-var))
	)
)
(define (apply-env env search-var)
	(let [(val (env search-var))] (if (thung? val) (eval-exp (thung-exp val) (thung-env val)) (if (list? val) (thung-value val) val)))	
)

(define (extend-list-env list-var list-val env)
  (cond [(null? list-var) env]
        [else (extend-list-env (cdr list-var) (cdr list-val) (extend-env (car list-var) (car list-val) env))]))

(define (init-env)
  (extend-list-env '("pow" "make_list" "reverse" "reverse_all" "set" "merge" "merge_sort" "eval")
               (list (lambda(list) (apply pow (map thung-value list)))
                     (lambda(list) (apply make_list (map thung-value list)))
                     (lambda(list) (apply reverse (map thung-value list)))
                     (lambda(list) (apply reverse_all (map thung-value list)))
                     (lambda(list) (apply set (map thung-value list)))
                     (lambda(list) (apply merge (map thung-value list)))
                     (lambda(list) (apply merge_sort (map thung-value list)))
                     (lambda(list) (eval (car list) init-env)))
               (empty-env)))

(define (thung-list l env)
  (cond [(list? l) (map (lambda (x) (make-thung x env)) l)]
        [else (make-thung l env)]
  ))


;Eval

(define (eval stmt env)
  (displayln stmt)
  
  (cond [(eqv? "sinerf" (apply-env env "return"))
         (match stmt
           [(command ucmds) (displayln ucmds)
                        (cond [(not (empty? ucmds))
                               (let [(new-env (eval (car ucmds) env))] (eval (make-command (cdr ucmds)) new-env))]
                              [else env])]
                         
           [(ucmd uc) (displayln uc)
                      (match uc
                        [(ucmd-while exp command) (cond [(eval-exp exp env) (let [(new-env (eval command env))] (eval stmt new-env))] [else env])]
                        [(ucmd-if exp command1 command2) (if (eval-exp exp env) (eval command1 env) (eval command2 env))]
                        [(ucmd-assign var exp) (extend-env var (make-thung exp env) env)]
                        [(ucmd-assign-func var body) (extend-env var (letrec ((f (lambda (call-list)
                                                                                   (apply-env (eval (ucmd-func-body body) (extend-list-env (ucmd-func-vars body) call-list (extend-env var f env))) "return")
                                                                                   ))) f) env)]
                        [(ucmd-assign-call var call) (extend-env var (make-thung (make-exp-func (apply-env env (ucmd-call-name call)) (map (lambda (x) (thung-list x env)) (ucmd-call-vars call))) env) env)]
                        [(ucmd-return exp) (extend-env "return" (eval-exp exp env) env)]
                        )])]
        [else env]))

(define (map-bool l b)
  (if (null? l) '() (cons (or (car l) b) (map-bool (cdr l) b)))
  )

(define (map-bool-and l b)
  (if (null? l) '() (cons (and (car l) b) (map-bool-and (cdr l) b)))
  )

(define (eval-exp exp env)
  (displayln exp)
  (match exp
     [(exp-uneq op exp1 exp2) (let [(e1 (eval-exp exp1 env)) (e2 (eval-exp exp2 env))]
                                (cond
                                  [(and (number? e1) (number? e2)) (op e1 e2)]
                                  [(and (string? e1) (string? e2)) (if (eqv? op >) (string>? e1 e2) (string>? e2 e1))]
                                  [(and (list? e1) (or (number? e2) (string? e2))) (and (eval-exp (make-exp-uneq op (car e1) e2) env) (eval-exp (make-exp-uneq op (cdr e1) e2) env))] 
                                  [(and (list? e2) (or (number? e1) (string? e1))) (and (eval-exp (make-exp-uneq op (car e2) e1) env) (eval-exp (make-exp-uneq op (cdr e2) e1) env))]
                                  [else (error "Comparison not implemented or list is empty" e1 e2)]
                                  ))]
     [(exp-== exp1 exp2) (let [(e1 (eval-exp exp1 env)) (e2 (eval-exp exp2 env))]
                           (cond
                             [(and (number? e1) (number? e2)) (= e1 e2)]
                             [(and (string? e1) (string? e2)) (string? e1 e2)]
                             [(and (null? e1) (null? e2)) #t]
                             [(and (boolean? e1) (boolean? e2)) (eqv? e1 e2)]
                             [(and (list? e1) (list? e2)) (if (xor (empty? e1) (empty? e2)) #f
                                                              (and (eval-exp (make-exp-== (car e1) (car e2)) env) (eval-exp (make-exp-== (cdr e1) (cdr e2)) env)))]
                             [(list? e1) (and (eval-exp (make-exp-== (car e1) e2) env) (eval-exp (make-exp-== (cdr e1) e2) env))]
                             [(list? e2) (eval-exp (make-exp-== e2 e1) env)]
                             [else #f]
                             ))]
     [(exp-!= exp1 exp2) (not (eval-exp (make-exp-!= exp1 exp2) env))]
     [(exp-operation op exp1 exp2) (let [(e1 (eval-exp exp1 env))]
                                         (cond [(and (number? e1) (equal? e1 0) (eqv? op *)) 0]
                                               [(and (boolean? e1) (equal? e1 #f) (eqv? op *)) #f]
                                               [else (let [(e2 (eval-exp exp2 env))]
                                      (cond
                                        [(and (number? e1) (number? e2)) (if (and (eqv? op *) (equal? e1 0)) 0 (op e1 e2))]
                                        [(and (string? e1) (string? e2) (eqv? op +)) (string-append e1 e2)]
                                        [(and (boolean? e1) (boolean? e2) (eqv? op +)) (or e1 e2)]
                                        [(and (boolean? e1) (boolean? e2) (eqv? op *)) (and e1 e2)]
                                        [(and (list? e1) (list? e2) (eqv? op +)) (append e1 e2)]
                                        [(and (list? e1) (boolean? e2) (eqv? op +)) (map-bool e1 e2)]
                                        [(and (list? e2) (boolean? e1) (eqv? op +)) (map-bool e2 e1)]
                                        [(and (list? e1) (boolean? e2) (eqv? op *)) (map-bool-and e1 e2)]
                                        [(and (list? e2) (boolean? e1) (eqv? op *)) (map-bool-and e2 e1)]
                                        [(and (list? e1)) (if (empty? e1) e1 (cons (eval-exp (make-exp-operation op (car e1) e2) env) (eval-exp (make-exp-operation op (cdr e1) e2) env)))]
                                        [(and (list? e2)) (if (empty? e2) e2 (cons (eval-exp (make-exp-operation op e1 (car e2)) env) (eval-exp (make-exp-operation op e1 (cdr e2)) env)))]
                                        [else (error "Unsupported operation or list is empty" exp1 op exp2)]
     ))]))]
     [(exp-neg exp) (let ((e (eval-exp exp env)))
                      (cond
                        [(number? e) (- e)]
                        [(boolean? e) (not e)]
                        [(list? e) (if (empty? e) '() (cons (eval-exp (make-exp-neg (car e)) env) (eval-exp (make-exp-neg (cdr e)) env)))]
                        [else (error "Unsupported negation or list is empty" e)]
                        ))]
     [(exp-listmem exp listmem) (let ((e (eval-exp exp env)))
                                  (if (empty? listmem)
                                      exp
                                      (if (list? e)
                                          (let ((first (eval-exp (car listmem) env)))
                                          (if (> (length e) first)
                                              (if (<= 0 first)
                                                  (eval-exp (make-exp-listmem (list-ref e first) (cdr listmem)) env)
                                                  (error "negative index" first))
                                              (error "index out of range" first)))
                                          (error "is not a list" e)))
                                  )]
     [(exp-var var) (if (boolean? var) var (let ((res (apply-env env var))) res))]
    [(exp-func body vars) (if (exp-func? body) ((eval-exp body) vars) (body vars))]
     [else (cond
             [(null? exp) '()]
             [(list? exp) (cons (eval-exp (car exp) env) (eval-exp (cdr exp) env))]
             [else exp])]
    ))


(define (evaluate addr)
  (define in-port (open-input-file addr))
  (define lexer1 (lex-it mylexer in-port))
  (define ret-val (let ((parser-res (myparser lexer1))) (eval parser-res (extend-env "return" "sinerf" (init-env)))))
  (close-input-port in-port)
  (apply-env ret-val "return"))

(define lex-it (lambda (lexer input) (lambda () (lexer input))))
;(define lexer1 (lex-it mylexer (open-input-string "b = reverse([2, [2, 5]]);
;return b")))
;
;(displayln "Test Lexer")
;(lexer1)
;(displayln "Test Parser")
;(let ((parser-res (myparser lexer1))) (ucmd-ucmd (car (command-ucmds parser-res))))

(evaluate "./all/lazy3.txt")
