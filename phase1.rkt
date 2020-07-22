#lang racket

(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)
(provide (all-defined-out))

(define-lex-abbrevs
  (str (re-: "\"" (re-* (re-~ "\"")) "\""))
  (id-chars (char-range "A" "z"))
  (id (re-+ id-chars))
  (posnumber (re-or (re-+ (char-range #\0 #\9)) (re-: (re-+ (char-range #\0 #\9)) #\. (re-+ (char-range #\0 #\9)))))            
  )

; lexer

(define-tokens tokens1 (ID POSNUM STR))
(define-empty-tokens tokens2 (== != = - + / * WHILE DO END IF THEN ELSE ENDIF NULL FALSE TRUE SEMI COMMA < > PO PC LO LC RETURN EOF))

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
   ("null" (token-NULL))
   ("false" (token-FALSE))
   ("true" (token-TRUE))
   (";" (token-SEMI))
   ("," (token-COMMA))
   ("<" (token-<))
   (">" (token->))
   ("(" (token-PO))
   (")" (token-PC))
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
      ((FALSE) #f)
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
    )
   ))


;Eval

(define state '(()))

(define result null)

(define (eval stmt)
  (displayln stmt)
  (cond [(null? result)
         (match stmt
           [(command ucmds) (displayln ucmds)
                        (cond [(not (empty? ucmds))
                               (eval (car ucmds)) (eval (make-command (cdr ucmds)))])]

           [(ucmd uc) (displayln uc)
                      (match uc
                        [(ucmd-while exp command) (cond [(eval-exp exp) (eval command) (eval stmt)])]
                        [(ucmd-if exp command1 command2) (if (eval-exp exp) (eval command1) (eval command2))]
                        [(ucmd-return exp) (set! result (eval-exp exp))]
                        [(ucmd-assign var exp) (set-var var (eval-exp exp))]
                        )])])
  result)

(define (eval-exp exp)
  (displayln exp)
  (match exp
     [(exp-uneq op exp1 exp2) (let [(e1 (eval-exp exp1)) (e2 (eval-exp exp2))]
                                (cond
                                  [(and (number? e1) (number? e2)) (op e1 e2)]
                                  [(and (string? e1) (string? e2)) (if (eqv? op >) (string>? e1 e2) (string>? e2 e1))]
                                  [(and (list? e1) (or (number? e2) (string? e2))) (and (eval-exp (make-exp-uneq op (car e1) e2)) (eval-exp (make-exp-uneq op (cdr e1) e2)))] 
                                  [(and (list? e2) (or (number? e1) (string? e1))) (and (eval-exp (make-exp-uneq op (car e2) e1)) (eval-exp (make-exp-uneq op (cdr e2) e1)))]
                                  [else (error "Comparison not implemented or list is empty" e1 e2)]
                                  ))]
     [(exp-== exp1 exp2) (let [(e1 (eval-exp exp1)) (e2 (eval-exp exp2))]
                           (cond
                             [(and (number? e1) (number? e2)) (= e1 e2)]
                             [(and (string? e1) (string? e2)) (string? e1 e2)]
                             [(and (null? e1) (null? e2)) #t]
                             [(and (boolean? e1) (boolean? e2)) (eqv? e1 e2)]
                             [(and (list? e1) (list? e2)) (if (xor (empty? e1) (empty? e2)) #f
                                                              (and (eval-exp (make-exp-== (car e1) (car e2)) (eval-exp (make-exp-== (cdr e1) (cdr e2))))))]
                             [(list? e1) (and (eval-exp (make-exp-== (car e1) e2)) (eval-exp (make-exp-== (cdr e1) e2)))]
                             [(list? e2) (eval-exp (make-exp-== e2 e1))]
                             [else #f]
                             ))]
     [(exp-!= exp1 exp2) (not (eval-exp (make-exp-!= exp1 exp2)))]
     [(exp-operation op exp1 exp2) (let [(e1 (eval-exp exp1)) (e2 (eval-exp exp2))]
                                      (cond
                                        [(and (number? e1) (number? e2)) (op e1 e2)]
                                        [(and (string? e1) (string? e2) (eqv? op +)) (string-append e1 e2)]
                                        [(and (boolean? e1) (boolean? e2) (eqv? op +)) (or e1 e2)]
                                        [(and (boolean? e1) (boolean? e2) (eqv? op *)) (and e1 e2)]
                                        [(and (list? e1) (list? e2) (eqv? op +)) (append e1 e2)]
                                        [(and (list? e1)) (if (empty? e1) e1 (cons (eval-exp (make-exp-operation op (car e1) e2)) (eval-exp (make-exp-operation op (cdr e1) e2))))]
                                        [(and (list? e2)) (if (empty? e2) e2 (cons (eval-exp (make-exp-operation op e1 (car e2))) (eval-exp (make-exp-operation op e1 (cdr e2)))))]
                                        [else (error "Unsupported operation or list is empty" exp1 op exp2)]
     ))]
     [(exp-neg exp) (let ((e (eval-exp exp)))
                      (cond
                        [(number? e) (- e)]
                        [(boolean? e) (not e)]
                        [(and (list? e) (not (empty? e))) (cons (eval-exp (make-exp-neg (car e))) (eval-exp (make-exp-neg (cdr e))))]
                        [else (error "Unsupported negation or list is empty" e)]
                        ))]
     [(exp-listmem exp listmem) (let ((e (eval-exp exp)))
                                  (if (empty? listmem)
                                      exp
                                      (if (list? e)
                                          (if (> (length e) (car listmem))
                                              (if (< 0 (car listmem))
                                                  (eval-exp (make-exp-listmem (list-ref e (car listmem)) (cdr listmem)))
                                                  (error "negative index" (car listmem)))
                                              (error "index out of range" (car listmem)))
                                          (error "is not a list" e)))
                                  )]
     [(exp-var var) (get-var var)]
     [else exp]
     ))

(define (set-var var val)
  (set! state (cons (cons (list var val) (car state)) (cdr state)))
  (displayln state)
  )
     
(define (get-var var)
  (get-var-state var state)
  )

(define (get-var-state var st)
  (if (empty? st)
      (error "var not defined" var)
      (if (empty? (car st))
          (get-var-state var (cdr st))
          (if (equal? (caaar st) var)
              (cadaar st)
              (get-var-state var (cons (cdar st) (cdr st)))))))

(define (evaluate addr)
  (define in-port (open-input-file addr))
  (define lexer1 (lex-it mylexer in-port))
  (set! result null)
  (set! state '(()))
  (define ret-val (let ((parser-res (myparser lexer1))) (eval parser-res)))
  (close-input-port in-port)
  ret-val)

(define lex-it (lambda (lexer input) (lambda () (lexer input))))
(define lexer1 (lex-it mylexer (open-input-string "a = 5;
l = [\"b\"];
while a > 0 do
	l = \"a\" + l + \"c\";
	l = l + [\"b\"];
	a = a - 1
end;
return l
")))

;(displayln "Test Lexer")
;(lexer1)

;(displayln "Test Parser")
;(let ((parser-res (myparser lexer1))) (ucmd-ucmd (car (command-ucmds parser-res))))

(evaluate "code4.fab")
