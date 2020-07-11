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
(define-struct make-null ())

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
     ((IF expg DO commandg ELSE commandg ENDIF) (make-ucmd-if $2 $4 $6))
     )
    (ucmdg-ret
     ((RETURN expg) (make-ucmd-return $2))
     )(ucmdg-assign
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
      ((NULL) make-null)
      ((ID) $1)
      ((TRUE) #t)
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
;(let ((parser-res (myparser lexer1))) (command-ucmds parser-res))

