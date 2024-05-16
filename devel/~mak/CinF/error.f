GET-CURRENT
ALSO  TEMP-WORDLIST DUP  CONTEXT ! DEFINITIONS

: yy ( n -- )
 DROP
 PARSE-WORD 2DROP PARSE-WORD 2DROP
 PARSE-WORD 2DROP PARSE-WORD 2DROP
 PARSE-WORD OVER C@ [CHAR] ' =
 IF DROP 1+ C@
 ELSE  EVALUATE
 THEN C, POSTPONE \ ;

: xx yy ;

SWAP SET-CURRENT

HERE TO kind

~mak\CinF\TOKEN.F
\ DBG_STOP 

PREVIOUS

FREE-WORDLIST


: expect  { tok set }
   t tok =
   IF gettok TO t
   ELSE ." syntax error; expecting "   tok EMIT ABORT
   THEN
;

: test { tok set }
   t tok  =
   IF gettok TO t
   ELSE tok 0 expect
   THEN
;

