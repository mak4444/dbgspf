\ ~mak
\ слова с префиксом B_ работают аналогично соответствующим словам без B_,
\ но используют другой стек для передачи  orig и dest.

REQUIRE A_BEGIN ~mak\LIB\a_if.f

 VARIABLE B_CS-SP
 99 CS-CELLS ALLOT
 HERE B_CS-SP !

: B_CS-SP><  \  stack substitution
  B_CS-SP @ SP@ CELL+ B_CS-SP ! SP! ;

: B_BEGIN	B_CS-SP>< POSTPONE BEGIN	B_CS-SP>< ; IMMEDIATE
: B_WHILE	B_CS-SP>< POSTPONE WHILE	B_CS-SP>< ; IMMEDIATE 
: B_AHEAD	B_CS-SP>< POSTPONE AHEAD	B_CS-SP>< ; IMMEDIATE
: B_IF		B_CS-SP>< POSTPONE IF		B_CS-SP>< ; IMMEDIATE
: B_ELSE	B_CS-SP>< POSTPONE ELSE		B_CS-SP>< ; IMMEDIATE
: B_THEN	B_CS-SP>< POSTPONE THEN		B_CS-SP>< ; IMMEDIATE
: B_AGAIN	B_CS-SP>< POSTPONE AGAIN 	B_CS-SP>< ; IMMEDIATE
: B_REPEAT	B_CS-SP>< POSTPONE REPEAT	B_CS-SP>< ; IMMEDIATE

\EOF TEST

: TST BEGIN 1 EMIT KEY [CHAR] Q =
       IF   2 EMIT   B_AHEAD \ GOTO M
       THEN 3 EMIT KEY [CHAR] Q =
      UNTIL 4 EMIT   B_THEN  \ M:
;
  TST    
 