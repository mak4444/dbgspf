REQUIRE [DEFINED] lib\include\tools.f
REQUIRE B_AHEAD ~mak\LIB\B_IF.F
REQUIRE DUPENDCASE ~mak\case.f
REQUIRE F. lib\include\float.f   12 F-SIZE ! 

: FCELL F-SIZE @ ;
WARNING @ WARNING 0! 
WARNING ! 
USER  RFSAVE 
C" uAddDepth" FIND NIP
[IF]
 : F>R  S"  RP@ -3 CELLS + DUP RP! F!" EVALUATE  3 CELLS uAddDepth +! ; IMMEDIATE
 : FR>  S"  RP@ F@ RDROP RDROP RDROP " EVALUATE ( -3 CELLS uAddDepth +!) ; IMMEDIATE
[ELSE]
 : F>R  S" RFSAVE !  ROT >R 2>R RFSAVE @" EVALUATE ; IMMEDIATE
 : FR>  S" RFSAVE ! 2R> R> -ROT RFSAVE @" EVALUATE ; IMMEDIATE
[THEN]

: F0<> F0= 0= ;
: F+!  ( f: r | addr -- )
  DUP F@ F+ F! ;

: F1+!  (  addr -- )
  1e F+! ;

C" REL!" FIND NIP 0=
[IF] : REL! ( ADDR' ADDR  --  )
       TUCK - SWAP ! ;
[THEN]

0
CELL FIELD ->forward
CELL FIELD ->back
CONSTANT \::$

: VLABEL ' >BODY
          DUP >R ->forward @  
          BEGIN DUP
          WHILE 1+ DUP @ HERE CELL -  ROT   REL!
          REPEAT  DROP
          HERE R> ->back !  
;   IMMEDIATE

: VJUMP  ' >BODY
         DUP >R ->back @ DUP
         IF  BRANCH,
         ELSE DROP
           HERE
           R@ ->forward @  0xE9 C, ,
           R@ ->forward !
         THEN  RDROP
 ;    IMMEDIATE

: $LIT ' >BODY ->back @ POSTPONE LITERAL
;    IMMEDIATE

: $CREATE ' >BODY
          DUP >R ->forward @  
          BEGIN  DUP
          WHILE  DUP @ HERE  ROT !
          REPEAT  DROP
          HERE R> ->back !  
;    IMMEDIATE


: ::$ CREATE HERE \::$ ALLOT IMMEDIATE
      DOES>
         DUP >R ->back @ DUP 0=
         IF DROP R@ ->forward @ 
            HERE R@ ->forward !
         THEN  RDROP    POSTPONE LITERAL
; 
0
::$  $1 ::$  $2 ::$  $3 ::$ $4 ::$ $5 ::$ $6 ::$ $7 ::$ $8 ::$ $9
::$ $10 ::$ $11 ::$ $12 ::$ $13 ::$ $14 ::$ $15 ::$ $16 ::$ $17 ::$ $18 ::$ $19

CREATE $LIST  0 C,
?DUP [IF] , 0 >IN ! [THEN] 
LATEST NAME> >BODY HERE OVER 1+ - SWAP C!

C" BOUNDS" FIND NIP 0=
[IF] : BOUNDS OVER + SWAP ;
[THEN]

: $INIT $LIST COUNT BOUNDS
   DO  I @ DUP ->back 0!  ->forward 0!
       CELL
  +LOOP ;

$INIT 

1 8 LSHIFT CONSTANT ...BufSize

USER-CREATE ...Buf ...BufSize USER-ALLOT
USER  ...N ...N 0!
USER  RP_SV RP_SV 0!
USER  ...FLAG ...FLAG 0!


C" &RP+!" FIND NIP 0= 
[IF]
: &RP+!
  POSTPONE RP@
  POSTPONE +
  POSTPONE RP!
;     IMMEDIATE
[THEN]

:  >... ...Buf ...N @ ...BufSize 1- AND + C! -1 ...N +! ;
:  ...> ...N 1+! ...Buf ...N @  ...BufSize 1- AND + C@ ;
: .i. FALSE  >... ;  IMMEDIATE
: .f. TRUE   >... ;  IMMEDIATE

: TO... ( -- )
      CELL
      POSTPONE DUP>R \ резерв для CALL
      BEGIN  ...N @
      WHILE  ...> IF 3 CELLS + POSTPONE F>R 
                  ELSE CELL+   POSTPONE  >R
                  THEN 
      REPEAT LIT,
      POSTPONE DUP
      POSTPONE &RP+!
;     IMMEDIATE

: .... TRUE ...FLAG !
      S" RP@ RP_SV ! NEGATE CELL+ &RP+!"  EVALUATE        
;     IMMEDIATE

VARIABLE F_VAR
0 VALUE L_VAR

TRUE VALUE SP_CL
TRUE VALUE ~RET

: RET SP_CL IF POSTPONE R@ POSTPONE SP! 0 TO ~RET THEN ; IMMEDIATE

: BIG_PROC
  ...N 0!
  F_VAR ! 
  L_VAR LIT,
  POSTPONE @
  POSTPONE NEGATE
  POSTPONE &RP+!
  SP_CL IF POSTPONE SP@ POSTPONE >R TRUE TO ~RET THEN
;

: END_PROC
  SP_CL ~RET AND IF POSTPONE R> POSTPONE SP! ELSE POSTPONE RDROP THEN
  F_VAR @  POSTPONE LITERAL 
  L_VAR LIT,
  POSTPONE @
  POSTPONE + 
  POSTPONE &RP+!
  ; IMMEDIATE

: &F ( -- )
  SP_CL IF POSTPONE CELL+ THEN
  ...FLAG @ IF POSTPONE CELL+ THEN
  L_VAR LIT,
  S" @ + RP@ + " EVALUATE ; IMMEDIATE

: &L ( n -- )
  SP_CL IF POSTPONE CELL+ THEN
  ...FLAG @ IF POSTPONE CELL+ THEN
  S" RP@ + " EVALUATE ; IMMEDIATE

[UNDEFINED] C>S
[IF]
: C>S ( c -- n )  0xFF AND [ 0x7F INVERT ] LITERAL XOR 0x80 + ;
[THEN]

[UNDEFINED] W>S
[IF]
: W>S ( w -- n )  0xFFFF AND [ 0x7FFF INVERT ] LITERAL XOR 0x8000 + ;
[THEN]

: C2UP DUP 0x60 > IF 0xDF AND THEN ;

: HS," ( -- )
  [CHAR] " PARSE
 BOUNDS DO        I    C@ [CHAR] % =
           IF \ Расшифровка %xx представления
                3 I 1+ C@ C2UP 16 DIGIT 0= IF 0  THEN 4 LSHIFT \ 3 n1*16
                  I 2+ C@ C2UP 16 DIGIT    IF OR THEN          \ 3 n1*16|n2
           ELSE 1 I    C@ 
           THEN        C,
       +LOOP ;

[UNDEFINED] <=
[IF] : <=  > 0= ;
[THEN]

: (LIT,) R> DUP CELL+ >R @ ;
: $$L, ['] (LIT,) COMPILE, ;

[DEFINED] XLIT.
[IF]
  : (LIT,).
      [ ' XLIT. >BODY @ COMPILE, ]
      DUP ['] (LIT,) ." L=" 2DUP . . =
      IF  >R
           DUP @ H. CELL+
          R>
      THEN  ;
' (LIT,). TO  XLIT.
;
[THEN]

VARIABLE  A_CSP    \ Указатель стека контроля

: A_CASE 
   CS-SP><  A_CSP @ SP@ A_CSP ! CS-SP><  ; IMMEDIATE

: A_DUPENDCASE CSP @
 CS-SP>< A_CSP @ CSP !   POSTPONE DUPENDCASE
         CSP @ A_CSP !
 CS-SP><       CSP ! ; IMMEDIATE

VARIABLE  B_CSP    \ Указатель стека контроля

: B_CASE 
   B_CS-SP><  B_CSP @ SP@ B_CSP ! B_CS-SP><  ; IMMEDIATE

: B_DUPENDCASE CSP @
 B_CS-SP>< B_CSP @ CSP !   POSTPONE DUPENDCASE
           CSP @ B_CSP !
 B_CS-SP><       CSP ! ; IMMEDIATE

: FOR_WHILE  
  S" 0= A_WHILE AHEAD BEGIN" EVALUATE ;  IMMEDIATE

: FOR_BODY
  S" A_REPEAT A_CASE" EVALUATE ;  IMMEDIATE

: FOR_REPEAT
  S" AGAIN THEN A_DUPENDCASE" EVALUATE ;  IMMEDIATE

: SWITCH  POSTPONE A_CASE
 POSTPONE CASE DUP ;       IMMEDIATE

: DEFAULT:
 SP@ CSP @ U<
 IF  POSTPONE B_THEN
       BEGIN SP@ CELL+ CSP @ <> 
       WHILE POSTPONE DUP  LIT,
           POSTPONE <>
           POSTPONE UNTIL
       REPEAT  DROP
 THEN ; IMMEDIATE

: ENDSWITCH 
  S" A_AHEAD DEFAULT: [  CSP ! ] A_DUPENDCASE  DROP" EVALUATE ;  IMMEDIATE

: C:[ POSTPONE BEGIN POSTPONE [ ; IMMEDIATE

: ST[ POSTPONE AHEAD POSTPONE [ ; IMMEDIATE
: ]ST  ] POSTPONE THEN ; IMMEDIATE
