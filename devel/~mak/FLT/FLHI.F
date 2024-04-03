\ Float-библиотека для spfopt.exe
\ Слова высокого уровня

~mak\flt\flkern_.f
~mak\flt\flkern.f

USER FFORM
USER FFORM-EXP
USER ?PRINT-EXP       \ печать с экспонентой или без нее
USER F-SIZE
USER PAST-COMMA \ число знаков после точки
USER ?IS-COMMA  \ появилась точка или нет
USER CountBuf \ счетчик в буфере

: 2e_ 2.E_ ;
: 1e_ 1.E_ ;
: .e_ .E_ ;
: 2e 2.E_ F>T ;
: 1e 1.E_ F>T ;
: .e .E_  F>T ;

VARIABLE FCON-E

: PRINT-EXP TRUE ?PRINT-EXP ! ;
: PRINT-FIX FALSE ?PRINT-EXP ! ;

: FSINGLE 4 F-SIZE ! ;
: FDOUBLE 8 F-SIZE ! ;
: FLONG 10  F-SIZE ! ;

: FLOATS F-SIZE @ * ;
: FLOAT+ F-SIZE @ + ;

: FENV.  \ *
  28 ALLOCATE THROW DUP >R F>ENV
  R@ 8 + W@ 
  ." FPU registers usage:" CR
  8 0 DO
        ." reg " 8 I - . ." :" 
        DUP DUP 1 AND 
        SWAP 2 AND 
        0= IF 1 = IF ."  zero" ELSE ."  valid number" THEN
           ELSE 1 = IF ."  empty" ELSE ."  invalid or infinity" THEN
           THEN
        2 RSHIFT
        CR
      LOOP
  DROP
  R> FREE THROW
;
: stackIsEmpty
   ." FPU: Float stack is empty!"
;

HEX
: F, ( r -- )
         HERE F! F-SIZE @ ALLOT  
;

: DF, ( r -- )
         HERE DF! F-SIZE ALLOT  
;

: SF, ( r -- )
         HERE SF! F-SIZE ALLOT  
;

DECIMAL

HEX
CREATE FINF-ADDR 0 , 80 , FF7F0000 ,  \ Infinity

: FINF_ FINF-ADDR F@_ ;

: TNUM ( addr u -- d )       \ *
   0. 2SWAP >NUMBER 2DROP
;
: GET-EXP ( addr u -- d )    \ *
  FCON-E 1 SEARCH
  IF                    \ addr u
    >R 1+ DUP C@        \ addr1 c
    DUP [CHAR] - =
    IF DROP 1+ R> 2 - TNUM DNEGATE   
    ELSE [CHAR] + = IF 1+ R> 2 - TNUM ELSE R> 1- TNUM THEN
    THEN
  ELSE
    2DROP 0.
  THEN
;

\ Идем до первой не цифры, точку пропускаем
\ Идем первые 40 символов
: FRAC>F_ ( addr -- F: r )              \ использует 2 регистра FPU
  .e_
  DUP 40 + SWAP
  DO
    ?IS-COMMA @ IF PAST-COMMA 1+! THEN
    I C@ 10 DIGIT 
    IF DS>F_ F+_ F10*_
    ELSE I C@ [CHAR] . = IF TRUE ?IS-COMMA ! 
                         ELSE LEAVE
                         THEN
    THEN
  LOOP 
;

: >FLOAT-ABS_  ( addr u -- F: r )
   GETFPUCW >R UP-MODE 
   2DUP GET-EXP DROP         \  addr u u2 - экспонента
   NIP                      
   SWAP FRAC>F_             \ u2
   ?IS-COMMA @ 0= IF PAST-COMMA 1+! THEN
   PAST-COMMA @ - F10X_  F*_  ?OE ?IE OR 
   DUP IF FDROP THEN INVERT
   R> SETFPUCW
;

: SKIP1
   1- >R 1+ R>
;
   
: >FLOAT_ ( addr u -- F: r )  \ *
  PAST-COMMA 0! FALSE ?IS-COMMA !
  OVER C@ DUP [CHAR] - =    \ addr u c flag
  IF DROP SKIP1 >FLOAT-ABS_ FNEGATE_
  ELSE [CHAR] + = IF SKIP1 >FLOAT-ABS_
                  ELSE >FLOAT-ABS_
                  THEN
  THEN
;
: >FLOAT  ( addr u --  r )  \ *
  >FLOAT_ F>T ;

HEX

: FABORT
      ?IE IF FINIT C0000090  THROW THEN    \ invalid operation
      ?OE IF FINIT C0000091 THROW THEN     \ overflow
      ?ZE IF FINIT C000008E THROW THEN     \ divide by zero
;

DECIMAL
\ на выходе: x.xxxxxx

: fnormalize-big_ ( F: r -- F: r1 u ) \ *
    LOW-MODE
    FDUP_ F[LOG]_
    F>DS_ DUP
    F10X_ F/_
;

\ на выходе: x.xxxxxx
: fnormalize-small_ ( F: r -- r1 u ) \ *
  0 
  BEGIN
    F--DS_ 0=
  WHILE
    F10*_ 1+
  REPEAT 
;

: +Count ( char -- )
    CountBuf @ FLOAT-TIB + C!
    CountBuf 1+!
;
: FLOAT<1_ ( -- f )
      1 FD<_ DUP
      IF
         [CHAR] 0 +Count
         [CHAR] . +Count
      THEN
;         

\ выводим число, пришедшее как x.xxxxxx
: fprint-frac_ ( F: r D: u -- )
  TRUNC-MODE
  0
  DO           
    I 15 >
    IF 
      [CHAR] 0 +Count
    ELSE  
      F--DS_ DUP 10 =
      IF
        [CHAR] 1 +Count
        [CHAR] 0 +Count
      ELSE  
        DUP 48 + +Count
      THEN  
      DS>F_ F-_ F10*_
    THEN
  LOOP
  FDROP_
;

\ Вывести целую часть числа
: fprint-high_ ( F: r -- )
    fnormalize-big_ 1+
    fprint-frac_
;

\ выводим число в формате без экспоненты
: fprint-noexp_ ( F: r -- )
  
    TRUNC-MODE
    FLOAT<1_  0=
    IF
      FDUP_ FLOAT>DATA fprint-high_ DATA>FLOAT
      [CHAR] . +Count
      FDUP_ FINT_ F-_
    THEN   F10*_ FFORM @ fprint-frac_
;

: format-exp_ ( ud1 -- ud2 ) \ *
  UP-MODE      
  2DUP 2DUP D0= IF 2DROP 0 ELSE D>F_ F[LOG]_ F>DS_ THEN
  FFORM-EXP @ MAX 
  0
  DO # LOOP 
;

: Buf+word ( addr u )
    2DUP FLOAT-TIB CountBuf @ + SWAP CMOVE
    NIP CountBuf +!
;

\ выводим число в формате с экспонентой
: fprint-exp_ ( F: r -- )
   1 FD<_
   IF FDUP_ F0=_ IF 0 ELSE fnormalize-small_ -1 * THEN
   ELSE fnormalize-big_ 10 FD>_ IF F10/_ 1+ THEN
   THEN
   fprint-noexp_
   S>D
   DUP >R DABS <# format-exp_ R> SIGN [CHAR] E HOLD #>
   Buf+word
;
HEX

: >FNUM_ ( F: r  -- addr u )
      FABORT FDEPTH 0= IF C0000092 THROW THEN
      0 CountBuf ! 
      FDUP_ FINF_ F=_
      IF
         FDROP_ S" +Infinity" Buf+word
      ELSE 
        FDUP_ FINF_ FNEGATE_ F=_
          IF FDROP_ S" -Infinity" Buf+word
          ELSE
            GETFPUCW >R
            BASE @ DECIMAL 
            FDUP_ F0<_ IF FABS_ [CHAR] - +Count THEN
            ?PRINT-EXP @
            IF fprint-exp_
            ELSE fprint-noexp_
            THEN
            BASE !
            R> SETFPUCW
          THEN
      THEN  
      FLOAT-TIB CountBuf @
;
 
: >FNUM ( r  -- addr u )
     T>F >FNUM_ 
;

: F. ( F: r -- )
     T>F >FNUM_ TYPE
;   
DECIMAL

\ DB 2D - TBYTE
\ DD 05 - QWORD
\ D9 05 - DWORD

: FS. ( F: r -- )
    ?PRINT-EXP @
    PRINT-EXP
    F.
    ?PRINT-EXP !
;
: FLOOR ( F: r1 -- r2 )
    GETFPUCW >R
    LOW-MODE
    FINT
    R> SETFPUCW
;
: FROUND ( F: r1 -- r2 )
    GETFPUCW >R
    ROUND-MODE
    FINT
    R> SETFPUCW
;

: DFLOAT+ ( addr1 -- addr2 )
    8 +
;
: DFLOATS ( n1 -- n2 )
    3 LSHIFT
;
: SFLOAT+ ( addr1 -- addr2 )
    4 +
;
: SFLOATS ( n1 -- n2 )
    2 LSHIFT
;
: PRECISION ( -- u )
    FFORM @
;
: SET-PRECISION ( u -- )
    FFORM !
;

: FLIT,
    ['] _FLIT-CODE8 COMPILE,
    HERE 8 ALLOT DF!_ 
;

: FLITERAL' ( F: r -- )
      STATE @ IF FLIT, ELSE F>T  THEN
; IMMEDIATE

: FLITERAL ( F: r -- )
    T>F FLITERAL'
; IMMEDIATE


: ?FLOAT  ( addr u -- flag )  \ *
      DUP 1 = IF 2DROP 0 EXIT THEN          \ только 1 символ
      BASE @ 10 <> IF 2DROP 0 EXIT THEN     \ система не десятичная   
      2DUP 0 ROT ROT OVER + SWAP
      DO 
        I C@ 
        FCON-E @ = 
        IF 1+ THEN 
      LOOP
      DUP 0= IF 2DROP DROP 0 EXIT ELSE 1 > IF 2DROP 0 EXIT THEN THEN
      2DUP 0 ROT ROT OVER + SWAP
      DO 
        I C@ 
        [CHAR] . = 
        IF 1+ THEN 
      LOOP
      1 > IF 2DROP 0 EXIT THEN
      -1 ROT ROT OVER + SWAP 
      DO 
        I C@ DUP >R
        47 58 OVER - >R - R> U< 
        R@ FCON-E @ =
        R@ [CHAR] . =
        R@ [CHAR] - =
        R> [CHAR] + =
        OR OR OR OR 0= IF DROP 0 LEAVE THEN
      LOOP
;

\ преобразовать строку в число
: ?FLITERAL1 ( addr u -> ... )
  2DUP 2>R ['] ?SLITERAL3 CATCH DUP -2003 =
  IF DROP
    2DROP 2R>
    FABORT
    2DUP ?FLOAT
    IF >FLOAT_ INVERT IF FABORT THEN
       [COMPILE] FLITERAL'
    ELSE
       -2003 THROW
    THEN
  ELSE  2R> 2DROP THROW 
  THEN
;


: F~ ( F1 F2 F3 -- FLAG ) \ FLOAT-EXT
    3T>F
    FDUP_ F0=_
    IF
      FDROP_ F=_ EXIT
    THEN
    FDUP_ FDUP_ F0<_ F0=_ OR INVERT
    IF
      FROT_ FROT_ F-_ FABS_ FSWAP_
    ELSE
      FNEGATE_ FROT_ FROT_ FOVER_ FABS_ FOVER_ FABS_ F+_ FROT_ FROT_
      F-_ FABS_ FROT_ FROT_ F*_
    THEN
    F<_
;

: FALOG  \ *
      T>F   10.E_ FSWAP_ F**_ F>T
;

: FSINH \ *
      T>F  FEXP_ FDUP_ 1.E_ FSWAP_ F/_ F-_ 2.E_ F/_ F>T
;

: FCOSH T>F  FEXP_ FDUP_ 1.E_ FSWAP_ F/_ F+_ 2.E_ F/_ F>T ;

: FTANH T>F   2.E_ F*_ FEXPM1_ FDUP_ 2.E_ F+_ F/_ F>T ;

: FATANH T>F  FDUP F0< >R FABS 1.E FOVER F- F/  2.E F* FLNP1 2.E F/
           R> IF FNEGATE THEN F>T ;


: FASINH T>F  FDUP FDUP F* 1.E F+ FSQRT F/ FATANH F>T ;

: FTO 
   BL WORD FIND
   IF
     STATE @ 
     IF   >BODY LIT, ['] F! COMPILE,
     ELSE >BODY F!
     THEN
   ELSE -321 THROW
   THEN
; IMMEDIATE

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: FLOAT-VER
    ." Floating point support"   CR
    ." Developed by Dmitry Yakimov, day@forth.org.ru."  CR
    ." ver 1.6 - for SP-FORTH 3.70" CR
;
(
: FHYPTMP_ \ *
   F1+_ FLOG2_ FLN2_ F*_
;

)

: HIGH-FINIT
     10 FFORM !
     2 FFORM-EXP !
     FLONG
     FINIT
     PRINT-EXP
     [CHAR] e FCON-E !
     ['] ?FLITERAL1 TO ?SLITERAL
;

\ ' HIGH-FINIT TO <FLOAT-INIT>
HIGH-FINIT

: FALIGN
; IMMEDIATE
: FALIGNED 
; IMMEDIATE
: SFALIGN
; IMMEDIATE
: SFALIGNED
; IMMEDIATE
: DFALIGN
; IMMEDIATE
: DFALIGNED
; IMMEDIATE

: FVARIABLE 
      CREATE .e F, (DOES1) (DOES2)
;

: FCONSTANT   \ *
      CREATE F,
      (DOES1) (DOES2)  F@
;

: FVALUE FCONSTANT ;
