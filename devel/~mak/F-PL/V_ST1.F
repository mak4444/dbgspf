\                                     MAKSIMOV M.O.
\  DATA TYPES INDEPENDENCE
\ EARLY LINKAGE
\ NEEDS DMULDIV.SEQ
~mak\F-PL\M_KERN.F
~mak\flt\flhi.f
~mak\flt\froll.f
REQUIRE CASE  lib\ext\case.f
REQUIRE SEE lib\ext\disasm.f 


C" BOUNDS" FIND NIP 0=
[IF] : BOUNDS OVER + SWAP ;
[THEN]

C" MU/MOD" FIND NIP 0=
[IF]
: MU/MOD        ( ud# un1 -- rem d#quot )
\  Divide unsigned double by a single, leaving a remainder and quotient.
                >R  0  R@  UM/MOD  R>  SWAP  >R  UM/MOD  R>   ;
[THEN]

C" BETWEEN" FIND NIP 0=
[IF] : BETWEEN 1+ WITHIN ;
[THEN]

C" LCOUNT" FIND NIP 0=
[IF] : LCOUNT   CELL+ DUP CELL- @ ; 
[THEN]

C" -CELL" FIND NIP 0=
[IF] -1 CELLS CONSTANT -CELL
[THEN]

C" OFF" FIND NIP 0=
[IF] : OFF 0! ;
[THEN]

C" ON" FIND NIP 0=
[IF] : ON -1 SWAP ! ;
[THEN]

\- \S : \S \EOF ;

FORTH ALSO  DEFINITIONS

: 2CELLS CELLS 2* ;
: 3+ 2+ 1+ ;

VARIABLE S0SAVE
VOCABULARY M_FORTH_2
VOCABULARY M_FORTH_1
-8 CONSTANT EXIT_COD

 CLASS: WRD
 CLASS: DBL
 CLASS: FLT
 CLASS: (WRD)
 CLASS: (DBL)
 CLASS: (FLT)
 CLASS: (())

M_FORTH_1 DEFINITIONS

 INIT_MODELS

: ?COMPILE,   STATE @
              IF   COMPILE, EXIT
              THEN EXECUTE  ;

: M_IMPORT:
    BEGIN
      BL WORD DUP C@
      IF  DUP COUNT S" ;M_IMPORT"  COMPARE 0= IF DROP EXIT THEN
              FIND  0= IF -321 THROW THEN
              >R :  R> COMPILE,  POSTPONE ;  IMMEDIATE
      ELSE    DROP REFILL  0=  IF EXIT THEN
      THEN
    AGAIN   ;

: M_1:   \  DETERMINANT OF A SENDING
        CREATE  DOES>  M_PUSH 40 UMIN +
              STATE @ IF      @ COMPILE, EXIT
                      THEN     @ EXECUTE ;

: M_DUP:   \  DETERMINANT OF A SENDING
        CREATE  DOES>  M_DUP_PUSH 40 UMIN +
              STATE @ IF      @ COMPILE, EXIT
                      THEN     @ EXECUTE ;

: M_I:   \  DETERMINANT OF A SENDING
        CREATE  DOES>  M_PUSH 40 UMIN +   _!_ WRD _!_
              STATE @ IF      @ COMPILE, EXIT
                      THEN     @ EXECUTE ;

M_DUP: DUP_2   ' DUP , ' 2DUP ,  ' FDUP ,
               ' DUP , '  DUP ,  '  DUP , M_;

: M_BOUNDS   
        M_DUP_PUSH
        CASE
        DBL  OF 1  ENDOF
        FLT  OF 2  ENDOF
        DROP 0 DUP
        ENDCASE 
  MODELS @ CELL+ ROT CELLS BOUNDS
  OVER MODELS0 > ABORT" STACK UNDERFLOW" ;

0 VALUE ALL_FLT?
: ?_R>  ALL_FLT? IF  POSTPONE  R> THEN  ;
: ?_>R  ALL_FLT? IF  POSTPONE  >R THEN  ;

:  L>R_SD    ( n -- n1 )
        TRUE TO ALL_FLT?
     M_BOUNDS  
     DO I @ CASE
        WRD  OF 1+ FALSE TO ALL_FLT? ENDOF
        DBL  OF 2+ FALSE TO ALL_FLT? ENDOF
        FLT  OF 3+                   ENDOF
       (WRD) OF 1+ FALSE TO ALL_FLT? ENDOF
       (DBL) OF 1+ FALSE TO ALL_FLT? ENDOF
       (FLT) OF 1+ FALSE TO ALL_FLT? ENDOF
       (())  OF 1+ FALSE TO ALL_FLT? ENDOF
             ENDCASE
CELL +LOOP
;

: WRD_ROLL2   L>R_SD
  STATE @ IF  ?_>R
              [COMPILE] LITERAL
               POSTPONE  ROLL
              ?_R>  EXIT
          THEN 
 ALL_FLT? IF SWAP >R ROLL R> EXIT
          THEN  ROLL ;


: DBL_ROLL2   L>R_SD 
  STATE @ IF ?_>R
                 DUP [COMPILE] LITERAL
                     POSTPONE  ROLL
                     [COMPILE] LITERAL
                     POSTPONE  ROLL
             ?_R>    EXIT
           THEN
 ALL_FLT?  IF SWAP >R DUP >R ROLL R> ROLL R>  EXIT
           THEN       DUP >R ROLL R> ROLL ;

: FLT_ROLL2   L>R_SD 
             STATE @
              IF [COMPILE] LITERAL
                  POSTPONE 3/FROLL  EXIT
              THEN         3/FROLL  ;

M_: ROLL2
 ' WRD_ROLL2 , ' DBL_ROLL2 , ' FLT_ROLL2 ,
 ' WRD_ROLL2 , ' WRD_ROLL2 , ' WRD_ROLL2 ,   M_;

: WRD_ROLL DUP >R  _!_ R> ROLL DUP _!_ ROLL2 ;

: WRD_PICK2   L>R_SD 
   STATE @
       IF ?_>R
          [COMPILE] LITERAL
           POSTPONE  PICK
           ?_R> EXIT
       THEN
 ALL_FLT?  IF SWAP >R PICK R>  EXIT
           THEN       PICK ;

: DBL_PICK2   L>R_SD 
   STATE @
       IF ?_>R
          DUP [COMPILE] LITERAL
              POSTPONE  PICK
              [COMPILE] LITERAL
              POSTPONE  PICK
           ?_R> EXIT
       THEN
 ALL_FLT?  IF SWAP >R DUP >R PICK R> PICK R>  EXIT
           THEN       DUP >R PICK R> PICK ;

: FLT_PICK2  L>R_SD 
              DUP >R
             STATE @
              IF   POSTPONE >R
                 [COMPILE] LITERAL
                  POSTPONE PICK
              R@ [COMPILE] LITERAL
                  POSTPONE PICK
              R> [COMPILE] LITERAL
                  POSTPONE PICK
                  POSTPONE R> EXIT
              THEN  3/FPICK RDROP ;

M_: PICK2
 ' WRD_PICK2 ,  '  DBL_PICK2 , ' FLT_PICK2 ,  M_;

: WRD_PICK  DUP >R  _!_ R> PICK DUP _!_ PICK2 ;


VECT RED_TYPE  \ REDUCTION OF TYPES
: WRD_RED M_PUSH
        CASE
        DBL OF STATE @
               IF POSTPONE  S>D
               ELSE        S>D
               THEN     _!_ DBL _!_  ENDOF
        FLT OF
 STATE @
               IF POSTPONE  DS>F
               ELSE         DS>F
               THEN     _!_ FLT _!_  ENDOF
            M_ERR
        ENDCASE ;
: >S>D>  2>R S>D 2R> ;
: DBL_RED M_PUSH
        CASE
        WRD OF STATE @
               IF POSTPONE  >S>D>
               ELSE         >S>D>
               THEN  _!_ DBL _!_  ENDOF
        FLT OF STATE @
               IF POSTPONE  D>F
               ELSE         D>F
               THEN   _!_ FLT _!_  ENDOF
            M_ERR
        ENDCASE ;
: FLT_RED M_PUSH
        CASE
        WRD OF STATE @
               IF      POSTPONE  >R
                    3 [COMPILE] LITERAL
                       POSTPONE  ROLL
                       POSTPONE  R>
                       POSTPONE  DS>F
               ELSE >R  3 ROLL  R>  DS>F
               THEN   _!_ FLT _!_  ENDOF
        DBL OF STATE @
               IF      POSTPONE  >R
                    3 [COMPILE] LITERAL
                       POSTPONE  ROLL
                    3 [COMPILE] LITERAL
                       POSTPONE  ROLL
                       POSTPONE  R>
                       POSTPONE    D>F
               ELSE >R  3 ROLL 3 ROLL R> D>F
               THEN  _!_ FLT _!_  ENDOF
            M_ERR
        ENDCASE
                STATE @
                IF POSTPONE FSWAP
                ELSE        FSWAP
                THEN
 ;

M_: RED_TYPE2  ' WRD_RED , ' DBL_RED , ' FLT_RED , M_;

:  (RED_TYPE) _!_  2DUP = >R _!_ R>
        IF  M_DROP
        ELSE  RED_TYPE2
        THEN
;
' (RED_TYPE) TO RED_TYPE

CREATE  TAB_R_T
    ' NOOP , ' S>D  , ' DS>F ,
    ' D>S  , ' NOOP , '  D>F ,
    ' F>DS , ' F>D  , ' NOOP ,

: >>TYPE ( N -- )
    M_PUSH OVER M_POP
    3 * + 9  CELLS   /MOD ABORT" TYPE ERROR"
    TAB_R_T + @
      STATE @   IF   COMPILE, 
                ELSE EXECUTE
                THEN
;

: >WRD WRD >>TYPE ;
: >DBL DBL >>TYPE ;
: >FLT FLT >>TYPE ;

: D*  D>F_ D>F_        F*_ F>D_  ;
: D/  D>F_ D>F_ FSWAP_ F/_ F>D_  ;
: DMOD  ( UD# UN1 --  REM )
          MU/MOD  2DROP ;

M_1:  +_2      ' +   ,  '  D+   ,  ' F+ ,  M_;
M_1:  -_2      ' -   ,  '  D-   ,  ' F- ,  M_;
M_1:  *_2      ' *   ,  '  D*   ,  ' F* ,  M_;
M_1:  /_2      ' /   ,  '  D/   ,  ' F/ ,  M_;
M_1:  >_2      ' >   ,  '  D>   ,  ' F> ,  M_;
M_1:  <_2      ' <   ,  '  D<   ,  ' F< ,  M_;
M_1:  =_2      ' =   ,  '  D=   ,  ' F= ,  M_;
M_1:  =_2      ' =   ,  '  D=   ,  ' F= ,  M_;
M_1:  MOD_2    ' MOD ,  '  DMOD ,       M_;

\ \

: V_?SLITERAL1 ( c-addr u -> ... )
  \ преобразовать строку в число
  0 0 2SWAP
  OVER C@ [CHAR] - = IF 1- SWAP 1+ SWAP TRUE ELSE FALSE THEN >R
  >NUMBER
  DUP 1 > IF -2001 THROW THEN \ ABORT" -?"
  IF C@ [CHAR] . <> IF -2002 THROW THEN \ ABORT" -??"
       R> IF DNEGATE THEN
       [COMPILE] 2LITERAL   _!_ DBL  _!_
  ELSE DROP D>S
       R> IF NEGATE THEN
       [COMPILE] LITERAL    _!_ WRD  _!_
  THEN
;

: V_?SLITERAL2 ( c-addr u -- ... )
  ( расширенный вариант ?SLITERAL1:
    если строка - не число, то пытаемся трактовать её
    как имя файла для авто-INCLUDED)
  2DUP 2 MIN S" 0x" COMPARE 0= 
  IF HHH-SLITERAL 0= THROW _!_ WRD  _!_
 EXIT THEN
  2DUP 2>R ['] V_?SLITERAL1 CATCH
  IF   2DROP 2R>
       OVER C@ [CHAR] " = IF 2 - SWAP 1+ SWAP THEN ( убрал кавычки, если есть)
       2DUP + 0 SWAP C!
       ['] INCLUDED CATCH
       DUP 2 = OVER 3 = OR ( файл не найден или путь не найден )
       IF -2003 THROW \ ABORT"  -???"
       ELSE THROW THEN
  ELSE 2R> 2DROP
  THEN
;

: V_?FLITERAL1 ( addr u -> ... )
  2DUP 2>R ['] V_?SLITERAL2 CATCH
  IF
    2DROP 2R>
    FABORT
    2DUP ?FLOAT
    IF >FLOAT_ INVERT IF FABORT THEN
       [COMPILE] FLITERAL' _!_ FLT  _!_
    ELSE
       -2003 THROW
    THEN
  ELSE 2R> 2DROP
  THEN
;


\ - WORKING WORDS -

:  DUP_V   M_DUP  DUP_2  ;

M_:  DROP_V '     DROP , '     2DROP  ,  '    FDROP ,
            '     DROP , '      DROP  ,  '     DROP ,  M_;
M_1: ._V    '        . , '        D.  ,  '    F.    ,
            '       U. , '        U.  ,  '    U.    ,  M_;

: SWAP_V  1 WRD_ROLL ;
: ROT_V   2 WRD_ROLL ;
: OVER_V  1 WRD_PICK ;

M_1: CELLS_V  ' CELLS , ' 2CELLS , ' FLOATS , ' CELLS , M_;

: +_V 
       M_DUP_PUSH WRD =
       IF _!_ OVER >R _!_ R>
           (WRD) (()) BETWEEN
           IF _!_ DROP DUP (WRD) -  _!_ CELLS_V 
              ['] + ?COMPILE, EXIT
           THEN
       THEN
       M_DUP_PUSH
       (WRD) (()) BETWEEN
       IF M_ SWAP M_PUSH WRD <>
            IF M_ERR THEN
            ['] SWAP ?COMPILE, CELLS_V
            ['] +    ?COMPILE, EXIT
       THEN
       RED_TYPE M_DUP +_2 ;
: -_V  RED_TYPE M_DUP -_2 ;
: *_V  RED_TYPE M_DUP *_2 ;
: /_V  RED_TYPE M_DUP /_2 ;
: >_V  RED_TYPE  >_2   _!_ WRD _!_ ;
: <_V  RED_TYPE  <_2   _!_ WRD _!_ ;
: ?WRD  M_DUP_PUSH WRD <> ABORT" NEED WRD " ;
: 0=_V ?WRD ['] 0= ?COMPILE, ;
: <=_V >_V 0=_V ;
: >=_V <_V 0=_V ;
: =_V  RED_TYPE  =_2   _!_ WRD _!_ ;
\ : <> RED_TYPE <>_2 _!_ WRD _!_ ;
: MOD_V ?WRD _!_ SWAP _!_
        MOD_2 ;
M_DUP: NEGATE_V  ' NEGATE , ' DNEGATE , ' FNEGATE ,  M_;
M_DUP:    ABS_V  '    ABS , '    DABS , '    FABS ,  M_;
: AND_V RED_TYPE   ?WRD ['] AND ?COMPILE, ;
: XOR_V RED_TYPE   ?WRD ['] XOR ?COMPILE, ;
: OR_V  RED_TYPE   ?WRD ['] OR  ?COMPILE, ;
M_I: 0=_V   ' 0=  ,  ' D0= , ' F0= ,  M_;
: D0<> D0= 0= ;
: F0<> F0= 0= ;
M_I: 0<>_V  ' 0<> ,  ' D0<> , ' F0<> ,  M_;
\ : D0<  NIP 0< ;

M_1: (@)  '  @ , ' 2@ , ' F@ , ' @ , M_;
: @_V  _!_ DUP  (WRD) (FLT) BETWEEN 0= IF M_ERR THEN
       (WRD) - DUP 
       _!_ (@)
       _!_ DUP (WRD) =       \   (())
       IF  (WRD) +  THEN
       _!_  ;
M_1: (!)  '  ! , ' 2! , ' F! , ' ! , M_;
: !_V  _!_ DUP
       (WRD) U< IF M_ERR THEN
       (WRD) -  TUCK 
        DUP  (WRD) =
       IF    (WRD) - THEN
       <> IF M_ERR THEN 
       _!_   (!) 
;

VARIABLE   CSP    \ Указатель стека контроля

\  AND ETC.
:  M_?CSP
   BEGIN    SP@ CSP @ <>
   WHILE              \  CHECK AND REMOVAL OF  DATA TYPES
          M_PUSH <> ABORT" NOT CONCURRENCE OF TYPES"
   REPEAT   CSP !
;
CREATE   SAVE_VOCS[] 10 1+ CELLS ALLOT
VARIABLE SAVE_CURRENT 

: SAVE_VOCS
   CURRENT @ SAVE_CURRENT !
   GET-ORDER  SAVE_VOCS[] !
   SAVE_VOCS[] LCOUNT 1- CELLS OVER +  DO I ! -CELL +LOOP
;

: REST_VOCS
   SAVE_CURRENT @ CURRENT !
   SAVE_VOCS[]  LCOUNT CELLS
   BOUNDS  DO  I @ CELL +LOOP
   SAVE_VOCS[] @  SET-ORDER
;

: :-VAR    CREATE IMMEDIATE , DOES> LCOUNT M_POP POSTPONE LITERAL  ;

ALSO  M_FORTH_2 DEFINITIONS

: {  \  IS APPLIED IN A KIND:
     \  {  DATATYPES }RETURN
      CSP @ SP@ CSP !
  ;  IMMEDIATE

: RET   \ ТИПА THROW
     HANDLER @ RP! 
     R> HANDLER !
     R> DROP
     EXIT_COD   \ В качестве нормального выхода
;     IMMEDIATE

: }RETURN
\  IS APPLIED IN A KIND:
           \  {  DATA TYPES }RETURN
           \  RETUIRN FROM DATA TYPES INDEPENDED FORTH.
           \  DATA TYPES IS WORDS DEFINED OF CLASS:
           M_?CSP 
     HANDLER @ RP! 
     R> HANDLER !
     R> DROP
           EXIT_COD
;          IMMEDIATE

: }       \  IS APPLIED IN A KIND:
           \  M_{  DATA TYPES }
          _!_
  S0SAVE @ S0 !
;             IMMEDIATE


   WARNING OFF
M_IMPORT:
 WRD   WRD    DBL   DBL    FLT   FLT
(WRD) (WRD)  (DBL) (DBL)  (FLT) (FLT)
 _!_  _!_  M_ M_  NOOP NOOP
DUP_V DUP  DROP_V DROP ._V .  
+_V + -_V - SWAP_V SWAP OVER_V OVER ROT_V ROT
*_V *  /_V / MOD_V MOD  >_V  >  <_V <  =_V =  0=_V 0= 0=_V NOT
AND_V AND  OR_V OR  0=_V 0=  XOR_V XOR
NEGATE_V NEGATE ABS_V ABS  !_V ! @_V @
SEE SEE  \S \S  ( (  ." ."   CR CR
ELSE ELSE  THEN THEN  THEN ENDIF
BEGIN BEGIN  REPEAT REPEAT
;M_IMPORT

:  \  POSTPONE  \  ; IMMEDIATE
: IF    ?WRD M_DROP [COMPILE] IF ;    IMMEDIATE
: WHILE ?WRD M_DROP [COMPILE] WHILE ; IMMEDIATE
: UNTIL ?WRD M_DROP [COMPILE] UNTIL ; IMMEDIATE

: WRD-CRE POSTPONE (WRD) :-VAR ;
: DBL-CRE POSTPONE (DBL) :-VAR ;
: FLT-CRE POSTPONE (FLT) :-VAR ;

: WRD-VAR WRD-CRE 0 , ;
: DBL-VAR DBL-CRE 0 , 0 , ;
: FLT-VAR FLT-CRE 0e F,   ;

ALSO FORTH DEFINITIONS

   WARNING ON

ONLY FORTH  ALSO

: M1_QUIT 
   S0 @ S0SAVE !
   [ M_FORTH_1  ] _!_  SP@ S0 !
  BEGIN INTERPRET REFILL 0=
  UNTIL [ FORTH ]
;

:  M1_{    \  IS APPLIED IN A KIND:
           \  M_{  DATA TYPES }
           \  TRANSITION IN  TYPES INDEPENDED FORTH.
           \  DATA TYPES IS WORDS DEFINED OF CLASS:
   [ M_FORTH_1  ]
   SAVE_VOCS  ONLY M_FORTH_2  DEFINITIONS
   ['] ?SLITERAL >BODY @ >R
   ['] V_?FLITERAL1 TO ?SLITERAL
   ['] M1_QUIT CATCH
   REST_VOCS
   R> TO ?SLITERAL
   ?DUP IF DUP EXIT_COD <> 
          IF THROW
          THEN DROP
        THEN
   [ FORTH ]
;    IMMEDIATE

ONLY FORTH DEFINITIONS

