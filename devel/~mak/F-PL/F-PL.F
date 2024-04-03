\ F-PL                                             MAKSIMOV M.O.
S" ~mak\F-PL\V_ST1.F" INCLUDED


C" <=" FIND NIP 0=
[IF] : <= > 0= ;
[THEN]

C" >=" FIND NIP 0=
[IF] : >= < 0= ;
[THEN]

C" 2," FIND NIP 0=
[IF] : 2, HERE 2! 2 CELLS ALLOT ;
[THEN]


FORTH DEFINITIONS


VOCABULARY ]-VOC0
VOCABULARY ]-VOC
VOCABULARY F-PL

ONLY FORTH
ALSO M_FORTH_1
ALSO F-PL DEFINITIONS
ALSO FORTH


 INIT_MODELS

 CREATE OP HERE 0x20 CELLS ALLOT  HERE SWAP !  \ STACK OF OPERATIONS

 : >OP  ( A -- ) -CELL  OP +! OP @ !  ;
 :  OP@ ( -- A )
    OP @ @   ;
 :  OP> ( -- A )  [ OP @ ] LITERAL @ OP @ =
 ABORT" BRACKET IS EXPECTED"
   OP@  CELL OP +! ;


 : >OP> ( N -- )   \ N IS PRIORITY
        DUP >R
        BEGIN OP@ <=
        WHILE OP> DROP 
              OP> EXECUTE R@
        REPEAT RDROP ;

:  #2-OP ( N -- )  \ N IS PRIORITY
     CREATE IMMEDIATE 2,
     DOES>  2@ >R
            DUP >R >OP>
           R> R>  >OP >OP ;

:   2-OP ( N -- )  \ N IS PRIORITY
     >IN @  ' SWAP
     >IN !  #2-OP ;

 : 1-OP 10 2-OP ;

0 VALUE ?:=

ALSO M_FORTH_2

WARNING 0!
   3 2-OP OR  3 2-OP XOR  4 2-OP AND    5 2-OP =
   6 2-OP <   6 2-OP >
   6 2-OP <=  6 2-OP >=
   7 2-OP +   7 2-OP -
   8 2-OP *   8 2-OP /    8 2-OP MOD
     1-OP NOT   1-OP ABS    1-OP NEGATE
 2 ' IF     #2-OP IIF        \ не из FORTH
 2 ' WHILE  #2-OP W-L
 2 ' UNTIL  #2-OP U-L

PREVIOUS

: ( 0 >OP  ; IMMEDIATE
: ) 1 >OP>  OP> DROP ; IMMEDIATE

: }  \  EXIT FROM F-PL
   [COMPILE] ) 
  0 TO ?:=    PREVIOUS
  [ OP @ ] LITERAL OP @ <> 
   ABORT" BRACKET IS EXPECTED"
; IMMEDIATE

: ;  \  OPERATOR :=  CLOSES
    [COMPILE] }
     _!_  OVER WRD (WRD) -  +  >R _!_ R>   >>TYPE
      SWAP_V !_V
; IMMEDIATE

WARNING ON

: ]] ] ;

: ]0   0 TO ?:=  PREVIOUS +_V ;

: ]1  +_V @_V ;

: ] [COMPILE] )      \  AT USE OF A ARRAY UP TO :=
;   IMMEDIATE

      ALSO ]-VOC DEFINITIONS

: ]   [COMPILE] )      \  AT USE OF A ARRAY DOWN TO :=
    ."  ]> "  +_V @_V
      PREVIOUS
;  IMMEDIATE


           ]-VOC0 DEFINITIONS

: ]  PREVIOUS          \  AT DEFINITIONS ARRAY
   [COMPILE]  } *_V
     ?WRD ALLOT 
;  IMMEDIATE


ONLY FORTH ALSO M_FORTH_1
ALSO F-PL ALSO M_FORTH_2 DEFINITIONS


: :=   ALSO F-PL       \ OPERATOR OF ASSIGNMENT
     [ FORTH OP @  F-PL ]] LITERAL OP !
     [COMPILE]  ( TRUE TO ?:=
     [ FORTH ]
;    IMMEDIATE

M_FORTH_2

: F-PL{ POSTPONE :=
     [ FORTH ]
;  IMMEDIATE

M_FORTH_2 

: IF{
     [COMPILE]  :=
     [  F-PL ]]
     [COMPILE]  IIF
     [  FORTH ]
 ;   IMMEDIATE

M_FORTH_2

: WHILE{
     [COMPILE]  :=
     [  F-PL ]]
     [COMPILE]   W-L
     [  FORTH ]
;  IMMEDIATE

M_FORTH_2

: UNTIL{
     [COMPILE]  :=
     [  F-PL ]]
     [COMPILE]   U-L
     [  FORTH ]
 ;  IMMEDIATE

M_FORTH_2

: VAR[]
       [ ALSO FORTH ]
       STATE @
       IF  [COMPILE] LITERAL
       THEN   ?:=
       IF   [ F-PL ]]      [COMPILE] (  ['] ]1 >OP 2 >OP [ FORTH ]
       ELSE [ M_FORTH_2 ]] [COMPILE] := ['] ]0 >OP 2 >OP [ FORTH ]
       THEN ;
PREVIOUS

: VAR_I[]
     CREATE IMMEDIATE
            [COMPILE] F-PL{ ALSO ]-VOC0
            2 M_ WRD
     DOES>  VAR[] M_ (WRD)   [ ALSO FORTH ]
     ; IMMEDIATE
PREVIOUS
: VAR_I
     CREATE
        [ ALSO FORTH ]
         >IN @  BL WORD                       \ GET A WORD
                COUNT S" :=" COMPARE
                IF   >IN ! 0 ,
                ELSE DROP M_ (WRD)
                     HERE CELL ALLOT
                     [COMPILE] :=
                     INTERPRET
                THEN IMMEDIATE
     DOES>    M_ (WRD)
       STATE @
       IF  [COMPILE] LITERAL
       THEN  ?:=
       IF    @_V
       THEN ;
PREVIOUS
: VAR_D[]
     CREATE IMMEDIATE
            [COMPILE] F-PL{ ALSO ]-VOC0
            4 M_ WRD
     DOES>  VAR[] M_ (DBL)   [ ALSO FORTH ]
       ; IMMEDIATE
PREVIOUS

: VAR_D
        [ ALSO FORTH ]
     CREATE  >IN @  BL WORD                                 \ GET A WORD
                COUNT S" :=" COMPARE
                IF   >IN ! 0 , 0 ,
                ELSE DROP  M_ (DBL)
                     HERE  2 CELLS ALLOT
                     [COMPILE] :=
                     INTERPRET
                THEN IMMEDIATE
     DOES>    M_ (DBL)
       STATE @
       IF  [COMPILE] LITERAL
       THEN  ?:=
       IF    @_V
       THEN ;
PREVIOUS

: VAR_F[]
     CREATE IMMEDIATE
            [COMPILE] F-PL{ ALSO ]-VOC0
            6 M_ WRD
     DOES>  VAR[] M_ (FLT)   [ ALSO FORTH ]
       ; IMMEDIATE
PREVIOUS

: VAR_F
        [ ALSO FORTH ]
     CREATE  >IN @ BL WORD                                 \ GET A WORD
                COUNT S" :=" COMPARE
                IF   >IN ! 0e F, 
                ELSE DROP  M_ (FLT)
                     HERE  1 FLOATS ALLOT
                     [COMPILE] :=
                     INTERPRET
                THEN IMMEDIATE
     DOES>   M_ (FLT)
       STATE @
       IF  [COMPILE] LITERAL
       THEN   ?:=
       IF     @_V
       THEN ;


ONLY FORTH ALSO DEFINITIONS

