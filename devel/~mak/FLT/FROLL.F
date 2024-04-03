
: 3/FROLL SWAP >R DUP >R ROLL R@ ROLL R> ROLL R> ;

: FROLL 3 * 2+ \ 3(x+1)-1 = 3x+2
   3/FROLL ;

: 3/FPICK SWAP >R DUP >R PICK R@ PICK R> PICK R> ;

: FPICK 3 * 2+ 3/FPICK ;

0 [IF]

CODE: -7FROLL_
      FXCH  ST(7)
      FXCH  ST(6)
      FXCH  ST(5)
      FXCH  ST(4)
      FXCH  ST(3)
      FXCH  ST(2)
      FXCH  ST(1)
      RET
;CODE

HERE 1- CONSTANT 0ROLL-CFA_

: -FROLL_ -2 * 0ROLL-CFA_ + EXECUTE ;

: FROLL_ DUP 0 DO DUP >R -FROLL_ R> LOOP DROP ;

CODE: 3FPICK_  \ *
       FLD ST(2)
       RET
;CODE
CODE: 4FPICK_  \ *
       FLD ST(3)
       RET
;CODE

CODE: 5FPICK_  \ *
       FLD ST(4)
       RET
;CODE

CODE: 6FPICK_  \ *
       FLD ST(5)
       RET
;CODE

CODE: 7FPICK_  \ *
       FLD ST(6)
       RET
;CODE

CODE: 8FPICK_  \ *
       FLD ST(7)
       RET
;CODE

CREATE FPICK_-TAB
'  FDUP_   , '   FOVER_ , '  3FPICK_ , '  4FPICK_ ,
'  5FPICK_ , '  6FPICK_ , '  7FPICK_ , '  8FPICK_ ,

: FPICK_ ( n -- )
  CELLS FPICK_-TAB + @
  STATE @ IF    COMPILE, EXIT
          THEN  EXECUTE ;   IMMEDIATE
[THEN]


