
: _ASSERT
 CR ." Assertion failed: file " TYPE ." , line " . CR ABORT
;
:  doassert
        CURSTR @
        POSTPONE  LITERAL
        CURFILE @ ?DUP IF ASCIIZ> ELSE S" H-STDIN" THEN
        POSTPONE SLITERAL
        POSTPONE _ASSERT
;
: assert ( flag -- )
 POSTPONE 0= 
 POSTPONE IF doassert
 POSTPONE THEN ;   IMMEDIATE
: _ZZZ CR ." zz=" . . CR ;
:  ZZZ POSTPONE _ZZZ  doassert ; IMMEDIATE
: (Z  POSTPONE ZZZ POSTPONE (* ; IMMEDIATE
: \Z  POSTPONE ZZZ POSTPONE \  ; IMMEDIATE

