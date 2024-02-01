\ наблюдение работы методов оптимизазии			Михаил Максимов

CREATE    DTST-TAB   0x800 CELLS ALLOT
          DTST-TAB   0x800 CELLS ERASE 
CREATE OldDTST-TAB   0x800 CELLS ALLOT
       OldDTST-TAB   0x800 CELLS ERASE 

REQUIRE REST lib\ext\disasm.f 

C" H." FIND NIP 0=
[IF]  : H. BASE @ >R HEX U. R> BASE ! ;
[THEN]

: CntDTST
 DUP 1 AND 0= 
 IF   DUP 2 =
      OVER 4 = OR
      OVER 4 = OR  OFF-EAX 0= AND 
     IF DROP EXIT THEN
     DUP  0x800 2* 1- U>
     IF DROP EXIT THEN
     2*  DTST-TAB +  1+! 
 ELSE DROP THEN ;

: R.
  >R 0 <# #S #> R> OVER - 0 MAX DUP 
    IF 0 DO SPACE LOOP
    ELSE DROP THEN TYPE 
;

: SAVE-CntDTST ( S" DTST.F" -- )
  H-STDOUT >R
   R/W CREATE-FILE THROW TO H-STDOUT
   0x800 0 DO I CELLS DTST-TAB + @
              ?DUP IF CR 6 R. I 2* ."  0x" H. ." >OT" THEN
           LOOP  CR
  H-STDOUT CLOSE-FILE
  R> TO H-STDOUT
  THROW
;
: >OT 2*  OldDTST-TAB + ! ;
: SUBTAB.
   0x800 0 DO I CELLS    DTST-TAB + @
              I CELLS OldDTST-TAB + @ -
              ?DUP IF CR . I 2* ."  0x" H. ." >OT" THEN
           LOOP  CR
;
: DoDTST
  :-SET >R
  J-SET >R
 BASE @ >R HEX
        >R
 DUP 1 AND 0= 
 IF   CR SOURCE TYPE
 THEN   
     BEGIN  CR R@ . R@ 0< R@ 1 AND 0= OR IF ." *" DUP @ U. THEN
                2DUP U. U.  ." J=" J-SET U. :-SET U. ." EAX+" OFF-EAX U.
            J-SET OP8 @ UMAX DUP U.  REST
           KEY DUP 
           [CHAR] Q = THROW
           [CHAR] D <> DUP 0=
           IF  DROP  S" [ DDD.F ] " EVALUATE KEY DROP 
                    \ S" [  ] " EVALUATE
               FALSE
           THEN   
     UNTIL
  RDROP
 R> BASE ! 
 R> TO J-SET
 R> TO :-SET
 ;

\ [ ' DoDTST TO DTST ]
