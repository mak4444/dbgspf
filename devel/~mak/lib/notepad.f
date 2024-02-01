REQUIRE SYSCALL0 ~mak\lib\syscall.f 
CREATE EDITBUFF 256 ALLOT
: $EDIT 
 
 S" C:\Program Files\Notepad++\notepad++.exe -lnormal -n" EDITBUFF $!
 0 (D.)  EDITBUFF $+!
  BL  EDITBUFF $C+!
 EDITBUFF $+! 
  EDITBUFF +NULL
 EDITBUFF  COUNT SYSCALL0
;
 
: N++ ' NEAR_NFA DROP ?DUP 
  IF 9 - DUP @ COUNT 2>R CELL- @  2R> ROT
  $EDIT 
  THEN
;

: N++_ERROR ( ERR-NUM -> ) \ показать расшифровку ошибки
  [ ' ERROR >BODY @ COMPILE, ]
  ERR-FILE NIP \ cmdDBG ED_WATE <> AND
  IF   ERR-FILE 
        ERR-LINE# $EDIT
  THEN
;

: N++_ERR_SET ['] N++_ERROR TO ERROR  ;

CREATE CUR-NFILEBUFF 256 ALLOT

: $CUR-NFILE  CUR-NFILEBUFF $! ;

: CUR-NFILE  PARSE-NAME $CUR-NFILE ;

: N++ED  CUR-NFILEBUFF COUNT 0  $EDIT  ;

: NLOED  CUR-NFILEBUFF COUNT INCLUDED ;

\ S" D:\masm32\asmspf\src\macroopt.f" 111 $EDIT 
\ S" notepad++" SYSCALL0
\ S" notepad++ -lnormal -n77 D:\masm32\asmspf\src\macroopt.f" SYSCALL0
