( ������������� USER-����������.
  Copyright [C] 1992-1999 A.Cherezov ac@forth.org
  �������� 1999
)

VARIABLE MAINX
ALIGN-BYTES-CONSTANT CONSTANT ALIGN-BYTES-CONSTANT
TC-USER-HERE ALIGNED ' USER-OFFS EXECUTE !

: AT-THREAD-STARTING ( -- ) ...  ;
: AT-PROCESS-STARTING ( -- ) ... ;
: AT-THREAD-FINISHING ( -- ) ... ;

: POOL-INIT ( n -- )
  EXC-HANDLER 0!
  SP@  + CELL+ S0 !
  RP@ R0 !
  DECIMAL
  ATIB TO TIB
  0 TO SOURCE-ID
  0 TO SOURCE-ID-XT
  S-O TO CONTEXT FORTH DEFINITIONS
  POSTPONE [
  HANDLER 0!
  CURSTR 0!
  CURFILE 0!
  INCLUDE-DEPTH 0!
  TRUE WARNING !
  12 C-SMUDGE !
  ALIGN-BYTES-CONSTANT ALIGN-BYTES !
  INIT-MACROOPT-LIGHT
;

: USER-INIT ( n )
\ n - ������ ����������, �-� Windows �������� callback ��������� (� ������)
  CREATE-HEAP
  <SET-EXC-HANDLER>
  POOL-INIT
  AT-THREAD-STARTING  
;

: ERR-EXIT ( xt -- )
  CATCH
  ?DUP IF ['] ERROR CATCH IF 4 ELSE 3 THEN HALT THEN
  \ ������� � ����� ������ 3, ���� ������� ������ ��� ������������� 
  \ 4 - ���� ���������
;

: (ADDR.) BASE @ >R HEX 8 .0 R> BASE ! ;
: ADDR. ( n -- ) (ADDR.) SPACE ;


\ ���� ��� �� ������
: PROCESS-INIT ( n )
  ERASE-IMPORTS
  ['] NOOP       TO <PRE>
  ['] FIND1      TO FIND
  ['] ?LITERAL3  TO ?LITERAL
  ['] ?SLITERAL3 TO ?SLITERAL
  ['] OK1        TO OK.
  ['] ERROR2     TO ERROR
  ['] (ABORT1")  TO (ABORT")
  ['] ACCEPT1    TO ACCEPT
  ['] TYPE1      TO TYPE
  ['] KEY1       TO KEY
  ['] FALSE      TO FPOP
  CREATE-HEAP
  <SET-EXC-HANDLER>
  POOL-INIT
  ['] AT-PROCESS-STARTING ERR-EXIT  
  ['] AT-THREAD-STARTING  ERR-EXIT
;

: USER-EXIT
  AT-THREAD-FINISHING
  DESTROY-HEAP
\  TlsIndex@ FREE DROP
;

' USER-INIT (TO) FORTH-INSTANCE>
' USER-EXIT (TO) <FORTH-INSTANCE
' QUIT      (TO) <MAIN>

VARIABLE IN-EXCEPTION

: STACK-ADDR. ( addr -- addr )
      DUP U. ." :  "
      DUP ['] @ CATCH 
      IF DROP 
      ELSE DUP U. WordByAddr TYPE CR THEN
;

: EXC-DUMP1 ( exc-info -- )
  IN-EXCEPTION @ IF DROP EXIT THEN
  TRUE IN-EXCEPTION !
  BASE @ SWAP
  HEX
  ." EXCEPTION! " 
  DUP @ ."  CODE:" U. 
  DUP 3 CELLS + @ ."  ADDRESS:" DUP U. ."  WORD:" WordByAddr TYPE SPACE
  ."  REGISTERS:"
  DUP 4 CELLS + @ CELLS + \ ����� ���� ������� �������� ��������� �� 2 CELLS
  176 + DUP 12 CELLS DUMP CR
  ." USER DATA: " TlsIndex@ U. ." THREAD ID: " 36 FS@ U.
  ." HANDLER: " HANDLER @ U.
  ." RETURN STACK:" CR
  6 CELLS + DUP @ \ DUP 65000 > IF NIP ELSE DROP CELL+ CELL+ @ THEN
  DUP HANDLER @ U< IF NIP ELSE DROP 4 CELLS + @ THEN
  DUP HANDLER @ U<
  IF
    25 >R
    BEGIN
      DUP HANDLER @ CELL+ U< R@ 0 > AND
    WHILE
      STACK-ADDR.
      CELL+ R> 1- >R
    REPEAT DROP RDROP
  ELSE
    DROP
    50 0 DO 
      R0 @ I 1+ CELLS - STACK-ADDR. DROP
    LOOP
  THEN
  ." END OF EXCEPTION REPORT" CR  
  BASE !
  FALSE IN-EXCEPTION !
;
' EXC-DUMP1 (TO) <EXC-DUMP>

: (TITLE)
  ." SP-FORTH - ANS FORTH 94 for Win95/98/ME/NT/2000/XP" CR
  ." Open source project at http://spf.sf.net" CR
  ." Russian FIG at http://www.forth.org.ru ; Started by A.Cherezov" CR
  ." Version " VERSION 1000 / 0 <# # # [CHAR] . HOLD # #> TYPE
  ."  Build " VERSION 0 <# # # # #> TYPE
  ."  at " BUILD-DATE COUNT TYPE CR CR
;
: TITLE  CGI? @ 0=  ?GUI 0= AND IF (TITLE) THEN ;
' TITLE ' MAINX EXECUTE !

: SPF-INI
  S" SPF4.INI" INCLUDE-PROBE
  IF  S" SPF4.INI" +ModuleDirName INCLUDE-PROBE DROP  THEN
;

\ Scattering a Colon Definition
: ... 0 BRANCH, >MARK DUP , 1 >RESOLVE ; IMMEDIATE 
: ..: '  >BODY DUP @  1 >RESOLVE ] ;
: ;..  DUP CELL+ BRANCH, >MARK SWAP ! [COMPILE] [ ; IMMEDIATE

TRUE VALUE SPF-INIT?

\ Startup
\ ����� ����� ��� �������:

VECT  IYDP><DP

' NOOP (TO) IYDP><DP

0 VALUE DYDP


: SKIP
 CR ." SKIP>PSKIP"
 STATE @
 IF  ['] PSKIP COMPILE, EXIT THEN PSKIP  ; IMMEDIATE

: (INIT)
  \ �������������� ��������� USER-SIZE ��������� ������� user-�������
  USER-OFFS @ EXTRA-MEM @ + USER-SIZE !  
  HERE YDP0 @ U< DYDP AND \ 0 AND
  IF   HERE YDP0 @ YDP @ YDP0 @ - CMOVE
       DYDP YDP0 @ + YDP0 @  YDP @ YDP0 @ - CMOVE>
       0 TO DYDP
  THEN
  OP0 0! SetOP 0 ,
\  HERE OP0 OpBuffSize + !
  0 TO H-STDLOG
  CONSOLE-HANDLES
  ['] CGI-OPTIONS ERR-EXIT
  MAINX @ ?DUP IF ERR-EXIT THEN
  SPF-INIT?
  IF
    ['] SPF-INI ERR-EXIT
    ['] OPTIONS ERR-EXIT
  THEN   
  CGI? @ 0= POST? @ OR IF ['] <MAIN> ERR-EXIT THEN
  BYE
;


' PROCESS-INIT TO TC-FORTH-INSTANCE>
' (INIT) WNDPROC: INIT
' INIT OVER - OVER 1+ +!

' INIT 22 DUMP
' FORTH-INSTANCE> TO TC-FORTH-INSTANCE>
