\ ~mak
\ ᫮�� � ��䨪ᮬ A_ ࠡ���� �������筮 ᮮ⢥�����騬 ᫮��� ��� A_,
\ �� �ᯮ����� ��㣮� �⥪ ��� ��।��  orig � dest.

REQUIRE AHEAD lib\include\tools.f

[UNDEFINED] CS-CELLS [IF] : CS-CELLS 2* CELLS ; [THEN]

 VARIABLE CS-SP
 99 CS-CELLS ALLOT
 HERE CS-SP !

: CS-SP><  \  stack substitution
 CS-SP @ SP@ CELL+ CS-SP ! SP! ;

: A_BEGIN	CS-SP>< POSTPONE BEGIN	CS-SP>< ; IMMEDIATE
: A_WHILE	CS-SP>< POSTPONE WHILE	CS-SP>< ; IMMEDIATE 
: A_AHEAD	CS-SP>< POSTPONE AHEAD	CS-SP>< ; IMMEDIATE
: A_IF		CS-SP>< POSTPONE IF	CS-SP>< ; IMMEDIATE
: A_ELSE	CS-SP>< POSTPONE ELSE	CS-SP>< ; IMMEDIATE
: A_THEN	CS-SP>< POSTPONE THEN	CS-SP>< ; IMMEDIATE
: A_AGAIN	CS-SP>< POSTPONE AGAIN 	CS-SP>< ; IMMEDIATE
: A_REPEAT	CS-SP>< POSTPONE REPEAT	CS-SP>< ; IMMEDIATE

\EOF TEST

: TST BEGIN 1 EMIT KEY [CHAR] Q =
       IF   2 EMIT   A_AHEAD \ GOTO M
       THEN 3 EMIT KEY [CHAR] Q =
      UNTIL 4 EMIT   A_THEN  \ M:
;
  TST    
 