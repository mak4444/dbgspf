\ �. �������, 7.04.2002
\ ��������� ���������� CASE:
\ 1. END-CASE �� ������� ����� �� �����
\ 2. ��������� <OF< � =OF

REQUIRE CASE ~mak/case.f 

( 10 20 <OF< ������ �� ����� � �������� 10..20 ?  )
: <OF<  
  POSTPONE 2>R POSTPONE DUP POSTPONE 2R> POSTPONE 1+ POSTPONE WITHIN
  [COMPILE] IF POSTPONE DROP
; IMMEDIATE

( OF ������ ��� ��������  5 < =OF )
: =OF 
  [COMPILE] IF POSTPONE DROP
; IMMEDIATE


: END-CASE POSTPONE DUPENDCASE ; IMMEDIATE
