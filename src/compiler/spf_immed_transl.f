\ $Id: spf_immed_transl.f,v 1.7 2006/12/04 21:15:59 ygreks Exp $

( ����� ������������ ����������, ������������ � ������ ����������.
  ��-����������� �����������.
  Copyright [C] 1992-1999 A.Cherezov ac@forth.org
  �������������� �� 16-���������� � 32-��������� ��� - 1995-96��
  ������� - �������� 1999
)

: TO \ 94 CORE EXT
\ �������������: ( x "<spaces>name" -- )
\ ���������� ������� ������� � �������� name, ������������ ��������.
\ �������� x � name. �������������� �������� ���������, ���� name ��
\ ���������� ����� VALUE.
\ ����������: ( "<spaces>name" -- )
\ ���������� ������� ������� � �������� name, ������������ ��������.
\ �������� ��������� ������� ����������, ������ ����, � �������� �����������.
\ �������������� �������� ���������, ���� name �� ���������� ����� VALUE.
\ ����� ����������: ( x -- )
\ �������� x � name.
\ ����������: �������������� �������� ���������, ���� POSTPONE ��� [COMPILE]
\ ����������� � TO.
  '
  9 + STATE @
  IF COMPILE, ELSE EXECUTE THEN
; IMMEDIATE

: POSTPONE \ 94
\ �������������: ��������� �� ����������.
\ ����������: ( "<spaces>name" -- )
\ ���������� ������� �����������. �������� ���, ������������ ���������.
\ ����� ���. �������� ��������� ���������� ����� � ������� �����������.
  ?COMP
  PARSE-NAME SFIND DUP
  0= IF -321 THROW THEN
  1 = IF COMPILE,
      ELSE LIT, ['] COMPILE, COMPILE, THEN
; IMMEDIATE

: \   \ 94 CORE EXT
\ ����������: ��������� ��������� ����������, ������ ����.
\ ����������: ( "ccc<eol>" -- )
\ �������� � ��������� ������� ����������� �������.
\ \ - ����� ������������ ����������.
  1 PARSE 2DROP
; IMMEDIATE

: [DEFINED] ( -- f ) \ "name"
  PARSE-NAME  SFIND  IF DROP TRUE ELSE 2DROP FALSE THEN
; IMMEDIATE

: [UNDEFINED]  ( -- f ) \ "name"
  POSTPONE [DEFINED] 0=
; IMMEDIATE

: \+    POSTPONE [UNDEFINED]	IF POSTPONE \ THEN ; IMMEDIATE
: \-    POSTPONE [DEFINED]	IF POSTPONE \ THEN ; IMMEDIATE

: .(  \ 94 CORE EXT
\ ����������: ��������� ��������� ����������, ������ ����.
\ ����������: ( "ccc<paren>" -- )
\ �������� � ������� �� ������� ccc, ������������ ������ ������� ")".
\ .( - ����� ������������ ����������.
  [CHAR] ) PARSE TYPE
; IMMEDIATE

: (  ( "ccc<paren>" -- ) \ 94 FILE
\ ��������� ��������� CORE (, �������:
\ ����� ����������� ��������� ����, ���� ����� ����������� ������� ���������
\ ������, ��� ������� ������ ������, ����� ��������� ������� ����� ���������
\ ������� �� �����, ���������� >IN � ���� � ���������� ������, ��������
\ ���� ������� �� ��� ���, ���� �� ����� ������� ������ ������ ��� ��
\ ����� ��������� ����� �����.
  BEGIN
    [CHAR] ) DUP PARSE + C@ = 0=
  WHILE
    REFILL 0= IF EXIT THEN
  REPEAT
; IMMEDIATE

: [COMPILE]  \ 94 CORE EXT
\ �������������: ��������� ������������.
\ ����������: ( "<spaces>name" -- )
\ ���������� ������� �������. �������� name, ������������ ���������.
\ ����� name. ���� ��� ����� ���� ��������� ����������, ��� "��-���������", 
\ �������� �� � ������� �����������; ����� �������� ��������� ���������� name.
\ �������������� �������� ���������, ���� name �� �������.
  ?COMP
  '
  COMPILE,
; IMMEDIATE

: ; ( -- )
  ?CSP
  RET, [COMPILE] [ SMUDGE
  ClearJpBuff
  0 TO LAST-NON
; IMMEDIATE

: EXIT
  RET,
; IMMEDIATE

: \EOF  ( -- )
\ ����������� ���������� �������� ������
  BEGIN REFILL 0= UNTIL
  POSTPONE \
;

: FIELD  ( offset size "new-name< >" -- offset+size )
      : OVER
        DUP IF   DUP  LIT,  ['] + COMPILE,
            THEN DROP
       POSTPONE ;
       + ;  

0 [IF]
: --
  CREATE OVER , +
  (DOES1) (DOES2) @ +
;

[ELSE]
: -- FIELD ;
[THEN]

