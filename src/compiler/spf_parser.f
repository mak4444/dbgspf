( ������ ������ � �������� ������� ��������� �� �����.
  Copyright [C] 1992-1999 A.Cherezov ac@forth.org
  �������� 1999: PARSE � SKIP ������������� �� CODE
  � ��������������� �����������. ���������� ������������� � USER.
)

USER #TIB ( -- a-addr ) \ 94 CORE EXT
\ a-addr - ����� ������, ���������� ����� �������� � ������ TIB.

USER >IN ( -- a-addr ) \ 94
\ a-addr - ����� ������, ���������� �������� ��������� ������ �� �������
\ ��������� ������.

1024  VALUE  C/L \ ������������ ������ ������, ������� ����� ������ � TIB

USER-VALUE  TIB ( -- c-addr ) \ 94 CORE EXT
\ ����� ������������� �������� ������.

USER-CREATE ATIB
\ �������� �������� TIB
2048 TC-USER-ALLOT

: SOURCE ( -- c-addr u ) \ 94
\ c-addr - ����� �������� ������. u - ���������� �������� � ���.
  TIB #TIB @
;
: SOURCE! ( c-addr u -- ) 
\ ����������  c-addr u ������� ������� (������, �������� ������� - PARSE-AREA)
  #TIB ! TO TIB >IN 0!
;

: EndOfChunk ( -- flag )
  >IN @ SOURCE NIP < 0=        \ >IN �� ������, ��� ����� �����
;

: CharAddr ( -- c-addr )
  SOURCE DROP >IN @ +
;

: PeekChar ( -- char )
  CharAddr C@       \ ������ �� �������� �������� >IN
;

: IsDelimiter1 ( char -- flag )
  BL 1+ <
;
VECT IsDelimiter 
' IsDelimiter1 ' IsDelimiter TC-VECT!

: GetChar ( -- char flag )
  EndOfChunk
  IF 0 FALSE
  ELSE PeekChar TRUE THEN
;

: OnDelimiter ( -- flag )
  GetChar SWAP IsDelimiter AND
;

: SkipDelimiters ( -- ) \ ���������� ���������� �������
  BEGIN
    OnDelimiter
  WHILE
    >IN 1+!
  REPEAT
;

: OnNotDelimiter ( -- flag )
  GetChar SWAP IsDelimiter 0= AND
;

: SkipWord ( -- ) \ ���������� ������������ �������
  BEGIN
    OnNotDelimiter
  WHILE
    >IN 1+!
  REPEAT
;
: SkipUpTo ( char -- ) \ ���������� �� ������� char
  BEGIN
    DUP GetChar >R <> R> AND
  WHILE
    >IN 1+!
  REPEAT DROP
;

: ParseWord ( -- c-addr u )
  CharAddr >IN @
  SkipWord
  >IN @ - NEGATE
;

: PARSE-NAME ( -- c-addr u )
 \ http://www.complang.tuwien.ac.at/forth/ansforth/PARSE-NAME.html 
  \ ��� ����� ������ ����� ������������ � INTERPRET
  \ - �������: �� ���������� WORD �, ��������������, �� ������� � HERE;
  \ � ������������� ������� ��� ��� <=BL, � ��� ����� TAB � CRLF
  SkipDelimiters ParseWord
\  >IN 1+! \ ���������� ����������� �� ������
  >IN @ 1+ #TIB @ MIN >IN !   \ ��� ������������� � spf3.16
;

: NextWord  PARSE-NAME ;

: PARSE ( char "ccc<char>" -- c-addr u ) \ 94 CORE EXT
\ �������� ccc, ������������ �������� char.
\ c-addr - ����� (������ �������� ������), � u - ����� ���������� ������.
\ ���� ����������� ������� ���� �����, �������������� ������ ����� �������
\ �����.
  CharAddr >IN @
  ROT SkipUpTo
  >IN @ - NEGATE
  >IN 1+!
;

: PSKIP ( char "ccc<char>" -- )
\ ���������� ����������� char.
  BEGIN
    DUP GetChar >R = R> AND
  WHILE
    >IN 1+!
  REPEAT DROP
;


\ PARSE � SKIP ��������� ��� �������������, ������ �� ������������
\ ��� ���������� ��������� ������

: SKIP1 ( addr u -- addr+1 u-1 )
   DUP 0 >
   IF 1 - SWAP CHAR+ SWAP THEN
;