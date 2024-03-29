( ���������� ����� �������.
  ��-����������� �����������.
  Copyright [C] 1992-1999 A.Cherezov ac@forth.org
  �������������� �� 16-���������� � 32-��������� ��� - 1995-96��
  ������� - �������� 1999, ���� 2000
)

  USER CURRENT     \ ���� wid �������� ������� ����������

  VARIABLE (DP)    \ ����������, ���������� HERE �������� ������
5 CONSTANT CFL     \ ����� ����, �������������� CREATE � ������� CS.
  USER     DOES>A  \ ��������� ���������� - ����� ��� DOES>

: SET-CURRENT ( wid -- ) \ 94 SEARCH
\ ���������� ������ ���������� �� ������, ���������������� wid.
  CURRENT !
;

: GET-CURRENT ( -- wid ) \ 94 SEARCH
\ ���������� wid - ������������� ������ ����������.
  CURRENT @
;

: IS-TEMP-WL ( -- flag )
\ ���������, �������� �� ������� ������� ���������� ��������� (�������)
  GET-CURRENT CELL- @ -1 =
;
: DP ( -- addr ) \ ����������, ���������� HERE �������� ������
  IS-TEMP-WL
  IF GET-CURRENT 6 CELLS + ELSE (DP) THEN
;

: ALLOT ( n -- ) \ 94
\ ���� n ������ ����, ��������������� n ���� ������������ ������. ���� n ������ 
\ ���� - ���������� |n| ���� ������������ ������. ���� n ����, �������� 
\ ��������� ������������ ������ ����������.
\ ���� ����� ����������� ALLOT ��������� ������������ ������ �������� � n
\ ������ ������� ������, �� �������� ����������� � ����� ALLOT.
\ ���� ����� ����������� ALLOT ��������� ������������ ������ �������� ��
\ ������� ������� � n ������ ������� �������, �� �������� ����������� ��
\ ������� ������� � ����� ALLOT.
  DP +!
;

: , ( x -- ) \ 94
\ ��������������� ���� ������ � ������� ������ � ��������� x � ��� ������.
  DP @ 4 ALLOT !
;

: C, ( char -- ) \ 94
\ ��������������� ����� ��� ������� � ������� ������ � ��������� ���� char.
  DP @ 1 CHARS ALLOT C!
;

: W, ( word -- )
\ ��������������� ����� ��� word � ������� ������ � ��������� ���� char.
  DP @ 2 ALLOT W!
;
