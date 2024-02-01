( ���������� ������� � ������� Windows Portable Executable.
  Copyright [C] 1992-1999 A.Cherezov ac@forth.org
  ������� - �������� 1999
)

( HERE �� ������ ������ ����������)
DECIMAL
DUP        VALUE ORG-ADDR      \ ����� ���������� ����
DUP        VALUE IMAGE-BEGIN   \ ����� �������� ����
1024 1024 * 100 * VALUE IMAGE-SIZE    \ ������� ����� ������������� ��� 
                               \ �������� ������ ����
DUP 8 1024 * - CONSTANT IMAGE-BASE \ ����� �������� ������ ������

1 [IF]

VARIABLE RESOURCES-RVA
VARIABLE RESOURCES-SIZE


: WRITE-FILES ( addr len idf -- ior )
  >R
  BEGIN DUP 0xFFFF0000 AND
  WHILE OVER 0xFFFF R@ WRITE-FILE
        DUP IF EXIT THEN DROP
        0xFFFF - SWAP 0xFFFF + SWAP
  REPEAT  R> WRITE-FILE
;

HEX

: XHERE YDP @ HERE UMAX ;

: SAVE ( c-addr u -- ) \ �������� S" My Forth Program.exe" SAVE
  ( ���������� ������������ ����-������� � EXE-����� ������� PE - Win32 )
  R/W CREATE-FILE THROW >R
  ModuleName R/O OPEN-FILE-SHARED THROW >R
  XHERE 400 R@ READ-FILE THROW 400 < THROW
  R> CLOSE-FILE THROW
  ?GUI IF 2 ELSE 3 THEN XHERE 0DC + C!
  2000    XHERE A8 +  ! ( EntryPointRVA )
  IMAGE-BEGIN 2000 -  XHERE B4 +  ! ( ImageBase )
  IMAGE-SIZE 2000 + 
          XHERE D0 +  ! ( ImageSize )
  IMAGE-SIZE
          XHERE 1A8 + ! ( VirtualSize )
  XHERE IMAGE-BEGIN -  1FF + 200 / 200 *
          XHERE 1B0 + ! ( PhisicalSize )

  2 XHERE 086 + W!
  RESOURCES-RVA @ XHERE 108 + !
  RESOURCES-SIZE @ XHERE 10C + !
  XHERE 1C8 + 38 ERASE

  XHERE 400 R@ WRITE-FILE THROW ( ��������� � ������� ������� )
  XHERE 200 ERASE
  IMAGE-BEGIN XHERE OVER - 1FF + 200 / 200 * \ 2/ 2/ \ H. H. BYE
 R@ WRITE-FILES BYE THROW
  R> CLOSE-FILE THROW
;

DECIMAL

[THEN]

: OPTIONS ( -> ) \ ���������������� ��������� ������
  SAVE-SOURCE N>R  
  -1 TO SOURCE-ID \ ����� REFILL �� ����� ( ��� �� EVALUATE ), �.�. ����� �� ATIB.
  GetCommandLineA ASCIIZ> SOURCE!
  PeekChar [CHAR] " = IF [CHAR] " ELSE BL THEN  
  WORD DROP  \ ��� ���������
  <PRE> ['] INTERPRET CATCH ?DUP IF ERROR THEN
  NR> RESTORE-SOURCE
;
