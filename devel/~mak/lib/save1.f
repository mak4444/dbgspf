\ save.f
\ Andrey Filatkin, af@forth.org.ru
\ Mihail Maksimov

\ save v2
\ ���࠭�� � exe � ����ᠬ� �� fres 䠩��
( addr_exe u_exe addr_fres u_fres -- )
\ �᫨ ������ �� �㦭�, � u_fres = 0
\ ���� save ᮧ���� �� �᭮�� resources.f �. ������ (~yz\lib\resources.f )
\ � ���� SaveWithRes
DECIMAL

\ IMAGE-SIZE 0x8000 + TO IMAGE-SIZE

GET-CURRENT

TEMP-WORDLIST ALSO CONTEXT ! DEFINITIONS
REQUIRE [DEFINED]  lib\include\tools.f


0x080 CONSTANT START-PE-HEADER
0x400 CONSTANT SIZE-HEADER
0x2000 CONSTANT BASEOFCODE
0 VALUE END-CODE-SEG
0 VALUE END-RES-SEG
0 VALUE START-RES-TABLE

: relocate ( adr xt -- ) 
\ �ਬ����� �� �ᥬ ����⠬ ��⠫��� adr ᫮�� xt
  >R \ f7_ed
  DUP 12 + W@ ( ���������� �����)
 OVER 14 + W@ ( ������������ �����)
 +
  SWAP 16 + SWAP
  BEGIN ( adr #) DUP WHILE
    OVER
 CELL+ @
 0x7FFFFFFF AND
 END-CODE-SEG +
 R@ EXECUTE
  SWAP
 2 CELLS +
 ( ����� �����) SWAP 1-
  REPEAT 2DROP
  RDROP
;

: relocate3 ( leaf --)
 IMAGE-SIZE
 BASEOFCODE
 +  CR ." RES="  2DUP H. DUP H. END-CODE-SEG - H.
 SWAP +! ;
: relocate2 ( dir -- ) ['] relocate3
 relocate ;
: relocate1 ( dir -- ) ['] relocate2
 relocate ;

: XHERE YDP @ HERE UMAX ;

: YALIGN YDP><DP ALIGN YDP><DP ;
: XALIGN HERE YDP @ U< IF YALIGN EXIT THEN ALIGN ;

: YALLOT YDP><DP ALLOT YDP><DP ;
: XALLOT HERE YDP @ U< IF YALLOT EXIT THEN ALLOT ;

: ADD-RES ( addr u -- )
  DUP IF
    R/O OPEN-FILE THROW >R
    END-CODE-SEG R@ FILE-SIZE 2DROP R@ READ-FILE THROW XALLOT
    END-CODE-SEG ['] relocate1 relocate \ �������� �� �ᥬ ���ᠬ ����ᮢ
     \ IMAGE-SIZE BASEOFCODE +
    R> CLOSE-FILE DROP
  ELSE
    2DROP
  THEN
;

: MSAVE ( c-addr u -- )
 HEX
  ( ��࠭���� ��ࠡ�⠭��� ���-��⥬� � EXE-䠩�� �ଠ� PE - Win32 )
 F7_ED
    HERE YDP0 @ - TO DYDP

    YDP0 @ HERE YDP @ YDP0 @ - ( 0x1000 + ) CMOVE
    DYDP YDP +! 

2SWAP R/W CREATE-FILE THROW >R

XHERE ." Z=" DUP H.  >R

ALIGN-BYTES @ 512 ALIGN-BYTES ! XALIGN ALIGN-BYTES !
XHERE ." Z=" DUP H.  TO END-CODE-SEG 

 ." R=" 2DUP  TYPE ADD-RES 

ALIGN-BYTES @ 512 ALIGN-BYTES ! XALIGN ALIGN-BYTES !
XHERE ." Z=" DUP H.  TO END-RES-SEG


XHERE ." Z=" H.
\ R> -ROT

  ModuleName R/O OPEN-FILE-SHARED THROW >R
  XHERE SIZE-HEADER R@ READ-FILE THROW SIZE-HEADER < THROW
  R> CLOSE-FILE THROW

		3	XHERE START-PE-HEADER 0x06  + + W! ( Num of Objects)  \   ?Res 2 
   		2	XHERE START-PE-HEADER 0x5C  + + W!        \ ?GUI 3
  BASEOFCODE            XHERE START-PE-HEADER 0x28  + +  ! ( EntryPointRVA )
  IMAGE-BASE            XHERE START-PE-HEADER 0x34  + +  ! ( ImageBase )
  IMAGE-SIZE BASEOFCODE + END-RES-SEG END-CODE-SEG - 0xFFF + 0x1000 / 0x1000 * +
                   XHERE START-PE-HEADER 0x50  + +  ! ( ImageSize )

  IMAGE-SIZE BASEOFCODE +    XHERE START-PE-HEADER 0x88  + + ! \ ?Res 0
  END-RES-SEG END-CODE-SEG - XHERE START-PE-HEADER 0x8C  + + ! \ ?Res 0

  IMAGE-SIZE            XHERE START-PE-HEADER 0x128 + + ! ( VirtualSize code)
  END-CODE-SEG IMAGE-BEGIN -
                        XHERE START-PE-HEADER 0x130 + + ! ( PhisicalSize code)

  XHERE 0x1C8 + TO START-RES-TABLE
  START-RES-TABLE 0x38 ERASE
\  ?Res IF
    S" .rsrc"							START-RES-TABLE SWAP CMOVE
    END-RES-SEG END-CODE-SEG - 0xFFF + 0x1000 / 0x1000 *	START-RES-TABLE 0x08 + !
    IMAGE-SIZE BASEOFCODE +					START-RES-TABLE 0x0C + !
    END-RES-SEG END-CODE-SEG -					START-RES-TABLE 0x10 + !
    END-CODE-SEG IMAGE-BEGIN - SIZE-HEADER +			START-RES-TABLE 0x14 + !
								START-RES-TABLE 0x18 + 0xC ERASE
    0x40 0x40000000 OR						START-RES-TABLE 0x24 + !
\  THEN

\  HERE YDP0 @ U< 
\  IF YDP0 @ HERE YDP @ YDP0 @ - CMOVE
\  THEN

 R>

    XHERE SIZE-HEADER R@ WRITE-FILE THROW

 XHERE  -  
    IMAGE-BEGIN XHERE
 OVER
 - 
  ROT
 XALLOT
 DYDP
 NEGATE
 YDP +!
  R@ WRITE-FILE THROW
  R> CLOSE-FILE THROW \ BYE
;


 SET-CURRENT

\ DROP TYPE TYPE \EOF

MSAVE
CONTEXT @
PREVIOUS
FREE-WORDLIST
