\ save.f
\ Andrey Filatkin, af@forth.org.ru
\ Mihail Maksimov

\ save v2
\ Сохраняет в exe с ресурсами из fres файла
( addr_exe u_exe addr_fres u_fres -- )
\ если ресурсы не нужны, то u_fres = 0
\ Либа save создана на основе resources.f Ю. Жиловца (~yz\lib\resources.f )
\ и либы SaveWithRes
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
TRUE VALUE ?Res
0 VALUE START-RES-TABLE

: relocate ( adr xt -- ) 
\ применить ко всем элементам каталога adr слово xt
  >R
  DUP 12 + W@ ( именованные записи) OVER 14 + W@ ( неименованные записи) +
  SWAP 16 + SWAP
  BEGIN ( adr #) DUP WHILE
    OVER CELL+ @ 0x7FFFFFFF AND END-CODE-SEG + R@ EXECUTE
  SWAP 2 CELLS + ( длина записи) SWAP 1-
  REPEAT 2DROP
  RDROP
;

: relocate3 ( leaf --) IMAGE-SIZE BASEOFCODE + SWAP +! ;
: relocate2 ( dir -- ) ['] relocate3 relocate ;
: relocate1 ( dir -- ) ['] relocate2 relocate ;

: XHERE YDP @ HERE UMAX ;

: YALIGN YDP><DP ALIGN YDP><DP ;
: XALIGN HERE YDP @ U< IF YALIGN EXIT THEN ALIGN ;

: YALLOT YDP><DP ALLOT YDP><DP ;
: XALLOT HERE YDP @ U< IF YALLOT EXIT THEN ALLOT ;

: ADD-RES ( addr u -- )
  DUP IF
    R/O OPEN-FILE THROW >R
    END-CODE-SEG R@ FILE-SIZE 2DROP R@ READ-FILE THROW XALLOT
    END-CODE-SEG ['] relocate1 relocate \ добавить ко всем адресам ресурсов
     \ IMAGE-SIZE BASEOFCODE +
    R> CLOSE-FILE DROP
  ELSE
    2DROP
  THEN
;

:  XXXX-
  HERE YDP0 @ U<  0 AND
  IF  YDP0 @ HERE - - 
  THEN ;

\   S" spf4mwc.exe" S" spf4wc.fres" RSAVE
: RSAVE ( c-addr1 u1 c-addr2 u2 -- )

  ( сохранение наработанной форт-системы в EXE-файле формата PE - Win32 )
\ F7_ED
    HERE YDP0 @ - TO DYDP

    YDP0 @ HERE YDP @ YDP0 @ - ( 0x1000 + ) CMOVE
    DYDP YDP +! 

XHERE ." Z=" DUP H.  >R

ALIGN-BYTES @ 512 ALIGN-BYTES ! XALIGN ALIGN-BYTES !
XHERE ." Z=" DUP H. XXXX- TO END-CODE-SEG 

 ." R=" 2DUP  TYPE ADD-RES 

ALIGN-BYTES @ 512 ALIGN-BYTES ! XALIGN ALIGN-BYTES !
XHERE ." Z=" DUP H. XXXX- TO END-RES-SEG

R>

XHERE ." Z=" DUP H. -  
-ROT

  R/W CREATE-FILE THROW >R
  ModuleName R/O OPEN-FILE-SHARED THROW >R
  XHERE SIZE-HEADER R@ READ-FILE THROW SIZE-HEADER < THROW
  R> CLOSE-FILE THROW

  \ если ресурсов нет (u_coff = 0), то END-CODE-SEG = END-RES-SEG
  END-CODE-SEG END-RES-SEG = IF FALSE ELSE TRUE THEN TO ?Res

  ?Res IF 3 ELSE 2 THEN XHERE START-PE-HEADER 0x06  + + W! ( Num of Objects)
  ?GUI IF 2 ELSE 3 THEN XHERE START-PE-HEADER 0x5C  + + W!
  BASEOFCODE            XHERE START-PE-HEADER 0x28  + +  ! ( EntryPointRVA )
  IMAGE-BASE            XHERE START-PE-HEADER 0x34  + +  ! ( ImageBase )
  IMAGE-SIZE BASEOFCODE + END-RES-SEG END-CODE-SEG - 0xFFF + 0x1000 / 0x1000 * +
                   XHERE START-PE-HEADER 0x50  + +  ! ( ImageSize )
  ?Res IF IMAGE-SIZE BASEOFCODE + ELSE 0 THEN
                        XHERE START-PE-HEADER 0x88  + + !
  ?Res IF END-RES-SEG END-CODE-SEG - ELSE 0 THEN
                        XHERE START-PE-HEADER 0x8C  + + !

  IMAGE-SIZE            XHERE START-PE-HEADER 0x128 + + ! ( VirtualSize code)
  END-CODE-SEG IMAGE-BEGIN -
                        XHERE START-PE-HEADER 0x130 + + ! ( PhisicalSize code)

  XHERE 0x1C8 + TO START-RES-TABLE
  START-RES-TABLE 0x38 ERASE
  ?Res IF
    S" .rsrc"             START-RES-TABLE SWAP CMOVE
    END-RES-SEG END-CODE-SEG - 0xFFF + 0x1000 / 0x1000 *
                          START-RES-TABLE 0x08 + !
    IMAGE-SIZE BASEOFCODE +
                          START-RES-TABLE 0x0C + !
    END-RES-SEG END-CODE-SEG -
                          START-RES-TABLE 0x10 + !
    END-CODE-SEG IMAGE-BEGIN - SIZE-HEADER +
                          START-RES-TABLE 0x14 + !
                          START-RES-TABLE 0x18 + 0xC ERASE
    0x40 0x40000000 OR    START-RES-TABLE 0x24 + !
  THEN

\  HERE YDP0 @ U< 
\  IF YDP0 @ HERE YDP @ YDP0 @ - CMOVE
\  THEN

    XHERE SIZE-HEADER R@ WRITE-FILE THROW
    IMAGE-BEGIN XHERE OVER -   ROT XALLOT DYDP NEGATE YDP +!
  R@ WRITE-FILE THROW
  R> CLOSE-FILE THROW \ BYE
;


 SET-CURRENT

\ DROP TYPE TYPE \EOF

MSAVE
CONTEXT @
PREVIOUS
FREE-WORDLIST
