CR .( S" VIEW" *INCL )

REQUIRE [IF]            ~MAK\CompIF1.f
REQUIRE CASE            ~MAK\case.f
REQUIRE [IFNDEF]        ~nn\lib\ifdef.f
\ REQUIRE UP_BKEY               ~mak\ATMEL\PROTOOS\RUN\keys.f
\ REQUIRE {             ~MAK\locals4.f
\ HEX

[IFNDEF] COLOR!  : COLOR! TO  COLOR@ ; [THEN]

0x1F VALUE VIEWCOLOR

\ CREATE T_IMG    VIDBUF-SIZE ALLOT

C" /STRING" FIND NIP 0=
[IF] : /STRING DUP >R - SWAP R> + SWAP ;
[THEN]



[IFNDEF] SCAN
: SCAN ( adr len char -- adr' len' )
\ Scan for char through addr for len, returning addr' and len' of char.
        >R 2DUP R> -ROT
        OVER + SWAP
        ?DO DUP I C@ =
                IF LEAVE
                ELSE >R 1 -1 D+ R>
                THEN
        LOOP DROP ;
[THEN]

[IFNDEF] COLS   0x50 CONSTANT COLS
[THEN]

[IFNDEF] ROWS   0x19 CONSTANT ROWS
[THEN]

\ 40 ' COLS >BODY ! 24 ' ROWS >BODY !
\ 0x4E ' COLS >BODY ! 0x17 ' ROWS >BODY !

[IFNDEF] VIEW-SIZE 0xF000 CONSTANT VIEW-SIZE
[THEN]

[IFNDEF] ON : ON TRUE SWAP ! ;
[THEN]

[IFNDEF] OFF : OFF FALSE SWAP ! ;
[THEN]

[IFNDEF] BREAK : BREAK POSTPONE EXIT POSTPONE THEN ; IMMEDIATE
[THEN]
[IFNDEF] U=< : U=< 1+ U< ;
[THEN]

?CREATE VIEW_BUF      VIEW-SIZE CELL+ ALLOT

VARIABLE VIEW-MAX-POINT
ROWS 2- VALUE MAX-VIEW-Y
VARIABLE VIEW-#Y0
VARIABLE VIEW-X
VARIABLE VIEW-Y
VARIABLE VIEW0-X
VARIABLE VIEW0-Y
VIEW_BUF VALUE ADDR_CUR
VARIABLE BLOCK-BIG
VARIABLE BLOCK-END
VARIABLE BLOCK-#


VECT VVVEMIT
VECT VVVGETXY
VECT VVVSETXY


: MOCK_SETXY  VIEW0-Y !  VIEW0-X ! ;
: MOCK_GETXY  VIEW0-X @  VIEW0-Y @ ;

: CUR@  ( -- N )
  VVVGETXY COLS * + ;

:  MOCK_EMIT ( c -- )
 DROP  CUR@ 1+  COLS /MOD MOCK_SETXY ;

: VIEW_CUR@  ( -- N )
 VIEW-X @ VIEW-Y @ 1+ COLS *  + ;

0 VALUE MOCK_FLG


: REAL_OUT
        [']   EMIT TO  VVVEMIT
        [']  GETXY TO  VVVGETXY
        [']  SETXY TO  VVVSETXY
        FALSE TO MOCK_FLG
;

: MOCK_OUT
\ EXIT
  ID_SHIFT@ IF EXIT THEN
        [']  MOCK_EMIT TO  VVVEMIT
        [']  MOCK_GETXY TO  VVVGETXY
        [']  MOCK_SETXY TO  VVVSETXY
        TRUE TO MOCK_FLG
        GETXY MOCK_SETXY
;


' EMIT TO  VVVEMIT


: EMITTAB

  ID_SHIFT@ IF

\           GKEY DROP
 THEN


  DUP 9 =
        IF DROP
         BEGIN BL VVVEMIT VVVGETXY DROP 7 AND 0=
         UNTIL
        ELSE  VVVEMIT
        THEN ;


: AA-DEL ( ADDR1 ADDR2 -- )
  2DUP DUP  VIEW_BUF - VIEW-MAX-POINT @ - NEGATE CMOVE
  - NEGATE VIEW-MAX-POINT +! ;

: PBACK ( ADDR -- )
  DUP 1- AA-DEL
\   DUP 1- DUP  VIEW_BUF - VIEW-MAX-POINT @ - NEGATE 1- CMOVE
\  -1 VIEW-MAX-POINT +!
 ;

: LINE-END?  ( ADDR -- ADDR+1 C FLAG )

 COUNT
 DUP 0xD = IF DROP  DUP PBACK 1- COUNT  THEN

 DUP 0xA = IF FALSE EXIT THEN
 OVER VIEW_BUF - VIEW-MAX-POINT @ U=< ;

\ : COLOR-SET   ( ADDR C -- ADDR C )
\  OVER 1- BLOCK-BIG @ 1+ BLOCK-END @ 1+ WITHIN  0xC0 AND VIEWCOLOR XOR  COLOR! ;

: COLOR-SET   ( ADDR -- ADDR )
\  DUP BLOCK-BIG @ 1+ BLOCK-END @ 1+ WITHIN  0xC0 AND VIEWCOLOR XOR  COLOR! ;
  DUP BLOCK-BIG @  BLOCK-END @  WITHIN  0x33 AND VIEWCOLOR XOR  COLOR! ;

: VIEW-EMIT  ( ADDR C -- ADDR )
\  COLOR-SET
 VVVGETXY DROP COLS 1- U< 0=
        IF      COLOR@ 0xFF XOR COLOR!
                DROP [CHAR] > VVVEMIT
                COLOR@ 0xFF XOR COLOR!
                VVVGETXY NIP COLS 1- SWAP 1- VVVSETXY EXIT
        THEN  EMITTAB  ;

:  VIEW-LINE-FROM ( ADDR C -- ADDR )
  DROP
  MOCK_FLG IF  VIEW0-Y 1+!  VIEW0-X 0!  EXIT THEN
\  COLOR-SET
  COLS VVVGETXY DROP - DUP 1 <>
  IF  SPACES ELSE DROP CR THEN
\ 1- DUP IF DUP SPACES THEN DROP
  ;

: VIEW-LINE-XY2A-. ( ADDR -- ADDR1 )

   BEGIN  VIEW_CUR@ CUR@ U>
          IF

  ID_SHIFT@
  IF

        BLOCK-BIG @ ADDR_CUR =
        IF DUP 1+ BLOCK-BIG ! ELSE

        BLOCK-END @ ADDR_CUR =
        IF DUP 1+ BLOCK-END !
        THEN    THEN

        BLOCK-END @ BLOCK-BIG @ U<
 IF
        BLOCK-END @ BLOCK-BIG @
        BLOCK-END ! BLOCK-BIG !
 THEN

\  GETXY DROP
\ IF 1-  COLOR-SET  GETXY SWAP 1- SWAP SETXY  COUNT EMITTAB

\ THEN


  THEN

 DUP 1+ TO ADDR_CUR

          THEN

  COLOR-SET
  LINE-END?


   WHILE  VIEW-EMIT
   REPEAT VIEW-LINE-FROM
    ADDR_CUR VIEW-MAX-POINT @ VIEW_BUF + UMIN TO ADDR_CUR
    ;

: VIEW-LINE-A2XY-. ( ADDR -- ADDR1 )
   BEGIN  ADDR_CUR OVER U< 0=
          IF VVVGETXY 1- VIEW-Y ! VIEW-X !
          THEN
        COLOR-SET
        LINE-END?
   WHILE  VIEW-EMIT
   REPEAT VIEW-LINE-FROM ;

:  VIEW-PAGE-INIT ( -- ADDR MAX-VIEW-Y 0 )
   0 1 VVVSETXY   VIEW-#Y0 @ VIEW_BUF +
   MAX-VIEW-Y 0 ;

: VIEW-PAGE1   \  set VIEW-XY
   VIEW-PAGE-INIT
   ?DO  VIEW-LINE-A2XY-. LOOP DROP ;

: VIEW-PAGE \ set ADDR_CUR

  ID_SHIFT@
  IF

        BLOCK-BIG @ ADDR_CUR <>
        BLOCK-END @ ADDR_CUR <> AND
        IF ADDR_CUR DUP BLOCK-BIG ! BLOCK-END !
        THEN
  THEN

	VIEW-#Y0 @ VIEW_BUF +

  ID_SHIFT@
  IF

        BLOCK-BIG @ ADDR_CUR =
        IF DUP BLOCK-BIG ! ELSE

        BLOCK-END @ ADDR_CUR =
        IF DUP BLOCK-END !
        THEN    THEN

        BLOCK-END @ BLOCK-BIG @ U<
 IF
        BLOCK-END @ BLOCK-BIG @
        BLOCK-END ! BLOCK-BIG !
 THEN

  THEN

 TO ADDR_CUR

   VIEW-PAGE-INIT
   DO  VIEW-LINE-XY2A-. LOOP DROP
 ;


: NEXT-LINE ( ADDR LEN -- ADDR1 LEN1 )
     0xA SCAN DUP IF  1 /STRING  THEN  ;

: VIEW-#Y0>      ( -- OFFSET LEN )
 VIEW-#Y0 @ DUP VIEW-MAX-POINT @ - NEGATE
 >R VIEW_BUF + R>
 ;

: UP_ ( offset -- )
        1-
        DUP
        IF      BEGIN 1-
                        DUP VIEW_BUF + C@ 0xA =
                        OVER 0=    OR
                UNTIL
	        DUP VIEW_BUF + C@ 0xA = IF 1+ THEN
        THEN
        VIEW-#Y0  !
;

20 CONSTANT VSEARCHBUFLEN
CREATE VSEARCHBUF VSEARCHBUFLEN ALLOT
0 VALUE VSEARCHLEN
0 VALUE VSEARCHX

0 VALUE FILECHANGED?

CREATE VIEWFILENAME 0x104 ALLOT
CREATE VIEWFILENAMEBAK 0x104 ALLOT

: EDITFILEREAD

  VIEWFILENAME COUNT
  R/W OPEN-FILE THROW >R
  R@ FILE-SIZE  THROW DROP 0x400 / 4 + 0x400 * TO VIEW-SIZE
  VIEW_BUF -1 <> IF VIEW_BUF FREE DROP THEN
  VIEW-SIZE ALLOCATE THROW TO VIEW_BUF
  VIEW_BUF VIEW-SIZE R@  READ-FILE DROP VIEW-MAX-POINT !
   R> CLOSE-FILE DROP  0 TO FILECHANGED?

;

: EDITFILESAVE
   VIEWFILENAME COUNT VIEWFILENAMEBAK $!
  [CHAR] -  VIEWFILENAMEBAK COUNT + 1- C!
   VIEWFILENAME COUNT + 0!
   VIEWFILENAMEBAK COUNT + 0!
  VIEWFILENAME 1+ VIEWFILENAMEBAK 1+  ZRENAME-FILE DROP
  VIEWFILENAME COUNT W/O CREATE-FILE DROP >R
     VIEW_BUF VIEW-MAX-POINT @ R@ WRITE-FILE DROP
   R> CLOSE-FILE DROP  0 TO FILECHANGED? ;

: ALT_F3_DO
 ADDR_CUR 1+ VIEW-MAX-POINT @ VIEW_BUF - ADDR_CUR 1+ + 
			 VSEARCHBUF VSEARCHLEN SEARCH
			IF DROP DUP TO ADDR_CUR VIEW_BUF - VIEW-#Y0 !
                        6 0     
                               DO   VIEW-#Y0 @  UP_
				 VIEW-#Y0 @ 0= IF LEAVE THEN
                                LOOP
ADDR_CUR DUP BLOCK-BIG ! VSEARCHLEN + BLOCK-END ! 
  VIEW-PAGE1
			ELSE 2DROP
			THEN

;
: FUNC-KEY ( C -- )
\  0xFF AND EMIT EXIT

            CASE
 LEFT_BKEY      OF  VIEW-X @
                        IF      -1  VIEW-X +!
                                MOCK_OUT VIEW-PAGE

\                               ADDR_CUR 1- C@ 0xA > IF ADDR_CUR 1- TO ADDR_CUR THEN
                        THEN    ENDOF

 RIGHF_BKEY     OF  VIEW-X @  COLS 1- U<
                        IF      VIEW-X 1+!
                                MOCK_OUT VIEW-PAGE
\                               ADDR_CUR C@ 0xA > IF ADDR_CUR 1+ TO ADDR_CUR THEN
                        THEN ENDOF

 UP_BKEY                OF  VIEW-Y @
                        IF   -1  VIEW-Y +!  MOCK_OUT
                        ELSE  VIEW-#Y0 @ ?DUP
                           IF UP_
                           THEN
                        THEN VIEW-PAGE ENDOF    \ Up

 DOWN_BKEY      OF  VIEW-Y @  MAX-VIEW-Y 1- U<
                        IF    VIEW-Y 1+!    MOCK_OUT
                        ELSE  VIEW-#Y0> \ OFFSET LEN
                                MAX-VIEW-Y 0 ?DO NEXT-LINE LOOP NIP
                                IF  VIEW-#Y0>  NEXT-LINE DROP
                                    VIEW_BUF - VIEW-#Y0   !
                                THEN
                        THEN    VIEW-PAGE       ENDOF    \ Down

 PAGE_UP_BKEY   OF  VIEW-#Y0 @
                        IF MAX-VIEW-Y 0
                                ?DO VIEW-#Y0 @  UP_
                                   VIEW-#Y0 @ 0= IF LEAVE THEN
                                LOOP
                        THEN VIEW-PAGE
        ENDOF   \  Page Up

 PAGE_DOWN_BKEY OF
                        MAX-VIEW-Y 0
                        ?DO VIEW-#Y0> \ OFFSET LEN
                           MAX-VIEW-Y 0
                           ?DO NEXT-LINE LOOP NIP
                                IF VIEW-#Y0>  NEXT-LINE DROP
                                   VIEW_BUF - VIEW-#Y0   !
                                THEN
                        LOOP VIEW-PAGE  ENDOF   \ Page Down

 HOME_BKEY      OF VIEW-X 0! MOCK_OUT VIEW-PAGE  ENDOF   \ Home

 END_BKEY       OF COLS 1- VIEW-X ! MOCK_OUT VIEW-PAGE
                        ADDR_CUR 1- TO ADDR_CUR MOCK_OUT VIEW-PAGE1
                ENDOF   \ End


 BACK_BKEY      OF  ADDR_CUR VIEW_BUF -
                  IF

                    ADDR_CUR  ADDR_CUR 1-   AA-DEL
                        ADDR_CUR 1- TO ADDR_CUR
        VIEW-PAGE1
                  THEN
                ENDOF

 DEL_BKEY       OF   ID_SHIFT@
                     IF BLOCK-BIG @ BLOCK-END @  OVER - CLIPBOARD!
                        BLOCK-END @ BLOCK-BIG @  DUP BLOCK-END !
                     ELSE
                        ADDR_CUR 1+ ADDR_CUR
                     THEN   AA-DEL      VIEW-PAGE1  -1 TO FILECHANGED?
                ENDOF

 CTRL_INS_BKEY  OF   BLOCK-BIG @ BLOCK-END @  OVER - CLIPBOARD!
                ENDOF

      INS_BKEY  OF  ID_SHIFT@ CLIPBOARD? VIEW-MAX-POINT @ + VIEW-SIZE U< AND
			IF
		ADDR_CUR DUP CLIPBOARD# +
  \ DUP TO ADDR_CUR
                OVER VIEW_BUF - VIEW-MAX-POINT @  - NEGATE
		CMOVE>               \ allocate memory
	 	CLIPBOARD@ ADDR_CUR SWAP CMOVE
		CLIPBOARD# VIEW-MAX-POINT +!  
		ADDR_CUR DUP BLOCK-BIG !
                CLIPBOARD# + DUP
                BLOCK-END ! TO ADDR_CUR
		VIEW-PAGE1  -1 TO FILECHANGED?

			THEN
                ENDOF
        F2_BKEY OF EDITFILESAVE
                ENDOF
   CTRL_F2_BKEY OF EDITFILEREAD VIEW-PAGE
                ENDOF
        F3_BKEY OF
			GETXY
			VSEARCHX 0 SETXY  VSEARCHBUFLEN SPACES
			VSEARCHX 0 SETXY 
	  		VSEARCHBUF  VSEARCHBUFLEN  ACCEPT TO VSEARCHLEN
			SETXY
			ALT_F3_DO
		ENDOF

\    DUP   0xFF AND ?DUP        IF


	ALT_F3_BKEY
		OF  ALT_F3_DO
		ENDOF

                DUP

                DUP 0xD = IF DROP 0xA THEN

	                ADDR_CUR DUP DUP 1+ DUP TO ADDR_CUR
        	        OVER VIEW_BUF - VIEW-MAX-POINT @  - NEGATE
                	CMOVE>  C!
	                VIEW-MAX-POINT @ 1+ VIEW-SIZE UMIN VIEW-MAX-POINT !
        	        VIEW-PAGE1  -1 TO FILECHANGED?
\       THEN

            ENDCASE
\ VIEW-PAGE
;

: VTST
ADDR_CUR 1+ VIEW-MAX-POINT @ VIEW_BUF - ADDR_CUR 1+ + 
 ;

: VVVVT
VSEARCHBUF  VSEARCHBUFLEN
;

: SSSSS  SEARCH ;


0 VALUE VIEW#Y0
0 VALUE VIEW#X0


: _VIEW
\  VIDBUF T_IMG  VIDBUF-SIZE CMOVE

  COLOR@ >R
  GETXY 2>R
\   0x3 COLOR!  0 0 SETXY  COLS SPACES
   VIEW-#Y0 0! VIEW#X0 VIEW-X !
   VIEW_BUF DUP TO ADDR_CUR DUP BLOCK-BIG ! BLOCK-END !
   


        VIEW#Y0 4 - 0 MAX 0
        ?DO VIEW-#Y0> \ OFFSET LEN
           MAX-VIEW-Y 0
           ?DO NEXT-LINE LOOP NIP
                IF VIEW-#Y0>  NEXT-LINE DROP
                   VIEW_BUF - VIEW-#Y0   !
                THEN
        LOOP
	VIEW#Y0 3 MIN  VIEW-Y !
        0 TO VIEW#Y0
        0 TO VIEW#X0

   REAL_OUT      VIEW-PAGE

        BEGIN 0xF COLOR!  REAL_OUT
\     SETXY? 2>R 0 0 SETXY ID_SHIFT@ . 2R> SETXY
0 0 SETXY  COLS SPACES
0 0 SETXY VIEWFILENAME COUNT TYPE
 FILECHANGED? IF ."  *" THEN
 ."  Point" 9 EMIT ADDR_CUR VIEW_BUF - U. ." / " VIEW-MAX-POINT @ U.
   ADDR_CUR C@ ." $" H. 
\ ." Line "  ???? VIEW-Y @ +  U.

 ." Blk "  BLOCK-BIG @ VIEW_BUF - U. ." - "  BLOCK-END @ VIEW_BUF - U.

  ." sh "  GETXY DROP TO VSEARCHX VSEARCHBUF  VSEARCHLEN TYPE

        VIEW-X @  VIEW-Y @ 1+ SETXY

         GKEY DUP F10_BKEY  <>
	OVER ESC_BKEY <> AND
        WHILE
  FUNC-KEY
        REPEAT DROP

  2R> SETXY
  R>  COLOR!
\  T_IMG VIDBUF  VIDBUF-SIZE CMOVE
;

: $VIEW  (  adrR lenR \ idfR  -- )

    VIEWFILENAME $!
  EDITFILEREAD

  VIEWFILENAME COUNT
  R/W OPEN-FILE THROW >R
  R@ FILE-SIZE  THROW DROP 0x400 / 4 + 0x400 * TO VIEW-SIZE
  VIEW_BUF -1 <> IF VIEW_BUF FREE DROP THEN
  VIEW-SIZE ALLOCATE THROW TO VIEW_BUF
  VIEW_BUF VIEW-SIZE R@  READ-FILE DROP VIEW-MAX-POINT !
   R> CLOSE-FILE DROP
  _VIEW   ;


: $EDIT
  0 TO FILECHANGED?
   $VIEW
 FILECHANGED? DUP
  IF  DROP
  CR  ." SAVE THE FILE? Y/N"
    KEY 0x20 OR
\    [CHAR] n <>
     [CHAR] y = 
  THEN
 IF EDITFILESAVE
 THEN CR

;

: EDIT  PARSE-WORD  $EDIT ;

: EDIT_ERROR

  ERROR2

 ERRFILE C@ IF
  CR  ." PRESS ANY KEY TO EDIT"
    KEY DROP


  ERR-LINE#  TO VIEW#Y0  ERR-IN#  TO VIEW#X0
  ERR-FILE  $EDIT THEN


;

' EDIT_ERROR TO ERROR
