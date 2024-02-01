\ Console operations
REQUIRE   { ~mak\locals4.f
REQUIRE AT-XY ~day\common\console.f
\ REQUIRE /WIN32_FIND_DATA       ~ac/lib/win/file/findfile.f
REQUIRE [IF] ~mak/CompIF.f
REQUIRE [IFNDEF] ~nn\lib\ifdef.f
REQUIRE CASE ~mak\case.f 
REQUIRE DISASSEMBLER lib\ext\disasm.f 
 
[IFNDEF] LPLACE
: LPLACE         ( addr len dest -- )
                2DUP 2>R
                CELL+ SWAP MOVE
                2R> ! ;
[THEN]

[IFNDEF] LCOUNT		: LCOUNT   CELL+ DUP CELL- @ ; [THEN]

[IFNDEF]  PARSE-WORD	: PARSE-WORD PARSE-NAME ; [THEN]


WINAPI: FindFirstFileA       KERNEL32.DLL
WINAPI: FindNextFileA        KERNEL32.DLL
WINAPI: FindClose            KERNEL32.DLL

16 CONSTANT FILE_ATTRIBUTE_DIRECTORY

  0
  4 -- dwFileAttributes
  8 -- ftCreationTime
  8 -- ftLastAccessTime
  8 -- ftLastWriteTime
  4 -- nFileSizeHigh
  4 -- nFileSizeLow
  4 -- dwReserved0
  4 -- dwReserved1
260 -- cFileName          \ [ MAX_PATH ]
 14 -- cAlternateFileName \ [ 14 ]
100 + CONSTANT /WIN32_FIND_DATA


: SETXY  ( x y -- )  AT-XY ;

CREATE CONSOLE_SCREEN_BUFFER_INFO 22 ALLOT

WINAPI: GetConsoleScreenBufferInfo kernel32.dll
\ WINAPI: SetConsoleTextAttribute		KERNEL32.DLL

: GETXY  ( -- x y )
  CONSOLE_SCREEN_BUFFER_INFO H-STDOUT GetConsoleScreenBufferInfo DROP
  CONSOLE_SCREEN_BUFFER_INFO 4 + DUP W@ SWAP 2+ W@ ;


: COLOR! ( color -- )
   H-STDOUT SetConsoleTextAttribute
 DROP
\ ?WinError
 ;

: COLOR@ ( -> color )
	0 0 0 0 0 0 SP@ H-STDOUT GetConsoleScreenBufferInfo DROP
	2DROP >R 2DROP DROP R>  ;

0x148 CONSTANT UP_BKEY
0x150 CONSTANT DOWN_BKEY
0x14D CONSTANT RIGHF_BKEY
0x14B CONSTANT LEFT_BKEY
0x12A CONSTANT LSHIFT_BKEY
0x136 CONSTANT RSHIFT_BKEY
0x149 CONSTANT PAGE_UP_BKEY
0x151 CONSTANT PAGE_DOWN_BKEY
0x147 CONSTANT HOME_BKEY
0x14F CONSTANT END_BKEY
0x153 CONSTANT DEL_BKEY
0x13C CONSTANT F2_BKEY
0x13D CONSTANT F3_BKEY
0x144 CONSTANT F10_BKEY
0x15F CONSTANT CTRL_F2_BKEY
0x16A CONSTANT ALT_F3_BKEY
0x192 CONSTANT CTRL_INS_BKEY
0x152 CONSTANT INS_BKEY


0x1B CONSTANT ESC_BKEY
0x8 CONSTANT BACK_BKEY

0 VALUE ID_SHIFT@
0 VALUE ID_ALT@
0 VALUE ID_CTR@

: EKEY>GCHAR ( u -- u false | char true ) \ 93 FACILITY EXT
  DUP  0x11D0000 = IF -1 TO ID_CTR@   FALSE EXIT THEN
  DUP  0x01D0000 = IF  0 TO ID_CTR@   FALSE EXIT THEN
  DUP  0x1380000 = IF -1 TO ID_ALT@   FALSE EXIT THEN
  DUP  0x0380000 = IF  0 TO ID_ALT@   FALSE EXIT THEN
  DUP  0x1A20000 = IF -1 TO ID_SHIFT@ FALSE EXIT THEN
  DUP  0x0A20000 = IF  0 TO ID_SHIFT@ FALSE EXIT THEN
  DUP  0xFF000000 AND  0=   IF FALSE    EXIT THEN
  DUP  0x000000FF AND  DUP IF
    ID_ALT@
   IF   DROP 0x10 RSHIFT
   ELSE NIP
   THEN TRUE EXIT THEN DROP
  DUP  0x00FF0000 AND  DUP IF NIP 0x10 RSHIFT 0x100 OR TRUE EXIT THEN DROP
  FALSE
;

VARIABLE PENDING-CHAR

: GKEY? ( -- flag ) \ 94 FACILITY
  PENDING-CHAR @ 0 > IF TRUE EXIT THEN
  BEGIN
    EKEY?
  WHILE
    EKEY  EKEY>GCHAR
    IF PENDING-CHAR !
       TRUE EXIT
    THEN
    DROP
  REPEAT FALSE
;

VECT KEY

: GKEY ( -- char ) \ 94
  PENDING-CHAR @ 0 >
  IF PENDING-CHAR @ -1 PENDING-CHAR ! EXIT THEN
  BEGIN
    EKEY  EKEY>GCHAR 0=
  WHILE
    DROP
  REPEAT
;

CREATE FFBLKBUF /WIN32_FIND_DATA ALLOT 0x7777 ,
FFBLKBUF  /WIN32_FIND_DATA ERASE 
: ff_name ( buf -- zadr  )
\ cAlternateFileName EXIT
 DUP cAlternateFileName DUP C@ 0=
 IF DROP DUP cFileName THEN NIP

 ;


: ff_attrib dwFileAttributes ;
: FA_DIREC FILE_ATTRIBUTE_DIRECTORY ;

-1 VALUE IDDIR

: FIRSTFILE   { addr u -- flg }
  IDDIR DUP -1 <> IF DUP  FindClose DROP THEN  DROP
  0 addr u + C!
  FFBLKBUF addr FindFirstFileA TO IDDIR
  IDDIR -1 <>
;

: NEXTFILE ( -- flg )
     FFBLKBUF IDDIR FindNextFileA 0=
;


WINAPI: MoveFileA              KERNEL32.DLL
WINAPI: CopyFileA              KERNEL32.DLL

: ZRENAME-FILE   ( z-addr1 z-addr2 -- ior )
  SWAP  MoveFileA  ERR
;

WINAPI: GetCurrentDirectoryA KERNEL32.DLL
WINAPI: SetCurrentDirectoryA KERNEL32.DLL


: CUR_DIR@ ( -- addr len  )
  PAD DUP 1024 GetCurrentDirectoryA  ;

: CHDIR_ ( addr len -- )
	PAD ASCII-Z SetCurrentDirectoryA 0=
;


: ?DEF ( cfa <name> -- )
 >IN @
 BL WORD FIND NIP
 IF 2DROP POSTPONE \
 ELSE
 >IN ! EXECUTE
 THEN
 ;

: ?CREATE          ['] CREATE        ?DEF ;

: ?VARIABLE        ['] VARIABLE      ?DEF ;

: ?USER-CREATE     ['] USER-CREATE   ?DEF ;

: ?USER            ['] USER          ?DEF ;

0xFFFF VALUE VIEW-SIZE

-1 VALUE VIEW_BUF

VIEW-SIZE ALLOCATE THROW
 VALUE CLIPBOARD

256 CONSTANT MAXDIR

: ERRFILE  ( -- addr )
  ERR-DATA err.file
;


: ERR-LINE# ( -- num )
  ERR-DATA err.line#
;
: ERR-IN#   ( -- num )
  ERR-DATA err.in#
;

 VARIABLE SAVEERR?

: SAVEERR
 DUP IF PROCESS-ERR ( err -- err ) THEN ;

: GGGG   BEGIN      GKEY DUP H.  BL =  UNTIL   ;

REQUIRE $VIEW ~mak\VIEW.F
REQUIRE ACCEPT2 ~mak\ACC.F

REQUIRE NC ~mak\NC.F

