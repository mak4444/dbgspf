REQUIRE { ~mak\locals4.f

C" SCAN" FIND NIP 0=
[IF]
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

CREATE C_SOURCE 0x101 ALLOT

: M_SkipDelimiters
  CharAddr   SkipDelimiters
  CharAddr OVER - DUP 
  IF C_SOURCE $+!
  ELSE 2DROP
  THEN 
;

: MTOKEN_  { CTABL -- ADDR LEN }
\  F7_ED
 SkipDelimiters EndOfChunk IF CharAddr 0 EXIT THEN
  CTABL COUNT PeekChar SCAN NIP 
  IF CharAddr 1 DUP >IN +! EXIT THEN
  CharAddr
  BEGIN 1 >IN +!
     EndOfChunk 
     IF  TRUE
     ELSE   CTABL COUNT  PeekChar SCAN NIP 
     THEN
  UNTIL   CharAddr OVER -
\  ." MT=<" 2DUP TYPE ." >"
;

CREATE PREPRO_TABL HERE
  S"  !@$%^&*()_+-*/\|{}:;<>?=<>,.~`"  S",
  0x9 C, CHAR " C,
HERE OVER - 1- SWAP C!

MODULE: PREPRO

: VVVVV
  S" 44444"
 C_SOURCE
 $+! ;

: #define
 GET-CURRENT ['] PREPRO >BODY @ SET-CURRENT CREATE
 SET-CURRENT  [CHAR] \ PARSE  DUP>R HERE $! R> 1+ ALLOT
 DOES>  COUNT  C_SOURCE $+! ;

;MODULE

TRUE VALUE ~YET?

: PASS\N1
  BEGIN  SkipDelimiters  EndOfChunk   
  WHILE REFILL 0= IF TRUE  EXIT THEN
	BEGIN		C_SOURCE 0! TRUE TO ~YET?
	    BEGIN M_SkipDelimiters  PREPRO_TABL
		 MTOKEN_ ?DUP
	    WHILE 2DUP ['] PREPRO >BODY @  SEARCH-WORDLIST
	    	IF NIP NIP  EXECUTE  0 TO ~YET?
		ELSE   C_SOURCE $+!
		THEN
	    REPEAT  DROP
	    C_SOURCE COUNT TIB SWAP CMOVE
		TIB C_SOURCE C@ SOURCE!  >IN 0! ~YET?
\	CR SOURCE TYPE
	UNTIL
  REPEAT
   FALSE ;

: MTOKEN  ( CTABL -- ADDR LEN )
  PASS\N1 IF  0  EXIT THEN
 MTOKEN_ ;

: SMTOKEN { TABL Tlen -- ADDR alen }
  PASS\N1
  IF CharAddr  0  EXIT THEN
  TABL Tlen PeekChar SCAN NIP 
  IF CharAddr 1 DUP >IN +! EXIT THEN
  CharAddr
  BEGIN 1 >IN +!
     EndOfChunk 
     IF  TRUE
     ELSE   TABL Tlen  PeekChar SCAN NIP 
     THEN
  UNTIL   CharAddr OVER - \ ." MT=<" 2DUP TYPE ." >"
;

\EOF TEST
CREATE ZZZ HERE
  S"  ~!@#$%^&*()+|{}:<>?`-=\[];',./"  S",
  0x9 C, CHAR " C,
HERE OVER - SWAP C!

\ CREATE ZZZ HERE 0 C, BL C, 0x9 C, CHAR , C,
\ HERE OVER - SWAP C!
: TEST BEGIN ZZZ MTOKEN DUP 0= IF EXIT THEN DUP .
 ." {" TYPE KEY DROP ." }" AGAIN ;
TEST &+ QQQQ,WWWW EEEE
RRR$	TTTTTT
YYYYYYYYY UUUUU
IIII OOOOOO
