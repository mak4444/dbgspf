\ See license at end of file
REQUIRE [IF] ~mak/CompIF.f
REQUIRE [IFNDEF] ~nn\lib\ifdef.f

[IFNDEF] >> : >> RSHIFT ; [THEN]
[IFNDEF] << : << LSHIFT ; [THEN]

[IFNDEF] WBSPLIT
: WBSPLIT  ( l -- b.low b.high )
   DUP  0xFF AND  SWAP   8 >>
        0xFF AND
;
[THEN]

[IFNDEF] LWSPLIT
: LWSPLIT ( l -- w.low w.high )  \ split a long into two words
   DUP  0xFFFF AND  SWAP 0x10 >>
;
[THEN]

[IFNDEF] LBSPLIT
: LBSPLIT ( L -- B.LO B.1 B.2 B.HI )  LWSPLIT >R WBSPLIT R> WBSPLIT  ;
[THEN]

[IFNDEF] BWJOIN
: BWJOIN  (  b.low b.high -- w )  8 << +  ;
[THEN]

[IFNDEF] WLJOIN
: WLJOIN  ( w.low w.high -- l )  0x10 <<  SWAP  0xFFFF AND  OR  ;
[THEN]

[IFNDEF] BLJOIN
: BLJOIN  ( b.lo b.1 b.2 b.hi -- l )  BWJOIN  >R BWJOIN  R> WLJOIN   ;
[THEN]

