:  PARSE-WORD PARSE-NAME  ;
REQUIRE OF\ ~mak\case.f
REQUIRE $! ~mak\place.f
REQUIRE uAddDepth ~mak\locals4.f
REQUIRE A_AHEAD ~mak\lib\a_if.f                                        
REQUIRE (* ~af\LIB\comments.f 
\ REQUIRE ($ ~mak\LIB\commentM.f 
REQUIRE (Z ~mak\LIB\assert.f
[DEFINED] CASE-INS [IF] CASE-INS OFF [THEN]
C" BETWEEN" FIND NIP 0=
[IF] : BETWEEN 1+ WITHIN ;
[THEN]

C" CS-DUP" FIND NIP 0=
[IF] : CS-DUP 2DUP ;
[THEN]

C" A_AGAIN_C" FIND NIP 0=
[IF] : A_AGAIN_C CS-SP>< CS-DUP POSTPONE AGAIN CS-SP>< ; IMMEDIATE
[THEN]

CREATE C_DPBuff 44444 ALLOT
VARIABLE C_DP  C_DPBuff C_DP !
HERE VALUE C_:-SET
HERE VALUE C_J-SET

: ><CDP- ;
: ><CDP1 ;
: ><C_DP

    C_:-SET    :-SET
 TO C_:-SET TO :-SET

    C_J-SET    J-SET
 TO C_J-SET TO J-SET

 DP @  C_DP @
 DP !  C_DP !  ;

CREATE GC_DPBuff 44444 ALLOT
VARIABLE GC_DP  GC_DPBuff GC_DP !

: G><C_DP

    C_:-SET    :-SET
 TO C_:-SET TO :-SET

    C_J-SET    J-SET
 TO C_J-SET TO J-SET

 DP @  GC_DP @
 DP !  GC_DP !  ;

VECT ><CDP

' NOOP TO ><CDP

C" H." FIND NIP 0=
[IF] : H.  BASE @ HEX SWAP U. BASE !  ;
[THEN]

C" >=" FIND NIP 0=
[IF] : >= < 0= ;
[THEN]

C" 0>" FIND NIP 0=
[IF] : 0> 0 > ;
[THEN]

C" CELL/" FIND NIP 0=
[IF] : CELL/  CELL / ;
[THEN]

: &[]  CELLS + ;
:  []  CELLS + @ ;
:  []! CELLS + ! ;


1 VALUE QZZZV
: _QZZZ  QZZZV  0= IF CR ." zz=" . . CR THEN QZZZV ;
: QZZZ POSTPONE _QZZZ POSTPONE assert ; IMMEDIATE
: QZZZ- QZZZV 1- TO QZZZV ;

: YYY 
 EXIT
 SOURCE POSTPONE SLITERAL
 S" 2>R CR .S 2R> CR TYPE KEY DROP" EVALUATE ; IMMEDIATE

: _: : ;
\ : : :  S" 2DUP 13 13 D= uuuuu AND IF ZZZ THEN " EVALUATE   ;
0 VALUE uuuuu
0 VALUE Z_DEPTH
0 VALUE CCCC 
5 VALUE MMMM

0 VALUE qqq1 
0 VALUE qqq2

: QQQQ
\  DUP H.
\ qqq1 qqq2 WITHIN
  2DUP  32 36 D=
 uuuuu AND
;

: Z<PRE>
 STATE @ IF S" QQQQ IF ZZZ THEN " EVALUATE 
         THEN ;
 
: [P]  POSTPONE POSTPONE ; IMMEDIATE
MODULE: ZIF
: ^IF    [P] IF   ; IMMEDIATE
: ^ELSE  [P] ELSE ; IMMEDIATE
: ^THEN  [P] THEN ; IMMEDIATE
(*

: QQQ1
    DEPTH Z_DEPTH 1+  <> uuuuu AND
;

: SetZ_D  DEPTH TO Z_DEPTH ;
: Set1Z_D  DEPTH 1- TO Z_DEPTH ;

: BEGIN  [P] SetZ_D [P] BEGIN  ; IMMEDIATE
: WHILE  [P] WHILE  [P] QQQQ [P]  IF [P] ZZZ [P] THEN ; IMMEDIATE
: REPEAT            [P] QQQQ [P]  IF [P] ZZZ [P] THEN
         [P] REPEAT [P] QQQQ [P]  IF [P] ZZZ [P] THEN ; IMMEDIATE
: CASE   [P] Set1Z_D [P] CASE  ; IMMEDIATE
: OF     [P] OF     [P] QQQQ [P]  IF [P] ZZZ [P] THEN ; IMMEDIATE
: ENDOF             [P] QQQQ [P]  IF [P] ZZZ [P] THEN [P] ENDOF  ; IMMEDIATE
: ELSE              [P] QQQQ [P]  IF [P] ZZZ [P] THEN
         [P] ELSE ; IMMEDIATE
: IF     [P] Set1Z_D  [P] IF  ; IMMEDIATE
: THEN   [P] THEN   [P] QQQQ [P] ^IF [P] ZZZ [P] THEN ; IMMEDIATE
*)
;MODULE

\ : _; [P] ; ; IMMEDIATE

\ : ;  S" QQQQ IF  ZZZ THEN " EVALUATE [P] ; ; IMMEDIATE


\ <PRE>
\ ' ZINTER &INTERPRET !

: .warning ." warning:" ;
: warning"  POSTPONE .warning POSTPONE ." ; IMMEDIATE

: $" ( -- )
    HERE >R   [CHAR] " PARSE DUP 1+ ALLOT
    R> $! \ 0 C,
 ;

~mak\CinF\ForC.f

C" F1+!" FIND NIP 0=
[IF] : F1+! DUP >R F@ 1e F+ R> F! ;
[THEN]

CREATE C_BUFF 0 C, 0x100 ALLOT

: C_TYPE C_BUFF $+! ;

: C_CR C_BUFF COUNT EVALUATE C_BUFF 0! ;

VECT TOEVAL   ' C_TYPE  TO TOEVAL

VECT DOEVAL   ' C_CR  TO DOEVAL

VECT DOCR     ' C_CR  TO DOCR


MODULE: C_MODULE


: TOEVALZ ( addr -- )
  ASCIIZ>  TOEVAL  ;

: TOEVALBL TOEVAL S"  " TOEVAL ;

: TEC ( c -- )
  SP@ 1 TOEVAL DROP ;

:  TED. ( d -- )
  DUP >R DABS <# BL HOLD #S R> SIGN #>
  TOEVAL ;
: FTED. ( f: r -- )
 >FNUM  TOEVAL S" e " TOEVAL
;
: TE. ( n -- ) \ 94
\ Ќапечатать n в свободном формате.
  S>D TED. ;

: TE" POSTPONE S" POSTPONE TOEVAL ; IMMEDIATE
: TYPEZ ( addr -- )  ASCIIZ> TYPE ;

ALSO  ZIF
(*
: C@
 STATE @ IF POSTPONE C@  S"  QQQQ IF ZZZ THEN " EVALUATE 
         ELSE C@
         THEN ; IMMEDIATE
: @
 STATE @ IF POSTPONE @  S"  QQQQ IF ZZZ THEN " EVALUATE 
         ELSE @
         THEN ; IMMEDIATE
*)

0 VALUE t

\ ' Z<PRE> TO <PRE>
~mak\CinF\config.f 

S" ~mak\CinF\c.f" INCLUDED
~mak\CinF\string.f
S" ~mak\CinF\sym.f" INCLUDED
~mak\CinF\tree.f
S" ~mak\CinF\bytecode.f" INCLUDED
~mak\CinF\list.f   
S" ~mak\CinF\types.f" INCLUDED
PREVIOUS
type_init
MODULE: tok_VOC
FORTH
: do    xDO ;
: while WHLE ;
: for   FOR ;
: if ( -- n ) xIF ;
: else ( -- n ) xELSE ;
: case ( -- n ) xCASE ;
: switch ( -- n ) xSWITCH ;
: void ( -- n )   voidtype -}u.sym @ TO tsym  VOID ;
: int  ( -- n )    inttype -}u.sym @ TO tsym   INT ;
: char ( -- n )   chartype -}u.sym @ TO tsym  xCHR ;
: float ( -- n ) floattype -}u.sym @ TO tsym  FLOT ;
: unsigned  ( -- n )  UNSIGNED ;
: return ( -- n ) RETURN ;
: break ( -- n ) BREAK ;
: default ( -- n ) DEFAULT ;
: static  ( -- n )  STATIC ;
: typedef ( -- n ) TYPEDEF ;
: struct ( -- n ) STRUCT ;
: goto   ( -- n ) GOTO ;
: sizeof ( -- n ) SIZEOF ;
: union  ( -- n ) UNION ;
;MODULE

S" ~mak\CinF\MTOKEN1.F"  INCLUDED

CREATE tval symbol ALLOT

CREATE gettokTABL HERE
  S"  ~!@#$%^&*()+|{}:<>?`-=\[];',./"  S",
  0x9 C, CHAR " C,
HERE OVER - 1- SWAP C!

CREATE FgettokTABL HERE
  S"  ~!@#$%^&*()+|{}:<>?`-=\[];',/"  S",
  0x9 C, CHAR " C,
HERE OVER - 1- SWAP C!

\ HERE TO CCCC
 0 , \ !!!!!!!!!

: gettokSYM
           2DUP ['] tok_VOC >BODY @ SEARCH-WORDLIST
           IF  NIP NIP  EXECUTE 
           ELSE
><CDP            HERE TO token S",
><CDP            token identifiers @ lookup  TO tsym ID
           THEN
;
: \S",   ( addr u -- )
  HERE 0!
  BOUNDS  \ addr+u addr
  BEGIN  2DUP <>
  WHILE  COUNT
      DUP [CHAR] \ =
      IF DROP COUNT
          CASE
          [CHAR] t OF   0x9 ENDOF
          [CHAR] n OF   0xA ENDOF
          [CHAR] r OF   0xD ENDOF
          DUP [CHAR] 0 [CHAR] 9 BETWEEN
          IF  [CHAR] 0 -
          THEN
        DUPENDCASE
      THEN 
 HERE $C+!  
 REPEAT 2DROP
   HERE C@ 1+ ALLOT
;
CREATE F_BUFF  0x100 ALLOT

: gettok   ( -- u )
\ ." GT="
   A_BEGIN       
   A_BEGIN       
   A_BEGIN
   gettokTABL MTOKEN DUP
   IF 
       OVER  C@ [CHAR] - =  IF DROP
            PeekChar [CHAR] -  =  IF DROP >IN 1+! DECR  EXIT THEN
            PeekChar [CHAR] >  =  IF DROP >IN 1+! DEREF EXIT THEN
                    C@ EXIT THEN

       OVER  C@ [CHAR] + =  IF DROP
            PeekChar [CHAR] +  =  IF DROP >IN 1+! INCR  EXIT THEN
                    C@ EXIT THEN

       OVER  C@ [CHAR] & =  IF DROP
            PeekChar [CHAR] &  =  IF DROP >IN 1+! ANDAND  EXIT THEN
                    C@ EXIT THEN

       OVER  C@ [CHAR] | =  IF DROP
            PeekChar [CHAR] |  =  IF DROP >IN 1+! OROR  EXIT THEN
                    C@ EXIT THEN

       OVER  C@ [CHAR] = =  IF DROP
            PeekChar [CHAR] = =  IF DROP >IN 1+! EQL    EXIT THEN
                    C@ EXIT THEN

       OVER  C@ [CHAR] < =  IF DROP
            PeekChar [CHAR] = =  IF DROP >IN 1+! LEQ    EXIT THEN
                    C@ EXIT THEN

       OVER  C@ [CHAR] > =  IF DROP
            PeekChar [CHAR] = =  IF DROP >IN 1+! GEQ   EXIT THEN
                    C@ EXIT THEN

       OVER  C@ [CHAR] " =  IF 2DROP
                [CHAR] " PARSE 
><CDP            HERE >R \S", 0 C,
><CDP          chartype R@ C@ 1+ 0 array tval -}stype !
               R> tval -}v !
                tval TO tsym          SCON EXIT THEN

       OVER  C@ [CHAR] ' =  IF DROP 2 >IN +!
               1+ C@ 0 tval -}v  2!
               inttype tval -}stype !
                tval TO tsym          ICON EXIT THEN

       OVER  C@ [CHAR] . =  IF DROP
            PeekChar [CHAR] .  =  IF DROP 2 >IN +! ELLIPSIS EXIT THEN
                    C@ EXIT THEN

       OVER  C@ [CHAR] / =  IF DROP
            PeekChar [CHAR] /  =  IF DROP POSTPONE \ A_AGAIN THEN
            PeekChar [CHAR] *  =  IF DROP
             BEGIN  [CHAR] / PARSE  NIP 0=
               IF   REFILL   0=
               ELSE CharAddr 1- C@ [CHAR] / = THEN
             UNTIL                                   A_AGAIN THEN
            PeekChar [CHAR] F  =  IF DROP >IN 1+!
                  0 PARSE TOEVAL DOCR                A_AGAIN THEN
                    C@ EXIT THEN
       OVER  C@ [CHAR] 0 <  IF  DROP C@   EXIT THEN
       OVER  C@ [CHAR] 9 <= IF
       PeekChar [CHAR] . =  IF  F_BUFF $!
             FgettokTABL MTOKEN F_BUFF $+!
                 F_BUFF COUNT  >FLOAT 0=
            IF        [CHAR] e  F_BUFF $C+!
                 F_BUFF COUNT  >FLOAT 0=  ABORT" syntax error"
            THEN tval -}v  F!
       floattype tval -}stype !
                tval TO tsym          FCON EXIT THEN
       0 0 2SWAP >NUMBER 2DROP
       inttype tval -}stype !
               tval -}v  2!
                tval TO tsym          ICON EXIT THEN
       OVER  C@ [CHAR] A <  IF  DROP C@    EXIT THEN
       OVER  C@ [CHAR] Z <= IF  gettokSYM  EXIT THEN
       OVER  C@ [CHAR] _ <  IF  DROP C@    EXIT THEN
       OVER  C@ [CHAR] z <= IF  gettokSYM  EXIT THEN
   DROP C@   
      ELSE 2DROP EOI 
      THEN
 ;

\ \

: EndOfChunk ( -- flag )
  >IN @
 SOURCE
 NIP < 0=        \ >IN не меньше, чем длина чанка
;

: GetChar ( -- char flag )
  EndOfChunk
  IF 0 FALSE
  ELSE PeekChar TRUE THEN
;

: OnDelimiter ( -- flag )
  GetChar
  SWAP
  IsDelimiter
  AND
;

: SkipDelimiters ( -- ) \ пропустить пробельные символы
  BEGIN
    OnDelimiter
  WHILE
    >IN 1+!
  REPEAT
;
\ \
: getchr ( -- c )
   BEGIN 
         SkipDelimiters
         EndOfChunk
   WHILE
         REFILL 0=
         IF 0 EXIT
         THEN
   REPEAT
   PeekChar
;
ALSO  ZIF
~mak\CinF\error.f
S" ~mak\CinF\expr0.f"  INCLUDED
S" ~mak\CinF\expr.f"  INCLUDED
~mak\CinF\simp.f
S" ~mak\CinF\enode.F"  INCLUDED
~mak\CinF\stmt.f
~mak\CinF\init.f 
S" ~mak\CinF\decl.f"  INCLUDED
\ ' NOOP TO  <PRE>
PREVIOUS
EXPORT
\ EOF
: _C_ 
    gettok [CHAR] { <> ABORT"  {"
    gettok TO t
    program
;  IMMEDIATE


;MODULE

 1 TO uuuuu
