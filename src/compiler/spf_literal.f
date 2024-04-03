\ $Id: spf_literal.f,v 1.10 2008/04/24 18:24:25 ruv Exp $

( Ïðåîáðàçîâàíèå ÷èñëîâûõ ëèòåðàëîâ ïðè èíòåðïðåòàöèè.
  ÎÑ-íåçàâèñèìûå îïðåäåëåíèÿ.
  Copyright [C] 1992-1999 A.Cherezov ac@forth.org
  Ïðåîáðàçîâàíèå èç 16-ðàçðÿäíîãî â 32-ðàçðÿäíûé êîä - 1995-96ãã
  Ðåâèçèÿ - ñåíòÿáðü 1999
)

0 VALUE DOUBLE?

0 VALUE ?MINUS

: NUMBER?       ( addr len -- d1 f1 )
                FALSE TO DOUBLE?                \ initially not a double #
                OVER C@ [CHAR] - =
                OVER SWAP 0< AND DUP >R
                IF      1 /STRING
                THEN
                DUP 0=
                IF      RDROP      FALSE TO ?MINUS
                        2DROP 0 0 FALSE EXIT   \ always return zero on failure
                THEN
                0 0 2SWAP >NUMBER
                OVER C@ [CHAR] . =              \ next char is a '.'
                OVER SWAP 0< AND                     \ more chars to look at
                IF
			BEGIN
                        1 /STRING >NUMBER
                        DUP 0=
                        IF      TRUE TO DOUBLE? \ mark as a double number
                        THEN
  OVER C@ [CHAR] . <>			UNTIL 
                THEN    NIP 0=
                R> ?MINUS XOR
                IF      >R DNEGATE R>
                THEN  FALSE TO ?MINUS
;

: SNUMBER ( addr len -- d1 )
 NUMBER? 0= THROW ;
 

: NUMBER ( a1 -- d1 )
\ Convert count delimited string at a1 into a double number.

\  0 0 ROT COUNT >NUMBER THROW DROP

 COUNT SNUMBER
 ;

: NUMBER,      ( d -- )
                DOUBLE? 0= IF DROP THEN
                STATE @
                IF	DOUBLE? IF  DLIT, ELSE 	LIT,  THEN
                THEN
;

: XXX-SLITERAL ( addr u -> d true | false ) 
   NUMBER?
 IF NUMBER, TRUE  EXIT
 THEN
   2DROP FALSE
;

: BIN-SLITERAL ( addr u -> d true | false )
  BASE @ >R 2 BASE !
  XXX-SLITERAL
  R> BASE !
;

: HHH-SLITERAL ( addr u -> d true | false )
  BASE @ >R HEX
  2- SWAP 2+ SWAP
  XXX-SLITERAL
  R> BASE !
;
-1 VALUE ?HBTEM
: YYY-SLITERAL ( addr u -> d true | false )
  DUP 1 >
     IF
	 2DUP 2>R
         OVER C@ [CHAR] - = DUP  TO ?MINUS
         IF    1 /STRING 
         THEN

         OVER W@ 0x7830 ( 0x) = 
         IF     HHH-SLITERAL RDROP RDROP  EXIT
         THEN

          OVER C@ [CHAR] $ = 
         IF   1+ SWAP 1- SWAP HHH-SLITERAL
		RDROP RDROP EXIT
         THEN
	?HBTEM
	IF
              2DUP + 1- C@ 0x20 OR [CHAR] h =
         IF  1+  SWAP 2- SWAP  HHH-SLITERAL
		RDROP RDROP EXIT
         THEN

             2DUP + 1- C@ 0x20 OR [CHAR] b = BASE @ 0x10 <> AND
         IF   1- BIN-SLITERAL
		RDROP RDROP EXIT
         THEN
	THEN

             OVER @ 0xFF00FF  AND 0x270027 ( '\0')  = 
             OVER 3 = AND
         IF  DROP @ 8 RSHIFT 0xFF AND
		STATE @ IF LIT, THEN
             RDROP RDROP TRUE EXIT
         THEN 
         2DROP 2R>
  THEN
 XXX-SLITERAL
;

: ?SLITERAL3_H  ( c-addr u -- ... )
  ( ðàñøèðåííûé âàðèàíò ?SLITERAL1:
    åñëè ñòðîêà - íå ÷èñëî, òî ïûòàåìñÿ òðàêòîâàòü å¸
    êàê èìÿ ôàéëà äëÿ àâòî-INCLUDED)
  2DUP 2>R 
    YYY-SLITERAL 0=
    IF  2R>	
       OVER C@ [CHAR] " = OVER 2 > AND
       IF 2 - SWAP 1+ SWAP THEN ( óáðàë êàâû÷êè, åñëè åñòü)
       2DUP + 0 SWAP C!
       ['] INCLUDED CATCH
       DUP 2 = OVER 3 = OR OVER 161 = OR ( ôàéë íå íàéäåí èëè ïóòü íå íàéäåí,
       èëè íåðàçðåøåííîå èìÿ ôàéëà)
       IF  -2003 THROW \ ABORT"  -???"
       ELSE  THROW THEN EXIT
   THEN
   RDROP RDROP
;

: ?SLITERAL3 ?SLITERAL3_H ;

: ?LITERAL3 ( c-addr -- ... )
  ( ðàñøèðåííûé âàðèàíò ?LITERAL1:
    åñëè ñòðîêà - íå ÷èñëî, òî ïûòàåìñÿ òðàêòîâàòü å¸
    êàê èìÿ ôàéëà äëÿ àâòî-INCLUDED)
  COUNT ?SLITERAL3
;

