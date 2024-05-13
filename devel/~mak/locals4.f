( 28.Mar.2000 Andrey Cherezov  Copyright [C] RU FIG

  Èñïîëüçîâàíû èäåè ñëåäóþùèõ àâòîðîâ:
  Ruvim Pinka; Dmitry Yakimov; Oleg Shalyopa; Yuriy Zhilovets;
  Konstantin Tarasov; Michail Maximov.

  !! Ðàáîòàåò òîëüêî â SPF4.
)

( Ïðîñòîå ðàñøèðåíèå ÑÏ-Ôîðòà ëîêàëüíûìè ïåðåìåííûìè.
  Ðåàëèçîâàíî áåç èñïîëüçîâàíèÿ LOCALS ñòàíäàðòà 94.

  Îáúÿâëåíèå âðåìåííûõ ïåðåìåííûõ, âèäèìûõ òîëüêî âíóòðè
  òåêóùåãî ñëîâà è îãðàíè÷åííûõ âðåìåíåì âûçîâà äàííîãî
  ñëîâà âûïîëíÿåòñÿ ñ ïîìîùüþ ñëîâà "{". Âíóòðè îïðåäåëåíèÿ 
  ñëîâà èñïîëüçóåòñÿ êîíñòðóêöèÿ, ïîäîáíàÿ ñòåêîâîé íîòàöèè Ôîðòà
  { ñïèñîê_èíèöèàëèçèðîâàííûõ_ëîêàëîâ \ ñï.íåèíèö.ëîêàëîâ -- ÷òî óãîäíî }
  Íàïðèìåð:

  { a b c d \ e f -- i j }

  Èëè { a b c d \ e f[ EVALUATE_âûðàæåíèå ] -- i j }
  Ýòî çíà÷èò ÷òî äëÿ ïåðåìåííîé f[ áóäåò âûäåëåí íà ñòåêå âîçâðàòîâ ó÷àñòîê
  ïàìÿòè äëèíîé n áàéò. Èñïîëüçîâàíèå ïåðåìåííîé f[ äàñò àäðåñ íà÷àëà ýòîãî
  ó÷àñòêà. \Â ñòèëå MPE\

  Èëè { a b c d \ e [ 12 ] f -- i j }
  Ýòî çíà÷èò ÷òî äëÿ ïåðåìåííîé f áóäåò âûäåëåí íà ñòåêå âîçâðàòîâ ó÷àñòîê
  ïàìÿòè äëèíîé 12 áàéò. Èñïîëüçîâàíèå ïåðåìåííîé f äàñò àäðåñ íà÷àëà ýòîãî
  ó÷àñòêà. 

  ×àñòü "\ ñï.íåèíèö.ëîêàëîâ" ìîæåò îòñóòñòâîâàòü, íàïðèìåð:

  { item1 item2 -- }

  Ýòî çàñòàâëÿåò ÑÏ-Ôîðò àâòîìàòè÷åñêè âûäåëÿòü ìåñòî â
  ñòåêå âîçâðàòîâ äëÿ ýòèõ ïåðåìåííûõ â ìîìåíò âûçîâà ñëîâà
  è àâòîìàòè÷åñêè îñâîáîæäàòü ìåñòî ïðè âûõîäå èç íåãî.

  Îáðàùåíèå ê òàêèì ëîêàëüíûì ïåðåìåííûì - êàê ê VALUE-ïåðåìåííûì
  ïî èìåíè. Åñëè íóæåí àäðåñ ïåðåìåííîé, òî èñïîëüçóåòñÿ "^ èìÿ"
  èëè "AT èìÿ".


  Âìåñòî \ ìîæíî èñïîëüçîâàòü |
  Âìåñòî -> ìîæíî èñïîëüçîâàòü TO

  Ïðèìåðû:

  : TEST { a b c d \ e f -- } a . b . c .  b c + -> e  e .  f .  ^ a @ . ;
   Ok
  1 2 3 4 TEST
  1 2 3 5 0 1  Ok

  : TEST { a b -- } a . b . CR 5 0 DO I . a . b . CR LOOP ;
   Ok
  12 34 TEST
  12 34
  0 12 34
  1 12 34
  2 12 34
  3 12 34
  4 12 34
   Ok

  : TEST { a b } a . b . ;
   Ok
  1 2 TEST
  1 2  Ok

  : TEST { a b \ c } a . b . c . ;
   Ok
  1 2 TEST
  1 2 0  Ok

  : TEST { a b -- } a . b . ;
   Ok
  1 2 TEST
  1 2  Ok

  : TEST { a b \ c -- d } a . b . c . ;
   Ok
  1 2 TEST
  1 2 0  Ok

  : TEST { \ a b } a . b .  1 -> a  2 -> b  a . b . ;
   Ok
  TEST
  0 0 1 2  Ok

  Èìåíà ëîêàëüíûõ ïåðåìåííûõ ñóùåñòâóþò â äèíàìè÷åñêîì
  âðåìåííîì ñëîâàðå òîëüêî â ìîìåíò êîìïèëÿöèè ñëîâà, à
  ïîñëå ýòîãî âû÷èùàþòñÿ è áîëåå íåäîñòóïíû.

  Èñïîëüçîâàòü êîíñòðóêöèþ "{ ... }" âíóòðè îäíîãî îïðåäåëåíèÿ ìîæíî
  òîëüêî îäèí ðàç.

  Êîìïèëÿöèÿ ýòîé áèáëèîòåêè äîáàâëÿåò â òåêóùèé ñëîâàðü êîìïèëÿöèè
  Òîëüêî äâà ñëîâà:
  ñëîâàðü "vocLocalsSupport_M" è "{"
  Âñå îñòàëüíûå äåòàëè "ñïðÿòàíû" â ñëîâàðå, èñïîëüçîâàòü èõ
  íå ðåêîìåíäóåòñÿ.
)

[IFDEF] vocLocalsSupport_M
.( vocLocalsSupport_M  isn't unique)
\EOF
[THEN]

REQUIRE [IF] ~MAK\CompIF1.f

USER uAddDepth

0 VALUE {\0}?

MODULE: vocLocalsSupport_M

USER widLocals
USER uLocalsCnt
USER uLocalsUCnt
USER uPrevCurrent

: LocalOffs ( n -- offs )
  CELLS uAddDepth @ +
;

BASE @ HEX
 
' RP@ 7 + @ 0xC3042444 = 

[IF]

: R_ALLOT, 
  DUP  SHORT?
  OPT_INIT SetOP
  IF    8D C, 64 C, 24 C,  C, \ mov esp, offset [esp]
  ELSE  8D C, A4 C, 24 C,  , \ mov esp, offset [esp]
  THEN
  OPT_CLOSE
;  

C" MACRO," FIND NIP 0= 
[IF] : MACRO, INLINE,  ;
[THEN]

: CompileLocalRec ( u -- )
  LocalOffs DUP
  ['] DUP MACRO,
  SHORT?
  OPT_INIT SetOP
  IF    8D C, 44 C, 24 C, C, \ lea eax, offset [esp]
  ELSE  8D C, 84 C, 24 C,  , \ lea eax, offset [esp]
  THEN  OPT
  OPT_CLOSE
;

: CompileLocal@ ( n -- )
  ['] DUP MACRO,
  LocalOffs DUP  SHORT?
  OPT_INIT SetOP
  IF    8B C, 44 C, 24 C, C, \ mov eax, offset [esp]
  ELSE  8B C, 84 C, 24 C,  , \ mov eax, offset [esp]
  THEN  OPT
  OPT_CLOSE
;

: CompileLocal! ( n -- )
  LocalOffs DUP  SHORT?
  OPT_INIT SetOP
  IF    89 C, 44 C, 24 C, C, \ mov  offset [esp], eax
  ELSE  89 C, 84 C, 24 C,  , \ mov  offset [esp], eax
  THEN  OPT
  OPT_CLOSE
  ['] DROP MACRO,
;

\ : CompileLocal@ ( n -- )
\   LocalOffs LIT, POSTPONE RP+@
\ ;


[ELSE]

: R_ALLOT,
 ] POSTPONE LITERAL S" RP@ + RP! " EVALUATE
 POSTPONE [ ;

: CompileLocalRec ( u -- )
  LocalOffs
  POSTPONE LITERAL
  S" RP@ + " EVALUATE
;

: CompileLocal@ ( n -- )
  CompileLocalRec
  S" @ " EVALUATE
;

: CompileLocal! ( n -- )
  CompileLocalRec
  S" ! " EVALUATE
;

[THEN]

: CompileLocalsInit
  uPrevCurrent @ SET-CURRENT
  uLocalsUCnt @ ?DUP
  IF {\0}?
	IF	LIT, POSTPONE (RALLOT)
	ELSE	NEGATE CELLS R_ALLOT,
	THEN
  THEN
  uLocalsCnt @ uLocalsUCnt @ - ?DUP 
  IF DUP CELLS NEGATE uAddDepth +!  0 DO  S" >R " EVALUATE LOOP THEN
;


\ : CompileLocal@ ( n -- )
\   LocalOffs LIT, POSTPONE RP+@
\ ;


BASE !

: LocalsStartup
  TEMP-WORDLIST widLocals !
  GET-CURRENT uPrevCurrent !
  ALSO vocLocalsSupport_M
  ALSO widLocals @ CONTEXT ! DEFINITIONS
  uLocalsCnt 0!
  uLocalsUCnt 0!
  uAddDepth 0!
;
: LocalsCleanup
  PREVIOUS PREVIOUS
  widLocals @ FREE-WORDLIST
;

: ProcessLocRec ( "name" -- u )
  [CHAR] ] PARSE
  STATE 0!
  EVALUATE CELL 1- + CELL / \ äåëàåì êðàòíûì 4
  -1 STATE ! 
\  DUP uLocalsCnt +!
  uLocalsCnt @
;

: CreateLocArray
  [CHAR] [ PSKIP
  ProcessLocRec
  CREATE ,
  DUP uLocalsCnt +!  
;

: LocalsRecDoes@ ( -- u )
  DOES> @ CompileLocalRec
;

: LocalsRecDoes@2 ( -- u )
  ProcessLocRec , 
  DUP uLocalsCnt +!
  DOES> @ CompileLocalRec
;

: LocalsDoes@
  uLocalsCnt @ ,
  uLocalsCnt 1+!
  DOES> @ CompileLocal@
;

: ;; POSTPONE ; ; IMMEDIATE


: ^ 
  ' >BODY @ 
  CompileLocalRec
; IMMEDIATE


: -> ' >BODY @ CompileLocal!  ; IMMEDIATE

WARNING DUP @ SWAP 0!

: AT
  [COMPILE] ^
; IMMEDIATE

: TO ( "name" -- )
  >IN @ PARSE-NAME widLocals @ SEARCH-WORDLIST 1 =
  IF >BODY @ CompileLocal! DROP
  ELSE >IN ! [COMPILE] TO
  THEN
; IMMEDIATE

WARNING !

: â POSTPONE -> ; IMMEDIATE

WARNING @ WARNING 0!
\ ===
\ ïåðåîïðåäåëåíèå ñîîòâåòñòâóþùèõ ñëîâ äëÿ âîçìîæíîñòè èñïîëüçîâàòü
\ âðåìåííûå ïåðåìåííûå âíóòðè  öèêëà DO LOOP  è íåçàâèñèìî îò èçìåíåíèÿ
\ ñîäåðæèìîãî ñòåêà âîçâðàòîâ  ñëîâàìè   >R   R>
C" DO_SIZE" FIND NIP 0=
[IF] 3 CELLS CONSTANT DO_SIZE
[THEN]

: UAD+    1 CELLS  uAddDepth +! ;
: UAD-   -1 CELLS  uAddDepth +! ;

: [UAD+]  UAD+ ; IMMEDIATE
: [UAD-]  UAD- ; IMMEDIATE

: [UAD@]  uAddDepth @ ; IMMEDIATE
: [UAD!]  uAddDepth ! ; IMMEDIATE

: DO    POSTPONE DO      DO_SIZE              uAddDepth +! ; IMMEDIATE
: ?DO   POSTPONE ?DO     DO_SIZE              uAddDepth +! ; IMMEDIATE
: LOOP  POSTPONE LOOP    DO_SIZE NEGATE       uAddDepth +! ; IMMEDIATE
: +LOOP POSTPONE +LOOP   DO_SIZE NEGATE       uAddDepth +! ; IMMEDIATE
: >R    POSTPONE >R     [  1 CELLS ] LITERAL  uAddDepth +! ; IMMEDIATE
: R>    POSTPONE R>     [ -1 CELLS ] LITERAL  uAddDepth +! ; IMMEDIATE
: RDROP POSTPONE RDROP  [ -1 CELLS ] LITERAL  uAddDepth +! ; IMMEDIATE
: 2>R   POSTPONE 2>R    [  2 CELLS ] LITERAL  uAddDepth +! ; IMMEDIATE
: 2R>   POSTPONE 2R>    [ -2 CELLS ] LITERAL  uAddDepth +! ; IMMEDIATE
\ : FOREACH POSTPONE FOREACH [ 3 CELLS ] LITERAL uAddDepth +! ; IMMEDIATE
\ : NEXT  POSTPONE NEXT   [ -3 CELLS ] LITERAL uAddDepth +! ; IMMEDIATE

: DUP>R S" DUP >R" EVALUATE ; IMMEDIATE

: EXIT  ?COMP
  uLocalsCnt  @ ?DUP 
  IF CELLS POSTPONE LITERAL S" RP@ + RP! " EVALUATE
  THEN POSTPONE EXIT  ; IMMEDIATE

\ ===

\  uLocalsCnt  @ ?DUP 
\  IF CELLS RLIT, ['] (LocalsExit) RLIT, THEN

: ;LOC
  LocalsCleanup
  uAddDepth @ IF -330 THROW THEN
  uLocalsCnt  @ ?DUP 
  IF  S"   RP@" EVALUATE CELLS POSTPONE LITERAL
      S" + RP!" EVALUATE
     uLocalsCnt 0!
  THEN
;

: ; ;LOC
     S" ;" EVAL-WORD
; IMMEDIATE

WARNING !

\ =====================================================================


EXPORT

: {
	LAST @ >R
  LocalsStartup
  BEGIN
    BL PSKIP PeekChar DUP [CHAR] \ <> 
                    OVER [CHAR] - <>  AND
                    OVER [CHAR] } <>  AND
                    OVER [CHAR] | <>  AND
                    SWAP [CHAR] ) XOR AND
  WHILE
    CREATE LocalsDoes@ IMMEDIATE
  REPEAT

  PeekChar >IN 1+! DUP [CHAR] } <>
  IF
     DUP [CHAR] \ =
    SWAP [CHAR] | = OR
    IF
      BEGIN
        BL PSKIP PeekChar DUP 
         DUP [CHAR] - <> 
        SWAP [CHAR] } <>  AND
        SWAP [CHAR] ) XOR AND
      WHILE
        PeekChar [CHAR] [ =
        IF  CreateLocArray  LocalsRecDoes@
        ELSE
             CREATE LATEST DUP C@ + C@
             [CHAR] [ =
             IF  
               LocalsRecDoes@2
             ELSE
               LocalsDoes@ 1
             THEN
        THEN \ DUP U.
        uLocalsUCnt +!
        IMMEDIATE
      REPEAT
    THEN
    [CHAR] } PARSE 2DROP
  ELSE DROP THEN
  CompileLocalsInit
	R> LAST !
;; IMMEDIATE

' ; CONSTANT VM';

;MODULE
