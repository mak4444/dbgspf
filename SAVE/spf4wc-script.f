
: .S DEPTH .SN ;

ALSO ASSEMBLER ALSO ASM-HIDDEN

: COMPILE-OPCODE1 ( COMPILE THE BYTES IN AN OPCODE )
        ( 0 -- | A -- | X \ A -- | X \ X' \ A -- )
        ( OS: X ... -- )
        ( A IS THE ADDRESS OF A TWO CELL DATA STRUCTURE: )
        ( OFFSET 0 -- XT OF THE ACTUAL ROUTINE TO COMPILE THE CODE )
        ( OFFSET 1 -- PARAMETER USED TO GENERATE THE CODE )
 CR ." C1="  .S
        ?DUP IF
                DUP CELL+ DATA-@ SWAP DATA-@ REGISTER-ASM

 CR ." C2="  .S
 EXECUTE
 CR ." C3="  .S
        THEN ;

: AAAAAA
  SAVE-INST
 CR ." S1="  .S

 COMPILE-OPCODE1
 CR ." S2="  .S
 RESET-FOR-NEXT-INSTR
 SAVE-DEPTH
 CR ." S4="  .S
 ;


: COMPILE-OPCODE1 ( COMPILE THE BYTES IN AN OPCODE )
        ( 0 -- | A -- | X \ A -- | X \ X' \ A -- )
        ( OS: X ... -- )
        ( A IS THE ADDRESS OF A TWO CELL DATA STRUCTURE: )
        ( OFFSET 0 -- XT OF THE ACTUAL ROUTINE TO COMPILE THE CODE )
        ( OFFSET 1 -- PARAMETER USED TO GENERATE THE CODE )
 CR ." C1="  .S
        ?DUP IF
                DUP CELL+ DATA-@ SWAP DATA-@ REGISTER-ASM

 CR ." C2="  .S
 EXECUTE
 CR ." C3="  .S
        THEN ;

: AAAAAA
  SAVE-INST
 CR ." S1="  .S

 COMPILE-OPCODE1
 CR ." S2="  .S
 RESET-FOR-NEXT-INSTR
 SAVE-DEPTH
 CR ." S4="  .S
 ;
PREVIOUS
PREVIOUS

CODE WWWW
 CALL ' WORDS
  0 AAAAAA
\  XOR EAX, EAX
 RET
END-CODE

\EOF
: CFA>NFA
 WordByAddr  DROP 1- ;

\EOF
: LIT
 F7_ED
  R>
 DUP @ 
SWAP CELL+
 >R ;

: 777.   [ ' LIT COMPILE, 777 , ] . ;

\EOF
0 [IF]
[IFDEF]
[ELSE] XXXXXX
[THEN]
[THEN]

\EOF
0 VALUE VMOD
0 VALUE VDATA
: xxx 
  TO VMOD
  TO VDATA
  0 VDATA
  begin
  VDATA * VMOD mod  SWAP 1+ SWAP 
   dup VDATA =
  until
  . .
;

: ZZZ 
  TO VMOD
  DUP TO VDATA
  2 0 DO VDATA * VMOD UMOD LOOP
  VMOD 2 - 1- 0 DO VDATA * VMOD UMOD LOOP

;

\EOF
IMAGE-BEGIN 0x400 - 0x30B54 + NEAR_NFA  44 DUMP 
 44 DUMP 