\ i386 assembler
\ now supports both real and protected modes
REQUIRE [IF] ~mak/CompIF.f
REQUIRE [IFNDEF] ~nn\lib\ifdef.f
REQUIRE WBSPLIT ~mak\Firmware\split.fth 
[IFNDEF] OCTAL : OCTAL 8 BASE ! ; [THEN]
[IFNDEF] >= : >= < 0= ; [THEN]
[IFNDEF] BETWEEN : BETWEEN 1+ WITHIN ; [THEN]
[IFNDEF] ALIAS
: ALIAS         ( xt -<name>- ) \ make another 'name' for 'xt'
    HEADER
   PARSE-NAME SFIND DUP 0= THROW
  1 = IF IMMEDIATE THEN
    0xE9 C, HERE CELL+ - ,  ;
[THEN]

ONLY FORTH ALSO DEFINITIONS
DECIMAL

\ ?condition is no longer used. remove it?
\ : ?condition true <> abort" conditionals not paired "  ;

VOCABULARY 386-ASSEMBLER
: ASSEMBLER  386-ASSEMBLER  ;
ASSEMBLER ALSO DEFINITIONS

\ The 8086 Assembler was written by Mike Perry, and modified for
\ 32-bit 386/486 use by Mitch Bradley, and modified for
\ memory modes by Mike Perry.
\ To create an assembler language definition, use the defining word "CODE".
\ It must be terminated with either "END-CODE" or its synonym "C;".
\ How the assembler operates is a very interesting example of the power
\ of "CREATE DOES>".
\ Basically the instructions are categorized and a defining word is
\ created for each category.  When the mnemonic for the instruction
\ is interpreted, it compiles the instruction.
\ The assembler is postfix. Operands and addressing modes leave values 
\ on the stack for the opcode mnemonics to resolve.

\ NEW: real/protected switching.
\ memory modes
0 CONSTANT REAL-MODE#
1 CONSTANT PROTECTED-MODE#
PROTECTED-MODE# VALUE MEMORY-MODE

: REAL-MODE	  ( - bool )         REAL-MODE# IS MEMORY-MODE  ;
: PROTECTED-MODE  ( - bool )    PROTECTED-MODE# IS MEMORY-MODE  ;

: REAL?		( - bool )   MEMORY-MODE      REAL-MODE# =  ;
: PROTECTED?	( - bool )   MEMORY-MODE PROTECTED-MODE# =  ;

: REAL-ONLY		( -- )    PROTECTED? ABORT" REAL MODE ONLY "  ;
: PROTECTED-ONLY	( -- )    REAL? ABORT" PROTECTED MODE ONLY "  ;

\ Deferring the definitions of the commas, marks, and resolves
\   allows the same assembler to serve for both the System- and the
\   Meta-Compiler.

\ in fact, doing so makes possible cross-compiling, transient definitions,
\ compiling to a buffer, compiling to a virtual buffer,

\ also, no reason not to load several assemblers at once.


DEFER ASM-SET-RELOCATION-BIT

DEFER ASM8!		FORTH ' C!    ASSEMBLER   IS ASM8!
DEFER HERE		FORTH ' HERE  ASSEMBLER   IS HERE
DEFER ASM-ALLOT		FORTH ' ALLOT ASSEMBLER   IS ASM-ALLOT

\ append values to the end of a code definition which is being built.
\ always little-endian:
: ASM8,   ( n -- )  HERE 1 ASM-ALLOT ASM8!  ;
: ASM16,  ( n -- )  WBSPLIT SWAP ASM8, ASM8,  ;
: ASM32,  ( n -- )  LWSPLIT SWAP ASM16, ASM16,  ;
: ASM16!  ( w adr -- )  >R WBSPLIT R@ 1+ ASM8!  R> ASM8!  ;
: ASM32!  ( l adr -- )  >R LWSPLIT R@ 2+ ASM16!  R> ASM16!  ;

FALSE VALUE ADDRESS-OV
FALSE VALUE DATA-OV
: OP:   ( -- )   TRUE IS DATA-OV     0x66 ASM8,  ;
: AD:   ( -- )   TRUE IS ADDRESS-OV  0x67 ASM8,  ;
: CLEAR-OV   ( -- )   FALSE IS DATA-OV   FALSE IS ADDRESS-OV  ;

: (ASM,)  ( flag -- )  REAL? XOR IF   ASM16,   ELSE   ASM32,   THEN  ;
: ADR,    ( n -- )  ADDRESS-OV (ASM,)  ;
: ASM,    ( n -- )  DATA-OV    (ASM,)  ;

: 16BIT?  REAL? ADDRESS-OV XOR  ;

: 16-ONLY	( -- )    16BIT? 0=  ABORT" REAL MODE ONLY "  ;
: 32-ONLY	( -- )    16BIT? ABORT" PROTECTED MODE ONLY "  ;

\ Now the fun begins...
\ In this 80x86 assembler, register names are cleverly defined constants.

\ The value returned by registers and by modes such as #) contains
\ both mode and register information. The instructions use the
\ mode information to decide how many arguments exist, and what to
\ assemble.

\ Like many CPUs, the 8086 uses many 3 bit fields in its opcodes
\ This makes octal ( base 8 ) natural for describing the registers

OCTAL

\ REG  creates a word which is a calculated constant.
\ REGS creates batches of words. It just puts REG in a DO LOOP.
: REG    ( group mode  -- )   11 *  SWAP 1000 * OR   CONSTANT  ;
: REGS   ( modes group -- )   SWAP 0 ?DO   DUP I REG   LOOP DROP  ;

: PMREG  ( group mode -- )   REG   DOES> @  PROTECTED-ONLY  ;
: PMREGS ( mode group -- )   SWAP 0 DO  DUP I PMREG  LOOP  DROP ;

: RMREG  ( group mode -- )   REG   DOES> @  REAL-ONLY  ;
: RMREGS ( mode group -- )   SWAP 0 DO  DUP I RMREG  LOOP  DROP ;

: 32REG  ( group mode -- )   REG   DOES> @  32-ONLY  ;
: 32REGS ( mode group -- )   SWAP 0 DO  DUP I 32REG  LOOP  DROP ;

: 16REG  ( group mode -- )   REG   DOES> @  16-ONLY  ;
: 16REGS ( mode group -- )   SWAP 0 DO  DUP I 16REG  LOOP  DROP ;

10 0 REGS        AL      CL      DL      BL     AH    CH    DH    BH
10 1 REGS        AX      CX      DX      BX     SP    BP    SI    DI

10 2 16REGS  [BX+SI] [BX+DI] [BP+SI] [BP+DI]   [SI]  [DI]  [BP]  [BX]

10 1 PMREGS     EAX     ECX     EDX     EBX    ESP     EBP   ESI   EDI
10 2 32REGS    [EAX]   [ECX]   [EDX]   [EBX]  [ESP]   [EBP] [ESI] [EDI]
 3 2 PMREGS     [AX]    [CX]    [DX]

 2 4 PMREG      [SP]

 6 3   REGS      ES      CS      SS      DS     FS      GS
 3 4   REGS       #      #)     S#)

\ notice that some words are defining words which create other words,
\ or in other words, some words make other words...
\ sorry, my mind was miles away!

\ a few addressing modes depend on the memory mode.
: [BX]  16BIT? IF  [BX]  ELSE  [EBX]  THEN  ;
: [SI]  16BIT? IF  [SI]  ELSE  [ESI]  THEN  ;
: [DI]  16BIT? IF  [DI]  ELSE  [EDI]  THEN  ;
: [BP]  16BIT? IF  [BP]  ELSE  [EBP]  THEN  ;


\ Not all of the following exist in all implementations of
\ x86 chips, Caveat Emptor.
 5 0 REG CR0  5 1 REG CR1  5 2 REG CR2  5 3 REG CR3
 5 4 REG CR4  5 5 REG CR5  5 6 REG CR6  5 7 REG CR7

 6 0 REG DR0  6 1 REG DR1  6 2 REG DR2  6 3 REG DR3
 6 4 REG DR4  6 5 REG DR5  6 6 REG DR6  6 7 REG DR7

 7 0 REG TR0  7 1 REG TR1  7 2 REG TR2  7 3 REG TR3
 7 4 REG TR4  7 5 REG TR5  7 6 REG TR6  7 7 REG TR7

\ Note! the "disp [ESP]" addressing mode doesn't exist.  That encoding is used
\ instead for scaled-index addressing, available only in protected mode.

10 1 REGS     /0    /1    /2    /3    /4    /5    /6    /7

[ESP] VALUE BASE-REG
[ESP] VALUE INDEX-REG
 000  VALUE SCALE-FACTOR

-1 CONSTANT [NOB]      \ "null" base register for scaled indexing

\ The 10000 bit is carefully chosen to lie outside the fields used
\ to identify the register type, so as not to confuse SPEC?

[ESP] 10000 OR  CONSTANT [SIB]      \ special code generated by *N words

\ Scaled indexing address mode.  Examples:
\   1234 [ESI]  [EBP] *4
\      0 [ESP]  [EBP] *1
\   5678 [NOB]  [ESI] *2

\ another defining word, scale:
: SCALE:  \ name  ( scale-factor -- )
   CREATE C,
   DOES>  	( disp base-reg index-reg apf -- disp mr )
      C@ IS SCALE-FACTOR  IS INDEX-REG  IS BASE-REG  [SIB]
;

000 SCALE: *1  100 SCALE: *2  200 SCALE: *4  300 SCALE: *8

\ The "no index" encoding isn't useful for any register
\ other than [ESP] because the other registers can be used
\ with the mod-r/m forms.
\      0 [ESP]  [NOX]
\     55 [ESP]  [NOX]
\   2345 [ESP]  [NOX]
\ XXX I don't think this is necessary anymore because of improvmements
\ in the handling of scaled indexing mode.
\ : [NOX]  ( disp base-reg -- disp mr )  [ESP] *1  ;

\ MD  defines words which test for various addressing modes.
: MD   CREATE  1000 * ,  DOES>  @ SWAP 7000 AND =  ;

\ R8? R16? MEM? SEG? #?  test for mode equal to 0 thru 4.
0 MD R8?   1 MD R16?   2 MD MEM?   3 MD SEG?   4 MD #?
\ 5 for   i MD   next    R8? R16? MEM? SEG? #?
\ or: " R8? R16? MEM? SEG? #?" " 5 for   i MD   next" eval-from

: SPEC?  ( n -- f )  [ ALSO FORTH ] 7000 AND 5000 >=  [ PREVIOUS ]  ;

\ REG?  tests for any register mode
: REG?   ( n -- f )   7000 AND 2000 <  ;

\ BIG?  tests offsets size. True if won't fit in one byte.
: SMALL?   ( n -- flag )   -200 177 BETWEEN  ;
: BIG?     ( n -- flag )   SMALL? 0=  ;

\ RLOW  mask off all but low register field.
: RLOW   ( n1 -- n2 )    7 AND ;

\ RMID  mask off all but middle register field.
: RMID   ( n1 -- n2 )   70 AND ;

\ SIZE  true for 16 or 32 bit, false for 8 bit.
VARIABLE SIZE   SIZE ON

: NORMAL   ( -- )   SIZE ON   CLEAR-OV  ;

\ BYTE  set size to 8 bit.
: BYTE   ( -- )   SIZE OFF ;

\ OP,  for efficiency. OR two numbers and assemble.
: OP,    ( N OP -- )   OR ASM8,  ;

\ WF,  assemble opcode with W field set for size of register.
: WF,   ( OP MR -- )   R16? 1 AND OP,  ;

\ SIZE,  assemble opcode with W field set for size of data.
: SIZE,  ( OP -- OP' )   SIZE @ 1 AND OP,  ;

\ ,/C,  assemble either 8 or 16 bits.
: ,/C,   ( n f -- )   IF  ASM,  ELSE  ASM8,  THEN  ;

: MOD-RM,  ( mr rmid mod -- )  -ROT RMID SWAP RLOW OR OP,  ;
: SIB,     ( base index scale -- )  MOD-RM,  ;

\ RR,  assemble register to register instruction.
: RR,    ( MR1 MR2 -- )   300 MOD-RM,  ;

\ These words perform most of the addressing mode encoding.
\ : SIB?   ( -- flag )   [SIB] =  ;

\ Assemble mod-r/m byte and s-i-b byte if necessary
: SOP,  ( mr rmid mod -- )
   PROTECTED? IF
      2 PICK  [SIB] =  IF			( [SIB] rmid mod )
	 [ESP] -ROT  MOD-RM,			( [SIB] ) \ Scaled index mode
	 DROP					( )
	 BASE-REG INDEX-REG SCALE-FACTOR SIB,
	 EXIT
      THEN					( MR RMID MOD )
      2 PICK  [ESP] =  IF			( mr rmid mod )
	 MOD-RM,				( )	\ DISP[ESP] USES SIB
	 [ESP] [ESP] 0 SIB,			( )
	 EXIT
      THEN					( mr rmid mod )
   THEN						( mr rmid mod )
   MOD-RM,					( )	\ Not scaled index mode
;

\ MEM,  handles memory reference modes.  It takes a displacement,
\   a mode/register, and a register, and encodes and assembles them.
: MEM,   ( DISP MR RMID -- )
   \ The absolute address mode is encoded in place of the
   \ (nonexistent) "<no-displacement> [EBP]" mode.

   OVER #) =  IF
      PROTECTED?  IF  5  ELSE  6  THEN
      SWAP 0 MOD-RM, DROP  ADR,  EXIT
   THEN  ( disp mr rmid )

   PROTECTED? IF
      \ Special case for "0 [EBP]" ; use short 0 displacement
      \ instead of [EBP] (there is no [EBP] addressing mode
      \ because that encoding is used for 32-bit displacement.)
      2 PICK 0=  2 PICK [EBP] =  AND  IF           ( disp mr rmid )
	 100 MOD-RM,  ASM8,  EXIT
      THEN                                         ( disp mr rmid )

      \ Special case for "disp32 [no-base-reg] [index-reg] *scale"
      OVER [SIB] =  IF                             ( disp mr rmid )
\	 PROTECTED-ONLY
	 BASE-REG [NOB] =  IF                      ( disp mr rmid )
	    0 MOD-RM,                              ( disp mr rmid )
	    5 INDEX-REG 0 SIB,                     ( disp )
	    R> ADR,                                ( )
	    EXIT
	 THEN                                      ( disp rmid mr )
      THEN                                         ( disp rmid mr )
   THEN

   2 PICK BIG?  IF  200 SOP, ADR,    EXIT  THEN ( disp mr rmid ) \ disp[reg]
   2 PICK 0<>   IF  100 SOP, ASM8,   EXIT  THEN ( disp mr rmid ) \ disp8[reg]
                      0 SOP, DROP               ( )              \ [reg]
;

\ WMEM,  uses MEM, after packing the register size into the opcode
: WMEM,   ( DISP MEM REG OP -- )   OVER WF, MEM,  ;

\ R/M,  assembles either a register to register or a register to
\  or from memory mode.
: R/M,   ( MR REG -- )
   OVER REG? IF  RR,  ELSE  MEM,  THEN  ;

\ WR/SM,  assembles either a register mode with size field, or a
\   memory mode with size from SIZE. Default is 16 (or 32) bit. Use BYTE
\   for 8 bit size.
: WR/SM,   ( R/M R OP -- )   2 PICK DUP REG?
   IF  WF, RR,  ELSE  DROP SIZE, MEM,  THEN  ;

\ INTER  true if inter-segment jump, call, or return.
VARIABLE INTER

\ FAR  sets INTER true.  Usage:  FAR JMP,   FAR CALL,   FAR RET.
: FAR    ( -- )   INTER ON  ;

\ ?FAR  sets far bit, clears flag.
: ?FAR   ( n1 -- n2 )   INTER @ IF  10 OR  THEN  INTER OFF ;

\
\ Create defining words for various classes of Machine Instructions
\

\ 0MI  define one byte constant segment overrides
: 0MI   CREATE  C,  DOES>  C@ ASM8,  ;

\ 1MI  define one byte constant instructions.
: 1MI   CREATE  C,  DOES>  C@ ASM8,  NORMAL  ;

\ 2MI  define ascii adjust instructions.
: 2MI   CREATE  C,  DOES>  C@ ASM8,  12 ASM8,  NORMAL  ;

\ 3MI  define branch instructions, with one or two bytes of offset.
: 3MI	\ conditional branches
   ( op -- )	CREATE  C,
   ( dest -- )	DOES>   C@		( dest op )
      SWAP HERE 2+ - 			( op disp )
      DUP SMALL? IF			( op disp8 )
	 SWAP ASM8, ASM8,
      ELSE				( op disp )
	 0x0F ASM8,  SWAP 0x10 + ASM8,
	 REAL? IF  2  ELSE  4  THEN  -	 ADR,
      THEN
      NORMAL
;

\ 4MI  define LDS, LEA, LES instructions.
: 4MI   CREATE  C,
   DOES>  C@  DUP 0xB2 0xB5 BETWEEN  IF 0x0F ASM8, THEN  ASM8,  MEM,
      NORMAL
;

\ 5MI  define string instructions.
: 5MI   CREATE  C,  DOES>  C@ SIZE,  NORMAL  ;

\ 6MI  define other string instructions.
: 6MI   CREATE  C,  DOES>  C@ SWAP WF,  NORMAL  ;

\ 7MI  define multiply and divide instructions.
: 7MI   CREATE  C,  DOES>  C@ 366 WR/SM, NORMAL  ;

: OUT  ( al | ax	dx | imm # -- )
   0xE6  SWAP  # =  IF  ( al|ax imm op )
      ROT WF, ASM8,      ( )
   ELSE                  ( al|ax op )
      10 OR  SWAP WF,    ( )
   THEN
   NORMAL
;
: IN  ( dx | imm,#	al | ax -- )
   0xE4  ROT  # =  IF   ( imm al|ax op )
      SWAP WF, ASM8,     ( )
   ELSE                  ( al|ax op )
      10 OR  SWAP WF,    ( )
   THEN
   NORMAL
;

\ 9MI  define increment/decrement instructions.
: 9MI   CREATE  C,  DOES>  C@  OVER R16?
      IF  100 OR SWAP RLOW OP,  ELSE  376 WR/SM,  THEN  NORMAL
;

\ 10MI  define shift/rotate instructions.
\ : 10MI  CREATE  C,  DOES>  C@ OVER CL =
\    IF  NIP 322  ELSE  320  THEN  WR/SM,  ;

\ *NOTE*  To allow both 'ax shl' and 'ax cl shl', if the register
\ on top of the stack is cl, shift second register by cl. If not,
\ shift top ( only) register by one.
\ ??? if we do this sort of thing, we should keep track of stack depth.
\ it is not hard; either sp@ or depth suffices.

\ For 'ax 5 # shl' and '5 # ax shl'
\ the immediate byte must be compiled after everything else.

: 10MI     ( op -- )   CREATE  C,
   DOES>  C@		( r/m cl op | r/m n # op | n # r/m op | r/m op )
      OVER #  = IF			( r/m n # op )
	 NIP SWAP DUP BIG? TUCK 2>R	( r/m op big? )
	 1 AND 300 OR WR/SM,    2R>	( n big? )
	 IF    ASM,   ELSE   ASM8,  THEN
	 EXIT
      THEN		( r/m cl op | n # r/m op | r/m op )
      2 PICK # = IF			( n # r/m op )
	 ROT DROP ROT DUP BIG? TUCK 2>R	( r/m op big? )
	 1 AND 300 OR WR/SM,    2R>	( n big? )
	 IF    ASM,   ELSE   ASM8,  THEN
	 EXIT
      THEN		( r/m cl op | r/m op )
      OVER CL = IF	( r/m cl op )
	 NIP 322	( r/m op op' )
      ELSE		( r/m op )	\ shift by 1 implicitly
	 320		( r/m op op' )
      THEN		( r/m op op' )
      WR/SM,
      NORMAL
;

\ 11MI  define calls and jumps.
\  notice that the first byte stored is E9 for jmp and E8 for call
\  so  C@ 1 AND  is 0 for call,  1 for jmp.
\  syntax for direct intersegment:   address segment #) FAR JMP

\ ???
: 11MI
   CREATE  C, C,   DOES>                        ( [ dst ] mode apf )
   OVER #) =  IF                                ( dst mode apf )
      NIP C@ INTER @  IF                        ( offset segment code )
         1 AND  IF  352  ELSE  232  THEN  ASM8, ( offset segment )
         SWAP ADR, ASM16,  INTER OFF            ( )
      ELSE					( dst code )
         SWAP HERE 2+ -  SWAP                   ( rel-dst code )
         2DUP 1 AND SWAP BIG? 0= AND  IF        ( rel-dst code )
            2 OP,  ASM8,                        ( )
         ELSE                                   ( rel-dst code )
	    ASM8,  REAL? IF
	       1- ASM16,
	    ELSE
	       3 - ASM32,
	    THEN				
         THEN					( )
      THEN					( )
   ELSE                                         ( mode apf )
      OVER S#) = IF  NIP #) SWAP  THEN          ( mode' apf )
      377 ASM8, 1+ C@ ?FAR  R/M,
   THEN
   NORMAL
;

\ 12MI  define pushes and pops.
: 12MI  ( dst mr -- )
   CREATE  C, C, C, C, C, DOES>       ( dst apf )
   OVER REG?  IF                      ( dst apf )   \ General register
      C@ SWAP RLOW OP,                ( )
   ELSE                               ( dst apf )
      1+ OVER SEG?  IF                ( dst apf' )  \ Segment register
         OVER FS >=  IF		      ( dst apf' )  \ FS or GS
	    0x0F ASM8,  3 + C@       ( dst opcode )
            SWAP GS = IF  10 OR  THEN ( opcode' )
	    ASM8,                     ( )
         ELSE			      ( dst apf' )  \ CS, DS, ES, or SS
            C@ RLOW SWAP RMID OP,     ( )
         THEN
      ELSE                            ( dst apf' )
         OVER # =  IF                 ( dst apf' )  \ Immediate
	    2+ C@                     ( val # opcode )
	    SIZE @  0= IF  2 OR  THEN ( val # opcode' )
	    ASM8,  DROP ASM,          ( )
         ELSE                         ( dst apf' )  \ Memory
            DUP 1+ C@ ASM8,  C@ MEM,  ( )
         THEN
     THEN
   THEN
   NORMAL
;

\ 14MI  defines returns.    RET    FAR RET    n +RET   n FAR +RET
: 14MI  ( op -- )
   CREATE  C,  DOES>
   \ This is definitely supposed to be asm16, not asm32
   C@ DUP ?FAR ASM8,  1 AND 0=  IF  ASM16,  THEN
   NORMAL
;

\ 13MI  define arithmetic and logical instructions.
: 13MI  ( src dst -- )
   CREATE  C,  DOES>                         ( src dst apf )
\  F7_ED
   C@ >R                                     ( src dst )  ( r: op )
   DUP REG?  IF                              ( src dst ) \ Dst is register
      OVER REG?  IF                          ( src dst )
         R> OVER WF, SWAP RR,                ( )         \ Register -> register
      ELSE                                   ( src dst )
         OVER DUP MEM? SWAP #) = OR  IF      ( src dst )
            R> 2 OR WMEM,                    ( src dst ) \ Memory -> register
         ELSE                                ( src dst )
            NIP  DUP RLOW 0=   IF            ( immed dst )
               R> 4 OR OVER WF, R16? ,/C,    ( )         \ imm -> accumulator
            ELSE                             ( immed dst )  \ imm -> register
               OVER BIG? OVER R16? 2DUP AND  ( immed dst big? r16? wbit )
              -ROT 1 AND SWAP INVERT OVER 0<>
			        AND 2 AND OR ( immed dst flag 0|1|3 )
               200 OP,                       ( immed dst flag  )
               SWAP RLOW 300 OR R> OP, ,/C,  ( )
            THEN
         THEN
      THEN
   ELSE                                      ( src disp dst )  \ Dst is memory
      ROT DUP REG?  IF                       ( src disp dst )  \ reg -> mem
         R> WMEM,                            ( )
      ELSE                                   ( disp src disp dst ) \ imm -> mem
         DROP                                ( disp src disp )
         2 PICK BIG? DUP INVERT 2 AND 200 OR
         SIZE, -ROT R> MEM,
         SIZE @ AND ,/C,
      THEN
   THEN
   NORMAL
;

\ Used for LGDT, SGDT, LIDT, SIDT, LLDT, SLDT,
: 15MI  \ name ( reg-field second-byte -- )
   CREATE  C,  3 << C,
   DOES>  0xF ASM8,  DUP C@ DUP >R ASM8,       ( adr ) ( r: mode )
   1+ C@  R> [ ALSO FORTH ] IF  MEM,  ELSE  R/M,  THEN  [ PREVIOUS ]
   NORMAL
;
0 1 15MI SGDT   1 1 15MI SIDT   0 0 15MI SLDT  1 0 15MI STR
2 1 15MI LGDT   3 1 15MI LIDT   2 0 15MI LLDT  3 0 15MI LTR

\ LSS, LFS, LGS
: 16MI  CREATE  C,  DOES>  C@  0x0F ASM8,  ASM8,  MEM,  NORMAL  ;

\ TEST  bits in dest
: TEST   ( source dest -- )
   DUP REG?  IF
      OVER REG?  IF  204 OVER WF, SWAP RR,  EXIT THEN

      OVER DUP MEM? SWAP #) = OR  IF   204 WMEM,  EXIT THEN

      NIP  DUP RLOW 0=  IF   250 OVER WF,  R16? ,/C,  EXIT THEN  \ imm -> acc

      366 OVER WF,  DUP RLOW 300 OP,  R16? ,/C,

   ELSE                                               \ *   -> mem
      ROT DUP REG?  IF  204 WMEM,  EXIT THEN          \ reg -> mem

      DROP  366 SIZE,  0 MEM,  SIZE @ ,/C,   \ imm -> mem
   THEN
   NORMAL
;

HEX
: ESC   ( source ext-opcode -- )   RLOW 0D8 OP, R/M,  ;

: SETIF  ( dest condition -- )  0F ASM8,  024 XOR ASM8,  R/M,  ;

\ INT  assemble interrupt instruction.
: INT   ( N -- )   0CD ASM8,  ASM8,  ;

\ XCHG  assemble register swap instruction.
: XCHG   ( MR1 MR2 -- )
   DUP REG?  IF
      DUP EAX =  IF
         DROP RLOW 90 OP,
      ELSE
         OVER EAX =  IF
            NIP  RLOW 90 OP,
         ELSE
            86 WR/SM,
         THEN
      THEN
   ELSE
      ROT 86 WR/SM,
   THEN
   NORMAL
;

\ Encoding of special register moves:
\ 0F c,
\ 0x22 for normal->special direction, 0x20 for special->normal direction
\ or with  0 for CRx, 1 for DRx, 4 for TRx
: SPECIAL-MOV  ( s d -- )
   0x0F ASM8,
   [ ALSO FORTH ]
   DUP SPEC?  IF  0x22  ELSE  SWAP 0x20  THEN   ( norm-reg spec-reg opcode )
   OVER 0xE00 AND CASE
      0xA00 OF  0  ENDOF
      0xC00 OF  1  ENDOF
      0xE00 OF  4  ENDOF
   ENDCASE                 ( norm-reg spec-reg opcode modifier )
   [ PREVIOUS ]
   OP,                     ( norm-reg spec-reg )
   RR,
;

\ MOV  as usual, the move instruction is the most complicated.
\  It allows more addressing modes than any other, each of which
\  assembles something more or less unique.

: (MOV)   ( S D -- )
   \ Stack diagram at the decision level is ( src dst )
   DUP SEG?  IF  8E ASM8, R/M,  EXIT   THEN         ( s d )

   DUP SPEC?  IF  SPECIAL-MOV  EXIT  THEN
   DUP REG?  IF                                     ( s d )  \ *   -> reg
      OVER SPEC?  IF  SPECIAL-MOV  EXIT  THEN
      OVER #) = OVER RLOW 0= AND  IF                ( s d )  \ abs -> acc
         A0 SWAP WF,   DROP ADR,  EXIT              ( s d )
      THEN

      OVER SEG?  IF  SWAP 8C ASM8, RR,  EXIT  THEN  ( s d )  \ seg -> reg

      OVER # =  IF                                  ( s d )  \ imm -> reg
         NIP DUP R16? SWAP RLOW
         OVER 8 AND OR B0 OP, ,/C,
         EXIT
      THEN

      8A OVER WF, R/M,                              ( )      \ r/m -> reg

   ELSE                                             ( s d d ) \ *   -> mem
      ROT DUP SEG?  IF  8C ASM8, MEM,  EXIT THEN    ( s d d ) \ seg -> mem

      DUP # =  IF                                   ( s d d ) \ imm -> mem
         DROP C6 SIZE, 0 MEM,  SIZE @ ,/C,  EXIT
      THEN

      OVER #) = OVER RLOW 0= AND   IF               ( s d d ) \ abs -> acc
         A2 SWAP WF,  DROP   ADR,  EXIT
      THEN

      88 OVER WF, R/M,                              ( )       \ reg -> mem
   THEN
;
: MOV,  (MOV)  NORMAL  ;

\ Most instructions are defined here. Those mnemonics in
\ parenthetic comments are defined earlier or not at all.

HEX
\ CS: DS: ES: SS: assemble segment over-ride instructions.
2E 0MI CS:
36 0MI SS:
3E 0MI DS:
26 0MI ES:
64 0MI FS:
65 0MI GS:

 37  1MI AAA,     D5  2MI AAD,     D4  2MI AAM,     3F  1MI AAS,
 10 13MI ADC,     00 13MI ADD,     20 13MI AND,  10 E8 11MI CALL,
 98  1MI CWDE,    F8  1MI CLC,     FC  1MI CLD,     FA  1MI CLI,
 F5  1MI CMC,     38 13MI CMP,     A6  5MI CMPS,    99  1MI CWD,
 27  1MI DAA,     2F  1MI DAS,     08  9MI DEC,     30  7MI DIV,
        ( ESC )   F4  1MI HLT,     38  7MI IDIV,    28  7MI IMUL,
        ( IN )    00  9MI INC,     6C  5MI INS,     ( INT )
0CE  1MI INTO,   0CF  1MI IRET,    E3  3MI JCXZ,

 77  3MI JA,      73  3MI JAE,     72  3MI JB,      76  3MI JBE,
 74  3MI JE,      7F  3MI JG,
 7D  3MI JGE,     7C  3MI JL,      7E  3MI JLE,  20 E9 11MI JMP,
 75  3MI JNE,     71  3MI JNO,     79  3MI JNS,     70  3MI JO,
 7A  3MI JPE,     7B  3MI JPO,     78  3MI JS,

 9F  1MI LAHF,
 C5  4MI LDS,     8D  4MI LEA,     C4  4MI LES,     B4 16MI LFS,
 B5 16MI LGS,     F0  1MI LOCK,   0AC  6MI LODS,    E2  3MI LOOPA,
 E1  3MI LOOPE,   E0  3MI LOOPNE,  B2 16MI LSS,

       ( MOV )   0A4  5MI MOVS,    20  7MI MUL,     18  7MI NEG,
 90  1MI NOP,      10  7MI NOT,     08 13MI OR,      ( OUT )
 6E  5MI OUTS,

    A1  58  8F 07 58 12MI POP,     60  1MI PUSHA,   9D  1MI POPF,
    A0  68 0FF 36 50 12MI PUSH,    61  1MI POPA,    9C  1MI PUSHF,
 10 10MI RCL      18 10MI RCR,
 F2  1MI REP      F2  1MI REPNZ,   F3  1MI REPZ,
 C3 14MI RET      00 10MI ROL,      8 10MI ROR,     9E  1MI SAHF,
 38 10MI SAR      18 13MI SBB,    0AE  5MI SCAS,          ( SEG )
 20 10MI SHL      28 10MI SHR,     F9  1MI STC,     FD  1MI STD,
 FB  1MI STI     0AA  6MI STOS,    28 13MI SUB,           ( TEST )
 9B  1MI WAIT           ( XCHG )  D7  1MI XLAT,    30 13MI XOR,
 C2 14MI +RET

\ Structured Conditionals
\ single pass forces fixed size. optimize for small, fast structures:
\ always use 8-bit offsets.

\ Assembler version of forward/backward mark/resolve.

VARIABLE LONG-OFFSETS  LONG-OFFSETS OFF

: >MARK     ( -- addr )  HERE  ;  \ Address of opcode, not offset byte
: >RESOLVE  ( addr -- )
   LONG-OFFSETS @  IF
      \ Opcode is 2 bytes long
      2 + HERE OVER   ( offset-adr target-adr offset-adr )
      REAL?  IF       ( offset-adr target-adr offset-adr )
         2 + -  SWAP ASM16!  \ 2-byte offset
      ELSE
         4 + -  SWAP ASM32!  \ 4-byte offset
      THEN
   ELSE
      1+ HERE OVER 1+ - DUP BIG? ABORT" branch offset is too large "
      SWAP ASM8!
   THEN
;
: <MARK     ( -- addr )  HERE   ;
: <RESOLVE  ( addr op -- )
   SWAP HERE 2+ -  DUP SMALL? IF	( op offset )
      SWAP ASM8,  ASM8,
   ELSE
      SWAP DUP 0x0EB = IF		( offset op )
	 DROP HERE 2+ +                 ( addr )
         #) JMP,
      ELSE
	 0x0F ASM8,  0x10 + ASM8,	( offset )
	 REAL? IF 2 ELSE 4 THEN - ADR,
      THEN
   THEN
;

: BUT  ( mark1 mark1 -- mark2 mark1 )  SWAP  ;

HEX

\ These conditional test words leave the opcodes of conditional
\ branches to be used by the structured conditional words.
\   For example,
\    5 # ECX CMP   0< IF   EAX EBX ADD   ELSE   EAX EBX SUB   THEN
\    begin   dx al in   tbe # al and   0<> until

\ It is tempting to use "CS" for "Carry Set", but that conflicts
\ with the CS register.

75 CONSTANT 0=   74 CONSTANT 0<>   79 CONSTANT 0<
75 CONSTANT  =   74 CONSTANT  <>   73 CONSTANT CARRY?   72 CONSTANT NO-CARRY?
78 CONSTANT 0>=  7D CONSTANT <     7C CONSTANT >=
7F CONSTANT <=   7E CONSTANT >     73 CONSTANT U<
72 CONSTANT U>=  77 CONSTANT U<=   76 CONSTANT U>
71 CONSTANT OV	 E3 CONSTANT CXNZ

\ One of the very best features of FORTH assemblers is the ability
\ to use structured conditionals instead of branching to nonsense
\ labels.
: IF,
   >MARK SWAP
   LONG-OFFSETS @  IF
      0x0F ASM8,  0x10 + ASM8,
      REAL?  IF  0 ASM16,  ELSE  0 ASM32,  THEN
   ELSE
      ASM8,  0 ASM8,
   THEN
;
: THEN,    >RESOLVE   ;
: BEGIN,   <MARK   ;
: UNTIL,   <RESOLVE   ;
: AHEAD,   0EB IF,          ;
: ELSE,    AHEAD,  BUT   THEN,  ;
: AGAIN,   0EB UNTIL,   ;
: WHILE,   IF,   BUT  ;
: REPEAT,  AGAIN,   THEN,   ;

\ why lose DO ???
\ XXX : DO      # ECX MOV,   HERE   ;
DECIMAL

\ aliases
: MOVB,    BYTE MOV,  ;
: MOVSB,   BYTE MOVS, ;
: LODSB,   BYTE LODS, ;
: STOSB,   BYTE STOS, ;
: INSB,    BYTE INS,  ;
: OUTSB,   BYTE OUTS, ;

ONLY FORTH ALSO DEFINITIONS
ASSEMBLER ALSO
ALIAS REAL? REAL?
ALIAS REAL-MODE REAL-MODE
ALIAS PROTECTED-MODE PROTECTED-MODE
ONLY FORTH ALSO DEFINITIONS
