~mak\CinF\ct.f

0
CELL FIELD -}name
CELL FIELD -}scope
CELL FIELD -}up
\ CELL FIELD -}uses
CELL FIELD -}sclass
\ CELL FIELD -}structarg

CELL FIELD -}addressed
\ CELL FIELD -}computed
CELL FIELD -}temporary \ !!!
CELL FIELD -}generated
CELL FIELD -}defined
CELL FIELD -}stype
FCELL FIELD -}ref
DUP
      CELL FIELD -}llabel
\      CELL FIELD -}equatedto  \ }l
OVER
      CELL FIELD -}cfields \ !!!
\      CELL FIELD -}vfields
\      CELL FIELD -}ftab       \  omit
      CELL FIELD -}flist      \  } s;
MAX OVER
      CELL FIELD -}value   \ !!!
\ MAX OVER
\      CELL FIELD -}idlist
MAX OVER
     FCELL FIELD -}min
     FCELL FIELD -}max
MAX OVER

     FCELL FIELD -}v
      CELL FIELD -}loc
MAX OVER
\      CELL FIELD -}pt
      CELL FIELD -}label
\      CELL FIELD -}ncalls
\      CELL FIELD -}callee
\ MAX OVER
\      CELL FIELD -}seg
MAX OVER
      CELL FIELD -}alias
\ MAX OVER
\      CELL FIELD -}cse
\      CELL FIELD -}replace
\      CELL FIELD -}next
MAX NIP
   Xsymbol FIELD -}sx

CONSTANT symbol

0
CELL FIELD -}op
CELL FIELD -}ttype
CELL FIELD -}align
CELL FIELD -}size
   0 FIELD -}u.sym
CELL FIELD -}f.oldstyle
CELL FIELD -}f.proto
\ CELL FIELD -}tx
CONSTANT type

0  -}ttype  CELL+
CELL FIELD  t-kids[0]
CELL FIELD  t-kids[1]
CELL FIELD  tree.node
   0 FIELD  tree.field
   0 FIELD  tree.sym
2 CELLS FIELD  tree.v
CONSTANT \tree

0
CELL+ \ -}name
CELL FIELD f.type
CELL FIELD f.offset
CELL FIELD f.bitsize \ !!!
CELL FIELD f.lsb \ !!!
CELL FIELD f.link
CONSTANT \field


0 VALUE tsym

1 CONSTANT CONSTANTS
2 CONSTANT LABELS
3 CONSTANT GLOBAL
4 CONSTANT PARAM
5 CONSTANT LOCAL

: isqual -}op @ CONST = ;

: unqual  ( t -- t' )  DUP isqual  IF -}ttype @ THEN ;

: isfunc  DUP 0= IF EXIT THEN  unqual -}op @ FUNCTION = ;

: isarray  unqual -}op @ ARRAY = ;

: isptr
   DUP 0= IF EXIT THEN
   unqual -}op @ POINTER = ;

: isenum ( t -- flag )   unqual -}op ENUM = ;

: isstruct { t -- flag }
  t unqual -}op @ STRUCT =
  t unqual -}op @ UNION  = OR ;

: isfloat
   unqual -}op @ FLOT = ;

: isunsigned
   unqual -}op @ UNSIGNED = ;

: isarith
   unqual -}op @ UNSIGNED <= ;

: isscalar
  DUP unqual -}op @ POINTER <=
  SWAP unqual -}op @   ENUM = OR ;

: isunion 
  unqual -}op @  UNION = ;

: isint
  DUP unqual -}op @    INT =
  SWAP unqual -}op @   UNSIGNED = OR ;

: ischar
    DUP  -}size @ 1 =
    SWAP isint AND ;

0 VALUE  kind

: kind[t] kind t + C@ ;

: istypename { t tsym -- flag }
  kind t + C@ xCHR =
  tsym DUP IF -}sclass C@ TYPEDEF =  t  ID = AND THEN OR
;

: isconst { t -- flag }
   t -}op @  CONST =
   t -}op @  CONST VOLATILE + = OR ;

: isvolatile { t -- flag }
   t -}op @  VOLATILE =
   t -}op @  CONST VOLATILE + = OR ;


0 VALUE Aflag           \ >= 0 if -A specified
\ 0 VALUE Pflag           \ != 0 if -P specified 
0 VALUE glevel          \ == [0-9] if -g[0-9] specified 
0 VALUE xref            \ != 0 for cross-reference data 

0 CONSTANT PERM
1 CONSTANT FUNC
2 CONSTANT STMT

: ones { n }
  n  32 >=
    IF   -1
    ELSE -1 n LSHIFT INVERT
    THEN
;

0
   1 FIELD m.size
   1 FIELD m.align
   1 FIELD m.outofline
DROP

CREATE charmetric        1 C,  1 C, 0 C,
CREATE shortmetric       2 C,  2 C, 0 C,
CREATE intmetric         4 C,  4 C, 0 C,
CREATE longmetric        4 C,  4 C, 0 C,
CREATE longlongmetric    4 C,  4 C, 0 C,
CREATE floatmetric      FCELL C, FCELL C, 0 C,
CREATE doublemetric     FCELL C, FCELL C, 0 C,
CREATE longdoublemetric FCELL C, FCELL C, 0 C,
CREATE ptrmetric         4 C,  4 C, 0 C,
CREATE structmetric      0 C,  4 C, 0 C,

0 VALUE entry
0 VALUE token

0
CELL FIELD  lp->x
CELL FIELD  lp->link
CONSTANT \list

VECT rmtypes
VECT decl

0 CONSTANT Blockbeg
1 CONSTANT Blockend
\ 2 CONSTANT Local
\ 3 CONSTANT Address
4 CONSTANT Defpoint
5 CONSTANT Label
6 CONSTANT Start
7 CONSTANT Gen
8 CONSTANT Jump
9 CONSTANT Switch

0
CELL FIELD -}kind
CELL FIELD -}prev
CELL FIELD c.next

\       union {
   0 FIELD u.begin
   0 FIELD u.forest

CELL FIELD block.level
CELL FIELD block.locals
CELL FIELD block.identifiers
\ CELL FIELD block.types
\ Env FIELD block.x
CONSTANT \code

0
CELL FIELD -}sym
CELL FIELD -}lab
CELL FIELD -}deflab
CELL+ \ FIELD -}size
CELL FIELD -}ncases
CELL FIELD -}values
CELL FIELD -}labels
CONSTANT \swtch

(*
                Symbol var;

                struct {
                        Symbol sym;
                        Symbol base;
                        long offset;
                } addr;
                struct {
                        Symbol sym;
                        Symbol table;
                        Symbol deflab;
                        int size;
                        long *values;
                        Symbol *labels;
                } swtch;

        } u;
};
*)


 0 VALUE cfunc
 0 VALUE retv
-1 VALUE ncalled

: sizeop   ( op -- n ) 10 LSHIFT ;
: specific ( op -- n )           0x3FF          AND ;
: opindex  ( op -- n )  4 RSHIFT 0x3F           AND ;
: optype   ( op -- n )           0xF            AND ;

VECT ttob
: mkop { op ty -- n }
    op  ty ttob + specific
;

VECT defsymbol
CREATE codehead \code  ALLOT
       codehead \code  ERASE
Start  codehead -}kind !
codehead VALUE codelist

: gop  4 LSHIFT CONSTANT ;
1 gop CNST
2 gop ARG
3 gop ASGN
4 gop INDIR
7 gop CVF
8 gop CVI
9 gop CVP
11 gop CVU
12 gop NEG
13 gop CALL
14 gop LOAD
15 gop $RET
16 gop ADDRG
17 gop ADDRF
18 gop ADDRL
19 gop ADD
20 gop SUB
21 gop LSH
22 gop $MOD
23 gop RSH
24 gop BAND
25 gop BCOM
26 gop BOR
27 gop BXOR
28 gop DIV
29 gop MUL
30 gop EQ
31 gop GE
32 gop GT
33 gop LE
34 gop LT
35 gop NE
36 gop JUMP
37 gop LABEL
38 gop $AND
39 gop $NOT
40 gop $OR
41 gop COND
42 gop RIGHT
43 gop $FIELD

: opsize 10 RSHIFT ;

: generic 0x3F0 AND ;

VECT doconst
VECT N_code
VECT eqtype
VECT expr
VECT asgntree
VECT Ilocal
VECT compound
VECT consttree
VECT statement
VECT cast
VECT simplify
VECT call
VECT dclr
VECT dclr1
VECT dclparam
VECT cnsttree
VECT initializer
VECT typename
VECT unary
VECT specifier
VECT addrof

0 VALUE chartype                  \ char
0 VALUE doubletype                \ double
0 VALUE floattype                 \ float
0 VALUE inttype                   \ signed int 
0 VALUE longdouble                \ long double 
0 VALUE longtype                  \ long 
0 VALUE longlong                  \ long long 
0 VALUE shorttype                 \ signed short int
0 VALUE signedchar                \ signed char
0 VALUE unsignedchar              \ unsigned char
0 VALUE unsignedlong              \ unsigned long int 
0 VALUE unsignedlonglong          \ unsigned long long int
0 VALUE unsignedshort             \ unsigned short int
0 VALUE unsignedtype              \ unsigned int
0 VALUE funcptype                 \ void (*)()
0 VALUE charptype                 \ char*
0 VALUE voidptype                 \ void*
0 VALUE voidtype                  \ basic types: void
0 VALUE unsignedptr               \ unsigned type to hold void*
0 VALUE signedptr                 \ signed type to hold void*
0 VALUE widechar                  \ unsigned type that represents wchar_t

0 VALUE optree

0 VALUE oper

: *optree[ CELLS optree + @ EXECUTE ;
: +U: UNSIGNED + ;
: +I: INT + ;
: +P: POINTER + ;
: +F: FLOT + ;
: +B: STRUCT + ;

: isaddrop ( op -- f )
  DUP  specific ADDRG +P: =
  OVER specific ADDRL +P: = OR
  SWAP specific ADDRF +P: = OR ;
0 [IF]
: roundup ( n align -- n' )
  2DUP
  MOD DUP IF - + ELSE 2DROP THEN ;
[ELSE]
: roundup ( n align -- n' )
 1- DUP >R + R> INVERT AND ;
[THEN]
0 VALUE QPRT
0 VALUE DOGS
0x7FFFFFFF CONSTANT INT_MAX
0 VALUE YYnull
0 VALUE YYcheck
\ 0 VALUE explicitCast
0 VALUE offset
