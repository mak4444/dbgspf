
:  Idefaddress { p -- }
   p -}sx x.name @ COUNT TOEVAL
   TE"  ," DOCR
;

:NONAME {  p }
        p -}scope @ CONSTANTS =
        IF        p -}stype @ ttob optype
                CASE
                INT OF p -}esym -}name @  p -}sx x.name !  ENDOF
           UNSIGNED OF p -}esym -}name @  p -}sx x.name !  ENDOF
               FLOT OF ZZZ \ p->x.name = stringf("%se", p->name); break;
                    ENDOF
            POINTER OF ZZZ \ p->x.name = stringf("%U", p->u.c.v.p); break;
                    ENDOF
                0 assert
               ENDCASE
        ELSE
         p -}scope @ LOCAL >=  p -}sclass @  STATIC = AND
        IF
><CDP           HERE p -}sx x.name !
             S" $" PAD $!
             1 genlabel U>D <# #S #> PAD $+!
             PAD COUNT S",
><CDP
\                p->x.name = stringf("$%d", genlabel(1));
        ELSE
        p -}scope @ LABELS = p -}generated @ OR
        IF
><CDP           HERE p -}sx x.name !
             S" $" PAD $!
             p -}name @ COUNT PAD $+!
             PAD COUNT S",
><CDP
\                p->x.name = stringf("$%s", p->name);
        ELSE
              p -}name @ p -}sx x.name !
        THEN THEN THEN

;   TO defsymbol

: Ispace ( n -- )
        S"  " TOEVAL
 U>D <# #S #> TOEVAL
   S"  ALLOT" TOEVAL DOCR
;

: Iglobal { p }
        p -}sx x.name @ 1+ C@ [CHAR] $ =
        IF S" $" TOEVAL
        THEN
        S" CREATE "     TOEVAL
        p -}sx x.name @ COUNT TOEVALBL
;

:NONAME  { p -- }
        offset p -}stype @ -}align @ roundup TO offset
\        p->x.name = stringf("%d", offset);
        offset p x.offset !
        p -}stype @ -}size @  offset + TO offset
; TO Ilocal

: Idefconst { suffix  size  v }
        suffix

        CASE
          INT OF  TE"  ," DOCR EXIT ENDOF
     UNSIGNED OF  TE"  ," DOCR EXIT ENDOF
      POINTER OF  TE"  ," DOCR EXIT ENDOF
         FLOT OF  TE" F," DOCR EXIT ENDOF
      0  assert
        ENDCASE
;

: Idefstring {  len str -- }
        TE"  HS," [CHAR] " TEC  BL TEC
        str 1+ len BOUNDS
        DO  I C@ BL <
            I C@ [CHAR] % = OR
            I C@ [CHAR] " = OR
            I C@ 0xF8  >    OR
            IF  BASE @ HEX
                 TE" %"  I C@ 0 <# # # #> TOEVAL
                BASE !
            ELSE I C@ TEC
            THEN
        LOOP     [CHAR] " TEC DOCR
;

\EOF

static void I(segment)(int n) {
        static int cseg;

        if (cseg != n)
                switch (cseg = n) {
                case CODE:  return;
                case DATA:  return;
                case BSS:   return;
                case LIT:   return;
                default: assert(0);
                }
}

static void I(address)(Symbol q, Symbol p, long n) {
        q->x.name = stringf(" %s %D +", p->x.name, n);
}


/*
static void dumptree_m(Node);

static void dumptree(Node p) {
//      print("{");
        dumptree_m(p);
//      print("}");
}
*/
#define  TPBufSize ( 1 << 8 )

char TPBuf[TPBufSize];
int TPBufIn=1;
int TPBufOut=0;




int xxxx;

static void I(export)(Symbol p) {
//      print("export %s\n", p->x.name);
}


static void I(import)(Symbol p) {
//      print("import %s\n", p->x.name);
}


static void I(progbeg)(int argc, char *argv[]) {}

static void I(progend)(void) {}


static void I(stabline)(Coordinate *cp) {
        static char *prevfile;
        static int prevline;

        if (cp->file && (prevfile == NULL || strcmp(prevfile, cp->file) != 0)) {
                print("file \"%s\"\n", prevfile = cp->file);
                prevline = 0;
        }
        if (cp->y != prevline)
                print("line %d\n", prevline = cp->y);
}

#define b_blockbeg blockbeg
#define b_blockend blockend

Interface bytecodeIR = {
        1, 1, 0,        /* char */
        2, 2, 0,        /* short */
        4, 4, 0,        /* int */
        4, 4, 0,        /* long */
        4, 4, 0,        /* long long */
        12, 12, 0,      /* float */
        12, 12, 0,      /* double */
        12, 12, 0,      /* long double */
        4, 4, 0,        /* T* */
        0, 4, 0,        /* struct */
        0,              /* little_endian */
        0,              /* mulops_calls */
        0,              /* wants_callb */
        0,              /* wants_argb */
        1,              /* left_to_right */
        0,              /* wants_dag */
        0,              /* unsigned_char */
        I(address),
        I(blockbeg),
        I(blockend),
        I(defaddress),
        I(defconst),
        I(defstring),
        I(defsymbol),
        I(emit),
        I(export),
        I(function),
        I(gen),
        I(global),
        I(import),
        I(local),
        I(progbeg),
        I(progend),
        I(segment),
        I(space),
        0,              /* I(stabblock) */
        0,              /* I(stabend) */
        0,              /* I(stabfend) */
        0,              /* I(stabinit) */
        I(stabline),
        0,              /* I(stabsym) */
        0,              /* I(stabtype) */
};
