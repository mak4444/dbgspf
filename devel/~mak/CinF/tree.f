
HERE
 $"  "
 $" CNS "
 $" ARG"
 $" ! "
 $" @ "
 $" CVC"
 $" CVD"
 $" CVF"
 $" CVI"
 $" CVP"
 $" CVS"
 $" CVU"
 $" NEGATE "
 $" CALL"
 $" *LOAD*"
 $" RET "
 $" &G"
 $" &F"
 $" &L"
 $" + "
 $" - "
 $" LSHIFT "
 $" MOD"
 $" RSHIFT "
 $" BAND"
 $" INVERT "
 $" BOR"
 $" BXOR"
 $" / "
 $" * "
 $" = "
 $" >= "
 $" > "
 $" <= "
 $" < "
 $" <> "
 $" JUMP"
 $" LABEL"
 $" AND "
 $" NOT "
 $" OR "
 $" COND"
 $" RIGHT"
 $" $FIELD"
0 C,

CREATE opnames
DUP , COUNT + DUP C@ [IF] >IN 0! [THEN] DROP
HERE opnames - CELL/ CONSTANT opnamesSize
HERE
    $" 0" $" F" $" D" $" C" $" S" $"  " $"  " $"  " $" V" $" B"
    $" 10" $" 11" $" 12" $" 13" $" 14" $" 15"
0 C,

CREATE suffixes
DUP , COUNT + DUP C@ [IF] >IN 0! [THEN] DROP

HERE
\      CVC
$" 0" $" F" $" D" $" C" $" S" $"  " $"  " $" P" $" V" $" B"
     $" 10" $" 11" $" 12" $" 13" $" 14" $" 15"
\      "CVD",
$" 0" $" F" $" D" $" C" $" S" $"  " $"  " $" P" $" V" $" B"
     $" 10" $" 11" $" 12" $" 13" $" 14" $" 15"
\      "CVF",
$" 0" $" F" $" D" $" C" $" S" $" F>DS" $" F>DS" $" P" $" V" $" B"
     $" 10" $" 11" $" 12" $" 13" $" 14" $" 15"
\      "CVI",
$" 0" $" DS>F" $" D" $" C" $" S" $"  " $"  " $"  " $" V" $" B"
     $" 10" $" 11" $" 12" $" 13" $" 14" $" 15"
\      "CVP",
$" 0" $" F" $" D" $" C" $" S" $"  " $"  " $" P" $" V" $" B"
     $" 10" $" 11" $" 12" $" 13" $" 14" $" 15"
\      "CVS",
$" 0" $" F" $" D" $" C" $" S" $"  " $"  " $" P" $" V" $" B"
     $" 10" $" 11" $" 12" $" 13" $" 14" $" 15"
\      "CVU",
$" 0" $" 0 D>F" $" D" $" C" $" S" $"  " $"  " $" P" $" V" $" B"
    $" 10" $" 11" $" 12" $" 13" $" 14" $" 15"
0 C,

CREATE cv
DUP , COUNT + DUP C@ [IF] >IN 0! [THEN] DROP

CREATE "name" 20 ALLOT
: opname { op -- "name" }
        op optype  UNSIGNED =
        op opindex 16 * GE >= AND
        op opindex 16 * LT <= AND
        IF    ." VV=" . . . 86 ZZZ
\          return stringf("U%s", opnames[opindex(op)]);
        THEN
        op opindex 16 * CVF  0x20 - >
        op opindex 16 * CVU <= AND
        IF   cv   op opindex 16 * CVF  0x20 - - op optype +  [] EXIT
        THEN

        op generic  $AND >=
        op generic $FIELD <= AND
        op opsize  0= AND
        IF  opnames op opindex [] COUNT "name" $+! "name"  EXIT
        THEN
        suffixes  op  optype []  COUNT  "name" $!

        op  opsize 1 = IF S" C" "name" $+! ELSE
        op  opsize 2 = IF S" W" "name" $+! THEN THEN
        op opindex opnamesSize U<
        IF opnames op opindex [] COUNT
        ELSE ." ZZ=" . . 11 ZZZ \ stringd(opindex(op))
        THEN                            "name" $+!
        "name"

;
\ :  opname
\ S" DUP 96 = IF ZZZ THEN opname_ " EVALUATE  ; IMMEDIATE

: tree {  op  type  left  right \  p -- p }
><CDP   HERE TO p    \tree ALLOT
><CDP   op    p -}op !
        type  p -}ttype !
        left  p t-kids[0] !
        right p t-kids[1] !
        p
;

0 VALUE     warn

: root (  p -- Tree )
         0 TO warn
\          root1
   DROP 0
;

: texpr { f  tok a \  p  -- p }
\        where TO save
\        a TO where
        tok f EXECUTE TO p
\        save TO where
        p
;

\EOF

static char rcsid[] = "$Id: tree.nw,v 2.8 1998/09/03 16:49:01 drh Exp $";

int where = STMT;
static int nid = 1;             /* identifies trees & nodes in debugging output */
static struct nodeid {
        int printed;
        Tree node;
} ids[500];                     /* if ids[i].node == p, then p's id is i */

static void printtree1(Tree, int, int);


int nodeid(Tree p) {
        int i = 1;

        ids[nid].node = p;
        while (ids[i].node != p)
                i++;
        if (i == nid)
                ids[nid++].printed = 0;
        return i;
}

/* printed - return pointer to ids[id].printed */
int *printed(int id) {
        if (id)
                return &ids[id].printed;
        nid = 1;
        return 0;
}

/* printtree - print tree p on fd */
void printtree(Tree p, int fd) {
        (void)printed(0);
        printtree1(p, fd, 1);
}

/* printtree1 - recursively print tree p */
static void printtree1(Tree p, int fd, int lev) {
        FILE *f = fd == 1 ? stdout : stderr;
        int i;
        static char blanks[] = "                                                   ";

        if (p == 0 || *printed(i = nodeid(p)))
                return;
        fprint(f, "#%d%S%S", i, blanks, i < 10 ? 2 : i < 100 ? 1 : 0, blanks, lev);
        fprint(f, "%s %t", opname(p->op), p->type);
        *printed(i) = 1;
        for (i = 0; i < NELEMS(p->kids); i++)
                if (p->kids[i])
                        fprint(f, " #%d", nodeid(p->kids[i]));
        if (p->op == FIELD && p->u.field)
                fprint(f, " %s %d..%d", p->u.field->name,
                        fieldsize(p->u.field) + fieldright(p->u.field), fieldright(p->u.field));
        else if (generic(p->op) == CNST)
                fprint(f, " %s", vtoa(p->type, p->u.v));
        else if (p->u.sym)
                fprint(f, " %s", p->u.sym->name);
        if (p->node)
                fprint(f, " node=%p", p->node);
        fprint(f, "\n");
        for (i = 0; i < NELEMS(p->kids); i++)
                printtree1(p->kids[i], fd, lev + 1);
}
