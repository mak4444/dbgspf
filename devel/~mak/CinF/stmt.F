
VARIABLE stmtlabs
 Start codehead -}kind !

: reachable { kind \ cp -- flag }
        kind Start >
        IF      codelist TO cp
                BEGIN  cp -}kind @ Label <
                WHILE  cp -}prev @ TO cp
                REPEAT
                cp -}kind @  Jump = cp -}kind @ Switch = OR
                IF 0 EXIT
                THEN
        THEN
        1
;


: definept { p \ cp -- }

        Defpoint  N_code TO cp
        ncalled 0>
        IF       ." DD=" . . 1 ZZZ
\                int n = findcount(cp->u.point.src.file,
\                        cp->u.point.src.x, cp->u.point.src.y);
\                if (n > 0)
\                        refinc = (float)n/ncalled;
        THEN
        glevel  2 >
        IF ." DD=" . . 42 ZZZ \ locus(identifiers, &cp->u.point.src);
        THEN
(* !!!!
        if (events.points && reachable(Gen))
                {
                        Tree e = NULL;
                        apply(events.points, &cp->u.point.src, &e);
                        if (e)
                                listnodes(e, 0, 0);
                }
*)
;

:NONAME  { kind \ cp -- cp }
        kind  reachable 0=
        IF warning" unreachable code" CR
        THEN
><CDP   HERE TO cp  \code ALLOT cp \code ERASE
><CDP   kind cp -}kind !
        codelist cp -}prev !
        cp  c.next 0!
        cp codelist  c.next !
        cp TO codelist
        cp
; TO N_code

: retcode { p \ ty -- }
        p 0=
        IF
\ !!!!               if (events.returns)
\                        apply(events.returns, cfunc, NULL);
            EXIT
        THEN
        p  pointer TO p
        cfunc -}stype @ freturn  p assign   TO ty
        ty 0=
        IF
            ." illegal return type; found"  p -}ttype @ 20 DUMP
            ." expected"  cfunc -}stype @ freturn     20 DUMP
                        ABORT
        THEN
           p  ty cast  TO p
        retv
        IF          ." FF=" . . 61 ZZZ
(*                {
                        if (iscallb(p))
                                p = tree(RIGHT, p->type,
                                        tree(CALL+B, p->type,
                                                p->kids[0]->kids[0], idtree(retv)),
                                        rvalue(idtree(retv)));
                        else
                                p = asgntree(ASGN, rvalue(idtree(retv)), p);
                        walk(p, 0, 0);
                        if (events.returns)
                                apply(events.returns, cfunc, rvalue(idtree(retv)));
                        return;
*)
         THEN
(*        if (events.returns)
                {
                        Symbol t1 = genident(AUTO, p->type, level);
                        addlocal(t1);
                        walk(asgn(t1, p), 0, 0);
                        apply(events.returns, cfunc, idtree(t1));
                        p = idtree(t1);
                }
*)      p -}ttype @ isfloat 0=
        IF         p p -}ttype @ promote cast TO p
        THEN
        p -}ttype isptr
        IF   ." KK=" . . 97 ZZZ
(*                {
                        Symbol q = localaddr(p);
                        if (q && (q->computed || q->generated))
                                warning("pointer to a %s is an illegal return value\n",
                                        q->scope == PARAM ? "parameter" : "local");
                        else if (q)
                                warning("pointer to %s `%s' is an illegal return value\n",
                                        q->scope == PARAM ? "parameter" : "local", q->name);
                }
*)
        THEN
         $RET p -}ttype @ mkop p -}ttype @ p 0 tree DROP \ 0 0 walk
;

: definelab {  lab \ cp p -- }
        lab findlabel TO p

        lab  assert
\        0 0 0 walk
	0  Label  N_code u.forest !
        codelist -}prev @ TO cp
        BEGIN cp -}kind @ Label <=
        WHILE cp -}prev @ TO cp
        REPEAT
        BEGIN
            cp u.forest @ DUP
            IF  DROP
                cp -}kind @ Jump =
\                cp u.forest @ ->kids[0]  AND
\                cp u.forest @ ->kids[0] node.op @ specific ADDRG POINTER + = AND
\                cp u.forest @ ->kids[0] ->syms[0]   p =         AND
            THEN
        WHILE
\                cp u.forest @ ->kids[0] ->syms[0] -}llabel @ lab = assert
                -1e p -}ref F+!
                cp c.next @ assert
                cp -}prev @ assert
                cp c.next @ cp -}prev @ c.next !
                cp -}prev @ cp c.next @ -}prev !
                cp -}prev @ TO cp
                BEGIN  cp -}prev @   Label <=
                WHILE  cp -}prev @ TO cp
                REPEAT
        REPEAT
;

: jump { lab \ p -- Node }
        lab findlabel TO p
        1e p -}ref F+!
        0
;

: branch {  lab \  cp  p  -- }
        lab findlabel TO p
        lab assert
\        0 0 0 walk
        lab jump  Label 
  N_code u.forest !
        codelist -}prev @ TO cp
        BEGIN   cp -}kind @ Label  <
        WHILE   cp -}prev TO cp
        REPEAT
        BEGIN  cp -}kind @  Label =
               cp   u.forest @ DUP
                 IF -}op @  LABEL VOID + = THEN AND DUP
            IF DROP
\Z               cp   u.forest @ ->syms[0]  p ." WW=" . . 48 ZZZ equal 0=
            THEN
        WHILE             ." WW=" . . 48 ZZZ
(*                equatelab(cp->u.forest->syms[0], p);
                assert(cp->next);
                assert(cp->prev);
                cp->prev->next = cp->next;
                cp->next->prev = cp->prev;
                cp = cp->prev;
                while (cp->kind < Label)
                        cp = cp->prev;
*)
        REPEAT
        cp -}kind @  Jump =
        cp -}kind @  Switch =  OR
        IF
\                p->ref--;
\                codelist->prev->next = NULL;
\                codelist = codelist->prev;
        ELSE
                Jump  codelist -}kind !
               cp -}kind @  Label =
               cp   u.forest @ DUP
                 IF -}op @  LABEL VOID + = THEN AND DUP
            IF DROP
\Z               cp   u.forest @ ->syms[0]  p equal 0=
            THEN
                IF warning" source code specifies an infinite loop"
                THEN
        THEN
;

:  conditional { tok \ p --  Tree }
        tok  expr TO p
         Aflag  1 >   p -}ttype @ isfunc AND
         IF \Z .warning  p funcname COUNT TYPE
               ." used in a conditional expression"
         THEN
         p cond
;

: ifstmt { lab  loop  swp  lev -- }
        gettok TO t
        [CHAR] ( 0 expect
        QPRT 2+ TO QPRT
        0 definept
        [CHAR] ) conditional DROP \ 0 lab walk
        QPRT 2- TO QPRT
        DOCR   TE" IF " DOEVAL
        refinc F@ 2.0e F/ refinc F!
        loop swp lev statement
        t  xELSE =
        IF    DOCR   TE" ELSE " DOEVAL
                lab  1+ branch
                gettok TO t
                lab definelab
                loop swp lev statement
                lab  1+ findlabel -}ref F@ F0<>
                IF    lab  1+ definelab
                THEN
        ELSE    lab  definelab
        THEN
        DOCR   TE" THEN "  DOEVAL
;

\ whilestmt - while ( expression ) statement
: whilestmt { lab  swp  lev \  e -- }

        refinc F@  10.0e F* refinc F!
        gettok TO t
        DOCR   TE" BEGIN " DOEVAL
        QPRT 2+ TO QPRT
        [CHAR] ( 0 expect
\        0 0 0 walk
        ['] conditional [CHAR]  ) FUNC texpr TO e
        QPRT 2- TO QPRT
        DOCR TE" WHILE A_CASE B_CASE " DOEVAL
        DUP >R
        lab  1+ branch
        lab definelab
        lab swp lev statement
        lab 1+ definelab
\        e lab 0 walk
        DUP R> <> IF ZZZ THEN
        TE" B_DUPENDCASE"
        DOCR TE" REPEAT A_DUPENDCASE "  DOEVAL \ DP @ TO LAST-HERE \ !!!!!!!!!
        lab  2+ findlabel -}ref F@ F0<>
        IF    lab  2+ definelab
        THEN
;

(* foldcond - check if initial test in for(e1;e2;e3) S is necessary *)
:  foldcond { e1  e2 \ op v -- n }
        e2 -}op @ generic TO op
        e1 0= e2 0= OR
        IF   0 EXIT
        THEN
(Z        if (generic(e1->op) == ASGN && isaddrop(e1->kids[0]->op)
        && generic(e1->kids[1]->op) == CNST) {
                v = e1->kids[0]->u.sym;
                e1 = e1->kids[1];
        } else
                return 0;
        if ((op==LE || op==LT || op==EQ || op==NE || op==GT || op==GE)
        && generic(e2->kids[0]->op) == INDIR
        && e2->kids[0]->kids[0]->u.sym == v
        && e2->kids[1]->op == e1->op) {
                e1 = simplify(op, e2->type, e1, e2->kids[1]);
                if (e1->op == CNST+I)
                        return e1->u.v.i;
        }
        return 0;
*) ;

:  forstmt { lab   swp  lev \ e1 e2 e3 once -- }
        0 TO once
        0 TO  e1 0 TO  e2 0 TO  e3
\        Coordinate pt2, pt3;

        gettok TO t
        [CHAR] ( 0 expect
        0 definept
        kind[t]  ID =
        IF ['] expr0 [CHAR]  ; FUNC texpr TO e1
        ELSE [CHAR] ; 0 expect
        THEN
        DOCR   TE" A_BEGIN " DOEVAL
\        e1 0 0 walk
        refinc F@  10.0e F* refinc F!
        kind[t]  ID =
        IF   QPRT 2+ TO QPRT
             ['] conditional [CHAR]  ; FUNC texpr TO e2
\             TE" 0="
             QPRT 2- TO QPRT
        ELSE [CHAR] ; 0 expect
               TE" TRUE "
        THEN
        DOCR   TE" FOR_WHILE " DOEVAL

        kind[t]  ID =
        IF  ['] expr0 [CHAR] ) FUNC texpr TO e3
         ELSE   [CHAR] ) stop0 test
        THEN
        DOCR   TE" FOR_BODY " DOEVAL
        e2
        IF     TE" \ "
               e1 e2 foldcond TO once DOCR
               once 0=
               IF     lab 3 + branch
               THEN
       THEN
        lab definelab
        lab swp lev statement
        lab 1+ definelab
\        definept(&pt3);
        e3
        IF
\           e3 0 0 walk
        THEN
        e2
        IF
               once 0=
               IF     lab 3 + definelab
               THEN
\               e2 lab 0 walk
        ELSE   lab branch
        THEN
        DOCR  TE" FOR_REPEAT " DOEVAL
        lab  2+ findlabel -}ref F@ F0<>
        IF    lab  2+ definelab
        THEN
;
(* dostmt - do statement while ( expression ) *)
:  dostmt { lab  swp  lev -- }
        refinc F@  10.0e F* refinc F!
        gettok TO t
        DOCR   TE" BEGIN A_CASE B_CASE " DOEVAL
        lab definelab
        lab swp lev statement
        lab 1+ definelab
        WHLE 0 expect
        [CHAR] ( 0 expect
        QPRT 2+ TO QPRT
           TE" B_DUPENDCASE " DOEVAL
        0 definept
        [CHAR] ) conditional DROP \ 0 lab walk
        QPRT 2- TO QPRT
        TE" 0= "
        DOCR TE" UNTIL A_DUPENDCASE " DOEVAL
        lab  2+ findlabel -}ref F@ F0<>
        IF    lab  2+ definelab
        THEN
;

512 CONSTANT SWSIZE

0 VALUE fistCS

: swstmt { loop  lab  lev \ e [ \swtch ] sw head tail -- }
        gettok TO t
        QPRT 2+ TO QPRT
        [CHAR] ( 0 expect
        0 definept
        [CHAR] )  expr  TO e
        QPRT 2- TO QPRT
        DOGS
        IF  DOGS TEC TE" @ " 0 TO DOGS
        THEN
        TE" SWITCH DUP"  DOCR
        1 TO fistCS
         e -}ttype @ isint 0=
        IF TRUE  ABORT" illegal type `%t' in switch expression\n" \ , e->type);
\                e = retype(e, inttype);
        THEN
        e e -}ttype @  promote  cast  TO e
(*        e -}op @ generic INDIR = e  t-kids[0] @  -}op @ isaddrop AND
        e t-kids[0] @ tree.sym @ -}stype @  e -}ttype @ = AND
        e t-kids[0] @ tree.sym @ -}stype @  isvolatile 0= AND
        IF
*)           e t-kids[0] @ tree.sym @ sw -}sym !
\                0 0 0  walk
(*        ELSE
                sw.sym = genident(REGISTER, e->type, level);
                addlocal(sw.sym);
                walk(asgn(sw.sym, e), 0, 0);
      THEN
*)
        ['] Switch  N_code TO head
        lab sw -}lab !
        sw -}deflab 0!
        sw -}ncases 0!
        SWSIZE sw -}size !
><CDP   HERE  sw -}values ! SWSIZE CELLS ALLOT
        HERE  sw -}labels ! SWSIZE CELLS ALLOT
><CDP   refinc F@  10.0e F/ refinc F!
        loop sw lev  statement
        sw -}deflab @ 0=
        IF      lab findlabel sw -}deflab !
                lab definelab
\                sw -}ncases @ 0=
\                IF  warning" switch statement with no cases\n"
\                THEN
        THEN
        lab 1+ findlabel -}ref F@ F0<>
        IF      lab 1+ definelab
        THEN
        codelist TO tail
        head -}prev @ TO codelist
        head -}prev 0!
\        codelist -}next 0!
(*        sw ->ncases  0>
        IF  swgen(&sw);
        THEN
*)      lab branch
        fistCS
        IF  TE" B_AHEAD"
        THEN
        DOCR TE" ENDSWITCH"  DOCR
        codelist   head c.next @ -}prev !
        head c.next @   codelist c.next !
        tail TO codelist
;

: stmtlabel { \ p -- }
        token stmtlabs @ lookup TO p
        p 0=
        IF      token stmtlabs 0 FUNC install TO p
                LABELS  p -}scope !
                1  genlabel p -}llabel !
        THEN
        p -}defined @
        IF TRUE ABORT" redefinition of label `%s' previously defined at %w\n" \ , p->name, &p->src);
        THEN
        1 p -}defined !
        p -}llabel @ definelab
        gettok TO t
        [CHAR] : 0 expect
        TE" VLABEL $" p -}llabel @ TE.

;

:NONAME  { loop  swp  lev \ e p lab -- }
        refinc F@ F>R
        Aflag 2 >=  lev 15 = AND
        IF warning" more than 15 levels of nested statements" CR
        THEN
        t
        CASE
         xIF  OF   2 genlabel  loop swp lev 1+  ifstmt
              ENDOF
        WHLE  OF   3 genlabel swp lev  1+ whilestmt
              ENDOF
        xDO  OF    3 genlabel swp lev  1+  dostmt
                   [CHAR] ; 0 expect
          ENDOF
        FOR  OF  4 genlabel   swp lev 1+   forstmt
             ENDOF
        BREAK  OF   \  0 0 0   walk
                      0  definept
                       swp DUP IF  -}lab @  loop > THEN
                       IF swp -}lab @ 1+ branch
                       ELSE loop
                           IF   loop  2 + branch
                           ELSE TRUE ABORT" illegal break statement\n"
                           THEN
                       THEN
                       gettok TO t
                       [CHAR] ; 0 expect
                       TE"  A_AHEAD "

               ENDOF
        CONTINUE  OF ZZZ
(*
  walk(NULL, 0, 0);
                       definept(NULL);
                       if (loop)
                        branch(loop + 1);
                       else
                        error("illegal continue statement\n");
                       t = gettok(); expect(';');
                                              break;

*)          ENDOF
       xSWITCH  OF   loop 2 genlabel  lev  1+  swstmt

            ENDOF
       xCASE   OF
                        1 genlabel TO lab
                        swp 0= ABORT" illegal case label\n"
                        lab definelab
                        BEGIN  t xCASE =
                        WHILE
\                                static char stop[] = { IF, ID, 0 };

                                gettok TO t
                                QPRT 2+ TO QPRT
                                fistCS 0=
                                IF DOCR TE" C:[ "
                                THEN
                                0 constexpr  TO p
                                fistCS
                                IF    TE" = B_IF " 0 TO fistCS
                                ELSE  TE"  ] "
                                THEN   DOEVAL
                                QPRT 2- TO QPRT
                                 [CHAR] : stop1 test
                        REPEAT
                        loop swp lev statement
          ENDOF
        DEFAULT  OF
                        swp 0= ABORT" illegal default label\n"
                        swp -}deflab @ ABORT" extra default label\n"
                        swp -}lab @ findlabel swp -}deflab !
                        swp  -}deflab @ -}llabel @ definelab

                        gettok TO t
                        [CHAR] : 0 expect
                         fistCS
                         IF 0 TO fistCS  TE"  A_AHEAD "
                         THEN
                         DOCR TE"  DEFAULT: "  DOEVAL
                        loop swp lev statement
                 ENDOF
         RETURN  OF
                       cfunc -}stype @  freturn >R \ rty

                       QPRT 2+ TO QPRT
                       TE" RET "
                       gettok TO t
                       0 definept
                       t [CHAR] ; <>
                        IF       R@ voidtype =
                                IF  TRUE ABORT" extraneous return value"
\                                        expr(0);
\                                        retcode(NULL);
                                ELSE
                                      0 expr retcode
                                THEN
                        ELSE    R@ voidtype <>
                                IF  warning" missing return value" CR
                                THEN
                                0 retcode
                        THEN    RDROP
                        DOGS
                        IF  DOGS TEC TE" @ " 0 TO DOGS
                        THEN
                        TE" VJUMP $1 "
                        cfunc -}label @ branch
                        QPRT 2- TO QPRT
                        [CHAR] ; 0 expect
                 ENDOF
      [CHAR]  {  OF    loop swp lev  1+ compound
                 ENDOF
        [CHAR] ; OF  0 definept  gettok TO t
                 ENDOF
        GOTO OF
\                        0 0 0 walk
                        0 definept
                       gettok TO t
                       t ID =
                       IF
                          token stmtlabs @ lookup TO p
                        p 0=
                        IF
                               token stmtlabs 0 FUNC install TO p
                               LABELS p -}scope !
                               1 genlabel p -}llabel !
                        THEN
\                        (p, src); use

                        p -}llabel @  branch
                        TE" VJUMP $" p -}llabel @ TE. DOCR

                        gettok TO t
                        ELSE
\Z                           error("missing label in goto\n"); expect(';');
                        THEN

           ENDOF
      DUP  ID = IF  getchr  [CHAR] : =
                    IF
                        stmtlabel
                        loop swp lev statement
                        DROP A_AHEAD  \ goto m
                    THEN
                THEN    DEPTH TO Z_DEPTH
                        0 definept
                        kind[t]  ID <>
                        IF TRUE  ABORT" unrecognized statement"
                        ELSE
                          0 expr0 TO e
\                          e 0 0 listnodes DROP
\                          nodecount  0=  nodecount 200 > OR
\                          IF \ 0 0 0 walk
\                          ELSE glevel
\                             IF \  0 0 0 walk
\                             THEN
\                          THEN
\                         deallocate(STMT);
                          [CHAR] ; 0 expect
                        THEN

        ENDCASE
               A_THEN     \  m:
        kind[t] xIF <>
        kind[t]  ID <>  AND
        t [CHAR] }  <>  AND
        t  EOI      <>  AND
        IF
\                static char stop[] = { IF, ID, '}', 0 };
            TRUE ABORT" illegal statement termination"
\                skipto(0, stop);
        THEN
        FR> refinc F!
; TO statement

\EOF



#define den(i,j) ((j-buckets[i]+1.0)/(v[j]-v[buckets[i]]+1))

float density = 0.5;

static int foldcond(Tree e1, Tree e2);
static void caselabel(Swtch, long, int);
static void cmp(int, Symbol, long, int);
static Tree conditional(int);
static void dostmt(int, Swtch, int);
static int equal(Symbol, Symbol);
static void forstmt(int, Swtch, int);
static void ifstmt(int, int, Swtch, int);
static Symbol localaddr(Tree);
static void stmtlabel(void);
static void swstmt(int, int, int);
static void whilestmt(int, Swtch, int);

void addlocal(Symbol p) {
        if (!p->defined) {
                code(Local)->u.var = p;
                p->defined = 1;
                p->scope = level;
        }
}

static void caselabel(Swtch swp, long val, int lab) {
        int k;

        if (swp->ncases >= swp->size)
                {
                long   *vals = swp->values;
                Symbol *labs = swp->labels;
                swp->size *= 2;
                swp->values = newarray(swp->size, sizeof *swp->values, FUNC);
                swp->labels = newarray(swp->size, sizeof *swp->labels, FUNC);
                for (k = 0; k < swp->ncases; k++) {
                        swp->values[k] = vals[k];
                        swp->labels[k] = labs[k];
                }
                }
        k = swp->ncases;
        for ( ; k > 0 && swp->values[k-1] >= val; k--) {
                swp->values[k] = swp->values[k-1];
                swp->labels[k] = swp->labels[k-1];
        }
        if (k < swp->ncases && swp->values[k] == val)
                error("duplicate case label `%d'\n", val);
        swp->values[k] = val;
        swp->labels[k] = findlabel(lab);
        ++swp->ncases;
        if (Aflag >= 2 && swp->ncases == 258)
                warning("more than 257 cases in a switch\n");
}
void swgen(Swtch swp) {
        int *buckets, k, n;
        long *v = swp->values;

        buckets = newarray(swp->ncases + 1,
                sizeof *buckets, FUNC);
        for (n = k = 0; k < swp->ncases; k++, n++) {
                buckets[n] = k;
                while (n > 0 && den(n-1, k) >= density)
                        n--;
        }
        buckets[n] = swp->ncases;
        swcode(swp, buckets, 0, n - 1);
}
void swcode(Swtch swp, int b[], int lb, int ub) {
        int hilab, lolab, l, u, k = (lb + ub)/2;
        long *v = swp->values;

        if (k > lb && k < ub) {
                lolab = genlabel(1);
                hilab = genlabel(1);
        } else if (k > lb) {
                lolab = genlabel(1);
                hilab = swp->deflab->u.l.label;
        } else if (k < ub) {
                lolab = swp->deflab->u.l.label;
                hilab = genlabel(1);
        } else
                lolab = hilab = swp->deflab->u.l.label;
        l = b[k];
        u = b[k+1] - 1;
        if (u - l + 1 <= 3)
                {
                        int i;
                        for (i = l; i <= u; i++)
                                cmp(EQ, swp->sym, v[i], swp->labels[i]->u.l.label);
                        if (k > lb && k < ub)
                                cmp(GT, swp->sym, v[u], hilab);
                        else if (k > lb)
                                cmp(GT, swp->sym, v[u], hilab);
                        else if (k < ub)
                                cmp(LT, swp->sym, v[l], lolab);
                        else
                                assert(lolab == hilab),
                                branch(lolab);
                        walk(NULL, 0, 0);
                }
        else {
                Tree e;
                Type ty = signedint(swp->sym->type);
                Symbol table = genident(STATIC,
                        array(voidptype, u - l + 1, 0), GLOBAL);
                (*IR->defsymbol)(table);
                if (!isunsigned(swp->sym->type) || v[l] != 0)
                        cmp(LT, swp->sym, v[l], lolab);
                cmp(GT, swp->sym, v[u], hilab);
                e = (*optree['-'])(SUB, cast(idtree(swp->sym), ty), cnsttree(ty, v[l]));
                if (e->type->size < unsignedptr->size)
                        e = cast(e, unsignedlong);
                walk(tree(JUMP, voidtype,
                        rvalue((*optree['+'])(ADD, pointer(idtree(table)), e)), NULL),
                        0, 0);
                code(Switch);
                codelist->u.swtch.table = table;
                codelist->u.swtch.sym = swp->sym;
                codelist->u.swtch.deflab = swp->deflab;
                codelist->u.swtch.size = u - l + 1;
                codelist->u.swtch.values = &v[l];
                codelist->u.swtch.labels = &swp->labels[l];
                if (v[u] - v[l] + 1 >= 10000)
                        warning("switch generates a huge table\n");
        }
        if (k > lb) {
                assert(lolab != swp->deflab->u.l.label);
                definelab(lolab);
                swcode(swp, b, lb, k - 1);
        }
        if (k < ub) {
                assert(hilab != swp->deflab->u.l.label);
                definelab(hilab);
                swcode(swp, b, k + 1, ub);
        }
}
static void cmp(int op, Symbol p, long n, int lab) {
        Type ty = signedint(p->type);

        listnodes(eqtree(op,
                        cast(idtree(p), ty),
                        cnsttree(ty, n)),
                lab, 0);
}
void equatelab(Symbol old, Symbol new) {
        assert(old->u.l.equatedto == NULL);
        old->u.l.equatedto = new;
        new->ref++;
}

/* localaddr - returns q if p yields the address of local/parameter q; otherwise returns 0 */
static Symbol localaddr(Tree p) {
        if (p == NULL)
                return NULL;
        switch (generic(p->op)) {
*)          ENDOF
        INDIR OF ZZZ
(*
 INDIR: case CALL: case ARG:
                return NULL;
*)          ENDOF
        INDIR OF ZZZ
(*
 ADDRL: case ADDRF:
                return p->u.sym;
*)          ENDOF
        INDIR OF ZZZ
(*
 RIGHT: case ASGN:
                if (p->kids[1])
                        return localaddr(p->kids[1]);
                return localaddr(p->kids[0]);
        case COND: {
                Symbol q;
                assert(p->kids[1] && p->kids[1]->op == RIGHT);
                if ((q = localaddr(p->kids[1]->kids[0])) != NULL)
                        return q;
                return localaddr(p->kids[1]->kids[1]);
                }
        default: {
                Symbol q;
                if (p->kids[0] && (q = localaddr(p->kids[0])) != NULL)
                        return q;
                return localaddr(p->kids[1]);
                }
        }
}


