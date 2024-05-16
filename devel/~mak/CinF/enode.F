

: isvoidptr { ty -- n }
       ty isptr
       ty -}ttype @ unqual  voidtype = AND
;

: isnullptr { e \ ty -- n }
        e -}ttype @ unqual TO ty

        e -}op @  generic  CNST = DUP
        IF
(Z     DROP      e tree.v @ 0=

                ty -}op @      INT  =   AND
                ty -}op @ UNSIGNED  =  e tree.v @ 0=  AND  OR
                ty  isvoidptr          e tree.v @ 0=  AND  OR
*)
        THEN

;

: assign { xty e \ yty -- Type }
        e -}ttype @  unqual  TO yty
        xty unqual TO xty
        xty isenum
        IF   xty -}ttype @ TO xty
        THEN
        xty  -}size @ 0= yty -}size @ 0= OR
        IF   ." KK=" . . 3 ZZZ   0 EXIT
        THEN
        xty isarith  yty isarith AND
        xty isstruct xty   yty = AND OR
        IF  xty EXIT
        THEN

        xty isptr  e isnullptr AND
        IF    xty EXIT
        THEN
           xty isvoidptr  yty isptr AND
           xty isptr  yty isvoidptr AND   OR
           xty -}ttype @ isconst     yty -}ttype @ isconst    0= OR
           xty -}ttype @ isvolatile  yty -}ttype @ isvolatile 0= OR  AND AND
        IF    xty EXIT
        THEN
           xty isptr  yty isptr  AND
           xty -}ttype @ unqual  yty -}ttype @ unqual 1  eqtype AND
           xty -}ttype @ isconst     yty -}ttype @ isconst    0= OR
           xty -}ttype @ isvolatile  yty -}ttype @ isvolatile 0= OR  AND AND 
        IF    xty EXIT
        THEN

(Z        if (isptr(xty) && isptr(yty)
        && (  (isconst(xty->type)    || !isconst(yty->type))
           && (isvolatile(xty->type) || !isvolatile(yty->type)))) {
                Type lty = unqual(xty->type), rty = unqual(yty->type);
                if (isenum(lty) && rty == inttype
                ||  isenum(rty) && lty == inttype) {
                        if (Aflag >= 1)
                                warning("assignment between `%t' and `%t' is compiler-dependent\n",
                                        xty, yty);
                        return xty;
                }
        }
*)       0
;

\  bittree - construct tree for l [& | ^ %] r
: bittree {  op  l  r \ ty -- Tree }
        inttype TO ty

         l -}ttype @ isint
         r -}ttype @ isint OR
        IF    l -}ttype @ r -}ttype @ binary TO ty
              l ty  cast TO l
              r ty  cast TO r
        ELSE
\Z                op l r  typeerror
        THEN
        op ty l r simplify
;

\ multree - construct tree for l [* /] r
: multree {  op  l  r \ ty -- Tree }
           inttype TO ty
        l -}ttype @ isarith r -}ttype @ isarith  AND
        CASE
        IF    l -}ttype @ r -}ttype @  binary  TO  ty
              l ty  cast TO l
              r ty  cast TO r
        ELSE ZZZ
(*              typeerror(op, l, r);
*)
        ENDCASE
      op ty l r simplify
;

: addtree {  op  l  r \ ty n -- Tree }
         inttype TO ty
        l -}ttype @ isarith r -}ttype @ isarith  AND
        CASE
        IF
             l -}ttype @ r -}ttype @  binary  TO  ty
              l ty  cast TO l
              r ty  cast TO r
        ELSE

        l -}ttype @ isptr r -}ttype @ isint  AND
        IF   ADD r l RECURSE EXIT
        ELSE
            r -}ttype @  isptr
            l -}ttype @ isint  AND
            r -}ttype @ -}ttype @ isfunc 0= AND
           IF     r -}ttype @  unqual  TO  ty
                  ty -}ttype @  unqual  -}size @ TO n
                  n 0= ABORT" unknown size for type `%t'\n" \ , ty->type);
                  l  l -}ttype @  promote  cast  TO l
                  n  1  >
                  IF   MUL  n 0 signedptr cnsttree   l multree TO l
                  THEN
                  YYcheck  r -}op @  isaddrop 0= AND              \ omit
                  IF \Z     return nullcall(ty, YYcheck, r, l);   \ omit
                  THEN
                  ADD ty l r  simplify EXIT
        ELSE
\Z                typeerror(op, l, r);
        THEN
        ENDCASE
      op ty l r simplify

;


\ subtree - construct tree for l - r
: subtree {  op  l  r \ ty -- Tree }
           inttype TO ty
        l -}ttype @ isarith r -}ttype @ isarith  AND
        CASE
        IF
             l -}ttype @ r -}ttype @  binary  TO  ty
             l -}ttype @ ty <> ty -}size @ FCELL = AND
             IF \ print("3 ROLL ");
               l ty  cast TO l
               TE" FSWAP "
             ELSE
               l ty  cast TO l
             THEN
              r ty  cast TO r
        ELSE ZZZ

(*
         if (isptr(l->type) && !isfunc(l->type->type) && isint(r->type)) {
                ty = unqual(l->type);
                n = unqual(ty->type)->size;
                if (n == 0)
                        error("unknown size for type `%t'\n", ty->type);
                r = cast(r, promote(r->type));
                if (n > 1)
                        r = multree(MUL, cnsttree(signedptr, n), r);
                if (isunsigned(r->type))
                        r = cast(r, unsignedptr);
                else
                        r = cast(r, signedptr);
                return simplify(SUB+P, ty, l, r);
        } else if (compatible(l->type, r->type)) {
                ty = unqual(l->type);
                n = unqual(ty->type)->size;
                if (n == 0)
                        error("unknown size for type `%t'\n", ty->type);
                l = simplify(SUB+U, unsignedptr,
                        cast(l, unsignedptr), cast(r, unsignedptr));
                return simplify(DIV+I, longtype,
                        cast(l, longtype), cnsttree(longtype, n));
        } else
                typeerror(op, l, r);
*)
        ENDCASE
      op ty l r simplify
;

\ andtree - construct tree for l [&& ||] r
: andtree  {  op  l  r -- Tree }
         l -}ttype @ isscalar 0=
         r -}ttype @ isscalar 0= OR
        IF \Z           typeerror(op, l, r);
        THEN
        op inttype l  r simplify

;

: cmptree {  op  l  r \ ty -- Tree }
         l -}ttype @ isarith
         r -}ttype @ isarith AND
         IF     l -}ttype @  r -}ttype @ binary TO ty
                l ty cast TO l
                r ty cast TO r
         ELSE
(Z          if (compatible(l->type, r->type)) {
                ty = unsignedptr;
                l = cast(l, ty);
                r = cast(r, ty);
        } else {
                ty = unsignedtype;
                typeerror(op, l, r);
*)      THEN
         op ty  mkop inttype l r simplify
;

: eqtree {  op  l  r \ yty xty -- Tree }
          l -}ttype @ TO xty
          r -}ttype @ TO yty

        xty isptr
         DUP IF \Z DROP  r isnullptr
             THEN
        xty isptr  xty -}ttype @  isfunc 0= AND
         DUP IF \Z DROP  yty isnullptr
             THEN                OR
        xty isptr  yty isptr AND OR
        xty -}ttype @  DUP
        IF  unqual
            yty -}ttype @ DUP
            IF  unqual 1 eqtype
            THEN
        THEN     AND
        IF
(Z             {
                Type ty = unsignedptr;
                l = cast(l, ty);
                r = cast(r, ty);
                return simplify(mkop(op,ty), inttype, l, r);
*)      THEN
        yty isptr
         DUP IF \Z DROP  l isnullptr
             THEN
        yty isptr yty -}ttype @ isfunc 0= AND
         DUP IF \Z DROP xty isnullptr
             THEN   OR
        IF ZZZ op r l RECURSE EXIT
        THEN
        op l r cmptree
;

\ shtree - construct tree for l [>> <<] r
: shtree
(Z {  op  l  r -- Tree }
        Type ty = inttype;
        if (isint(l->type) && isint(r->type)) {
                ty = promote(l->type);
                l = cast(l, ty);
                r = cast(r, inttype);
        } else
                typeerror(op, l, r);
        return simplify(op, ty, l, r);
*)
;

:NONAME  {  op  l  r \  aty  ty  tr ii  pref  -- Tree }
        DOGS 
        IF  DOGS TEC TE" @ " 0 TO DOGS
        THEN
        r pointer TO r
        l -}ttype @ r  assign  TO ty
        ty
        IF    r ty cast TO  r
        ELSE
(*                typeerror(ASGN, l, r);
                if (r->type == voidtype)
                        r = retype(r, inttype);
                ty = r->type;
*)      THEN
         l -}op  @ $FIELD <>
        IF   l lvalue TO l
        THEN
        l -}ttype @  TO aty
        aty isptr
        IF   aty unqual -}ttype @ TO aty
        THEN
         aty isconst
         aty isstruct  aty unqual -}u.sym @ -}cfields @  AND  OR
        IF ZZZ
(*                if (isaddrop(l->op)
                && !l->u.sym->computed && !l->u.sym->generated)
                        error("assignment to const identifier `%s'\n",
                                l->u.sym->name);
                else
                        error("assignment to const location\n");
*)
        THEN
        l -}op @ $FIELD =
        IF  ZZZ
(*
                long n = 8*l->u.field->type->size - fieldsize(l->u.field);
                if (n > 0 && isunsigned(l->u.field->type))
                        r = bittree(BAND, r,
                                cnsttree(r->type, (unsigned long)fieldmask(l->u.field)));
                else if (n > 0) {
                        if (r->op == CNST+I) {
                                n = r->u.v.i;
                                if (n&(1<<(fieldsize(l->u.field)-1)))
                                        n |= ~0UL<<fieldsize(l->u.field);
                                r = cnsttree(r->type, n);
                        } else
                                r = shtree(RSH,
                                        shtree(LSH, r, cnsttree(inttype, n)),
                                        cnsttree(inttype, n));
                }
        }
*)      THEN

        ty isstruct  l  -}op @  isaddrop  AND
        IF ZZZ
(*          r iscallb
            IF
                return tree(RIGHT, ty,
                        tree(CALL+B, ty, r->kids[0]->kids[0], l),
                        idtree(l->u.sym));
            THEN
*)     THEN
          l TO tr 0 TO ii BL TO pref
          BEGIN tr -}op @ 0x47 = tr -}op @ 0x37 = OR
          WHILE ii 1+ TO ii tr t-kids[0] @ TO tr
          REPEAT
          tr  tree.sym @ DUP IF -}stype @ -}size @ FCELL = THEN
          IF QPRT 1 >
            IF  TE" FDUP F! "
            ELSE    TE" F!" DOCR
            THEN
          ELSE
            tr tree.sym @ DUP IF  -}stype @ -}size @ 1 = THEN
            IF [CHAR] C TO pref
            THEN
            tr tree.sym @ DUP IF -}stype @ -}size @ 2 = THEN
            IF [CHAR] W TO pref
            THEN
            QPRT  1 >
            IF   TE" OVER " pref TEC  TE" ! "  pref TEC  TE" @ "
            ELSE TE" SWAP " pref TEC  TE" ! "  DOCR
            THEN
          THEN
          op  ty mkop  ty l r tree

;  TO    asgntree

~mak\CinF\OPTREE.F

:NONAME  { ty \ p -- p }
        CNST ty mkop  ty 0 0  tree TO p
        DOGS
        IF  DOGS TEC TE" @ " 0 TO DOGS
        THEN
        ty -}op @
        CASE
         INT      OF 2DUP  TED. p tree.v 2! ENDOF
         UNSIGNED OF 2DUP  TED. p tree.v 2! ENDOF
         FLOT     OF FDUP FTED. p tree.v F! ENDOF
         POINTER  OF 2DUP  TED. p tree.v  ! ENDOF
         0 assert
        ENDCASE
         p
; TO cnsttree

:NONAME  { n  ty -- tree }
        ty  isarray
        IF \Z ty atop TO ty
        ELSE ty isint assert
        THEN
        n 0 ty cnsttree
; TO consttree

\  addrof - address of p
:NONAME { p \ q -- Tree }
        p TO q

\        for (;;)
      A_BEGIN
        q -}op @ generic

                CASE
                 RIGHT OF
(Z                        assert(q->kids[0] || q->kids[1]);
                        q = q->kids[1] ? q->kids[1] : q->kids[0];
                        A_AGAIN_C
*)                       ENDOF
                 ASGN  OF
(Z
                        q = q->kids[1];
                        A_AGAIN_C
*)                       ENDOF
                 COND  OF
(Z                        Symbol t1 = q->u.sym;
                        q->u.sym = 0;
                        q = idtree(t1);
                        /* fall thru */
*)                          A_AHEAD
                         ENDOF
                 INDIR OF   A_THEN
                        p  q =
                        IF  q t-kids[0] @  EXIT
                        THEN
(Z                        q = q->kids[0];
                        return tree(RIGHT, q->type, root(p), q);
*)                     ENDOF
(Z       default:
                        error("addressable object required\n");
                        return value(p);
*)                ENDCASE
      A_AGAIN
; TO addrof

:  calltree { f  ty  args  t3 \ rty p -- p }
        DOGS
        IF  DOGS TEC TE" @ " 0 TO DOGS
        THEN
        f tree.sym @ -}name @ COUNT TOEVAL
         DOCR
        args
        IF   RIGHT f -}ttype @ args f  tree TO f
        THEN
        ty isstruct
        IF
(Z                assert(t3),
                p = tree(RIGHT, ty,
                        tree(CALL+B, ty, f, addrof(idtree(t3))),
                        idtree(t3));
*)
        ELSE
                ty TO rty
                ty isenum
                IF \Z      rty = unqual(ty)->type;
                THEN
                rty isfloat 0=
                IF   rty promote TO rty
                THEN
                CALL rty mkop rty f 0 tree TO p
                ty isptr  p -}ttype @ -}size @  ty -}size @ > OR
                IF p ty  cast TO p
                THEN
        THEN
         p
;

\ asgn - generate tree for assignment of expr e to symbol p sans qualifiers
: asgn { p  e \ ty -- e }
         p -}stype @ isarray
         IF
(Z                e = tree(ASGN+B, p->type, idtree(p),
                        tree(INDIR+B, e->type, e, NULL));
*)       ELSE
                p -}stype @ TO ty
                p -}stype @ unqual p -}stype !
                p -}stype @ isstruct
                p -}stype @  -}u.sym @ -}cfields @ AND
                IF
(Z                        p->type->u.sym->u.s.cfields = 0;
                        e = asgntree(ASGN, idtree(p), e);
                        p->type->u.sym->u.s.cfields = 1;
*)              ELSE          p idtree >R
                        e -}ttype @ -}size @ 12 <>
                        IF TE" SWAP "
                        THEN 0 TO DOGS
                        ASGN  R> e asgntree TO e
                THEN
                ty  p -}stype !
          THEN
         e
;



:NONAME  { f  fty \ n  args  r  e proto rty t3 q aty -- e }
   DUP >R
   0 TO n 0 TO args  0 TO r
   fty freturn unqual TO rty
   0 TO t3

        fty -}f.oldstyle @

        IF  0 TO proto
        ELSE   fty -}f.proto @  TO proto
        THEN
        f hascall
        IF  f TO r
        THEN
        rty isstruct
        IF
(Z                        t3 = temporary(AUTO, unqual(rty));
                        if (rty->size == 0)
                                error("illegal use of incomplete type `%t'\n", rty);
*)      THEN
        t  [CHAR] ) <>
        IF
                BEGIN
                         0  expr1 pointer TO q
                         proto DUP
                        IF   @ DUP
                        IF  voidtype  <>
                        THEN THEN
                        IF

                                 q  value TO q
                                 proto @ q  assign TO aty
                                 aty
                                 IF    q aty cast TO q
                                 ELSE  TRUE
     ABORT" type error in argument %d to %s; found `%t' expected `%t'\n" \ , n + 1, funcname(f), q->type, *proto);
                                 THEN
                                 q -}ttype @ isptr   q -}ttype @ isenum OR
                                 q -}ttype @ -}size @ inttype -}size @ <> AND
                                 IF   \Z     q = cast(q, promote(q->type));
                                 THEN
                                        proto CELL+ TO proto

                        ELSE
                                        fty -}f.oldstyle @ 0= proto DUP IF @ THEN 0= AND
                                        IF
\Z                                                error("too many arguments to %s\n", funcname(f));
                                        THEN
                                        q value TO q
                                        q -}ttype @ isarray  q -}ttype @ -}size @ 0= AND
                                        IF
\Z                                                error("type error in argument %d to %s; `%t' is illegal\n", n + 1, funcname(f), q->type);

                                        ELSE   q  q -}ttype @  promote cast TO q
                                        THEN
                                        proto DUP IF @  voidtype = THEN
                                        IF     q -}ttype @ -}size @ 12 =
                                               IF    TE" .f. "
                                               ELSE  TE" .i. "
                                               THEN
                                        THEN
                        THEN
                        q -}ttype @ isstruct
                        IF
(Z                                if (iscallb(q))
                                        q = addrof(q);
                                else {
                                        Symbol t1 = temporary(AUTO, unqual(q->type));
                                        q = asgn(t1, q);
                                        q = tree(RIGHT, ptr(t1->type),
                                                root(q), lvalue(idtree(t1)));
                                }
*)                      THEN
                        q -}ttype @ -}size @ 0=
                        IF
\Z                                q->type = inttype;
                        THEN
                        q hascall
                        IF
                                 r
                                 IF  RIGHT voidtype r q tree
                                 ELSE  q
                                 THEN TO r
                        THEN
                         ARG q -}ttype @  mkop q -}ttype @ q args  tree TO args
                        n 1+ TO n
                        Aflag  2 >=  n  32 = AND
                        IF
\Z                                warning("more than 31 arguments in a call to %s\n",  funcname(f));
                        THEN
                        t [CHAR] , =
                WHILE   gettok TO t
                REPEAT
        THEN
        [CHAR] ) 0  expect
        proto DUP IF @ THEN DUP
        IF  voidtype =
            IF TE" TO... "
            ELSE
\Z       error("insufficient number of arguments to %s\n",  funcname(f));
            THEN
        ELSE DROP
        THEN
        r
        IF RIGHT voidtype r args tree TO args
        THEN
        f rty args t3  calltree TO e
\        if (events.calls)
\                apply(events.calls, &src, &e);
          DUP R> <> IF ZZZ THEN
         e  
; TO call

\EOF
static Tree addtree(int, Tree, Tree);
static Tree andtree(int, Tree, Tree);
static Tree cmptree(int, Tree, Tree);
static int compatible(Type, Type);
static int isnullptr(Tree e);
static Tree multree(int, Tree, Tree);
static Tree subtree(int, Tree, Tree);
#define isvoidptr(ty) \
        (isptr(ty) && unqual(ty->type) == voidtype)

Tree vcall(Symbol func, Type ty, ...) {
        va_list ap;
        Tree args = NULL, e, f = pointer(idtree(func)), r = NULL;

        assert(isfunc(func->type));
        if (ty == NULL)
                ty = freturn(func->type);
        va_start(ap, ty);
        while ((e = va_arg(ap, Tree)) != NULL) {
                if (hascall(e))
                        r = r == NULL ? e : tree(RIGHT, voidtype, r, e);
                args = tree(mkop(ARG, e->type), e->type, e, args);
        }
        va_end(ap);
        if (r != NULL)
                args = tree(RIGHT, voidtype, r, args);
        return calltree(f, ty, args, NULL);
}
int iscallb(Tree e) {
        return e->op == RIGHT && e->kids[0] && e->kids[1]
                && e->kids[0]->op == CALL+B
                && e->kids[1]->op == INDIR+B
                && isaddrop(e->kids[1]->kids[0]->op)
                && e->kids[1]->kids[0]->u.sym->temporary;
}


static int compatible(Type ty1, Type ty2) {
        return isptr(ty1) && !isfunc(ty1->type)
            && isptr(ty2) && !isfunc(ty2->type)
            && eqtype(unqual(ty1->type), unqual(ty2->type), 0);
}

Tree condtree(Tree e, Tree l, Tree r) {
        Symbol t1;
        Type ty, xty = l->type, yty = r->type;
        Tree p;

        if (isarith(xty) && isarith(yty))
                ty = binary(xty, yty);
        else if (eqtype(xty, yty, 1))
                ty = unqual(xty);
        else if (isptr(xty)   && isnullptr(r))
                ty = xty;
        else if (isnullptr(l) && isptr(yty))
                ty = yty;
        else if (isptr(xty) && !isfunc(xty->type) && isvoidptr(yty)
        ||       isptr(yty) && !isfunc(yty->type) && isvoidptr(xty))
                ty = voidptype;
        else if ((isptr(xty) && isptr(yty)
                 && eqtype(unqual(xty->type), unqual(yty->type), 1)))
                ty = xty;
        else {
                typeerror(COND, l, r);
                return consttree(0, inttype);
        }
        if (isptr(ty)) {
                ty = unqual(unqual(ty)->type);
                if (isptr(xty) && isconst(unqual(xty)->type)
                ||  isptr(yty) && isconst(unqual(yty)->type))
                        ty = qual(CONST, ty);
                if (isptr(xty) && isvolatile(unqual(xty)->type)
                ||  isptr(yty) && isvolatile(unqual(yty)->type))
                        ty = qual(VOLATILE, ty);
                ty = ptr(ty);
        }
        switch (e->op) {
        case CNST+I: return cast(e->u.v.i != 0   ? l : r, ty);
        case CNST+U: return cast(e->u.v.u != 0   ? l : r, ty);
        case CNST+P: return cast(e->u.v.p != 0   ? l : r, ty);
        case CNST+F: return cast(e->u.v.d != 0.0 ? l : r, ty);
        }
        if (ty != voidtype && ty->size > 0) {
                t1 = genident(REGISTER, unqual(ty), level);
        /*      t1 = temporary(REGISTER, unqual(ty)); */
                l = asgn(t1, l);
                r = asgn(t1, r);
        } else
                t1 = NULL;
        p = tree(COND, ty, cond(e),
                tree(RIGHT, ty, root(l), root(r)));
        p->u.sym = t1;
        return p;
}



/* typeerror - issue "operands of op have illegal types `l' and `r'" */
void typeerror(int op, Tree l, Tree r) {
        int i;
        static struct { int op; char *name; } ops[] = {
                ASGN, "=",      INDIR, "*",     NEG,  "-",
                ADD,  "+",      SUB,   "-",     LSH,  "<<",
                MOD,  "%",      RSH,   ">>",    BAND, "&",
                BCOM, "~",      BOR,   "|",     BXOR, "^",
                DIV,  "/",      MUL,   "*",     EQ,   "==",
                GE,   ">=",     GT,    ">",     LE,   "<=",
                LT,   "<",      NE,    "!=",    AND,  "&&",
                NOT,  "!",      OR,    "||",    COND, "?:",
                0, 0
        };

        op = generic(op);
        for (i = 0; ops[i].op; i++)
                if (op == ops[i].op)
                        break;
        assert(ops[i].name);
        if (r)
                error("operands of %s have illegal types `%t' and `%t'\n",
                        ops[i].name, l->type, r->type);
        else
                error("operand of unary %s has illegal type `%t'\n", ops[i].name,
                        l->type);
}
