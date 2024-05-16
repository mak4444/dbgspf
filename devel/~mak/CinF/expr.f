
: prec[t] prec t + C@ ;

: rvalue { p \ ty -- Tree }
        p -}ttype @  deref TO ty
        ty unqual  TO ty
        DOGS
        IF  DOGS TEC TE" @ "
        THEN
          BL
          ty -}size @  1 = IF DROP [CHAR] C THEN
          ty -}size @  2 = IF DROP [CHAR] W THEN
          ty -}size @ 12 = IF DROP [CHAR] F THEN
          TO DOGS
        INDIR ty mkop  ty p  0  tree
;


: retype {  p  ty \ q -- Tree }
        p -}ttype @ ty =
        IF   p EXIT
        THEN
        p -}op @ ty p t-kids[0] @ p t-kids[1] @  tree TO q
\        p tree.node @ q tree.node !
        p tree.v @ q tree.v !
        q

;

: rightkid {  p -- Tree }
        BEGIN  p -}op @  RIGHT =
        WHILE   p t-kids[1] @ IF p t-kids[1] @ TO p ELSE
                p t-kids[0] @ IF p t-kids[0] @ TO p ELSE  0   assert
                THEN THEN
        REPEAT
        p assert
        p
;


: pointer {  p -- p }
        p -}ttype @  isarray
        IF       p  p -}ttype @ atop retype TO p
        ELSE p -}ttype @ isfunc
             IF  p  p -}ttype @  ptr retype TO p
             THEN
        THEN
         p
;

: oper[ CELLS oper + @ ;

: incr { op  v  e -- Tree }
        ASGN v
        op oper[  v e  op  *optree[ asgntree
;

: field { p name \  q  ty1 ty --  p }
        p -}ttype @ TO  ty

        ty isptr
        IF  ty deref TO ty
        THEN
        ty TO ty1
        ty  unqual TO ty
        name ty fieldref TO q
        q
        IF     q f.type @ isarray
               IF
(Z                        ty = q->type->type;
                        if (isconst(ty1) && !isconst(ty))
                                ty = qual(CONST, ty);
                        if (isvolatile(ty1) && !isvolatile(ty))
                                ty = qual(VOLATILE, ty);
                        ty = array(ty, q->type->size/ty->size, q->type->align);
*)              ELSE
                        q f.type @ TO ty
                        ty1 isconst ty isconst 0= AND
                        IF   \Z     ty = qual(CONST, ty);
                        THEN
                        ty1 isvolatile ty isvolatile 0= AND
                        IF \Z      ty = qual(VOLATILE, ty);
                        THEN
                        ty ptr TO ty
                THEN
                YYcheck p -}op @ isaddrop AND
                q f.offset @ 0> AND
                IF  \Z      p = nullcall(ty, YYcheck, p, consttree(q->offset, inttype));    \ omit
                ELSE   ADD +P: ty p  q f.offset @ inttype consttree  simplify TO p
                THEN
                q f.lsb @
                IF
(Z                        p = tree(FIELD, ty->type, rvalue(p), NULL);
                        p->u.field = q;
*)              ELSE  q f.type @ isarray 0=
                      IF   p  rvalue TO p
                      THEN
                THEN

        ELSE
\Z                error("unknown field `%s' of `%t'\n", name, ty);
\                p = rvalue(retype(p, ptr(inttype)));
        THEN
         p
;


: postfix  {  p \ ty sQPRT q -- p }
        BEGIN
          DUP >R
          t
          CASE
          CASE INCR  OF\ DECR OF;
                QPRT TO sQPRT
                       TE" DUP "
                       QPRT
                       IF  DOGS TEC TE" @ TUCK "
                          0 TO DOGS 0 TO QPRT
                       THEN
(*                       RIGHT
                             p -}ttype @
                                RIGHT p -}ttype @
                                        p
*) YYY                      t p 1 inttype  consttree incr DROP
YYY \                                           tree
\                                p       tree  TO p
                             sQPRT TO QPRT
                            gettok TO t RDROP DUP >R
                 ENDOF
    [CHAR] [ OF    
                   gettok TO t
                   DOGS
                   IF  DOGS TEC TE" @ " 0 TO DOGS
                   THEN
                   QPRT 2+ TO QPRT
                   [CHAR] ]  expr TO q
                   YYnull
                   IF
(Z                         if (isptr(p->type))
                                 p = nullcheck(p);
                           else if (isptr(q->type))
                                 q = nullcheck(q);
*)                THEN
                  ADD p pointer q pointer [CHAR] + *optree[ TO p
                  p -}ttype @ isptr  p -}ttype @ -}ttype @  isarray AND
                  IF   \Z     p = retype(p, p->type->type);
                  ELSE      p  rvalue TO p
                  THEN
                   QPRT 2- TO QPRT
              ENDOF
         [CHAR] ( OF
                                p pointer TO p
                                p -}ttype @ isptr
                                p -}ttype @ -}ttype @ isfunc AND
                                IF  p -}ttype @ -}ttype @ TO ty
                                ELSE
(Z                                        error("found `%t' expected a function\n", p->type);
                                        ty = func(voidtype, NULL, 1);
                                        p = retype(p, ptr(ty));
*)                              THEN
                                gettok  TO t
                                QPRT 2+ TO QPRT
                                p ty call TO p
                                QPRT 2- TO QPRT
                    ENDOF
         [CHAR] . OF      0 TO DOGS
                          gettok TO t
                            t  ID =
                            IF  p -}ttype @ isstruct
                                IF  p  addrof TO q
                                    q token field TO p
                                    q rightkid TO q
                                    q -}op @ isaddrop
                                    q tree.sym @ DUP IF -}temporary @ THEN AND
                                    IF  \Z      p = tree(RIGHT, p->type, p, NULL);
                                    THEN
                                ELSE
\Z       error("left operand of . has incompatible type `%t'\n",    p->type);
                                THEN
                                gettok TO t
                            ELSE
\Z                                error("field name expected\n"); break;
                            THEN
                  ENDOF
             DEREF OF
                          gettok TO t
                            p pointer TO p
                            t  ID =
                            IF     p -}ttype @ isptr
                                   p -}ttype @ -}ttype @ isstruct AND
                                   IF   YYnull
                                        IF
\Z                                                p = nullcheck(p);
                                        THEN
                                        p  token field TO p
                                   ELSE
\Z                                        error("left operand of -> has incompatible type `%t'\n", p->type);
                                    THEN
                                    gettok TO t
                             ELSE
\Z                                error("field name expected\n"); break;
                             THEN
                    ENDOF
                DROP  DUP R> <> IF ZZZ THEN
                  p EXIT
                ENDCASE
                     DUP R> <> IF ZZZ THEN  [UAD+]
             AGAIN
;


: idtree {  p \  op  e ty -- e }
        DUP >R
        p -}stype @
        IF p -}stype @ unqual
        ELSE voidptype
        THEN TO ty
        DOGS
        IF  DOGS TEC TE" @ " 0 TO DOGS
        THEN
        p -}scope  @ GLOBAL =
        p -}sclass @ STATIC = OR
        IF p -}sclass @ STATIC =
           IF cfunc
             IF
                p -}scope @  GLOBAL =
               IF   TE" [ $$L, " p -}sx x.name @ COUNT TOEVALBL TE" , ] "
               ELSE TE" $LIT "   p -}sx x.name @ COUNT TOEVALBL
               THEN
             THEN
           ELSE
                ty isfunc 0=
                IF   p -}sx x.name @ COUNT TOEVALBL
                THEN
           THEN
          ADDRG TO op
        ELSE    p -}scope @ PARAM =
           IF
                ADDRF TO op
                p x.offset @ TE. TE" &F "
(*
                if (isstruct(p->type) && !IR->wants_argb)
                        {
                                e = tree(mkop(op,voidptype), ptr(ptr(p->type)), NULL, NULL);
                                e->u.sym = p;
                                return rvalue(rvalue(e));
                        }
*)        ELSE
            p -}sclass @  EXTERN =
            IF  p -}alias @ assert
                p -}alias @ TO p
                ADDRG TO op
            ELSE  ADDRL TO op
                p -}ref F@ F0=
                IF  p Ilocal
                THEN
                p x.offset @ TE. TE" &L "
            THEN
          THEN
        THEN
        refinc F@  p -}ref F+!
        ty isarray
        IF        op voidptype mkop   p -}stype @     0 0  tree
        ELSE ty isfunc
             IF   op funcptype mkop   p -}stype @     0 0  tree
            ELSE op voidptype mkop   p -}stype @ ptr 0 0  tree
             THEN
        THEN  TO e
        p  e tree.sym !
        e  -}ttype @  isptr
        IF   e  rvalue  TO e
        THEN
        DUP  R>  <> IF ZZZ THEN
        e  
;

:  primary { \ p sp q -- p }
        DUP >R
        t [CHAR] ( <> assert
        t
        CASE
          CASE ICON OF\ FCON OF;
             CNST tsym -}stype @  mkop  tsym -}stype @ 0 0 tree TO p
                    tsym -}v F@ p tree.v F!
                    DOGS
                    IF  DOGS TEC TE" @ " 0 TO DOGS
                    THEN
\                    cfunc
\                    IF
                      p -}ttype @ -}size @ 12 =
                      IF    p tree.v F@ FTED.
                      ELSE  p tree.v 2@  TED.
                      THEN
\                    THEN
          ENDOF
         SCON OF tsym -}stype @ -}ttype @ ischar
               IF
><CDP                 tsym -}v @ COUNT
                      HERE tsym -}v !
                      S", 0 C,
><CDP
               ELSE
\Z                        tsym->u.c.v.p = memcpy(allocate(tsym->type->size, PERM), tsym->u.c.v.p, tsym->type->size);
               THEN tsym -}stype @  tsym -}v  constant TO tsym
                    tsym -}loc @ 0=
                   IF STATIC tsym -}stype @ GLOBAL genident tsym -}loc !
                   THEN
                   tsym -}loc @ idtree TO p
                   cfunc 0=
                   IF p tree.sym @ -}sx x.name @ COUNT  TOEVAL 
                   THEN

          ENDOF
         ID  OF  tsym  0=
                 IF
                                 token identifiers level FUNC install TO sp
                                 getchr [CHAR] ( =
                                 IF
                                        token externals @ lookup TO q
                                        inttype 0 1  func sp -}stype !
                                        EXTERN sp -}sclass !
                                        Aflag  1 >=
                                        IF    warning" missing prototype\n"
                                        THEN
                                        q DUP IF -}stype @ sp -}stype @ 1 eqtype  0= THEN
                                        IF warning" implicit declaration of `%s' does not match previous declaration at %w\n"
                                        THEN
                                        q 0=
                                        IF     sp -}name @ externals GLOBAL PERM install TO q
                                               sp -}stype @    q -}stype !
                                                EXTERN q -}sclass !
                                                q defsymbol
                                        THEN
                                        q sp -}alias !
                                 ELSE
(Z                                        error("undeclared identifier `%s'\n", p->name);
                                        p->sclass = AUTO;
                                        p->type = inttype;
                                        if (p->scope == GLOBAL)
                                                (*IR->defsymbol)(p);
                                        else
                                                addlocal(p);
*)
                                 THEN
                                gettok TO t
                                xref
                                IF \Z (p, src); use
                                THEN
                                   DUP  R>  <> IF ZZZ THEN
                                   [  1 CELLS  uAddDepth +!  ]
                                sp idtree EXIT
                        THEN
                   xref
                   IF ." WW=" . . 37 ZZZ
\                        use(tsym, src);
                   THEN
                   tsym -}sclass @  ENUM =
                   IF    ." WW=" . . 41 ZZZ
\                        p = consttree(tsym->u.value, inttype);
                   ELSE tsym -}sclass @  TYPEDEF =
                        IF ." illegal use of type name" tsym -}name COUNT TYPE
                            ABORT
                        THEN
                         tsym idtree TO p
                   THEN
             ENDOF
         FIRSTARG OF ZZZ
(*
                if (level > PARAM && cfunc && cfunc->u.f.callee[0])
                        p = idtree(cfunc->u.f.callee[0]);
                else {
                        error("illegal use of `%k'\n", FIRSTARG);
                        p = cnsttree(inttype, 0L);
                }
                break;
*)           ENDOF
t \Z               TRUE ABORT" illegal expression"
\                        p = cnsttree(inttype, 0L);
      ENDCASE
        gettok TO t
        DUP  R>  <> IF ZZZ THEN
         p
;

: lvalue {  p -- Tree }
        p -}op @ generic INDIR <> ABORT" lvalue required"
        p -}ttype @  unqual voidtype  =
        IF  p -}ttype @ H. warning" used as an lvalue"
        THEN
        p t-kids[0]  @
;

:NONAME  { \ p ty ty1 tt pty -- p }
        t
        CASE \  [ .( XX=) LAST-NON . KEY DROP ]
         [CHAR] * OF
             gettok  TO t   RECURSE TO p p pointer  TO p
                           p -}ttype @ isptr
                           p -}ttype @ -}ttype @ isfunc
                           p -}ttype @ -}ttype @ isarray OR AND
                           IF
\Z                            p = retype(p, p->type->type);
                           ELSE    YYnull
                                   IF \Z  p = nullcheck(p);
                                   THEN
                                   p  rvalue TO p
                           THEN
                ENDOF
         [CHAR] & OF
             gettok  TO t   RECURSE  TO p  0 TO DOGS
                        p -}ttype @ isfunc
                        p -}ttype @ isarray OR
                        IF
\Z                            p = retype(p, ptr(p->type));
                        ELSE  p lvalue  TO p
                        THEN
                        p -}op @ isaddrop
                        p  tree.sym @  -}sclass @ REGISTER =  AND
                        IF  \Z   error("invalid operand of unary &; `%s' is declared register\n", p->u.sym->name);
                        ELSE  p -}op @ isaddrop
                              IF  1 p tree.sym @ -}addressed  !
                              THEN
                        THEN
                  ENDOF
         [CHAR] + OF  gettok TO t   RECURSE TO p  p pointer TO p
                     p -}ttype @ isarith
                     IF    p p -}ttype @ promote cast TO p
                     ELSE \Z  typeerror(ADD, p, NULL);  break;
                     THEN
                  ENDOF
         [CHAR] -  OF
             gettok  TO t   RECURSE TO p p pointer  TO p
                    p -}ttype @ isarith
                    IF
                         p -}ttype @ promote TO ty
                         p ty cast TO p
                         ty isunsigned
                         IF
(Z                           warning("unsigned operand of unary -\n");
                             p = simplify(ADD, ty, simplify(BCOM, ty, p, NULL), cnsttree(ty, 1UL));
*)
                         ELSE NEG ty p 0 simplify TO p
                         THEN
                    ELSE  \Z typeerror(SUB, p, NULL); break;
                    THEN
                  ENDOF
         [CHAR] ~  OF 0 @ ." LL=" . . 62 ZZZ
(*        case '~':    t = gettok(); p = unary(); p = pointer(p);
                                                  if (isint(p->type)) {
                                                        Type ty = promote(p->type);
                                                        p = simplify(BCOM, ty, cast(p, ty), NULL);
                                                  } else
                                                        typeerror(BCOM, p, NULL);  break;
*)                ENDOF
         [CHAR] !  OF ." LL=" . . 70 ZZZ
(*        case '!':    t = gettok(); p = unary(); p = pointer(p);
                                                  if (isscalar(p->type))
                                                        p = simplify(NOT, inttype, cond(p), NULL);
                                                  else
                                                        typeerror(NOT, p, NULL); break;
*)                ENDOF
          CASE INCR OF\ DECR OF;
                      t TO tt  gettok TO t   RECURSE TO p
                      TE" DUP "
                      tt  p pointer  1 inttype consttree incr TO p
                  ENDOF
                    CASE
           TYPECODE OF\ SIZEOF OF;   t >R \ op
                                      0 TO p
                                      gettok TO t
                                      t  [CHAR] ( =
                                      IF
                                        gettok TO t
                                        t tsym istypename
                                        IF
                                                typename TO ty
                                                [CHAR] )  0 expect
                                        ELSE
\Z                                                p = postfix(expr(')'));
\Z                                                ty = p->type;
                                        THEN
                                      ELSE
\Z                                        p = unary();
\Z                                        ty = p->type;
                                      THEN
                                      ty assert
                                      R> ( op ) TYPECODE =
                                      IF
\Z                                        p = cnsttree(inttype, (long)ty->op);
                                      ELSE
                                        ty isfunc ty -}size @ 0= OR
                                        IF
\Z                                                error("invalid type argument `%t' to `sizeof'\n", ty);
                                        ELSE   p DUP IF  rightkid -}op @  $FIELD =  THEN
                                              IF
\Z                                                error("`sizeof' applied to a bit field\n");
                                              THEN
                                         ty -}size @ 0  unsignedlong cnsttree  TO  p
                                        THEN
                                      THEN
                 ENDOF
         [CHAR] ( OF  gettok   TO t
                 t tsym istypename
                IF
                        typename TO ty1
                       [CHAR] )  0 expect
                        ty1 unqual TO ty
                        ty isenum
                        IF
(Z                                Type ty2 = ty->type;
                                if (isconst(ty1))
                                        ty2 = qual(CONST, ty2);
                                if (isvolatile(ty1))
                                        ty2 = qual(VOLATILE, ty2);
                                ty1 = ty2;
                                ty = ty->type;
*)                        THEN
                        unary pointer TO p
                        p -}ttype @ TO pty
                        pty isenum
                        IF
\Z                                pty = pty->type;
                        THEN
                        pty isarith  ty  isarith  AND
                        pty isptr    ty  isptr      AND OR
                        IF
\                                explicitCast 1+ TO explicitCast
                                p ty cast TO p
\                                explicitCast 1- TO explicitCast

                        ELSE
(Z                         if (isptr(pty) && isint(ty)
                        ||       isint(pty) && isptr(ty)) {
                                if (Aflag >= 1 && ty->size < pty->size)
                                        warning("conversion from `%t' to `%t' is compiler dependent\n", p->type, ty);

                                p = cast(p, ty);
                        } else if (ty != voidtype) {
                                error("cast from `%t' to `%t' is illegal\n",
                                        p->type, ty1);
                                ty1 = inttype;
*)                      THEN
                        p -}op @ generic  INDIR  =  ty -}size @ 0= OR
                        IF
\Z                                p = tree(RIGHT, ty1, NULL, p);
                        ELSE
                                p ty1 retype TO p
                        THEN
                ELSE
                       [CHAR] ) expr postfix  TO  p
                THEN
                ENDOF  primary  postfix TO p
             ENDCASE
         p
; TO unary

\ HERE TO oper

: expr3 { k \  k1 p r op -- p }
         unary TO p
        prec[t] TO k1
        BEGIN k1  k  >=
        WHILE
                BEGIN  prec[t]  k1 =
                       PeekChar [CHAR]  = <> AND
                WHILE  t TO op
                        gettok  TO t
                        p pointer  TO p
                        op  ANDAND = op OROR = OR
                        IF    k1   RECURSE pointer TO r
(*                                if (events.points)
                                        apply(events.points, &pt, &r);
*)                      ELSE
                                 k1 1+  RECURSE pointer  TO r
                        THEN
                         op  oper[   p r   op  *optree[ TO p

                REPEAT
                k1 1- TO k1
         REPEAT
         p
;


:  expr2 { \ p -- p }
        4 expr3  TO p
        t [CHAR] ? =
        IF      ." DD=" . . 24 ZZZ
(*
                Tree l, r;
                Coordinate pts[2];
                if (Aflag > 1 && isfunc(p->type))
                        warning("%s used in a conditional expression\n",
                                funcname(p));
                p = pointer(p);
                t = gettok();
                pts[0] = src;
                l = pointer(expr(':'));
                pts[1] = src;
                r = pointer(expr2());
                if (events.points)
                        {
                                apply(events.points, &pts[0], &l);
                                apply(events.points, &pts[1], &r);
                        }
                p = condtree(p, l, r);
*)      THEN
        p
;

: value {  p \ op -- Tree }
        p  rightkid -}op @  generic TO op
        p -}ttype @ voidtype <>
        op $AND =
        op $OR = OR   op $NOT = OR   op EQ = OR   op NE = OR
        op LE = OR    op   LT = OR   op GE = OR   op GT = OR  AND
        IF   ." DD=" . . 33 ZZZ
\                p = condtree(p, consttree(1, inttype),
\                        consttree(0, inttype));
        THEN
        p
;


CREATE stop1 xIF C, ID C, 0 C,

: expr1 { tok \ p op -- p }
        expr2  TO p
        t [CHAR] = =
        prec[t]  6 >=  prec[t]  8 <= AND OR
        prec[t] 11 >=  prec[t] 13 <= AND OR
        IF      t  TO op
                gettok TO t
                QPRT 1+ TO QPRT
                op oper[ ASGN =
                IF  0 TO DOGS
                      ASGN p  0 RECURSE value  asgntree  TO p
                ELSE [CHAR] =  0 expect
                     TE" DUP "
                      op p 0 RECURSE  incr TO p
                THEN
                QPRT 1- TO QPRT
      THEN
        tok
        IF   tok stop1 test
        THEN
        p
;

CREATE stop0  xIF C, ID C,  CHAR } C, 0 C,

:NONAME  { tok \ p -- p }

        0 expr1  TO p
        BEGIN t [CHAR] , =
        WHILE         ." RR=" . . 5 ZZZ
(*
                Tree q;
                t = gettok();
                q = pointer(expr1(0));
                p = tree(RIGHT, q->type, root(value(p)), q);
*)      REPEAT
        tok
        IF   tok stop0 test
        THEN
        p
;      TO  expr


: super { ty -- Type }
        ty -}op @
             CASE
           INT  OF
                  ty -}size @       inttype -}size @  <
                  IF  inttype EXIT
                  THEN
                ENDOF
          UNSIGNED  OF
                  ty -}size @  unsignedtype -}size @  <
                  IF  unsignedtype EXIT
                  THEN
                ENDOF
          POINTER  OF   unsignedptr EXIT
                 ENDOF
             ENDCASE
           ty EXIT
;

:NONAME { p  type \ src dst -- Tree }
        p value TO p
        p -}ttype @ type =
        IF  p EXIT
        THEN
        type unqual TO dst
        p -}ttype @ unqual TO src
        src -}op @  dst -}op @ <> src -}size @  dst -}size @ <> OR
        IF
              src -}op @
             CASE
           INT  OF
                       src -}size @   inttype -}size @  <
                       IF CVI inttype p 0  simplify  TO p
                       THEN
                ENDOF
      UNSIGNED  OF
                       src -}size @   inttype -}size @  <
                       IF CVU inttype p 0  simplify  TO p
                       ELSE
                          src -}size @   unsignedtype -}size @  <
                          IF CVU  unsignedtype p 0  simplify  TO p
                          THEN
                       THEN
                ENDOF
          ENUM  OF
(Z
                case
                        p = retype(p, inttype);
                        break;
*)          ENDOF
          POINTER  OF
(Z
                case
                        if (isint(dst) && src->size > dst->size)
                                warning("conversion from `%t' to `%t' is undefined\n", p->type, type);
                        p = simplify(CVP, super(src), p, NULL);
                        break;
*)          ENDOF
          FLOT  OF  ENDOF
                0 assert
                ENDCASE
                        p -}ttype @ unqual TO src
                        dst  super TO dst
                        src -}op @  dst -}op @ <>
                        IF
                                src -}op @
                                CASE
                               INT  OF
                                      src -}size @   inttype -}size @  <
                                      IF CVI dst p 0  simplify TO p
                                      THEN
                                     ENDOF
                           UNSIGNED  OF
(*
                                        if (isfloat(dst)&0) {
                                                Type ssrc = signedint(src);
                                                Tree two = cnsttree(longdouble, (long double)2.0);
                                                p = (*optree['+'])(ADD,
                                                        (*optree['*'])(MUL,
                                                                two,
                                                                simplify(CVU, ssrc,
                                                                        simplify(RSH, src,
                                                                                p, consttree(1, inttype)), NULL)),
                                                        simplify(CVU, ssrc,
                                                                simplify(BAND, src,
                                                                        p, consttree(1, unsignedtype)), NULL));
                                        } else
                                                p = simplify(CVU, dst, p, NULL);
                                        break;
*)                                   ENDOF
                               FLOT  OF
                                        dst isunsigned
                                        IF
(Z                                                Type sdst = signedint(dst);
                                                Tree c = cast(cnsttree(longdouble, (long double)sdst->u.sym->u.limits.max.i + 1), src);
                                                p = condtree(
                                                        simplify(GE, src, p, c),
                                                        (*optree['+'])(ADD,
                                                                cast(cast(simplify(SUB, src, p, c), sdst), dst),
                                                                cast(cnsttree(unsignedlong, (unsigned long)sdst->u.sym->u.limits.max.i + 1), dst)),
                                                        simplify(CVF, sdst, p, NULL));
*)                                        ELSE
                                                CVF dst p 0 simplify TO p
                                 ENDOF
                                  0 assert
                                ENDCASE
                        THEN
                       type unqual TO dst

        THEN
        p -}ttype @ unqual TO src

             src -}op @
             CASE
           INT  OF
                       src -}op @ dst -}op @ <>
                       src -}size @  dst -}size @ <> OR
                       IF CVI dst p 0  simplify  TO p
                       THEN
                ENDOF
      UNSIGNED  OF
(*
                if (src->op != dst->op || src->size != dst->size)
                        p = simplify(CVU, dst, p, NULL);
                break;
*)              ENDOF
          FLOT  OF
(Z
                if (src->op != dst->op || src->size != dst->size)
                        p = simplify(CVF, dst, p, NULL);
                break;
*)          ENDOF
          POINTER  OF
(Z
                if (src->op != dst->op)
                        p = simplify(CVP, dst, p, NULL);
                else {
                        if (isfunc(src->type) && !isfunc(dst->type)
                        || !isfunc(src->type) &&  isfunc(dst->type))
                                warning("conversion from `%t' to `%t' is compiler dependent\n", p->type, type);

                        if (src->size != dst->size)
                                p = simplify(CVP, dst, p, NULL);
                }
                break;
*)          ENDOF
        0 assert
            ENDCASE
         p type retype EXIT
; TO cast


: xx(); \ #define xx(t) if (xty == t || yty == t) return t
   S"  DUP xty =  OVER yty = OR IF EXIT THEN DROP" EVALUATE
;  IMMEDIATE

: binary {  xty  yty -- Type }
        longdouble xx();
        doubletype xx();
        floattype xx();
        unsignedlonglong xx();
        longlong xx();
        unsignedlong xx();
          xty  longtype     =  yty  unsignedtype = AND
          xty  unsignedtype =  yty  longtype     = AND OR
        IF  ZZZ

(*                if (longtype->size > unsignedtype->size)
                        return longtype;
                else
                        return unsignedlong;
*)
        THEN
        longtype xx();
        unsignedtype xx();
        inttype
;

: expr0 (  tok -- Tree )
       expr  root
;

: cond { p \ op -- p }
         p rightkid -}op @ generic TO op

        op $AND =
        op $OR  = OR
        op $NOT = OR
        op  EQ  = OR
        op  NE  = OR
        op  LE  = OR
        op  LT  = OR
        op  GE  = OR
        op  GT  = OR
        IF p EXIT
        THEN
        p  pointer TO p
        NE p  0 inttype consttree optree NEQ [] EXECUTE
;

: hascall { p -- n }
        p 0=
        IF  0 EXIT
        THEN
        p -}op @ generic  CALL =
        IF  1 EXIT
        THEN
        p t-kids[0] @   RECURSE
        p t-kids[1] @   RECURSE OR
;


\EOF
(*
static char prec[] = {
#define xx(a,b,c,d,e,f,g) c,
#define yy(a,b,c,d,e,f,g) c,
#include "token.h"
};
*)

PREVIOUS

\EOF

static Tree expr2(void);
static Tree expr3(int);
static Tree nullcheck(Tree);
static Tree postfix(Tree);
static Tree unary(void);
static Tree primary(void);
static Type super(Type ty);

/* funcname - return name of function f or a function' */
char *funcname(Tree f) {
        if (isaddrop(f->op))
                return stringf("`%s'", f->u.sym->name);
        return "a function";
}
static Tree nullcheck(Tree p) {
        if (!needconst && YYnull && isptr(p->type)) {
                p = value(p);
                if (strcmp(YYnull->name, "_YYnull") == 0) {
                        Symbol t1 = temporary(REGISTER, voidptype);
                        p = tree(RIGHT, p->type,
                                tree(OR, voidtype,
                                        cond(asgn(t1, cast(p, voidptype))),
                                        vcall(YYnull, voidtype, (file && *file ? pointer(idtree(mkstr(file)->u.c.loc)) : cnsttree(voidptype, NULL)), cnsttree(inttype, (long)lineno)            , NULL)),
                                idtree(t1));
                }

                else
                        p = nullcall(p->type, YYnull, p, cnsttree(inttype, 0L));

        }
        return p;
}
Tree nullcall(Type pty, Symbol f, Tree p, Tree e) {
        Type ty;

        if (isarray(pty))
                return retype(nullcall(atop(pty), f, p, e), pty);
        ty = unqual(unqual(p->type)->type);
        return vcall(f, pty,
                p, e,
                cnsttree(inttype, (long)ty->size),
                cnsttree(inttype, (long)ty->align),
                (file && *file ? pointer(idtree(mkstr(file)->u.c.loc)) : cnsttree(voidptype, NULL)), cnsttree(inttype, (long)lineno)            , NULL);
}
