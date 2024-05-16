
REQUIRE { ~mak\locals4.f

C" BETWEEN" FIND NIP 0=
[IF] : BETWEEN 1+ WITHIN ;
[THEN]

VARIABLE nglobals
0 VALUE regcount
VARIABLE autos
VARIABLE registers

: defglobal { p }
      p Iglobal 
        1 p -}defined !
;

: initglobal { p  flag \ ty -- }
          
         t [CHAR] = = flag OR
        IF     p -}sclass @  STATIC = 
              IF
                        p -}stype @  TO ty
                        BEGIN  ty isarray
                        WHILE ty -}ttype @  TO ty
                        REPEAT
                        p
\                        ty isconst
\                        IF  LIT ELSE DATA THEN
                         defglobal
               ELSE     p defglobal
               THEN     
                t [CHAR] = =
                IF gettok TO t
                THEN
        DUP >R                 
                p -}stype @  0 initializer TO ty
        DUP R> <> IF ZZZ THEN
                p -}stype @ isarray  p -}stype @ -}size @ 0= AND
                IF
                      ty  p -}stype !
                THEN
                 p -}sclass @  EXTERN =
                IF
\Z                        p->sclass = AUTO;
                THEN
        THEN
;


: dclglobal {  sclass id ty \ p q -- p } 
        sclass 0=
        IF  AUTO TO sclass
        ELSE  sclass EXTERN <>  sclass STATIC <>  AND
             IF
                ." error: invalid storage class " sclass . ." for "
                       ty id COUNT TYPE CR   ABORT
                AUTO TO sclass
             THEN

        THEN
        id identifiers @ lookup  TO p
        p DUP IF -}scope @ GLOBAL = THEN
        IF
                 p -}sclass @ TYPEDEF <> ty p -}stype @ 1 eqtype AND
                IF
                         ty p -}stype @ compose ty
                ELSE
                        ." redeclaration of" p -}name @ COUNT TYPE
                        TRUE ABORT"  previously declared"
                THEN
                ty isfunc 0=  p -}defined @ AND [CHAR] t = = AND
                IF
                        ." redefinition of" p -}name COUNT TYPE
                        TRUE ABORT"  previously declared"
                THEN

                    p -}sclass @ EXTERN =  sclass  STATIC = AND
                    p -}sclass @ STATIC =  sclass    AUTO = AND OR
                    p -}sclass @   AUTO =  sclass  STATIC = AND OR
                IF  ." inconsistent linkage for " p -}name COUNT TYPE
                    warning"  previously declared"
                THEN

        THEN 
        p  DUP IF -}scope @ GLOBAL = THEN 0=
        IF
                id externals @ lookup DUP TO q
                IF    ." DD=" . . 56 ZZZ
(*                        if (sclass == STATIC || !eqtype(ty, q->type, 1))
                                warning("declaration of `%s' does not match previous declaration at %w\n", id, &q->src);

                        p = relocate(id, externals, globals);
                        p->sclass = sclass;
*)
                ELSE
                        id globals GLOBAL PERM  install TO p
                        sclass p -}sclass !
                        p defsymbol
                THEN
                p -}sclass @ STATIC <>
                IF
                        nglobals 1+!
                        Aflag  2 >=  nglobals @ 512 = AND
                        IF  ." warning:more than 511 external identifiers" CR
                        THEN
                THEN
        ELSE
              p -}sclass @  EXTERN =
              IF p -}sclass @ sclass =
              THEN
        THEN
        ty  p -}stype !
        CASE
        t [CHAR] = =   p -}stype @ isfunc  AND
        IF
(*              error("illegal initialization for `%s'\n", p->name);
                t = gettok();
                initializer(p->type, 0);
*)      ELSE
        t [CHAR] = =
        IF
        ['] G><C_DP TO ><CDP
                p 0 ['] initglobal CATCH 
        ['] NOOP   TO  ><CDP  THROW


        constants @ CONSTANTS ['] doconst 0 foreach
               S" $INIT" TOEVAL DOCR
            1 TO  label
\             glevel 0> && IR->stabsym)
\             IF ZZZ \ (*IR->stabsym)(p);
\             THEN
        ELSE
         p -}sclass @  STATIC =
         p -}stype @ isfunc 0= AND
         p -}stype @ -}size @  0= AND
         IF ." error: undefined size for" p -}stype @ . p -}name @ COUNT TYPE
         THEN
         p -}stype @  isfunc 0=
         IF  p  defglobal
             p -}stype @ -}size @ Ispace
         DUPENDCASE
   p
;

\ : enumdcl  DUP . 14 ZZZ ;

CREATE stop3  xIF C, xCHR C,  CHAR } C, 0 C,

: bits2bytes 7 + 8 / ;

: add { overflow x n -- x' }
     x
     inttype -}u.sym @ -}max @  n - >
     IF 1 overflow ! x
     ELSE x n +
     THEN
;

: fields { ty \  n p id fty ty1 bits off overflow a  q -- )

          BEGIN t tsym  istypename
          WHILE
                0 specifier TO ty1
                BEGIN
                        0 TO id
                        ty1 ^ id 0 0 dclr TO fty
                        id ty fty newfield TO p
                        Aflag  1 >=  p f.type @ hasproto 0= AND
                        IF \Z       warning("missing prototype\n");
                        THEN
                        t [CHAR] : =
                        IF
(Z                                if (unqual(p->type) != inttype
                                &&  unqual(p->type) != unsignedtype) {
                                        error("`%t' is an illegal bit-field type\n",
                                                p->type);
                                        p->type = inttype;
                                }
                                t = gettok();
                                p->bitsize = intexpr(0, 0);
                                if (p->bitsize > 8*inttype->size || p->bitsize < 0) {
                                        error("`%d' is an illegal bit-field size\n",
                                                p->bitsize);
                                        p->bitsize = 8*inttype->size;
                                } else if (p->bitsize == 0 && id) {
                                        warning("extraneous 0-width bit field `%t %s' ignored\n", p->type, id);

                                        p->name = stringd(genlabel(1));
                                }
                                p->lsb = 1;
*)
                        ELSE     id 0=
                                IF \Z  error("field name missing\n");
                                ELSE
                                 p f.type @ isfunc
                                IF
\Z                                        error("`%t' is an illegal field type\n", p->type);

                                ELSE
                                 p f.type @ -}size @ 0=
                                IF
\Z                                    error("undefined size for field `%t %s'\n", p->type, id);
                                THEN THEN THEN

                        THEN
                        p f.type @ isconst
                        IF
\Z                                ty->u.sym->u.s.cfields = 1;
                        THEN
                        p f.type @ isvolatile
                        IF
\Z                                ty->u.sym->u.s.vfields = 1;
                        THEN    n 1+ TO n
                        Aflag  2 >=  n  128 = AND
                        IF
\Z                                warning("more than 127 fields in `%t'\n", ty);
                        THEN
                        t [CHAR] , =
                WHILE gettok TO t
                REPEAT
                [CHAR] ; stop3 test
          REPEAT
          0 TO bits 0 TO off 0 TO overflow
          ty -}u.sym @ -}flist TO q
          structmetric m.align C@ ty -}align !
          q @ TO p
          BEGIN p
          WHILE
                p f.type @ -}align @ DUP 0=
                IF DROP 1
                THEN  TO a
                p f.lsb @
                IF
\Z                        a = unsignedtype->align;
                THEN
                ty -}op @  UNION =
                IF
                   0 TO off 0 TO  bits
                ELSE
                 p f.bitsize @ 0=
                 bits  0= OR
                 bits  1-  p f.bitsize @ +  unsignedtype -}size @ 8 *  > OR
                    IF

                        ^ overflow off bits 1- bits2bytes add TO off
                        0 TO bits
\                        chkoverflow(off, a - 1);
                        off a roundup TO off
                    THEN
                THEN
                a ty -}align @ >
                IF   a ty -}align !
                THEN
                off p f.offset !
                p f.lsb @
                IF
(Z                  {
                        if (bits == 0)
                                bits = 1;
                        if (IR->little_endian)
                                p->lsb = bits;
                        else
                                p->lsb = 8*unsignedtype->size - bits + 1
                                        - p->bitsize + 1;
                        bits += p->bitsize;
*)              ELSE
                  ^ overflow off p f.type @ -}size @ add TO off
                THEN
                off  bits 1- bits2bytes +  ty -}size @  >
                IF      off  bits 1- bits2bytes +  ty -}size !
                THEN
                p -}name @ 0=
                [CHAR] 1  p -}name @ 1+ C@ <=
                p -}name @ 1+ C@ [CHAR] 9  <= AND 0= OR
                IF     p q !
                       p f.link TO q
                THEN
                p f.link @ TO p
          REPEAT
          q 0!
\          chkoverflow(ty->size, ty->align - 1);
           ty -}size @ ty -}align @ roundup  ty -}size !
          overflow
          IF
\Z                error("size of `%t' exceeds %d bytes\n", ty, inttype->u.sym->u.limits.max.i);
\                ty->size = inttype->u.sym->u.limits.max.i&(~(ty->align - 1));
          THEN
;

CREATE stop2  xIF C, CHAR , C, 0 C,

: structdcl { op \ tag ty p -- ty }

        gettok TO t
        t  ID =
        IF   token TO tag
             gettok TO t
        ELSE S" " DROP TO tag
        THEN
        t   [CHAR] { =
        IF
                op tag newstruct TO ty
                1 ty -}u.sym @ -}defined !
                gettok TO t
                t tsym  istypename
                IF ty fields
                ELSE \Z    error("invalid %k field declarations\n", op);
                THEN
                [CHAR] } stop2 test
        ELSE
            tag @ DUP
            IF DROP tag  types @ lookup  DUP TO p  
                p -}stype @ -}op @  op = AND
            THEN
          IF
                p -}stype @ TO ty
                t [CHAR] ; = p -}scope @  level < AND
                IF  \Z       ty = newstruct(op, tag);
                THEN
          ELSE
(Z
                if (*tag == 0)
                        error("missing %k tag\n", op);
                ty = newstruct(op, tag);

*)
          THEN
        THEN
        tag  xref AND
        IF   \Z               use(ty->u.sym, pos);
        THEN
         ty      
;

\ : qual  DUP . 16 ZZZ ;


: tnode { op Ltype \ ty -- ty }
><CDP HERE TO ty type ALLOT
><CDP op ty -}op !
  Ltype ty -}ttype !
   ty
;

:NONAME { sclass \ cls cons sign size LType vol ty p tt -- ty }
 0 TO cls 0 TO  cons 0 TO sign 0 TO size 0 TO LType 0 TO vol 0 TO ty
  sclass 0=  IF AUTO TO cls
  THEN
  BEGIN
   t  TO tt
   t CASE
             CASE
        AUTO OF\
    REGISTER OF; level  GLOBAL <=  cls 0 =  AND
                 IF CR ." invalid use of ", t EMIT ABORT
                 THEN    ^ cls TO p   gettok TO t  ENDOF
             CASE
      STATIC OF\ EXTERN OF\
     TYPEDEF OF;  ^  cls TO p   gettok TO t  ENDOF
       CONST OF   ^ cons TO p   gettok TO t  ENDOF
    VOLATILE OF   ^  vol TO p   gettok TO t  ENDOF
             CASE
      SIGNED OF\
    UNSIGNED OF;  ^ sign TO p   gettok TO t  ENDOF
        LONG OF  size LONG =
                IF 0 TO size    LONG LONG + TO tt
                THEN
                  ^ size TO p   gettok TO t  ENDOF
       SHORT OF   ^ size TO p   gettok TO t  ENDOF
             CASE
       VOID OF\ xCHR OF\ INT OF\ FLOT OF\
     DOUBLE OF;  ^ LType TO p
                 tsym  -}stype @ TO ty
                 gettok TO t   ENDOF
       ENUM OF \Z  ^ LType TO p  t enumdcl TO ty
            ENDOF
            CASE
     STRUCT OF\
      UNION OF; ^ LType TO p  t structdcl TO ty ENDOF
         ID OF
                   t tsym  istypename
                   LType 0= AND
                   sign  0= AND
                   size  0= AND
                   IF  \ tsym src use
                        tsym  -}ttype @ TO ty
                        ty isqual
                        ty -}ttype @ DUP IF -}size @  ty -}size @ <> THEN AND
                                IF
                                        ty unqual TO ty
                                        tsym -}ttype @ isconst
                                        IF \Z CONST    ty qual TO ty
                                        THEN
                                        tsym -}ttype @ isvolatile
                                        IF \Z VOLATILE ty qual TO ty
\Z                                           ty tsym -}ttype !
                                        THEN
                                THEN
                                ^ LType TO p   gettok TO t
                   ELSE 0 TO p
                   THEN                 ENDOF
                0 TO p
                ENDCASE
        p
        WHILE   p @
                IF  CR ." invalid use of ", t EMIT ABORT
                THEN   tt p !
        REPEAT
        sclass  
        IF  cls sclass !
        THEN
        LType 0=          
        IF      INT TO LType
                inttype TO ty
        THEN
        size SHORT      = LType INT <>   AND
        size LONG+LONG  = LType INT <>   AND    OR
        size LONG       = LType INT <>   AND  LType DOUBLE <> AND  OR
        sign LType INT <>        AND LType xCHR <>        AND OR
        IF  CR TRUE ABORT" invalid LType specification"
        THEN
        CASE
        LType xCHR = sign AND
        IF  sign UNSIGNED =
            IF  unsignedchar
            ELSE signedchar
            THEN TO ty  ELSE
        size SHORT =
        IF  sign UNSIGNED =
            IF  unsignedshort
            ELSE shorttype
            THEN TO ty  ELSE

        size LONG =   LType DOUBLE = AND
        IF  longdouble TO ty  ELSE

        size LONG+LONG =
        IF  sign UNSIGNED =
            IF  unsignedlonglong
            ELSE longlong
            THEN TO ty
            Aflag 1  >=
            IF ." warning(" ty H. ." is a non-ANSI type\n"
            THEN              ELSE

        size LONG =
        IF  sign UNSIGNED =
            IF  unsignedlong
            ELSE longtype
            THEN TO ty        ELSE

        size UNSIGNED = LType INT = AND
        IF unsignedtype TO ty        ELSE
        DUPENDCASE
        cons CONST =
        IF \Z CONST ty  qual TO ty
        THEN
        vol  VOLATILE =
        IF \Z  VOLATILE ty  qual TO ty
        THEN
        ty
 ;
TO specifier

CREATE sentinel symbol ALLOT
       sentinel symbol ERASE

: parameters { fty  \ params Llist ty ty1 n sclass id -- params }

        0 TO Llist
        kind[t] STATIC =  t  tsym istypename  OR
        IF
                0 TO n
                0 TO ty1
                BEGIN
                         0 TO sclass
                         0 TO id
                        ty1  t  ELLIPSIS =  AND
                        IF      sentinel -}stype @ 0=
                                IF
                                        voidtype sentinel -}stype !
                                        1 sentinel -}defined !
                                THEN
                                 ty1  voidtype =
                                IF  \Z      error("illegal formal parameter types\n");
                                THEN
                                sentinel Llist append TO Llist
                                gettok TO t
                                A_AHEAD
                        THEN
                        t tsym  istypename 0=  t REGISTER <> AND
                        IF TRUE ABORT" missing parameter ty"
                        THEN
                        n 1+ TO n
                        ^ sclass specifier ^ id 0 1 dclr TO ty
                        ty  voidtype =  ty1 id OR  AND
                        ty1 voidtype = OR
                        IF  TRUE ABORT" illegal formal parameter types"
                        THEN
                        id  0=
                        IF \Z  n stringd TO id
                        THEN
                        ty  voidtype <>
                        IF   sclass id ty  dclparam  Llist append TO Llist
                        THEN
                        Aflag  1 >=  ty hasproto 0= AND
                        IF  warning" missing prototype"
                        THEN
                        ty1 0=
                        IF  ty TO ty1
                        THEN
                        t [CHAR] , =
                WHILE
                         gettok TO t
                REPEAT          A_THEN
><CDP-           HERE fty -}f.proto !
                Llist length 1+ CELLS  ALLOT
><CDP-
                ^ Llist FUNC ltov TO params
                0 TO n
                BEGIN   params n []
                WHILE   params n [] -}stype @
                          fty -}f.proto @ n []!
                        n 1+ TO n
                REPEAT
                0 fty -}f.proto @ n []!
                0 fty -}f.oldstyle !
        ELSE
            t ID  =
                IF   ." AA=" . . 91 ZZZ
(*                        for (;;) {
                                Symbol p;
                                if (t != ID) {
                                        error("expecting an identifier\n");
                                        break;
                                }
                                p = dclparam(0, token, inttype, &src);
                                p->defined = 0;
                                list = append(p, list);
                                t = gettok();
                                if (t != ',')
                                        break;
                                t = gettok();
*)
                THEN
                ^ Llist FUNC   ltov TO params
                0 fty -}f.proto !
                1 fty -}f.oldstyle !
        THEN

        [CHAR] ) 0 expect
        params
;


: exitparams { params -- }
        params assert
        params @ DUP
          IF    -}defined @ 0=
          THEN
        IF \Z        error("extraneous old-style parameter list\n");
        THEN
        level  PARAM >
        IF        exitscope
        THEN
        exitscope
;

:NONAME
 { id params abstract \ ty n -- ty }
        0 TO ty
        t
        CASE
        ID  OF  id
                IF   token id !
                ELSE ." extraneous identifier "  token COUNT TYPE ABORT
                THEN
                gettok  TO t
                ENDOF
      [CHAR] * OF   gettok TO t
                    t  CONST =  t  VOLATILE = OR
                    IF
(Z                          Type ty1;
                            ty1 = ty = tnode(t, NULL);
                            while ((t = gettok()) == CONST || t == VOLATILE)
                                      ty1 = tnode(t, ty1);
                            ty->type = dclr1(id, params, abstract);
                            ty = ty1;
*)                  ELSE  id params abstract dclr1 TO ty
                    THEN          POINTER ty tnode TO ty
                ENDOF
      [CHAR] ( OF
(Z
        case '(': t = gettok(); if (abstract
                                && (t == REGISTER || istypename(t, tsym) || t == ')')) {
                                        Symbol *args;
                                        ty = tnode(FUNCTION, ty);
                                        enterscope();
                                        if (level > PARAM)
                                                enterscope();
                                        args = parameters(ty);
                                        exitparams(args);
                                } else {
                                        ty = dclr1(id, params, abstract);
                                        expect(')');
                                        if (abstract && ty == NULL
                                        && (id == NULL || *id == NULL))
                                                return tnode(FUNCTION, NULL);
                                } break;
*)                ENDOF
      [CHAR] [ OF ENDOF
        ENDCASE
        BEGIN  t [CHAR] ( = t [CHAR] [ = OR
        WHILE
               t
           CASE
           [CHAR] ( OF gettok TO t

                       FUNCTION ty tnode TO ty
                        enterscope
                       level  PARAM >
                       IF enterscope
                       THEN
                       ty  parameters \ args
                       params  DUP IF @ 0= THEN
                       IF params !
                       ELSE ZZZ exitparams
                       THEN
                    ENDOF
           [CHAR] [ OF gettok TO t  0 TO n
                       kind[t]  ID =
                       IF TE"  ( "  [CHAR] ] 1 intexpr TO n  TE" ) "
                            n  0 <=
                            IF TRUE ABORT" `%d' is an illegal array size\n" \ , n);
                            THEN
                       ELSE
                       [CHAR] ] 0 expect
                       THEN
                       ARRAY ty tnode TO ty
                       n ty -}size !

                    ENDOF
                0 assert
                ENDCASE
        REPEAT
  ty
; TO dclr1

:NONAME  { basety id params abstract \ ty }
         id  params   abstract dclr1 TO ty
        BEGIN ty
        WHILE ty -}op @
                CASE
          POINTER OF  basety ptr TO basety ENDOF
         FUNCTION OF  basety ty -}f.proto @ ty -}f.oldstyle @ func TO basety
                  ENDOF
            ARRAY OF  basety ty -}size @ 0 array  TO basety
                  ENDOF
                  CASE
            CONST OF\
         VOLATILE OF; \Z  ty -}op basety qual TO basety
                  ENDOF
                  ABORT
                ENDCASE   ty -}ttype  @  TO ty
        REPEAT
         Aflag  2 >=  basety -}size @ 32767 > AND
        IF  ." warning: more than 32767 bytes in" basety U. CR
        THEN

        basety ;     TO dclr

:NONAME  { sclass id ty \  p -- p }
        ty isfunc
        IF
\Z                ty = ptr(ty);
        ELSE  ty isarray
             IF
\Z             ty = atop(ty);
             THEN
        THEN
        sclass  0=
        IF
            AUTO  TO sclass

        ELSE
(Z         if (sclass != REGISTER) {
                error("invalid storage class `%k' for `%t%s\n",
                        sclass, ty, stringf(id ? " %s'" : "' parameter", id));
                sclass = AUTO;
        } else if (isvolatile(ty) || isstruct(ty)) {
                warning("register declaration ignored for `%t%s\n",
                        ty, stringf(id ? " %s'" : "' parameter", id));
                sclass = AUTO;
*)      THEN

         id  identifiers @ lookup TO p
        p DUP IF -}scope @  level = THEN
        IF   TRUE ABORT" duplicate declaration for `%s' previously declared\n"

        ELSE
               id identifiers  level FUNC install  TO p
        THEN
        sclass p -}sclass !
        ty  p -}stype !
        1 p -}defined !
        t [CHAR] = =
        IF
             TRUE ABORT" illegal initialization for parameter `%s'\n"
                 gettok TO t
\Z                (void)expr1(0);
        THEN
        p

;      TO dclparam

: checkref  { p cl -- }
        p -}scope @  PARAM >=
        p -}stype @ isvolatile
        p -}stype @ isfunc OR  AND
        IF 1  p -}addressed !
        THEN
         Aflag  2 >=
         p -}defined @  AND
         p -}ref F@ F0=  AND
         IF     ZZZ
(*                if (p->sclass == STATIC)
                        warning("static `%t %s' is not referenced\n",
                                p->type, p->name);
                else if (p->scope == PARAM)
                        warning("parameter `%t %s' is not referenced\n",
                                p->type, p->name);
                else if (p->scope >= LOCAL && p->sclass != EXTERN)
                        warning("local `%t %s' is not referenced\n",
                                p->type, p->name);
*)       THEN

         p -}sclass @  AUTO =
         p -}scope @  PARAM =
          regcount  0 =        AND
         p -}scope @  LOCAL >= OR AND
         p -}addressed @ 0=    AND
         p -}stype @ isscalar  AND
         p -}ref F@ 3e  F< 0=  AND
        IF     REGISTER p -}sclass !
        THEN
        level  GLOBAL =
        p -}sclass @  STATIC =  AND
        p -}defined @ 0=        AND
        p -}stype  isfunc       AND
        p -}ref F@ F0<>         AND
        IF  ." undefined static "  p -}stype @ p -}name @ COUNT TYPE ABORT
        THEN
        level  GLOBAL =
        p -}sclass @  STATIC =  AND
        p -}defined @ 0=        AND
        p -}stype  isfunc       AND
        0= assert
;

:  dcllocal { sclass  id  ty  \  p q e -- p }
        sclass  0=
        IF   ty isfunc IF EXTERN ELSE AUTO THEN TO sclass
        ELSE
          ty isfunc  sclass  EXTERN <> AND
          IF TRUE ABORT" invalid storage class `%k' for `%t %s'\n" \ ,sclass, ty, id);
\                sclass = EXTERN;
          ELSE sclass  REGISTER = DUP
             IF DROP ty isvolatile ty isstruct ty  isarray  OR OR
             THEN
             IF    warning" register declaration ignored for `%t %s'\n" \ , ty, id);
                 AUTO TO sclass
             THEN
          THEN
        THEN
        id identifiers @ lookup TO q
        q DUP IF  -}scope @ level >= THEN
        q DUP IF  -}scope @ PARAM  = level  LOCAL = AND THEN  OR
        IF   ZZZ
(*                if (sclass == EXTERN && q->sclass == EXTERN
                && eqtype(q->type, ty, 1))
                        ty = compose(ty, q->type);
                else
                        error("redeclaration of `%s' previously declared at %w\n", q->name, &q->src);
*)
        THEN
        level  LOCAL >= assert
        id identifiers level
        sclass  STATIC =
        sclass  EXTERN = OR
        IF PERM
        ELSE FUNC
        THEN  install TO p
        ty p -}stype !
        sclass p -}sclass !
        sclass
        CASE
        EXTERN OF ZZZ
(*    q = lookup(id, globals);
                       if (q == NULL || q->sclass == TYPEDEF || q->sclass == ENUM) {
                        q = lookup(id, externals);
                        if (q == NULL) {
                                q = install(p->name, &externals, GLOBAL, PERM);
                                q->type = p->type;
                                q->sclass = EXTERN;
                                q->src = src;
                                (*IR->defsymbol)(q);
                        }
                       }
                       if (!eqtype(p->type, q->type, 1))
                        warning("declaration of `%s' does not match previous declaration at %w\n", q->name, &q->src);

                       p->u.alias = q; break;
*)                ENDOF
      STATIC OF
                       cfunc >R
                       0 TO cfunc
                       R@
                       IF  TE" ST[ "  DOEVAL
                       THEN
                       p defsymbol
        DUP >R         p 0 initglobal
        DUP R> <> IF ZZZ THEN
                       p -}defined @ 0=
                       IF
                          p -}stype @ -}size @ 0>
                         IF
                                p defglobal
                                p -}stype @ -}size @ Ispace
                         ELSE
                                ABORT" undefined size for `%t %s'\n" \ ,  p->type, p->name);
                         THEN
                       THEN
                       1 p -}defined !
                       R@
                       IF  TE" ]ST "  DOEVAL
                       THEN
                       R> TO cfunc

              ENDOF
   REGISTER OF ZZZ
(* REGISTER: registers = append(p, registers);
                       regcount++;
                       p->defined = 1;
 break;
*)                ENDOF
      AUTO OF     p autos @ append autos !
                  1  p -}defined  !
                 ENDOF
                 0 assert  ZZZ
        ENDCASE
        t [CHAR] = =
        IF
                sclass  EXTERN =
                IF   TRUE ABORT" illegal initialization of `extern %s'\n" \ , id);
                THEN
                gettok TO t
                0 definept
                p -}stype @ isscalar
                p -}stype @ isstruct t  [CHAR] { <>  AND OR
                IF       t [CHAR] { =
                        IF
(Z                                t = gettok();
                                e = expr1(0);
                                expect('}');
*)                       ELSE
                                0 expr1 TO e
                         THEN
                ELSE
(Z                        Symbol t1;
                        Type ty = p->type, ty1 = ty;
                        while (isarray(ty1))
                                ty1 = ty1->type;
                        if (!isconst(ty) && (!isarray(ty) || !isconst(ty1)))
                                ty = qual(CONST, ty);
                        t1 = genident(STATIC, ty, GLOBAL);
                        initglobal(t1, 1);
                        if (isarray(p->type) && p->type->size == 0
                        && t1->type->size > 0)
                                p->type = array(p->type->type,
                                        t1->type->size/t1->type->type->size, 0);
                        e = idtree(t1);
*)               THEN
                p e asgn root DROP \  0 0  walk
                1e p -}ref F!
        THEN
        p   -}stype @  isfunc 0=
        p -}defined @ AND
        p -}stype @  -}size @  0 <= AND
        IF ." undefined size for " p -}stype @ .  id COUNT TYPE ABORT
        THEN
        p
;

:NONAME  { loop swp lev \ cp nregs i j a p -- }
\        0 0 0 walk 
        Blockbeg  N_code TO cp
        enterscope
        level LOCAL >=  assert
        level LOCAL = ( events.entry !!!! ) 0 AND
        IF  \   apply(events.entry, cfunc, NULL);
        THEN
        0 definept
        [CHAR] { 0 expect
         0 autos ! 0 registers !
        level  LOCAL =
        cfunc -}stype @ freturn isstruct  AND
        IF
(Z              retv = genident(AUTO, ptr(freturn(cfunc->type)), level);
                retv->defined = 1;
                retv->ref = 1;
                registers = append(retv, registers);
*)
        THEN
        BEGIN  kind[t] xCHR =
               kind[t] STATIC = OR
               t  tsym istypename DUP
               IF DROP  getchr  [CHAR] : <>
               THEN  OR
        WHILE DUP >R
            ['] dcllocal decl
              DUP R> <> IF ZZZ THEN
        REPEAT
                autos  STMT  ltov  TO a
                registers @ length TO nregs
                0 TO i
                BEGIN  a  i []
                WHILE  a  i [] registers @ append registers !
                        i 1+ TO i
                REPEAT
                registers   FUNC  ltov
                 cp  block.locals !
(*

        if (events.blockentry)
                apply(events.blockentry, cp->u.block.locals, NULL);
*)
        BEGIN    
         kind[t] xIF =  kind[t] ID =  OR
        WHILE   loop swp lev  statement
        REPEAT
\        0 0 0 walk
\        identifiers @ level ['] checkref 0  foreach ZZZ
                nregs TO i
                BEGIN cp block.locals @ i [] DUP TO p
                WHILE
\                        for (j = i; j > nregs
\                                && cp->u.block.locals[j-1]->ref < p->ref; j--)
\                                cp->u.block.locals[j] = cp->u.block.locals[j-1];
\                       cp->u.block.locals[j] = p;
                        i 1+ TO i
                REPEAT
(*   !!!!
        if (events.blockexit)
                apply(events.blockexit, cp->u.block.locals, NULL);
*)
\       level cp block.level !
\       identifiers @ cp block.identifiers !
\        types cp block.types !

        cp 
        Blockend N_code u.begin !
        Gen  reachable
        IF 0 definept
        THEN
        level LOCAL >
        IF  exitscope
           [CHAR] } 0 expect
        THEN

; TO compound

: oldparam
(Z
 (Symbol p, void *cl)
        int i;
        Symbol *callee = cl;

        for (i = 0; callee[i]; i++)
                if (p->name == callee[i]->name) {
                        callee[i] = p;
                        return;
                }
        error("declared parameter `%s' is missing\n", p->name);
*)
;

 CREATE typetable_S 128 CELLS ALLOT
 CREATE ext_T table ALLOT
  0 LABELS table()  labels !
  0 LABELS table()  stmtlabs !

: funcdefn
 { sclass id ty params \ i n callee caller p rty proto cp PrmOffset --  }
        ty freturn  TO rty
        rty isstruct  rty -}size @ 0= AND
        IF TRUE ABORT" illegal use of incomplete type"
        THEN       
        -1
         DEPTH TO Z_DEPTH
          BEGIN 1+ DUP CELLS params + @ 0= UNTIL TO n
         DEPTH TO Z_DEPTH
        n 0> DUP IF DROP params  n 1- CELLS + @ -}name @ 0=  THEN
        IF  params n 1- DUP TO n CELLS + 0!
        THEN
        Aflag  2 >=  n  31 > AND
        IF  warning" more than 31 parameters in function " id COUNT TYPE
        THEN
        ty  -}f.oldstyle @
        IF     Aflag  1 >=
                IF
                    warning" old-style function definition for " id COUNT TYPE
                THEN
                params  TO caller
><CDP-           HERE TO callee
                n 1+ CELLS ALLOT
><CDP-
                caller callee  n 1+ CELLS MOVE
                enterscope
                level  PARAM = assert
                BEGIN kind[t] STATIC = t tsym istypename OR
                WHILE ['] dclparam decl
                REPEAT
                identifiers @ PARAM ['] oldparam callee  foreach

                0 TO i
                BEGIN callee i []  DUP TO p
                WHILE
                        p -}defined @  ." DD=" . . 0 ZZZ 0=
                        IF  0 p -}name @ inttype dclparam
                            callee i CELLS +!
                        THEN
           ." ZZ=" . . 11 ZZZ
(*                        *caller[i] = *p;
                        caller[i]->sclass = AUTO;
                        caller[i]->type = promote(p->type);
*)
                        i 1+ TO i
                REPEAT

                id identifiers @ lookup  TO p
                p DUP IF -}scope GLOBAL = p -}stype @ isfunc AND THEN
                IF
           ." ZZ=" . . 22 ZZZ
(*
                && p->type->u.f.proto) {
                        Type *proto = p->type->u.f.proto;
                        for (i = 0; caller[i] && proto[i]; i++) {
                                Type ty = unqual(proto[i]);
                                if (eqtype(isenum(ty) ? ty->type : ty,
                                        unqual(caller[i]->type), 1) == 0)
                                        break;
                                else if (isenum(ty) && !isenum(unqual(caller[i]->type)))
                                        warning("compatibility of `%t' and `%t' is compiler dependent\n",
                                                proto[i], caller[i]->type);
                        }
                        if (proto[i] || caller[i])
                                error("conflicting argument declarations for function `%s'\n", id);
*)
                ELSE
><CDP-                   HERE TO proto n 1+ CELLS ALLOT
><CDP-                   Aflag  1 >=
                        IF warning" missing prototype for " id COUNT TYPE
                        THEN
                        0 TO i
                        BEGIN i n <
                        WHILE   caller i [] -}stype @ proto i []!
                                i 1+ TO i
                        REPEAT
                        0  proto i []!
                        rty proto   1    func TO ty
                THEN

        ELSE
                params TO callee
><CDP-           HERE TO caller
                n CELLS ALLOT
><CDP-           0 TO i
                BEGIN
  callee i [] DUP TO p DUP IF  -}name @   DUP 20 DUMP THEN
                WHILE
><CDP-                HERE caller i []!
                      symbol ALLOT
><CDP-
                        p  caller i [] symbol CMOVE
                        p -}stype @ isint
                        IF   p -}stype @  promote caller i [] -}stype !
                        THEN
                        AUTO caller i [] -}sclass !
                        p -}name @ 1+ C@  [CHAR] 1 [CHAR] 9 BETWEEN
                        IF   TRUE ABORT" missing name for parameter %d to function `%s'\n"
                        THEN
                        i 1+ TO i
                REPEAT
                0 caller i []!

        THEN       
        0 TO PrmOffset
        0 TO i
        BEGIN callee i [] DUP TO p
        WHILE   p -}stype @  -}size @  0=
                IF   ." undefined size for parameter " p -}stype .
                     p -}name @ COUNT TYPE ABORT
\                    caller[i]->type = p->type = inttype;
                THEN
          PrmOffset  caller i []  x.offset !
          PrmOffset  callee i []  x.offset !
          PrmOffset  caller i []  -}stype @ -}size @ + TO PrmOffset
                 i 1+ TO i
        REPEAT     
        Aflag  2 >=
        sclass  STATIC <>   AND
\        id  main AND
        IF         ." DD=" . . 82 ZZZ
(*
                if (ty->u.f.oldstyle)
                        warning("`%t %s()' is a non-ANSI definition\n", rty, id);
                else if (!(rty == inttype
                        && (n == 0 && callee[0] == NULL
                        ||  n == 2 && callee[0]->type == inttype
                        && isptr(callee[1]->type) && callee[1]->type->type == charptype
                        && !variadic(ty))))
                        warning("`%s' is a non-ANSI definition\n", typestring(ty, id));
*)
        THEN
        id identifiers @ lookup TO p
        p DUP IF -}stype @ isfunc  p -}defined @ AND THEN
        IF  ." redefinition of "  p -}name @ COUNT TYPE ABORT
        THEN        
        sclass id ty dclglobal  TO cfunc
        1 genlabel cfunc -}label  !
\        callee     cfunc -}callee !
        1 cfunc -}defined !
        xref
        IF \Z use(cfunc, cfunc->src);
        THEN       
\        if (Pflag)
\                printproto(cfunc, cfunc->u.f.callee);
\        ncalled   0 >=
\        IF        ncalled = findfunc(cfunc->name, pt.file);
\        THEN
\         0 LABELS table()   labels !
\         0 LABELS table()   stmtlabs  !

   labels @ -}buckets HASHSIZE CELLS ERASE labels @ -}all 0!
 stmtlabs @ -}buckets HASHSIZE CELLS ERASE
constants @ -}buckets HASHSIZE CELLS ERASE constants @ -}all 0!
        1.0e refinc F!
          0 TO regcount
        codehead TO codelist
\        codelist \code ERASE
\        codelist c.next 0!

        DOCR TE" \ "
        cfunc -}stype @ -}f.proto @ TO proto
        BEGIN    proto DUP
              IF     @ DUP  voidtype <> AND
              THEN
        WHILE     proto @  -}size @ TE. proto CELL+ TO proto
        REPEAT
        DOCR

        TE"  HERE TO L_VAR 0 ,"
        DOCR
        ['] ><C_DP TO ><CDP
        typetable typetable_S 128 CELLS CMOVE
        ext_T externals !   ext_T table ERASE  GLOBAL ext_T !
        qqq1 0= IF DP @ TO qqq1 THEN
        p
        IF TE" :NONAME "
        ELSE  TE" : " cfunc -}sx x.name @ COUNT TOEVALBL
        THEN    DOCR
        proto DUP IF @  voidtype = THEN
        IF   TE"  ....  "
        ELSE TE"  [ ...FLAG 0!  ] "
        THEN

        BEGIN  i DUP 1- TO i
        WHILE
          caller i [] -}stype @ -}op @
          CASE
          0 +I: OF  TE" >R " ENDOF
          0 +U: OF  TE" >R " ENDOF
          0 +P: OF  TE" >R " ENDOF
          0 +F: OF  TE" F>R " ENDOF
          0 assert
          ENDCASE
        REPEAT
        TE"  [ "  PrmOffset TE. TE"  BIG_PROC ] "
        proto DUP IF @  voidtype = THEN
        IF TE"  RP_SV @ >R "
        THEN
        DOCR
\        C_DPBuff C_DP @ ." XX=" 2DUP . . C_DPBuff - 0x55 FILL \ ERASE
        C_DPBuff C_DP !  \ DP @ TO LAST-HERE
        0 0 0  ['] compound CATCH
        ['] NOOP   TO  ><CDP  THROW
        ['] ><C_DP TO  ><CDP  \ !!!!!!!!!

        DOCR TE" VLABEL $1 "  DP @ TO LAST-HERE
        proto DUP IF @  voidtype = THEN
        IF TE" R> RP! ;"
        ELSE TE"  END_PROC ;"
        THEN
        qqq2 0= IF DP @ TO qqq2 THEN

        ext externals !
        typetable_S typetable 128 CELLS CMOVE

        DOCR
        ['] NOOP   TO  ><CDP
        p
        IF  TE"  TO " id COUNT TOEVALBL
        THEN
        offset 3 + -4 AND TE.  TE"  L_VAR !" DOCR
        exitscope
        level PARAM = assert
        1 TO label
        constants @  CONSTANTS ['] doconst  0 foreach
        TE"  $INIT" DOCR
        exitscope
        [CHAR] } 0 expect
\        0 labels ! 0 stmtlabs !
\        0 TO retv ?????
        0 TO cfunc
;

:NONAME {  dcl \  sclass ty ty1 id params p -- }
 0 TO sclass
 ^ sclass specifier TO ty
 t  ID =
 t  [CHAR] * = OR
 t  [CHAR] ; = OR
 t  [CHAR] [ = OR
 IF     0 TO id
        level  GLOBAL = 
        IF      0 TO params
\                     ." XX=" DUP .
                ty   ^ id ^ params 0  dclr  TO ty1 
\   DUP 13 <>  OVER 14 <> AND IF t ZZZ THEN
                params
                id                          AND
                ty1 isfunc                  AND
                 t  [CHAR] { =
                 t tsym istypename       OR
                 kind[t] STATIC =
                 t   TYPEDEF <>      AND OR AND 
                IF  sclass  TYPEDEF =
                    IF  TRUE  ABORT" invalid use of `typedef'"
                    THEN
                    ty1  -}f.oldstyle  @ IF exitscope  THEN
                     sclass   id   ty1 params funcdefn EXIT
                ELSE
                     params
                     IF TE" VECT " id COUNT TOEVAL DOCR
                         params exitparams
                     THEN
                 THEN
        ELSE    ty  ^ id  0  0 dclr TO params
                params  TO ty1
        THEN
        BEGIN
                Aflag      1 =  ty1 hasproto 0= AND
                IF ." missing prototype" CR
                THEN
                id  0=
                IF ." missing identifier"
                ELSE    sclass TYPEDEF =
                        IF      id  identifiers @ lookup TO p
                                p DUP IF -}scope  @  level  <> THEN
                                IF ." redeclaration of"  id TYPEZ
                                THEN
                                id  identifiers level
                                level  LOCAL < IF PERM ELSE FUNC THEN
                                install  TO p
                                ty1 p -}ttype !
                                TYPEDEF p -}sclass  !
                        ELSE
                                sclass  id ty1   dcl EXECUTE DROP
                        THEN

                THEN
        t  [CHAR] ,  =
        WHILE
                gettok TO t
                        0 TO id
                ty1   ^ id 0 0  dclr DROP
        REPEAT
 ELSE
        ty  0=
        ty  @  isenum 0=  OR
        ty  isstruct
        ty  unqual -}u.sym -}name C@  [CHAR] 1 [CHAR] 9 WITHIN 0= AND OR
        IF  TRUE ABORT" empty declaration"
        THEN
 THEN   [CHAR] ; 0 test
;   TO decl

: program { \ n -- }
\  12   DEPTH TO Z_DEPTH
BEGIN  t EOI <> t [CHAR] } <> AND
WHILE  kind[t] xCHR =
       kind[t] STATIC = OR
       t  ID = OR
       t  [CHAR] * = OR
       t  [CHAR] ( = OR
      IF \ DUP 1+ DUP  DEPTH TO Z_DEPTH
          ['] dclglobal decl
    \ STMT deallocate
    \ glevel @  3 >=
    \ xref @ OR 0=
    \ IF FUNC deallocate THEN
      ELSE   t [CHAR] ; <> ABORT" unrecognized declaration"
             warning" empty declaration" CR gettok TO t
      THEN
REPEAT
;

:NONAME  {  p cl -- }
        p -}loc @
        IF
\                (p->u.c.loc->u.seg  0= assert
                p -}loc @  defglobal
                p -}stype @ isarray  p -}stype @ -}ttype @  widechar = AND
                IF
(Z                        unsigned int *s = p->u.c.v.p;
                        int n = p->type->size/widechar->size;
                        while (n-- > 0) {
                                Value v;
                                v.u = *s++;
                                (*IR->defconst)(widechar->op, widechar->size, v);
                        }
*)              ELSE  p -}stype @ isarray
                      IF  p  -}stype @ -}size @  p -}v @  Idefstring
                      ELSE
\Z                        (*IR->defconst)(p->type->op, p->type->size, p->u.c.v);
                      THEN
                THEN
                p -}loc 0!
        THEN
;  TO doconst

:NONAME
        0 specifier  { ty -- ty }
        t [CHAR] * =
        t [CHAR] ( = OR
        t [CHAR] [ = OR
        IF
(Z                ty = dclr(ty, NULL, NULL, 1);
                if (Aflag >= 1 && !hasproto(ty))
                        warning("missing prototype\n");
*)        THEN
        ty
; TO typename


