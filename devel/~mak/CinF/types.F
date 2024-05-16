REQUIRE -}size ~mak\CinF\c.f
0
type FIELD -}etype
CELL FIELD -}elink
TO entry

CREATE typetable 128 CELLS ALLOT

0 VALUE maxlevel

0 VALUE pointersym

: typeI { op ty size align sym \ tn h }
       op TO h
         op FUNCTION <>  op  ARRAY <>  size 0 > OR AND
        IF typetable h CELLS + @ TO tn
             BEGIN  tn
             WHILE
                    tn -}etype -}op @ op  =
                    tn -}etype -}ttype @ ty =  AND
                    tn -}etype -}size @  size  = AND
                    tn -}etype -}align @ align = AND
                    tn -}etype -}u.sym @ sym  = AND
               IF   tn -}etype EXIT
               THEN tn -}elink @ TO tn
             REPEAT
         THEN
><CDP   HERE  TO tn  entry ALLOT \ tn entry ERASE
><CDP   op    tn -}etype -}op    !
        ty    tn -}etype -}ttype !
        size  tn -}etype -}size  !
      	align tn -}etype -}align !
        sym   tn -}etype -}u.sym !
        typetable h CELLS + @  tn -}elink !
        cfunc 0=
        IF  tn typetable h CELLS + !
        THEN
        tn -}etype
;

: xxi { name op m \ p ty }
   name types GLOBAL PERM install TO p
        op 0 m m.size C@  m m.align C@ p typeI TO ty
        ty -}align 0= ty -}size ty -}align MOD 0= OR THROW
                ty p -}stype !
        m m.outofline C@ p -}addressed !
        CASE
        ty -}op @
        INT      OF
                ty -}size @ 8 *  ones 1 RSHIFT p -}max !
                 p -}max @ NEGATE 1-
                 p -}min !        ENDOF
        UNSIGNED OF
                ty -}size @ 8 * ones    p -}max !
                 p -}min 0!        ENDOF
        FLOT     OF \  79 ZZZ
  ENDOF
                 0 assert
                 ENDCASE  ty
;

: xx(v,t) { v t }
 v DUP 0=
  t -}size  @ voidptype -}size @ =  AND
  t -}align @ voidptype -}align @ =  AND
 IF DROP t THEN
;

: ptr ( ty -- ty' )
 POINTER SWAP
 ptrmetric m.size C@
 ptrmetric m.align C@ pointersym typeI  ;

: func { ty proto style -- ty }
 DUP >R
 ty  ty  isarray ty isfunc OR AND  ABORT" illegal return type `%t'\n"
 DUP R> <> IF ZZZ THEN
 FUNCTION ty 0 0 0 typeI TO ty
 proto ty -}f.proto  !
 style ty -}f.oldstyle !
 ty
;

: type_init { \ p }
    C" int"                INT      intmetric           xxi TO inttype
    C" char"               INT      charmetric          xxi TO chartype
    C" float"              FLOT     floatmetric         xxi TO floattype
    C" double"             FLOT     doublemetric        xxi TO doubletype
    C" long double"        FLOT     longdoublemetric    xxi TO longdouble
    C" long int"           INT      longmetric          xxi TO longtype
    C" long long int"      INT      longlongmetric      xxi TO longlong
    C" short"              INT      shortmetric         xxi TO shorttype
    C" signed char"        INT      charmetric          xxi TO signedchar
    C" unsigned char"      UNSIGNED charmetric          xxi TO unsignedchar
    C" unsigned long"      UNSIGNED longmetric          xxi TO unsignedlong
    C" unsigned short"     UNSIGNED shortmetric         xxi TO unsignedshort
    C" unsigned int"       UNSIGNED intmetric           xxi TO unsignedtype
    C" unsigned long long" UNSIGNED longlongmetric      xxi TO unsignedlonglong
    C" void" types  GLOBAL PERM   install  TO p

         VOID 0 0 0 p  typeI TO voidtype
                 voidtype    p -}stype !
    C" T*"  types    GLOBAL PERM  install TO pointersym
        ptrmetric m.outofline @ pointersym -}addressed !
        ptrmetric m.size @ ones pointersym -}max !
        pointersym -}min 0!
        voidtype  ptr  TO voidptype
         voidtype  0 1 func ptr TO funcptype

        chartype  ptr TO charptype            
        unsignedptr unsignedshort xx(v,t) TO unsignedptr
        unsignedptr unsignedtype  xx(v,t) TO unsignedptr
        unsignedptr unsignedlong  xx(v,t) TO unsignedptr
        unsignedptr unsignedlonglong xx(v,t) TO unsignedptr
        unsignedptr 0=
        IF   UNSIGNED 0
             voidptype -}size  @
             voidptype -}align @
             voidptype -}u.sym @ typeI TO unsignedptr
        THEN
        signedptr shorttype xx(v,t) TO signedptr
        signedptr inttype xx(v,t)   TO signedptr
        signedptr longtype xx(v,t)  TO signedptr
        signedptr longlong xx(v,t)  TO signedptr
        signedptr 0=
        IF   INT 0
             voidptype -}size  @
             voidptype -}align @
             voidptype -}u.sym @ typeI TO signedptr
        THEN
        unsignedshort TO widechar
;

: hasproto {  ty -- flag }
        ty 0=
        IF  1 EXIT
        THEN
        ty -}op @
        CASE
           CASE
           CONST OF\ VOLATILE OF\ CONST VOLATILE + OF\ POINTER OF\
           ARRAY OF;   ty -}ttype @ RECURSE EXIT                  ENDOF
        FUNCTION OF    ty -}ttype @ RECURSE ty -}f.proto @ AND EXIT ENDOF
           CASE
         STRUCT OF\ UNION OF\ VOID OF\ FLOT OF\
           ENUM OF\ INT OF\
       UNSIGNED OF;  1 EXIT ENDOF
        ENDCASE
       0 assert 0
;

: variadic { ty \ i -- flag }
        ty isfunc ty -}f.proto @ AND
        IF      0 TO i
                BEGIN ty -}f.proto @ @
                WHILE \Z i 1+ TO i
                REPEAT

                 i  1 >
                  ty -}f.proto i 1- []  voidtype = AND
                 EXIT
        THEN
        0 EXIT
;

:NONAME   { ty1 ty2 ret \ p1 p2 -- ret }
        ty1 ty2 = IF 1 EXIT THEN
        ty1 -}op @
        ty2 -}op @ <>  IF 0 EXIT THEN
        ty1 -}op @
        CASE
           CASE
           ENUM OF\  UNION OF\  STRUCT OF\
           UNSIGNED OF\ INT OF\  FLOT OF;         0 EXIT
             ENDOF
      POINTER OF
(Z
 POINTER:  return eqtype(ty1->type, ty2->type, 1);
*)             ENDOF
           CASE
          VOLATILE OF\ CONST VOLATILE + OF\
           CONST OF;
(Z
 CONST:    return eqtype(ty1->type, ty2->type, 1);
*)             ENDOF
      ARRAY OF
(*    !!!!!!!
 ARRAY:    if (eqtype(ty1->type, ty2->type, 1)) {
                        if (ty1->size == ty2->size)
                                return 1;
                        if (ty1->size == 0 || ty2->size == 0)
                                return ret;
                       }
*)                        0 EXIT
             ENDOF
      FUNCTION OF ty1 -}ttype @ ty2 -}ttype @ 1 eqtype
                   IF ty1 -}f.proto @ TO p1
                      ty2 -}f.proto @ TO p2
                       p1  p2 =
                       IF 1 EXIT
                       THEN

                       p1  p2 0<> AND
                       IF
                                BEGIN p1 @  p2 @ 0<> AND
                                WHILE
                                   p1 @ unqual p2 @ unqual 1 eqtype 0=
                                   IF  0 EXIT
                                   THEN
                                        p1 CELL+ TO p1
                                        p2 CELL+ TO p2
                                REPEAT
                                   p1 @ 0=  p2 @ 0= AND
                                   IF  1 EXIT
                                   THEN
                     ELSE
                                p1 IF ty1 ELSE ty2 THEN
                                variadic
                                IF  \Z      return 0;
                                THEN

                                p1 0=
                                IF    p2 TO p1
                                THEN
                                BEGIN p1 @
                                WHILE
(Z                                     Type ty = unqual(*p1);
                                        if (promote(ty) != (isenum(ty) ? ty->type : ty))
                                              return 0;
*)                                         p1 CELL+ TO p1
                                REPEAT
                                1 EXIT
                       THEN
                   THEN         0 EXIT
                ENDOF
        ENDCASE
        0 assert 0 EXIT
; TO eqtype

: compose
 2DUP = IF DROP EXIT THEN
         { ty1 ty2 \ p1 p2 ty tty tlist -- ty }
        ty1 -}op @ ty2 -}op @  = assert
        ty1 -}op @
          CASE
      POINTER OF
(Z

                return ptr(compose(ty1->type, ty2->type))
*)             ENDOF
   CONST VOLATILE + OF
(Z

                return qual(CONST, qual(VOLATILE,
                        compose(ty1->type, ty2->type)))
*)             ENDOF
         CASE
      CONST OF\ VOLATILE OF;
(Z

                return qual(ty1->op, compose(ty1->type, ty2->type))
*)             ENDOF
     ARRAY OF
(Z
     { Type ty = compose(ty1->type, ty2->type)
                         if (ty1->size && (ty1->type->size && ty2->size == 0 || ty1->size == ty2->size))
                                return array(ty, ty1->size/ty1->type->size, ty1->align)
                         if (ty2->size && ty2->type->size && ty1->size == 0)
                                return array(ty, ty2->size/ty2->type->size, ty2->align)
                         return array(ty, 0, 0)    }
*)             ENDOF
     FUNCTION OF      ty1 -}f.proto @ TO p1
                      ty2 -}f.proto @ TO p2
                      ty1 -}ttype @ ty2 -}ttype @ RECURSE TO ty
                         0 TO tlist
                         p1 0= p2 0= AND
                         IF  \Z     return func(ty, NULL, 1)
                         THEN
                         p1  p2 0= AND
                         IF  ty p1 ty1 -}f.oldstyle @ func EXIT
                         THEN
                         p2  p1 0= AND
                         IF  ty p2 ty2 -}f.oldstyle @ func EXIT
                         THEN
                         BEGIN p1 @ p2 @ AND
                         WHILE

                                 p1 @ unqual p2 @ unqual RECURSE  TO tty
                                 p1 @ isconst p2 @ isconst OR
                                IF  \Z      tty = qual(CONST, tty)
                                THEN
                                 p1 @ isvolatile p2 @ isvolatile OR
                                IF  \Z    tty = qual(VOLATILE, tty)
                                THEN
                               tty tlist  append TO tlist
                                        p1 CELL+ TO p1
                                        p2 CELL+ TO p2
                         REPEAT
                         p1 @ 0= p2 @ 0= AND assert
                         ty ^ tlist PERM  ltov   0 func  EXIT
                 ENDOF
 ENDCASE
      0  assert 0 EXIT
;

:NONAME  {  lev -- }
        maxlevel lev  >=
        IF            ." Z=" . . . 64 ZZZ
(*                int i
                maxlevel = 0
                for (i = 0 i < NELEMS(typetable) i++) {
                        struct entry *tn, **tq = &typetable[i]
                        while ((tn = *tq) != NULL)
                                if (tn->type.op == FUNCTION)
                                        tq = &tn->link
                                else if (tn->type.u.sym && tn->type.u.sym->scope >= lev)
                                        *tq = tn->link
                                else {
                                        if (tn->type.u.sym && tn->type.u.sym->scope > maxlevel)
                                                maxlevel = tn->type.u.sym->scope
                                        tq = &tn->link
                                }

                }
*)
        THEN
; TO rmtypes

: freturn { ty -- Type }
         ty isfunc
         IF  ty -}ttype @ EXIT
         THEN
        TRUE ABORT" type error: function expected"
;

:NONAME  {  ty -- n }
        ty -}op @

        CASE
         CASE
         CONST OF\ VOLATILE OF\ CONST VOLATILE +  OF;
(Z                return ttob(ty->type)
*)            ENDOF
              CASE
                VOID OF\ INT OF\ UNSIGNED OF\ FLOT OF;
                       ty -}op @  ty -}size @ sizeop + EXIT
                ENDOF
        POINTER OF   voidptype -}size @ sizeop POINTER + EXIT
                ENDOF
         FUNCTION
         OF
\Z                return POINTER + sizeop(funcptype->size)
         ENDOF
         CASE
         ARRAY OF\ STRUCT OF\ UNION OF;
               STRUCT EXIT
         ENDOF
         ENUM
         OF
\Z                return INT + sizeop(inttype->size)
         ENDOF
        0 assert \ return INT
        ENDCASE
; TO ttob

: deref { ty -- Type }
        ty  isptr 0= ABORT" type error: pointer expected"
        ty -}ttype @ TO ty
        ty isenum
        IF ty unqual -}ttype @
        ELSE ty
        THEN
;

: promote { ty -- Type }
        ty unqual TO ty
        ty -}op @
        CASE
         ENUM OF  inttype EXIT
              ENDOF
         INT  OF ty -}size @ inttype -}size @ <
                IF  inttype EXIT
                THEN
             ENDOF
         UNSIGNED  OF ." AA=" . . 16 ZZZ
(*        case UNSIGNED:
                if (ty->size < inttype->size)
                        return inttype
                if (ty->size < unsignedtype->size)
                        return unsignedtype
                break
*)            ENDOF
         FLOT  OF  ty -}size @  doubletype -}size @ <
                IF
\Z                        return doubletype
                THEN
            ENDOF
         ENDCASE
         ty
;

: array { ty  n  a -- Type }
        ty assert
        ty isfunc
        IF   ." illegal type `array of " ty .
             ABORT
        THEN
        ty isarray  ty -}size @  0= AND
            ABORT" missing array size"
        ty -}size @  0=
        IF
(Z                if (unqual(ty) == voidtype)
                        error("illegal type `array of %t'\n", ty)
                else if (Aflag >= 2)
                        warning("declaring type array of %t' is undefined\n", ty)

*)      ELSE   n  INT_MAX ty -}size @ / >
               IF ." size of `array of " ty . ." exceeds" INT_MAX ." bytes"
                   ABORT
               THEN
        THEN
            ARRAY ty  n ty -}size @ *
            a
            IF a
            ELSE    ty -}align @
            THEN
             0   typeI
;

: atop { ty -- Type }
        ty isarray
        IF   ty -}ttype @ ptr EXIT
        THEN
        TRUE  ABORT" type error: array expected"
;

: newstruct { op tag \ p -- Type }

        tag assert
        tag 0=
        IF
\Z                tag = stringd(genlabel(1))
        ELSE
                tag types @ lookup TO p
                p
                IF
(Z                p->scope == level
                || p->scope == PARAM && level == PARAM+1)) {
                  IF
                        if (p->type->op == op && !p->defined)
                                return p->type
                        error("redefinition of `%s' previously defined at %w\n",
                                p->name, &p->src)
                  THEN

*)
                THEN
        THEN
        tag types level PERM install TO p
        op 0 0 0 p  typeI p -}stype !
        p -}scope @  maxlevel >
        IF   p -}scope @ TO maxlevel
        THEN
        p -}stype @
;

: newfield { name  ty  fty \ p q -- p }
        ty -}u.sym @ -}flist TO q
        name 0=
        IF
\Z                name = stringd(genlabel(1))
        THEN
        q @ TO p
        BEGIN p
        WHILE   name COUNT p -}name  @ COUNT COMPARE 0=
\                p -}name @  name =
                IF
\Z                        error("duplicate field name `%s' in `%t'\n",  name, ty)
                THEN
                p f.link TO q
                q @ TO p
        REPEAT
><CDP HERE TO p \field ALLOT
><CDP         p \field ERASE

        p q  !
        name p -}name  !
        fty p f.type   !
        xref
        IF
(Z
                if (ty->u.sym->u.s.ftab == NULL)                        \ omit
                        ty->u.sym->u.s.ftab = table(NULL, level)       \ omit
                install(name, &ty->u.sym->u.s.ftab, 0, PERM)->src = src\ omit
*)      THEN
         p
;

\ isfield - if name is a field in flist, return pointer to the field structure
: isfield { name  flist -- flist }
        BEGIN  flist
        WHILE  flist -}name  @ COUNT  name COUNT  COMPARE
               A_IF
               flist f.link @  TO flist
        REPEAT A_THEN
        flist 
;

\ fieldref - find field name of type ty, return entry
: fieldref { name ty \ p -- p }
        name ty unqual -}u.sym @ -}flist @ isfield TO p
        p  xref AND
        IF
(Z                Symbol q
                assert(unqual(ty)->u.sym->u.s.ftab)
                q = lookup(name, unqual(ty)->u.sym->u.s.ftab)
                assert(q)
                use(q, src)
*)      THEN
        p
;

\EOF


Type qual(int op, Type ty) {
        if (isarray(ty))
                ty = type(ARRAY, qual(op, ty->type), ty->size,
                        ty->align, NULL)
        else if (isfunc(ty))
                warning("qualified function type ignored\n")
        else if (isconst(ty)    && op == CONST
        ||       isvolatile(ty) && op == VOLATILE)
                error("illegal type `%k %t'\n", op, ty)
        else {
                if (isqual(ty)) {
                        op += ty->op
                        ty = ty->type
                }
                ty = type(op, ty, ty->size, ty->align, NULL)
        }
        return ty
}
Type func(Type ty, Type *proto, int style) {
        if (ty && (isarray(ty) || isfunc(ty)))
                error("illegal return type `%t'\n", ty)
        ty = type(FUNCTION, ty, 0, 0, NULL)
        ty->u.f.proto = proto
        ty->u.f.oldstyle = style
        return ty
}
int eqtype(Type ty1, Type ty2, int ret) {
        if (ty1 == ty2)
                return 1
        if (ty1->op != ty2->op)
                return 0
        switch (ty1->op) {
        case ENUM: case UNION: case STRUCT:
        case UNSIGNED: case INT: case FLOAT:
                return 0
        case POINTER:  return eqtype(ty1->type, ty2->type, 1)
        case VOLATILE: case CONST+VOLATILE:
        case CONST:    return eqtype(ty1->type, ty2->type, 1)
        case ARRAY:    if (eqtype(ty1->type, ty2->type, 1)) {
                        if (ty1->size == ty2->size)
                                return 1
                        if (ty1->size == 0 || ty2->size == 0)
                                return ret
                       }
                       return 0
        case FUNCTION: if (eqtype(ty1->type, ty2->type, 1)) {
                        Type *p1 = ty1->u.f.proto, *p2 = ty2->u.f.proto
                        if (p1 == p2)
                                return 1
                        if (p1 && p2) {
                                for (  * p1 && *p2 p1++, p2++)
                                        if (eqtype(unqual(*p1), unqual(*p2), 1) == 0)
                                                return 0
                                if (*p1 == NULL && *p2 == NULL)
                                        return 1
                        } else {
                                if (variadic(p1 ? ty1 : ty2))
                                        return 0
                                if (p1 == NULL)
                                        p1 = p2
                                for (  *p1 p1++) {
                                        Type ty = unqual(*p1)
                                        if (promote(ty) != (isenum(ty) ? ty->type : ty))
                                                return 0
                                }
                                return 1
                        }
                       }
                       return 0
        }
        assert(0) return 0
}
Type signedint(Type ty) {
        if (ty->op == INT)
                return ty
        assert(ty->op == UNSIGNED)
#define xx(t) if (ty->size == t->size) return t
        xx(inttype)
        xx(longtype)
        xx(longlong)
#undef xx
        assert(0) return NULL
}
\ fieldlist - construct a flat list of fields in type ty
Field fieldlist(Type ty) {
        return ty->u.sym->u.s.flist
}


\ ftype - return a function type for rty function (ty,...)' 
Type ftype(Type rty, Type ty) {
        List list = append(ty, NULL)

        list = append(voidtype, list)
        return func(rty, ltov(&list, PERM), 0)
}

\ outtype - output type ty 
void outtype(Type ty, FILE *f) {
        switch (ty->op) {
        case CONST+VOLATILE: case CONST: case VOLATILE:
                fprint(f, "%k %t", ty->op, ty->type)
                break
        case STRUCT: case UNION: case ENUM:
                assert(ty->u.sym)
                if (ty->size == 0)
                        fprint(f, "incomplete ")
                assert(ty->u.sym->name)
                if (*ty->u.sym->name >= '1' && *ty->u.sym->name <= '9') {
                        Symbol p = findtype(ty)
                        if (p == 0)
                                fprint(f, "%k defined at %w", ty->op, &ty->u.sym->src)
                        else
                                fprint(f, p->name)
                } else {
                        fprint(f, "%k %s", ty->op, ty->u.sym->name)
                        if (ty->size == 0)
                                fprint(f, " defined at %w", &ty->u.sym->src)
                }
                break
        case VOID: case FLOAT: case INT: case UNSIGNED:
                fprint(f, ty->u.sym->name)
                break
        case POINTER:
                fprint(f, "pointer to %t", ty->type)
                break
        case FUNCTION:
                fprint(f, "%t function", ty->type)
                if (ty->u.f.proto && ty->u.f.proto[0]) {
                        int i
                        fprint(f, "(%t", ty->u.f.proto[0])
                        for (i = 1 ty->u.f.proto[i] i++)
                                if (ty->u.f.proto[i] == voidtype)
                                        fprint(f, ",...")
                                else
                                        fprint(f, ",%t", ty->u.f.proto[i])
                        fprint(f, ")")
                } else if (ty->u.f.proto && ty->u.f.proto[0] == 0)
                        fprint(f, "(void)")

                break
        case ARRAY:
                if (ty->size > 0 && ty->type && ty->type->size > 0) {
                        fprint(f, "array %d", ty->size/ty->type->size)
                        while (ty->type && isarray(ty->type) && ty->type->type->size > 0) {
                                ty = ty->type
                                fprint(f, ",%d", ty->size/ty->type->size)
                        }
                } else
                        fprint(f, "incomplete array")
                if (ty->type)
                        fprint(f, " of %t", ty->type)
                break
        default: assert(0)
        }
}

\ printdecl - output a C declaration for symbol p of type ty 
void printdecl(Symbol p, Type ty) {
        switch (p->sclass) {
        case AUTO:
                fprint(stderr, "%s\n", typestring(ty, p->name))
                break
        case STATIC: case EXTERN:
                fprint(stderr, "%k %s\n", p->sclass, typestring(ty, p->name))
                break
        case TYPEDEF: case ENUM:
                break
        default: assert(0)
        }
}

\ printproto - output a prototype declaration for function p 
void printproto(Symbol p, Symbol callee[]) {
        if (p->type->u.f.proto)
                printdecl(p, p->type)
        else {
                int i
                List list = 0
                if (callee[0] == 0)
                        list = append(voidtype, list)
                else
                        for (i = 0 callee[i] i++)
                                list = append(callee[i]->type, list)
                printdecl(p, func(freturn(p->type), ltov(&list, PERM), 0))
        }
}

\ prtype - print details of type ty on f with given indent 
static void prtype(Type ty, FILE *f, int indent, unsigned mark) {
        switch (ty->op) {
        default:
                fprint(f, "(%d %d %d [%p])", ty->op, ty->size, ty->align, ty->u.sym)
                break
        case FLOAT: case INT: case UNSIGNED: case VOID:
                fprint(f, "(%k %d %d [\"%s\"])", ty->op, ty->size, ty->align, ty->u.sym->name)
                break
        case CONST+VOLATILE: case CONST: case VOLATILE: case POINTER: case ARRAY:
                fprint(f, "(%k %d %d ", ty->op, ty->size, ty->align)
                prtype(ty->type, f, indent+1, mark)
                fprint(f, ")")
                break
        case STRUCT: case UNION:
                fprint(f, "(%k %d %d [\"%s\"]", ty->op, ty->size, ty->align, ty->u.sym->name)
                if (ty->x.marked != mark) {
                        Field p
                        ty->x.marked = mark
                        for (p = ty->u.sym->u.s.flist p p = p->link) {
                                fprint(f, "\n%I", indent+1)
                                prtype(p->type, f, indent+1, mark)
                                fprint(f, " %s@%d", p->name, p->offset)
                                if (p->lsb)
                                        fprint(f, ":%d..%d",
                                                fieldsize(p) + fieldright(p), fieldright(p))
                        }
                        fprint(f, "\n%I", indent)
                }
                fprint(f, ")")
                break
        case ENUM:
                fprint(f, "(%k %d %d [\"%s\"]", ty->op, ty->size, ty->align, ty->u.sym->name)
                if (ty->x.marked != mark) {
                        int i
                        Symbol *p = ty->u.sym->u.idlist
                        ty->x.marked = mark
                        for (i = 0 p[i] != NULL i++)
                                fprint(f, "%I%s=%d\n", indent+1, p[i]->name, p[i]->u.value)
                }
                fprint(f, ")")
                break
        case FUNCTION:
                fprint(f, "(%k %d %d ", ty->op, ty->size, ty->align)
                prtype(ty->type, f, indent+1, mark)
                if (ty->u.f.proto) {
                        int i
                        fprint(f, "\n%I{", indent+1)
                        for (i = 0 ty->u.f.proto[i] i++) {
                                if (i > 0)
                                        fprint(f, "%I", indent+2)
                                prtype(ty->u.f.proto[i], f, indent+2, mark)
                                fprint(f, "\n")
                        }
                        fprint(f, "%I}", indent+1)
                }
                fprint(f, ")")
                break
        }
}

\ printtype - print details of type ty on fd 
void printtype(Type ty, int fd) {
        static unsigned mark
        prtype(ty, fd == 1 ? stdout : stderr, 0, ++mark)
        fprint(fd == 1 ? stdout : stderr, "\n")
}

\ typestring - return ty as C declaration for str, which may be "" 
char *typestring(Type ty, char *str) {
        for (  ty ty = ty->type) {
                Symbol p
                switch (ty->op) {
                case CONST+VOLATILE: case CONST: case VOLATILE:
                        if (isptr(ty->type))
                                str = stringf("%k %s", ty->op, str)
                        else
                                return stringf("%k %s", ty->op, typestring(ty->type, str))
                        break
                case STRUCT: case UNION: case ENUM:
                        assert(ty->u.sym)
                        if ((p = findtype(ty)) != NULL)
                                return *str ? stringf("%s %s", p->name, str) : p->name
                        if (*ty->u.sym->name >= '1' && *ty->u.sym->name <= '9')
                                warning("unnamed %k in prototype\n", ty->op)
                        if (*str)
                                return stringf("%k %s %s", ty->op, ty->u.sym->name, str)
                        else
                                return stringf("%k %s", ty->op, ty->u.sym->name)
                case VOID: case FLOAT: case INT: case UNSIGNED:
                        return *str ? stringf("%s %s", ty->u.sym->name, str) : ty->u.sym->name
                case POINTER:
                        if (!ischar(ty->type) && (p = findtype(ty)) != NULL)
                                return *str ? stringf("%s %s", p->name, str) : p->name
                        str = stringf(isarray(ty->type) || isfunc(ty->type) ? "(*%s)" : "*%s", str)
                        break
                case FUNCTION:
                        if ((p = findtype(ty)) != NULL)
                                return *str ? stringf("%s %s", p->name, str) : p->name
                        if (ty->u.f.proto == 0)
                                str = stringf("%s()", str)
                        else if (ty->u.f.proto[0]) {
                                int i
                                str = stringf("%s(%s", str, typestring(ty->u.f.proto[0], ""))
                                for (i = 1 ty->u.f.proto[i] i++)
                                        if (ty->u.f.proto[i] == voidtype)
                                                str = stringf("%s, ...", str)
                                        else
                                                str = stringf("%s, %s", str, typestring(ty->u.f.proto[i], ""))
                                str = stringf("%s)", str)
                        } else
                                str = stringf("%s(void)", str)
                        break
                case ARRAY:
                        if ((p = findtype(ty)) != NULL)
                                return *str ? stringf("%s %s", p->name, str) : p->name
                        if (ty->type && ty->type->size > 0)
                                str = stringf("%s[%d]", str, ty->size/ty->type->size)
                        else
                                str = stringf("%s[]", str)
                        break
                default: assert(0)
                }
        }
        assert(0) return 0
}

