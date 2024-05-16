
\ genconst - generate/check constant expression e; return size
: genconst { e  def -- n }
        BEGIN
                e -}op @ generic
                CASE
            ADDRG OF  def
                        IF \ e tree.sym @  Idefaddress
                             TE"  ," DOCR
                        THEN
                         e -}ttype @ -}size @  EXIT
                   ENDOF
                 CNST OF e -}op @  CNST +P: = e -}ttype @  isarray AND
                        IF
(Z
                                e = cvtconst(e);
                                continue;
*)                      THEN
                        def
                        IF e -}ttype @ -}op @
                           e -}ttype @ -}size @
                           e tree.v  Idefconst  
                        THEN
                        e -}ttype @ -}size @ EXIT
                 ENDOF
                 RIGHT OF
(Z

                        assert(e->kids[0] || e->kids[1]);
                        if (e->kids[1] && e->kids[0])
                                error("initializer must be constant\n");
                        e = e->kids[1] ? e->kids[1] : e->kids[0];
                        continue;
*)               ENDOF
                 CVP OF
(Z

                        if (isarith(e->type))
                                error("cast from `%t' to `%t' is illegal in constant expressions\n",
                                        e->kids[0]->type, e->type);
                        /* fall thru */
*)                      A_AHEAD
                 ENDOF
                 CASE CVI OF\  CVU  OF\  CVF  OF;
                        A_THEN
                        e t-kids[0] @ TO e

                ENDOF   DROP
                        def
                        IF e -}ttype @ -}op @
                           e -}ttype @ -}size @
                           e tree.v Idefconst
                        THEN
                        e -}ttype @ -}size @ EXIT
\                     TRUE   ABORT" initializer must be constant\n"
\                        if (def)
\                                genconst(consttree(0, inttype), def);
\                        return inttype->size;

                ENDCASE
         AGAIN
;

(* initarray - initialize array of ty of <= len bytes; if len == 0, go to } *)
:  initarray { len  ty  lev \ n -- n }
        0 TO n

        BEGIN
                ty lev initializer  DROP
                n ty -}size @ + TO n
                len  0>  n  len >= AND  t  [CHAR] , <>  OR
                IF n EXIT
                THEN
                gettok TO t
         t [CHAR] } =
        UNTIL
         n
;

CREATE follow[] xIF , xCHR , STATIC , 0 ,

\ initializer - constexpr | { constexpr ( , constexpr )* [ , ] }
:NONAME  { ty  lev \ n e aty --  ty  }
        0 TO n
        0 TO aty
        DUP >R

        ty unqual TO ty
        ty isscalar
        IF       needconst 1+ TO needconst
                t [CHAR] { =
                IF
(Z                        t = gettok();
                        e = expr1(0);
                        initend(lev, follow);
*)
                ELSE  0 expr1 TO e
                THEN
                e  pointer TO e
                ty e assign TO aty
                aty
                IF   e aty cast TO e
                ELSE
\Z         error("invalid initialization type; found `%t' expected `%t'\n", e->type, ty);
                THEN
        DUP >R    e 1  genconst TO n  DUP R> <> IF ZZZ THEN
\                deallocate(STMT);
                 needconst 1- TO needconst

        THEN
        DUP R@ <> IF ZZZ THEN
        ty isunion ty isstruct OR  ty -}size @  0= AND
        IF
(Z                static char follow[] = { CHAR, STATIC, 0 };
                error("cannot initialize undefined `%t'\n", ty);
                skipto(';', follow);
                return ty;
*)      ELSE  ty isunion
              IF
(Z                if (t == '{') {
                        t = gettok();
                        n = initstruct(ty->u.sym->u.s.flist->type->size, ty, lev + 1);
                        initend(lev, follow);
                } else {
                        if (lev == 0)
                                error("missing { in initialization of `%t'\n", ty);
                        n = initstruct(ty->u.sym->u.s.flist->type->size, ty, lev + 1);
                }
*)
              ELSE ty isstruct
                     IF
(Z                if (t == '{') {
                        t = gettok();
                        n = initstruct(0, ty, lev + 1);
                        test('}', follow);
                } else if (lev > 0)
                        n = initstruct(ty->size, ty, lev + 1);
                else {
                        error("missing { in initialization of `%t'\n", ty);
                        n = initstruct(ty->u.sym->u.s.flist->type->size, ty, lev + 1);
*)
                     THEN
              THEN
        THEN
        DUP R@ <> IF ZZZ THEN
        ty  isarray
        IF    ty -}ttype @ unqual  TO aty
        THEN
        ty  isarray aty ischar  AND
        IF      t  SCON  =
                IF
                        ty -}size @ 0>
                        ty -}size @  tsym -}stype @  -}size @  1- =  AND
                        IF \Z tsym->type = array(chartype, ty->size, 0);
                        THEN
                        tsym -}stype @ -}size @  TO n
                        tsym -}stype @ -}size @  tsym -}v @ Idefstring
                        gettok TO t
                ELSE
(Z                 if (t == '{') {
                        t = gettok();
                        if (t == SCON) {
                                ty = initializer(ty, lev + 1);
                                initend(lev, follow);
                                return ty;
                        }
                        n = initchar(0, aty);
                        test('}', follow);
                } else if (lev > 0 && ty->size > 0)
                        n = initchar(ty->size, aty);
                else {  /* eg, char c[] = 0; */
                        error("missing { in initialization of `%t'\n", ty);
                        n = initchar(1, aty);
*)                THEN
        ELSE  ty isarray
              IF  t  SCON =  aty  widechar = AND
                IF
(Z                        int i;
                        unsigned int *s = tsym->u.c.v.p;
                        if (ty->size > 0 && ty->size == tsym->type->size - widechar->size)
                                tsym->type = array(widechar, ty->size/widechar->size, 0);
                        n = tsym->type->size;
                        for (i = 0; i < n; i += widechar->size) {
                                Value v;
                                v.u = *s++;
                                (*IR->defconst)(widechar->op, widechar->size, v);
                        }
                        t = gettok();
*)              ELSE  t [CHAR] { =
                     IF   gettok TO t
                          t  SCON =  aty  widechar = AND
                        IF
(Z                                ty = initializer(ty, lev + 1);
                                initend(lev, follow);
                                return ty;
*)                      THEN
                         0 aty lev 1+ initarray TO n
        DUP R@ <> IF ZZZ THEN
                        [CHAR] } follow[]  test
                     ELSE
(Z                      if (lev > 0 && ty->size > 0)
                        n = initarray(ty->size, aty, lev + 1);
                     else {
                        error("missing { in initialization of `%t'\n", ty);
                        n = initarray(aty->size, aty, lev + 1);
*)                   THEN
                THEN
            THEN
        THEN
        DUP R@ <> IF ZZZ THEN
        ty -}size @
        IF      n  ty -}size @ >
                IF \ TRUE ABORT" too many initializers\n"
                ELSE n  ty -}size @ <
                     IF   ty -}size @ n - Ispace
                     THEN
                THEN
        ELSE
           ty isarray  ty -}ttype @  -}size @  0>  AND
           IF   ty -}ttype @ n ty -}ttype @ -}size @  /  0  array TO ty
           ELSE
\Z              ty->size = n;
           THEN
        THEN
        DUP R> <> IF ZZZ THEN
         ty

; TO initializer

\EOF
#include "c.h"

static char rcsid[] = "$Id: init.nw,v 2.5 1998/04/30 20:41:04 drh Exp $";

static int curseg;              /* current segment */

/* defpointer - initialize a pointer to p or to 0 if p==0 */
void defpointer(Symbol p) {
        if (p) {
                (*IR->defaddress)(p);
                p->ref++;
        } else {
                static Value v;
                (*IR->defconst)(P, voidptype->size, v);
        }
}


/* initvalue - evaluate a constant expression for a value of integer type ty */
static Tree initvalue(Type ty) {
        Type aty;
        Tree e;

        needconst++;
        e = expr1(0);
        if ((aty = assign(ty, e)) != NULL)
                e = cast(e, aty);
        else {
                error("invalid initialization type; found `%t' expected `%t'\n",
                        e->type,  ty);
                e = retype(consttree(0, inttype), ty);
        }
        needconst--;
        if (generic(e->op) != CNST) {
                error("initializer must be constant\n");
                e = retype(consttree(0, inttype), ty);
        }
        return e;
}


/* initchar - initialize array of <= len ty characters; if len == 0, go to } */
static int initchar(int len, Type ty) {
        int n = 0;
        char buf[16], *s = buf;

        do {
                *s++ = initvalue(ty)->u.v.i;
                if (++n%inttype->size == 0) {
                        (*IR->defstring)(inttype->size, buf);
                        s = buf;
                }
                if (len > 0 && n >= len || t != ',')
                        break;
                t = gettok();
        } while (t != '}');
        if (s > buf)
                (*IR->defstring)(s - buf, buf);
        return n;
}

/* initend - finish off an initialization at level lev; accepts trailing comma */
static void initend(int lev, char follow[]) {
        if (lev == 0 && t == ',')
                t = gettok();
        test('}', follow);
}

/* initfields - initialize <= an unsigned's worth of bit fields in fields p to q */
static int initfields(Field p, Field q) {
        unsigned int bits = 0;
        int i, n = 0;

        do {
                i = initvalue(inttype)->u.v.i;
                if (fieldsize(p) < 8*p->type->size) {
                        if (p->type == inttype &&
                           (i < -(int)(fieldmask(p)>>1)-1 || i > (int)(fieldmask(p)>>1))
                        ||  p->type == unsignedtype && (i&~fieldmask(p)) !=  0)
                                warning("initializer exceeds bit-field width\n");
                        i &= fieldmask(p);
                }
                bits |= i<<fieldright(p);
                if (IR->little_endian) {
                        if (fieldsize(p) + fieldright(p) > n)
                                n = fieldsize(p) + fieldright(p);
                } else {
                        if (fieldsize(p) + fieldleft(p) > n)
                                n = fieldsize(p) + fieldleft(p);
                }
                if (p->link == q)
                        break;
                p = p->link;
        } while (t == ',' && (t = gettok()) != 0);
        n = (n + 7)/8;
        for (i = 0; i < n; i++) {
                Value v;
                if (IR->little_endian) {
                        v.u = (unsigned char)bits;
                        bits >>= 8;
                } else {        /* a big endian */
                        v.u = (unsigned char)(bits>>(8*(unsignedtype->size - 1)));
                        bits <<= 8;
                }
                (*IR->defconst)(U, unsignedchar->size, v);
        }
        return n;
}

/* initstruct - initialize a struct ty of <= len bytes; if len == 0, go to } */
static int initstruct(int len, Type ty, int lev) {
        int a, n = 0;
        Field p = ty->u.sym->u.s.flist;

        do {
                if (p->offset > n) {
                        (*IR->space)(p->offset - n);
                        n += p->offset - n;
                }
                if (p->lsb) {
                        Field q = p;
                        while (q->link && q->link->offset == p->offset)
                                q = q->link;
                        n += initfields(p, q->link);
                        p = q;
                } else {
                        initializer(p->type, lev);
                        n += p->type->size;
                }
                if (p->link) {
                        p = p->link;
                        a = p->type->align;
                } else
                        a = ty->align;
                if (a && n%a) {
                        (*IR->space)(a - n%a);
                        n = roundup(n, a);
                }
                if (len > 0 && n >= len || t != ',')
                        break;
                t = gettok();
        } while (t != '}');
        return n;
}
