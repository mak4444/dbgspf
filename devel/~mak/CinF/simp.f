0 VALUE needconst

:NONAME  { op  ty  l  r \ n p -- Tree }
        op optype 0=
        IF    op ty mkop TO  op
        THEN
        DOGS
        IF  DOGS TEC TE" @ " 0 TO DOGS
        THEN 
         op
        CASE
             ADD +U:  OF ENDOF
             ADD +I:  OF ENDOF
            CVI +I:  OF
(* !!!!!                     xcvtcnst(I,l->u.v.i,ty,i,(long)extend(l->u.v.i,ty));
                        break;
*)
             ENDOF
           CVU +I:   OF ZZZ
(*

                        if (l->op == CNST+U) {
                                if (!explicitCast && l->u.v.u > ty->u.sym->u.limits.max.i)
                                        warning("overflow in converting constant expression from `%t' to `%t'\n", l->type, ty);
                                if (needconst || !(l->u.v.u > ty->u.sym->u.limits.max.i))
                                        return cnsttree(ty, (long)extend(l->u.v.u,ty));
                        }
                        break;
*)
             ENDOF
           CVP +U:   OF ZZZ
(*

                        xcvtcnst(P,(unsigned long)l->u.v.p,ty,u,(unsigned long)l->u.v.p);
                        break;
*)
             ENDOF
          CVU +P:    OF ZZZ
(*

                        xcvtcnst(U,(void*)l->u.v.u,ty,p,(void*)l->u.v.u);
                        break;
*)
             ENDOF
           CVP +P:   OF ZZZ
(*

                        xcvtcnst(P,l->u.v.p,ty,p,l->u.v.p);
                        break;
*)
             ENDOF
           CVI +U:   OF ZZZ
(*

                        xcvtcnst(I,l->u.v.i,ty,u,((unsigned long)l->u.v.i)&ones(8*ty->size));
                        break;
*)
             ENDOF
           CVU +U:   OF ZZZ
(*

                        xcvtcnst(U,l->u.v.u,ty,u,l->u.v.u&ones(8*ty->size));
                        break;

*)
             ENDOF
            CVI +F:  OF 
(*

                        xcvtcnst(I,l->u.v.i,ty,d,(long double)l->u.v.i);
*)
             ENDOF
             CVU +F: OF ZZZ
(*

                        xcvtcnst(U,l->u.v.u,ty,d,(long double)l->u.v.u);
                        break;
*)
             ENDOF
             CVF +I: OF 
(*

                        xcvtcnst(F,l->u.v.d,ty,i,(long)l->u.v.d);
                        break;
*)
             ENDOF
             CVF +F: OF ZZZ
(*
  {
                        float d;
                        if (l->op == CNST+F)
                                if (l->u.v.d < ty->u.sym->u.limits.min.d)
                                        d = ty->u.sym->u.limits.min.d;
                                else if (l->u.v.d > ty->u.sym->u.limits.max.d)
                                        d = ty->u.sym->u.limits.max.d;
                                else
                                        d = l->u.v.d;
                        xcvtcnst(F,l->u.v.d,ty,d,(long double)d);
                        break;
                        }
*)
             ENDOF
             BAND +U: OF ZZZ
(*

                        foldcnst(U,u,&);
                        commute(r,l);
                        identity(r,l,U,u,ones(8*ty->size));
                        if (r->op == CNST+U && r->u.v.u == 0)
                                return tree(RIGHT, ty, root(l), cnsttree(ty, 0UL));
                        break;
*)
             ENDOF
             BAND +I: OF
(*

                        foldcnst(I,i,&);
                        commute(r,l);
                        identity(r,l,I,i,ones(8*ty->size));
                        if (r->op == CNST+I && r->u.v.u == 0)
                                return tree(RIGHT, ty, root(l), cnsttree(ty, 0L));
                        break;

*)
             ENDOF
             MUL +U: OF ZZZ
(*

                        commute(l,r);
                        if (l->op == CNST+U && (n = ispow2(l->u.v.u)) != 0)
                                return simplify(LSH, ty, r, cnsttree(inttype, (long)n));
                        foldcnst(U,u,*);
                        identity(r,l,U,u,1);
                        break;
*)
             ENDOF
             NE +I: OF  ENDOF
             EQ +I: OF
(*

                        cfoldcnst(I,i,==);
                        commute(r,l);
                        zerofield(EQ,I,i);
                        break;
*)
             ENDOF
             ADD +P: OF 
(*

                        foldaddp(l,r,I,i);
                        foldaddp(l,r,U,u);
                        foldaddp(r,l,I,i);
                        foldaddp(r,l,U,u);
                        commute(r,l);
                        identity(r,retype(l,ty),I,i,0);
                        identity(r,retype(l,ty),U,u,0);
                        if (isaddrop(l->op)
                        && (r->op == CNST+I && r->u.v.i <= longtype->u.sym->u.limits.max.i
                            && r->u.v.i >= longtype->u.sym->u.limits.min.i
                        || r->op == CNST+U && r->u.v.u <= longtype->u.sym->u.limits.max.i))
                                return addrtree(l, cast(r, longtype)->u.v.i, ty);
                        if (l->op == ADD+P && isaddrop(l->kids[1]->op)
                        && (r->op == CNST+I && r->u.v.i <= longtype->u.sym->u.limits.max.i
                            && r->u.v.i >= longtype->u.sym->u.limits.min.i
                        ||  r->op == CNST+U && r->u.v.u <= longtype->u.sym->u.limits.max.i))
                                return simplify(ADD+P, ty, l->kids[0],
                                        addrtree(l->kids[1], cast(r, longtype)->u.v.i, ty));
                        if ((l->op == ADD+I || l->op == SUB+I)
                        && l->kids[1]->op == CNST+I && isaddrop(r->op))
                                return simplify(ADD+P, ty, l->kids[0],
                                        simplify(generic(l->op)+P, ty, r, l->kids[1]));
                        if (l->op == ADD+P && generic(l->kids[1]->op) == CNST
                        && generic(r->op) == CNST)
                                return simplify(ADD+P, ty, l->kids[0],
                                        simplify(ADD, l->kids[1]->type, l->kids[1], r));
                        if (l->op == ADD+I && generic(l->kids[1]->op) == CNST
                        &&  r->op == ADD+P && generic(r->kids[1]->op) == CNST)
                                return simplify(ADD+P, ty, l->kids[0],
                                        simplify(ADD+P, ty, r->kids[0],
                                        simplify(ADD, r->kids[1]->type, l->kids[1], r->kids[1])));
                        if (l->op == RIGHT && l->kids[1])
                                return tree(RIGHT, ty, l->kids[0],
                                        simplify(ADD+P, ty, l->kids[1], r));
                        else if (l->op == RIGHT && l->kids[0])
                                return tree(RIGHT, ty,
                                        simplify(ADD+P, ty, l->kids[0], r), NULL);
                        break;

*)
             ENDOF
             ADD +F: OF 
(*

                        xfoldcnst(F,d,+,addd);
                        commute(r,l);
                        break;
*)
             ENDOF
             $AND +I: OF
(*

                        op = AND;
                        ufoldcnst(I,l->u.v.i ? cond(r ) : l);    /* 0&&r => 0, 1&&r => r */
                        break;
*)
             ENDOF
             $OR +I: OF 
(*

                        op = OR;
                        /* 0||r => r, 1||r => 1 */
                        ufoldcnst(I,l->u.v.i ? cnsttree(ty, 1L ) : cond(r));
                        break;
*)
             ENDOF
             BCOM +I: OF ZZZ
(*

                        ufoldcnst(I,cnsttree(ty, (long)extend((~l->u.v.i)&ones(8*ty->size), ty)));
                        idempotent(BCOM+U);
                        break;
*)
             ENDOF
             BCOM +U: OF ZZZ
(*

                        ufoldcnst(U,cnsttree(ty, (unsigned long)((~l->u.v.u)&ones(8*ty->size))));
                        idempotent(BCOM+U);
                        break;
*)
             ENDOF
             BOR +U: OF ZZZ
(*

                        foldcnst(U,u,|);
                        commute(r,l);
                        identity(r,l,U,u,0);
                        break;
*)
             ENDOF
             BOR +I: OF
(*

                        foldcnst(I,i,|);
                        commute(r,l);
                        identity(r,l,I,i,0);
                        break;
*)
             ENDOF
             BXOR +U: OF ZZZ
(*

                        foldcnst(U,u,^);
                        commute(r,l);
                        identity(r,l,U,u,0);
                        break;
*)
             ENDOF
             BXOR +I: OF ZZZ
(*

                        foldcnst(I,i,^);
                        commute(r,l);
                        identity(r,l,I,i,0);
                        break;
*)
             ENDOF
             DIV +F: OF 
(*

                        xfoldcnst(F,d,/,divd);
                        break;
*)
             ENDOF
             DIV +I: OF
(*

                        identity(r,l,I,i,1);
                        if (r->op == CNST+I && r->u.v.i == 0
                        ||  l->op == CNST+I && l->u.v.i == ty->u.sym->u.limits.min.i
                        &&  r->op == CNST+I && r->u.v.i == -1)
                                break;
                        xfoldcnst(I,i,/,divi);
                        break;
*)
             ENDOF
             DIV +U: OF ZZZ
(*

                        identity(r,l,U,u,1);
                        if (r->op == CNST+U && r->u.v.u == 0)
                                break;
                        if (r->op == CNST+U && (n = ispow2(r->u.v.u)) != 0)
                                return simplify(RSH, ty, l, cnsttree(inttype, (long)n));
                        foldcnst(U,u,/);
                        break;
*)
             ENDOF
             EQ +F: OF ZZZ
(*

                        cfoldcnst(F,d,==);
                        commute(r,l);
                        break;
*)
             ENDOF
             EQ +U: OF ZZZ
(*

                        cfoldcnst(U,u,==);
                        commute(r,l);
                        zerofield(EQ,U,u);
                        break;
*)
             ENDOF
             GE +F: OF ZZZ
(*
  cfoldcnst(F,d,>=); break;
*)
             ENDOF
             GE +I: OF 
(*
  cfoldcnst(I,i,>=); break;
*)
             ENDOF
             GE +U: OF ZZZ
(*

                        geu(l,r,1);     /* l >= 0 => (l,1) */
                        cfoldcnst(U,u,>=);
                        if (l->op == CNST+U && l->u.v.u == 0)   /* 0 >= r => r == 0 */
                                return eqtree(EQ, r, l);
                        break;
*)
             ENDOF
             GT +F: OF ZZZ
(*
  cfoldcnst(F,d, >); break;
*)
             ENDOF
             GT +I: OF
             ENDOF
             GT +U: OF ZZZ
(*

                        geu(r,l,0);     /* 0 > r => (r,0) */
                        cfoldcnst(U,u, >);
                        if (r->op == CNST+U && r->u.v.u == 0)   /* l > 0 => l != 0 */
                                return eqtree(NE, l, r);
                        break;
*)
             ENDOF
             LE +F: OF ZZZ
(*
  cfoldcnst(F,d,<=); break;
*)
             ENDOF
             LE +I: OF
(*
  cfoldcnst(I,i,<=); break;
*)
             ENDOF
             LE +U: OF ZZZ
(*

                        geu(r,l,1);     /* 0 <= r => (r,1) */
                        cfoldcnst(U,u,<=);
                        if (r->op == CNST+U && r->u.v.u == 0)   /* l <= 0 => l == 0 */
                                return eqtree(EQ, l, r);
                        break;
*)
             ENDOF
             LSH +I: OF ZZZ
(*

                        identity(r,l,I,i,0);
                        if (l->op == CNST+I && r->op == CNST+I
                        && r->u.v.i >= 0 && r->u.v.i < 8*l->type->size
                        && muli(l->u.v.i, 1<<r->u.v.i, ty->u.sym->u.limits.min.i, ty->u.sym->u.limits.max.i, needconst))
                                return cnsttree(ty, (long)(l->u.v.i<<r->u.v.i));
                        if (r->op == CNST+I && (r->u.v.i >= 8*ty->size || r->u.v.i < 0)) {
                                warning("shifting an `%t' by %d bits is undefined\n", ty, r->u.v.i);
                                break;
                        }

                        break;
*)
             ENDOF
             LSH +U: OF ZZZ
(*

                        identity(r,l,I,i,0);
                        sfoldcnst(<<);
                        if (r->op == CNST+I && (r->u.v.i >= 8*ty->size || r->u.v.i < 0)) {
                                warning("shifting an `%t' by %d bits is undefined\n", ty, r->u.v.i);
                                break;
                        }

                        break;

*)
             ENDOF
             LT +F: OF ZZZ
(*
  cfoldcnst(F,d, <); break;
*)
             ENDOF
             LT +I: OF ENDOF
             LT +U: OF ZZZ
(*

                        geu(l,r,0);     /* l < 0 => (l,0) */
                        cfoldcnst(U,u, <);
                        if (l->op == CNST+U && l->u.v.u == 0)   /* 0 < r => r != 0 */
                                return eqtree(NE, r, l);
                        break;
*)
             ENDOF
             $MOD +I: OF
(*

                        if (r->op == CNST+I && r->u.v.i == 1)   /* l%1 => (l,0) */
                                return tree(RIGHT, ty, root(l), cnsttree(ty, 0L));
                        if (r->op == CNST+I && r->u.v.i == 0
                        ||  l->op == CNST+I && l->u.v.i == ty->u.sym->u.limits.min.i
                        &&  r->op == CNST+I && r->u.v.i == -1)
                                break;
                        xfoldcnst(I,i,%,divi);
                        break;
*)
             ENDOF
             $MOD +U: OF ZZZ
(*

                        if (r->op == CNST+U && ispow2(r->u.v.u)) /* l%2^n => l&(2^n-1) */
                                return bittree(BAND, l, cnsttree(ty, r->u.v.u - 1));
                        if (r->op == CNST+U && r->u.v.u == 0)
                                break;
                        foldcnst(U,u,%);
                        break;
*)
             ENDOF
             MUL +F: OF 
(*

                        xfoldcnst(F,d,*,muld);
                        commute(l,r);
                        break;
*)
             ENDOF
             MUL +I: OF \ ZZZ
(*

                        commute(l,r);
                        xfoldcnst(I,i,*,muli);
                        if (l->op == CNST+I && r->op == ADD+I && r->kids[1]->op == CNST+I)
                                /* c1*(x + c2) => c1*x + c1*c2 */
                                return simplify(ADD, ty, simplify(MUL, ty, l, r->kids[0]),
                                        simplify(MUL, ty, l, r->kids[1]));
                        if (l->op == CNST+I && r->op == SUB+I && r->kids[1]->op == CNST+I)
                                /* c1*(x - c2) => c1*x - c1*c2 */
                                return simplify(SUB, ty, simplify(MUL, ty, l, r->kids[0]),
                                        simplify(MUL, ty, l, r->kids[1]));
                        if (l->op == CNST+I && l->u.v.i > 0 && (n = ispow2(l->u.v.i)) != 0)
                                /* 2^n * r => r<<n */
                                return simplify(LSH, ty, r, cnsttree(inttype, (long)n));
                        identity(r,l,I,i,1);
                        break;
*)
             ENDOF
             NE +F: OF ZZZ
(*

                        cfoldcnst(F,d,!=);
                        commute(r,l);
                        break;
*)
             ENDOF
             NE +U: OF ZZZ
(*

                        cfoldcnst(U,u,!=);
                        commute(r,l);
                        zerofield(NE,U,u);
                        break;
*)
             ENDOF
             NEG +F: OF ZZZ
(*

                        ufoldcnst(F,cnsttree(ty, -l->u.v.d));
                        idempotent(NEG+F);
                        break;
*)
             ENDOF
             NEG +I: OF 
(*

                        if (l->op == CNST+I) {
                                if (needconst && l->u.v.i == ty->u.sym->u.limits.min.i)
                                        warning("overflow in constant expression\n");
                                if (needconst || l->u.v.i != ty->u.sym->u.limits.min.i)
                                        return cnsttree(ty, -l->u.v.i);
                        }
                        idempotent(NEG+I);
                        break;
*)
             ENDOF
             $NOT +I: OF ZZZ
(*

                        op = NOT;
                        ufoldcnst(I,cnsttree(ty, !l->u.v.i));
                        break;
*)
             ENDOF
             RSH +I: OF ZZZ
(*

                        identity(r,l,I,i,0);
                        if (l->op == CNST+I && r->op == CNST+I
                        && r->u.v.i >= 0 && r->u.v.i < 8*l->type->size) {
                                long n = l->u.v.i>>r->u.v.i;
                                if (l->u.v.i < 0)
                                        n |= ~0UL<<(8*l->type->size - r->u.v.i);
                                return cnsttree(ty, n);
                        }
                        if (r->op == CNST+I && (r->u.v.i >= 8*ty->size || r->u.v.i < 0)) {
                                warning("shifting an `%t' by %d bits is undefined\n", ty, r->u.v.i);
                                break;
                        }

                        break;
*)
             ENDOF
             RSH +U: OF ZZZ
(*

                        identity(r,l,I,i,0);
                        sfoldcnst(>>);
                        if (r->op == CNST+I && (r->u.v.i >= 8*ty->size || r->u.v.i < 0)) {
                                warning("shifting an `%t' by %d bits is undefined\n", ty, r->u.v.i);
                                break;
                        }

                        break;
*)
             ENDOF
             SUB +F: OF
(*

                        xfoldcnst(F,d,-,subd);
                        break;
*)
             ENDOF
             SUB +I: OF   ENDOF
             SUB +U: OF   ENDOF
             SUB +P: OF ZZZ
(*

                        if (l->op == CNST+P && r->op == CNST+P)
                                return cnsttree(ty, (long)((char *)l->u.v.p - (char *)r->u.v.p));
                        if (r->op == CNST+I || r->op == CNST+U)
                                return simplify(ADD, ty, l,
                                        cnsttree(inttype, r->op == CNST+I ? -r->u.v. i : -(long)r->u.v.u));
                        if (isaddrop(l->op) && r->op == ADD+I && r->kids[1]->op == CNST+I)
                                /* l - (x + c) => l-c - x */
                                return simplify(SUB, ty,
                                        simplify(SUB, ty, l, r->kids[1]), r->kids[0]);
                        break;
*)             ENDOF
               0 assert
              ENDCASE
              op  opname COUNT TOEVALBL
              op ty l r tree
; TO simplify

: constexpr ( tok -- p )
        needconst 1+ TO needconst
         expr1
        needconst 1- TO needconst
;

: intexpr { tok  n \ p -- n }
         tok  constexpr TO p
        needconst 1+ TO needconst
         p -}op @ CNST +I: =
         p -}op @ CNST +U: =  OR
        IF     p inttype cast tree.v 2@ D>S TO n
        ELSE  TRUE ABORT" integer expression must be constant\n"
        THEN
        needconst 1+ TO needconst
        n
;

\EOF
#include "c.h"
#include <float.h>

static char rcsid[] = "$Id: simp.nw,v 2.18 1998/07/02 00:23:05 drh Exp $";

#define foldcnst(TYPE,VAR,OP) \
        if (l->op == CNST+TYPE && r->op == CNST+TYPE) \
                return cnsttree(ty, l->u.v.VAR OP r->u.v.VAR)
#define commute(L,R) \
        if (generic(R->op) == CNST && generic(L->op) != CNST) \
                do { Tree t = L; L = R; R = t; } while(0)
#define xcvtcnst(FTYPE,SRC,DST,VAR,EXPR) \
        if (l->op == CNST+FTYPE) do {\
                if (!explicitCast\
                &&  ((SRC) < DST->u.sym->u.limits.min.VAR || (SRC) > DST->u.sym->u.limits.max.VAR))\
                        warning("overflow in converting constant expression from `%t' to `%t'\n", l->type, DST);\
                if (needconst\
                || !((SRC) < DST->u.sym->u.limits.min.VAR || (SRC) > DST->u.sym->u.limits.max.VAR))\
                        return cnsttree(ty, (EXPR)); } while(0)
#define identity(X,Y,TYPE,VAR,VAL) \
        if (X->op == CNST+TYPE && X->u.v.VAR == VAL) return Y
#define zerofield(OP,TYPE,VAR) \
        if (l->op == $FIELD \
        &&  r->op == CNST+TYPE && r->u.v.VAR == 0)\
                return eqtree(OP, bittree(BAND, l->kids[0],\
                        cnsttree(unsignedtype, \
                                (unsigned long)fieldmask(l->u.field)<<fieldright(l->u.field))), r)
#define cfoldcnst(TYPE,VAR,OP) \
        if (l->op == CNST+TYPE && r->op == CNST+TYPE) \
                return cnsttree(inttype, (long)(l->u.v.VAR OP r->u.v.VAR))
#define foldaddp(L,R,RTYPE,VAR) \
        if (L->op == CNST+P && R->op == CNST+RTYPE) { \
                Tree e = tree(CNST+P, ty, NULL, NULL);\
                e->u.v.p = (char *)L->u.v.p + R->u.v.VAR;\
                return e; }
#define ufoldcnst(TYPE,EXP) if (l->op == CNST+TYPE) return EXP
#define sfoldcnst(OP) \
        if (l->op == CNST+U && r->op == CNST+I \
        && r->u.v.i >= 0 && r->u.v.i < 8*l->type->size) \
                return cnsttree(ty, (unsigned long)(l->u.v.u OP r->u.v.i))
#define geu(L,R,V) \
        if (R->op == CNST+U && R->u.v.u == 0) do { \
                warning("result of unsigned comparison is constant\n"); \
                return tree(RIGHT, inttype, root(L), cnsttree(inttype, (long)(V))); } while(0)
#define idempotent(OP) if (l->op == OP) return l->kids[0]

int needconst;
int explicitCast;
static int addi(long x, long y, long min, long max, int needconst) {
        int cond = x == 0 || y == 0
        || x < 0 && y < 0 && x >= min - y
        || x < 0 && y > 0
        || x > 0 && y < 0
        || x > 0 && y > 0 && x <= max - y;
        if (!cond && needconst) {
                warning("overflow in constant expression\n");
                cond = 1;
        }
        return cond;


}

static int addd(double x, double y, double min, double max, int needconst) {
        int cond = x == 0 || y == 0
        || x < 0 && y < 0 && x >= min - y
        || x < 0 && y > 0
        || x > 0 && y < 0
        || x > 0 && y > 0 && x <= max - y;
        if (!cond && needconst) {
                warning("overflow in constant expression\n");
                cond = 1;
        }
        return cond;


}

static Tree addrtree(Tree e, long n, Type ty) {
        Symbol p = e->u.sym, q;

        if (p->scope  == GLOBAL
        ||  p->sclass == STATIC || p->sclass == EXTERN)
                NEW0(q, PERM);
        else
                NEW0(q, FUNC);
        q->name = stringd(genlabel(1));
        q->sclass = p->sclass;
        q->scope = p->scope;
        assert(isptr(ty) || isarray(ty));
        q->type = isptr(ty) ? ty->type : ty;
        q->temporary = p->temporary;
        q->generated = p->generated;
        q->addressed = p->addressed;
        q->computed = 1;
        q->defined = 1;
        q->ref = 1;
        if (p->scope  == GLOBAL
        ||  p->sclass == STATIC || p->sclass == EXTERN) {
                if (p->sclass == AUTO)
                        q->sclass = STATIC;
                (*IR->address)(q, p, n);
        } else {
                Code cp;
                addlocal(p);
                cp = code(Address);
                cp->u.addr.sym = q;
                cp->u.addr.base = p;
                cp->u.addr.offset = n;
        }
        e = tree(e->op, ty, NULL, NULL);
        e->u.sym = q;
        return e;
}

/* div[id] - return 1 if min <= x/y <= max, 0 otherwise */
static int divi(long x, long y, long min, long max, int needconst) {
        int cond = y != 0 && !(x == min && y == -1);
        if (!cond && needconst) {
                warning("overflow in constant expression\n");
                cond = 1;
        }
        return cond;


}

static int divd(double x, double y, double min, double max, int needconst) {
        int cond;

        if (x < 0) x = -x;
        if (y < 0) y = -y;
        cond = y != 0 && !(y < 1 && x > max*y);
        if (!cond && needconst) {
                warning("overflow in constant expression\n");
                cond = 1;
        }
        return cond;

}

/* mul[id] - return 1 if min <= x*y <= max, 0 otherwise */
static int muli(long x, long y, long min, long max, int needconst) {
        int cond = x > -1 && x <= 1 || y > -1 && y <= 1
        || x < 0 && y < 0 && -x <= max/-y
        || x < 0 && y > 0 &&  x >= min/y
        || x > 0 && y < 0 &&  y >= min/x
        || x > 0 && y > 0 &&  x <= max/y;
        if (!cond && needconst) {
                warning("overflow in constant expression\n");
                cond = 1;
        }
        return cond;


}

static int muld(double x, double y, double min, double max, int needconst) {
        int cond = x >= -1 && x <= 1 || y >= -1 && y <= 1
        || x < 0 && y < 0 && -x <= max/-y
        || x < 0 && y > 0 &&  x >= min/y
        || x > 0 && y < 0 &&  y >= min/x
        || x > 0 && y > 0 &&  x <= max/y;
        if (!cond && needconst) {
                warning("overflow in constant expression\n");
                cond = 1;
        }
        return cond;


}
/* sub[id] - return 1 if min <= x-y <= max, 0 otherwise */
static int subi(long x, long y, long min, long max, int needconst) {
        return addi(x, -y, min, max, needconst);
}

static int subd(double x, double y, double min, double max, int needconst) {
        return addd(x, -y, min, max, needconst);
}

/* ispow2 - if u > 1 && u == 2^n, return n, otherwise return 0 */
int ispow2(unsigned long u) {
        int n;

        if (u > 1 && (u&(u-1)) == 0)
                for (n = 0; u; u >>= 1, n++)
                        if (u&1)
                                return n;
        return 0;
}

