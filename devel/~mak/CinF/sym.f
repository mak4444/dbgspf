
256 CONSTANT HASHSIZE
0
CELL FIELD -}level
CELL FIELD -}previous
        0
        symbol FIELD -}esym
        CELL FIELD -}link
        CONSTANT  tentry
HASHSIZE CELLS FIELD -}buckets
CELL FIELD -}all
CONSTANT table

CREATE cns table ALLOT  cns table ERASE  CONSTANTS cns !
CREATE ext table ALLOT  ext table ERASE  GLOBAL ext !
CREATE ids table ALLOT  ids table ERASE  GLOBAL ids !
CREATE tys table ALLOT  tys table ERASE  GLOBAL tys !

CREATE constants   cns ,
CREATE externals   ext ,
CREATE identifiers ids ,
CREATE globals     ids ,
CREATE types       tys ,
VARIABLE labels
GLOBAL VALUE level
0   VALUE tempid

: exitscope
        level rmtypes
        types @ -}level @  level  =
        IF types @ -}previous @  types !
        THEN
        identifiers @ -}level @  level =
        IF       Aflag  2 >=
                IF ZZZ
(*
                        int n = 0;
                        Symbol p;
                        for (p = identifiers->all; p && p->scope == level; p = p->up)
                                if (++n > 127) {
                                        warning("more than 127 identifiers declared in a block\n");
                                        break;
                                }
*)
                THEN
                 identifiers @ -}previous @ identifiers !
        THEN
        level GLOBAL >= assert
        level 1- TO  level
;

: lookup { name tp \ p h --  &p->sym }
        name 1+ C@ HASHSIZE 1- AND TO h
        tp assert
        BEGIN     tp -}buckets h [] TO p
             BEGIN  p
             WHILE  name COUNT p  -}esym -}name  @ COUNT COMPARE 0=
                   IF p -}esym  EXIT
                   THEN
                   p -}link @ TO p
             REPEAT
                  tp -}previous  @ DUP TO tp 0=
        UNTIL 0
;

: table() { tp  level \ new -- new }
><CDP   HERE TO new table ALLOT new table ERASE
><CDP   tp     new -}previous !
        level  new -}level !
        tp
        IF     tp -}all @ new -}all !
        THEN  new
;

: install { name tpp level arena \ p tp h -- &p->sym }
        tpp @ TO tp
        name 1+ @ HASHSIZE 1- AND TO h

        level 0=  level  tp -}level @ >= OR assert
        level 0>  tp -}level @ level < AND
        IF  tp level table() DUP TO tp tpp !
        THEN
><CDP   HERE TO p tentry ALLOT p tentry ERASE
><CDP   name  p -}esym -}name !
        level p -}esym -}scope !
        tp -}all @ p -}esym -}up !
        p -}esym tp -}all !
        tp -}buckets h [] p -}link !
        p tp -}buckets h []!
        p -}esym
;

: enterscope ( -- )
        level 1+ DUP TO level LOCAL =
        IF 0 TO tempid
        THEN
;

: foreach { tp lev apply cl \ p -- }
         tp assert
         BEGIN  tp DUP IF DROP  tp  -}level @  lev  > THEN

         WHILE  tp -}previous @ TO tp
         REPEAT
         tp DUP IF -}level @ lev  = THEN
         IF     tp -}all @ TO p
                BEGIN  p DUP IF -}scope @  lev = THEN
                WHILE  p cl  apply  EXECUTE
                       p -}up @ TO p
                REPEAT
         THEN
;


1 VALUE label

: genlabel ( n -- n1 )
   label TUCK + TO label
;

: findlabel { lab \  p h --  &sym }
        lab HASHSIZE 1- AND TO h

        labels @ -}buckets h [] TO p
        BEGIN  p
        WHILE      p -}llabel @ lab =
                IF p -}esym EXIT
                THEN
             p -}link @ TO p
        REPEAT

><CDP   HERE TO p tentry ALLOT p tentry ERASE
><CDP   lab stringd  p -}esym -}name !
        LABELS p -}esym -}scope !
        labels @ -}all @  p -}esym -}up !
        p -}esym labels @ -}all !
        labels @ -}buckets h []  p -}link !
        p labels @ -}buckets h []!
        1 p -}esym -}generated !
        lab  p -}esym -}llabel !
        p -}esym defsymbol
        p -}esym
;


: constant {  ty v \ p h -- Sym }
        v  @ 1+ C@ HASHSIZE 1- AND  TO h
        ty  unqual TO ty
        constants @  -}buckets h []  TO p
        BEGIN  p
        WHILE  ty p  -}esym -}stype @  1  eqtype
                IF  ty -}op @ ." C=" DUP . KEY DROP

                        CASE
                         INT       OF  v 2@ D>S p -}esym -}v 2@ D>S  =
                                         IF p -}esym EXIT
                                         THEN            ENDOF

                         UNSIGNED  OF  v 2@ D>S p -}esym -}v 2@ D>S ZZZ =
                                         IF p -}esym EXIT
                                         THEN            ENDOF
                         FLOT      OF ZZZ
(*                          v F@ F>S p -}esym  -}v F@ F. F. ZZZ F=
                                         IF p -}esym EXIT
                                         THEN
*)                                          ENDOF

                         FUNCTION  OF   v @  p -}esym -}v @ ZZZ =
                                         IF p -}esym EXIT
                                         THEN            ENDOF

                          ARRAY    OF   v @  p -}esym -}v @ ZZZ =
                                         IF p -}esym EXIT
                                         THEN            ENDOF

                         POINTER   OF   v @  p -}esym -}v @ ZZZ =
                                         IF p -}esym EXIT
                                         THEN            ENDOF
                                     0    assert
                                ENDCASE
                THEN
             p -}link @ TO p
        REPEAT
><CDP   HERE TO p tentry ALLOT p tentry ERASE
        HERE p -}esym -}name !
           v 2@ <# #S #>  S",
><CDP   CONSTANTS  p -}esym -}scope !
         ty        p -}esym -}stype !
        STATIC     p -}esym -}sclass !
        v F@ p -}esym -}v F!
        constants @ -}buckets h [] p -}link !
        constants @ -}all @ p -}esym -}up !
        p -}esym constants @ -}all !
        p constants @ -}buckets h  []!
\        S" CR .( ASDFDFG) " EVALUATE
        ty -}u.sym @
        DUP IF  -}addressed @ 0= THEN
        IF   p -}esym defsymbol
        THEN
        1 p -}esym -}defined !
        p -}esym
;

: genident { scls  ty  lev \ p --  p }
><CDP   HERE TO p  symbol ALLOT p symbol ERASE
><CDP
        1 genlabel   stringd  p -}name !
        lev   p -}scope !
        scls p -}sclass !
        ty p -}stype !
        1  p -}generated !
        lev  GLOBAL =
        IF p defsymbol
        THEN
        p
;

