
0 VALUE freenodes


\ length - # elements in list
: length { Llist \ n lp -- n }
        0 TO n
        Llist
        IF      Llist TO lp
                BEGIN n 1+ TO n
                 lp lp->link @  DUP TO lp
                 Llist =
                UNTIL
        THEN
         n
;

\ ltov - convert list to an NULL-terminated vector allocated in arena
: ltov { Llist arena \ array lp i -- array }
        0 TO i
><CDP   HERE TO array  Llist @ length  1+ CELLS ALLOT
><CDP    Llist @
         IF
                Llist  @ TO lp
                BEGIN
                        lp lp->link @ TO lp
                        lp lp->x @ array i []!
                        i 1+ TO i
                 lp  Llist @ =
                UNTIL
                0 Llist @ !
        THEN
        0 array i []!
          array
;

: append { x  Llist \ new -- new }
        freenodes DUP TO new
        IF  freenodes lp->link @ TO freenodes
        ELSE ><CDP HERE TO new  \list  ALLOT ><CDP
        THEN
        Llist
        IF   Llist lp->link @ new  lp->link !
             new  Llist  lp->link !
        ELSE new    new  lp->link !
        THEN
        x new lp->x !
        new
;
