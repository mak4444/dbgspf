\ KERNEL for   data types independence FORTH

\   STECK OF TYPES

100 CONSTANT   M_DEPTH

VARIABLE  MODELS          \ ADDRESS OF THE TOP ELEMENT

M_DEPTH ALLOT
 HERE       -1 , -1 , -1 , -1 ,  \ WEAK PROTECTION
       VALUE MODELS0          ( INITIAL TYPES STACK )
: INIT_MODELS MODELS0  MODELS ! ;
  INIT_MODELS

C" CODE:" FIND NIP
[IF]
CODE:   _!_                  \ EXCHANGE OF STACKS
     SUB     EBP  # 4
     MOV     DWORD^ { EBP }  EAX
        XCHG { MODELS }  EBP
     MOV     EAX  DWORD^ { EBP }
     ADD     EBP  # 4
        RET
;CODE
[ELSE]
: _!_  MODELS @ SP@ CELL+ MODELS ! SP! ;
[THEN]

\  EXECUTED ON A STACK OF MODELSS OF A WORD

: M_POP   >R _!_ R>     _!_    ;
: M_PUSH     _!_ >R     _!_ R> ;
: M_DUP_PUSH _!_ DUP >R  _!_ R> ;
: M_DROP     _!_ DROP   _!_    ;
: M_DUP      _!_ DUP    _!_    ;

: [M_]       _!_  R> DUP CELL+ >R @ EXECUTE _!_ ;

: M_         \  THE WORD FOLLOWING BY IT WILL BE EXECUTED
             \  ON A STACK OF MODELSS.
             STATE @
             IF      POSTPONE [M_] ' ,
             ELSE  _!_ ' EXECUTE _!_
             THEN
;  IMMEDIATE

VARIABLE LAST_CLASS

:   CLASS:  \ DEFINES A CLASS,
            \ WHICH IS THE NUMBER OR DISPLACEMENT
            \ IN A  ARRAY  OF  THE REFERENCES ON METHODS
           LAST_CLASS @ CONSTANT
      CELL LAST_CLASS +! ;

: M_ERR  INIT_MODELS
 TRUE ABORT" THE COMMAND DOES NOT CORRESPOND TO A TYPE" ;

: M_:   \  DETERMINANT OF A SENDING
        CREATE  DOES>  M_PUSH   20 CELLS UMIN +  @ EXECUTE ;

: [M_ERR]  INIT_MODELS
 TRUE ABORT" NONEXISTENT TYPE" ;

: M_;   40 0 DO ['] [M_ERR] , LOOP ;


