\ Ядро float библиотеки
\ Слова низкого уровня

WARNING @
WARNING 0!

BASE @
HEX

: F>T  [
 83 C, ED C, 0C C,   \       SUB     EBP , # 12
 DB C, 7D C, 00 C,   \       FSTP    EXTENDED 0 [EBP]
 C3 C,  ] ;

:  2F>T  [
 83 C, ED C, 18  C,    \   SUB     EBP , # 24
 DB C, 7D C, 0C  C,    \   FSTP    EXTENDED C [EBP]
 DB C, 7D C, 00  C,    \   FSTP    EXTENDED 0 [EBP]
 C3 C, ] ;  \           RET     NEAR

:  T>F   [
 DB C, 6D C, 00 C,   \       FLD     EXTENDED 0 [EBP]
 83 C, C5 C, 0C C,   \       ADD     EBP , # 12
 C3 C, ] ;  \           RET     NEAR

:  2T>F  [
 DB C, 6D C, 0C C,   \       FLD     EXTENDED C [EBP]
 DB C, 6D C, 00 C,   \       FLD     EXTENDED 0 [EBP]
 83 C, C5 C, 18 C,   \       ADD     EBP , # 24
 C3 C, ] ;  \           RET     NEAR

:  3T>F  [
 DB C, 6D C, 18 C,   \       FLD     EXTENDED 18 [EBP]
 DB C, 6D C, 0C C,   \       FLD     EXTENDED C [EBP]
 DB C, 6D C, 00 C,   \       FLD     EXTENDED 0 [EBP]
 83 C, C5 C, 24 C,   \       ADD     EBP , # 36
 C3 C, ] ;  \           RET     NEAR


:  2.E   2.E_  F>T ;

: 10.E  10.E_  F>T ;

: FPI   FPI_   F>T ;

: FLG2  FLG2_  F>T ;

: FLN2  FLN2_  F>T ;

: FL2T  FL2T_  F>T ;

: FL2E  FL2E_  F>T ;

:   .E    .E_  F>T ;

:  1.E   1.E_  F>T ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ Operations
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: FACOS   T>F FACOS_  F>T ;

: F0=      T>F F0=_ ;

: F0<      T>F F0<_ ;

: F=       2T>F F=_ ;

: F<       2T>F F<_ ;

: F>       T>F T>F F<_ ;

: FMAX     2T>F FMAX_  F>T ;

: FMIN     2T>F FMIN_  F>T ;

: FNEGATE  T>F FNEGATE_ F>T ;

: FCOS     T>F FCOS_  F>T ;

: FSIN     T>F FSIN_  F>T ;

: FSINCOS  2T>F FSINCOS_  2F>T ;

: FABS     2T>F FABS_  F>T ;

: F*       2T>F F*_    F>T ;

: F+       2T>F F+_    F>T ;

: F-       2T>F F-_    F>T ;

: F/       2T>F F/_    F>T ;

: FSQRT    T>F  FSQRT_    F>T ;

: FINT    T>F  FINT_  F>T ;

: FDROP NIP 2DROP  ; \ 3DROP

: FDUP    T>F  FDUP_ 2F>T ;

: FOVER  [
 DB C, 6D C, 0C C,   \       FLD     EXTENDED C [EBP]
 83 C, ED C, 0C C,   \       SUB     EBP , # 12
 DB C, 7D C, 00 C,   \       FSTP    EXTENDED 0 [EBP]
 C3 C, ] ;  \           RET     NEAR

:  FSWAP  [
 DB C, 6D C, 00 C,   \       FLD     EXTENDED 0 [EBP]
 DB C, 6D C, 0C C,   \       FLD     EXTENDED C [EBP]
 DB C, 7D C, 00 C,   \       FSTP    EXTENDED 0 [EBP]
 DB C, 7D C, 0C C,   \       FSTP    EXTENDED C [EBP]
 C3 C, ] ;  \           RET     NEAR
:  FROT   [
 DB C, 6D C, 00 C,   \       FLD     EXTENDED 0 [EBP]
 DB C, 6D C, 0C C,   \       FLD     EXTENDED C [EBP]
 DB C, 6D C, 18 C,   \       FLD     EXTENDED 18 [EBP]
 DB C, 7D C, 00 C,   \       FSTP    EXTENDED 0 [EBP]
 DB C, 7D C, 18 C,   \       FSTP    EXTENDED 18 [EBP]
 DB C, 7D C, 0C C,   \       FSTP    EXTENDED C [EBP]
 C3 C, ] ;  \           RET     NEAR


: F1+    T>F  F1+_  F>T ;

: D>F    D>F_  F>T ;

: F>D    T>F  F>D_ ;

\ Extention WORD^s

: DF!   >R  T>F R> DF!_  ;

: DF@   DF@_  F>T ;

: F!   >R  T>F R> F!_  ;

: F@   F@_  F>T ;

: SF!   >R  T>F R> SF!_  ;

: SF@   SF@_  F>T ;

: FLN    T>F  FLN_   F>T ;

: FLNP1  T>F  FLNP1_ F>T ;

: FLOG   T>F  FLOG_ F>T ;

\ e^(x) = 2^(x * LOG{2}e)

: FEXP   T>F  FEXP_   F>T ;

: FEXPM1 T>F  FEXPM1_ F>T ;

: F**    2T>F  F**_ F>T ;

: FTAN   T>F  FTAN_  F>T ;

: FATAN  T>F  FATAN_ F>T ;

: FATAN2  2T>F FATAN2_ F>T ;

: FACOS  T>F  FACOS_ F>T ;

: FASIN  T>F  FASIN_ F>T ;

\ My WORD^s

: F>DEG  T>F  F>DEG_ F>T ;

: F>RAD  T>F  F>RAD_ F>T ;


: DS>F  DS>F_ F>T ;

: F>DS  T>F F>DS_ ;

: F--DS  T>F F--DS_ ;

: F10X   >R T>F R> F10X_ F>T ;

: FLOG2 ( F: r1 -- r2 )
       T>F FLOG2_ F>T ;

: F[LOG] ( F: r1 -- F: r2 )
       T>F F[LOG]_ F>T ;

: F10*   \ *
       T>F F10*_ F>T ;
: F10/   \ *
       T>F F10*_ F>T ;

: FD<
      R> T>F R> FD<_ R> F>T R> ;

: FD<   
      R> T>F R> FD>_ R> F>T R> ;

: F**2
       T>F F**2_ F>T ;

: _FLIT-CODE8   [
 5B C,   \             POP     EBX
 DD C, 03 C,   \           FLD     DOUBLE [EBX]
 83 C, ED C, 0C C,   \         SUB     EBP , # 12
 DB C, 7D C, 00 C,   \         FSTP    EXTENDED 0 [EBP]
 83 C, C3 C, 08 C,   \         ADD     EBX , # 8
 FF C, E3 C, ] ;  \           JMP     EBX


: _FLIT-CODE10  [
 5B C,   \           POP     EBX
 DB C, 2B C,   \         FLD     EXTENDED [EBX]
 83 C, ED C, 0C C,   \       SUB     EBP , # 12
 DB C, 7D C, 00 C,   \       FSTP    EXTENDED 0 [EBP]
 83 C, C3 C, 0A C,   \       ADD     EBX , # 10
 FF C, E3 C, ] ;  \         JMP     EBX


: FACOSH   FDUP FDUP F* 1.E F- FSQRT F+ FLN ;

 BASE !
WARNING !

