\ ядро float библиотеки
\ —лова низкого уровн€

REQUIRE ?OE lib\include\float2.f

WARNING @
WARNING 0!

USER-CREATE FLOAT-TIB ( -- c-addr ) \ 94 CORE EXT
 \ c-addr - адрес области дл€ формировани€ строкового представлени€ float 
 \ числа 
256 USER-ALLOT


\ Constants
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: 2.E_  2.E ;

: 10.E_ 10.E ;

:  FPI_ FPI ;

:  FLG2_ FLG2 ;

:  FLN2_ FLN2 ;

: FL2T_ FL2T ;

: FL2E_ FL2E ;

: .E_ .E ;

: 1.E_ 1.E ;

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\ Operations
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: FACOS_ FACOS ;

: F0=_ F0= ;

: F0<_ F0< ;

: F<_ F< ;

: F=_ F= ;

: FMAX_ FMAX ;

: FMIN_ FMIN ;

: FNEGATE_ FNEGATE ;

: FCOS_ FCOS ;

: FSIN_ FSIN ;

: FSINCOS_ FSINCOS ;

: FABS_ FABS ;

: F*_ F* ;

: F+_ F+ ;

: F-_ F- ;

: F/_ F/ ;

: FSQRT_ FSQRT ;

: FDROP_ FDROP ;

: FDUP_ FDUP ;

: FOVER_ FOVER ;

: FINT_ FINT ;

: FSWAP_ FSWAP ;

: FROT_ FROT ;

: FDEPTH_ FDEPTH ;

\ —тек завернут в кольцо!!!
\ Ќумераци€ регистров в сопроцессоре видимо следующа€ (experimental)
\        0 7 6 5 4 3 2 1  где 0 - дно стека
\ FSTSW возвращает номер регистра
\ ѕришлось это обработать...

: F1+_ F1+ ;

: D>F_ D>F ;

: F>D_ F>D ;

: DF!_ DF! ;

: DF@_ DF@ ;

: F!_ F! ;

: FLOAT>DATA_ FLOAT>DATA ;

: DATA>FLOAT_ DATA>FLOAT ;

: F@_ F@ ;

: SF!_ SF! ;

: SF@_ SF@ ;

: FLN_ FLN ;

: FLNP1_ FLNP1 ;

: FLOG_ FLOG ;

\ e^(x) = 2^(x * LOG{2}e)

: FEXP_ FEXP ;

: FEXPM1_ FEXPM1 ;

: F**_ F** ;

: FTAN_ FTAN ;

: FATAN_ FATAN ;

: FATAN2_ FATAN2 ;

: FACOS_ FACOS ;

: FASIN_ FASIN ;

\ My WORD^s

: F>DEG_ F>DEG ;

: F>RAD_ F>RAD ;

: SETFPUCW_ SETFPUCW ;

: GETFPUCW_ GETFPUCW ;

: DS>F_ DS>F ;

: F>DS_ F>DS ;

: F--DS_ F--DS ;

: GETFPUSW_ GETFPUSW ; 

: ?OE_ ?OE ;

: ?IE_ ?IE ;

: ?ZE_ ?ZE ;

: FLOG2_ FLOG2 ;

: F[LOG]_ F[LOG] ;

: TRUNC-MODE_ TRUNC-MODE ;
: ROUND-MODE_ ROUND-MODE ;
: UP-MODE_ UP-MODE ;
: LOW-MODE_ LOW-MODE ;
: F10*_ F10* ;
: F10/_ F10/ ;
: F10X_ ( u -- R: 10^u )
   1.E
   DUP 0 > IF    0 DO F10*_ LOOP
           ELSE  NEGATE 0 ?DO F10/_ LOOP
           THEN
;
: F>ENV_ F>ENV ;
: FENV>_ FENV> ;
: FSAVE_ FSAVE ;
: FRSTOR_ FRSTOR ;
: FD<_ FD< ;
: FD>_ FD> ;
: `F1+_ `F1+ ;
: F**2_ F**2 ;
: `F1-_ `F1- ;
: _FLIT-CODE8_ _FLIT-CODE8 ;
: _FLIT-CODE10_ _FLIT-CODE10 ;

WARNING !
