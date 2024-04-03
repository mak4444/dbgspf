\ 40 30 20 10 3 ROLL .

 12 5  M1_{ WRD WRD }  /  DUP . { WRD }RETURN .( = ) .  CR
 12.0e 5.0e  M1_{ FLT FLT }  /  DUP .  { FLT }RETURN .( = ) F. CR


M1_{ }
  F-PL{ 5 + 6 } .
  VAR_I AA :=  0x40 ;
  VAR_F YY :=  4.5e ;
  VAR_F[] D[ 0x80 ]
  VAR_I[] C[ 0x80 ]
RET


: HH M1_{ } F-PL{ AA + 6 } { WRD }RETURN . ;
  HH

: SS   M1_{ }
        IF{ AA < YY }   YY  @  6 / YY !
        ELSE            YY := YY + 50  ;
        ENDIF  YY @ .
       { }RETURN ;
  SS
: MM
        M1_{ }
        BEGIN ." ZZ "  YY := YY - 1 ;
        UNTIL{  AA > YY }
         YY := YY + 6 ; YY @ .
        { }RETURN ;
  MM
: UU
        M1_{ }
        BEGIN ." ZZ "        YY := YY - 4 ;
        WHILE{  AA < YY }    YY := YY - 1 ;
        REPEAT  YY := YY + 60 ;
        { }RETURN ;
  UU

: VV M1_{ } 
  C[ 5 ] :=  4 ;
  C[ 0 ] :=  4 ;
  D[ 4 ] :=  5 ;
  D[ 1 ] :=  4 + D[ C[ 0 ] ] ;
  D[ 1 ] @ .
     RET ;

 VV
\EOF

SEE HH
SEE SS   
SEE MM
SEE UU


