REQUIRE CASE ~mak\case.f
\ REQUIRE INCLUDED_L ~mak\listing2.f

~mak\CinF\CinF.F 

: DOT . ;
_C_
{

int TST(int zz)
{ int xx=-7;
  do{ DOT(xx); } while(xx++);
  for(;;) 
    { if(zz<0) break;
        DOT(zz--);
    }  return zz;
};

typedef struct ztype {
	unsigned uix;
	unsigned char ucx;
	int ix;
        char cx;
 	struct ztype *xt;
} ztype;

int TST1()
{ return sizeof(ztype);
};

}
 5 TST . 
 TST1 .

: XXXX
  5
 F7_ED
 TST
 . 
;
CR .( XXXX - debug test - F7 ) CR
