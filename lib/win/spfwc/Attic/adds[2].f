\ ������祭�� �������⥫��� ���

S" lib\ext\spf-asm.f" INCLUDED
S" lib\ext\disasm.f"  INCLUDED
REQUIRE VOCS          lib\ext\vocs.f
REQUIRE F.            lib\include\float2.f
REQUIRE NextFrom     ~af\lib\4interp.f
REQUIRE (*           ~af\lib\comments.f
REQUIRE [[           ~yz\lib\automate.f

: SLITERAL2 \ ������ �⠭���⭮�� SLITERAL -
\ � ०��� ������樨 ��ப� ��७����� � 
\ ���������� ������. ��� �।᪠�㥬��, �� ������ ��窠 �����.
  STATE @ IF
    ['] _SLITERAL-CODE COMPILE,
    DUP C,
    HERE SWAP DUP ALLOT MOVE 0 C,
  ELSE
    TUCK HEAP-COPY SWAP
  THEN
; IMMEDIATE
' SLITERAL2 ' SLITERAL REPLACE-WORD


FALSE WARNING !
MDSW: FOREACH FOREACH
MDSW; NEXT NEXT
TRUE WARNING !

: >_double ( -- d ; f: f -- )
\ ���� �᫮ � ����⢥����� �⥪� � ������ ��� �� �⥪ ��ࠬ��஢
\ � COM-ᮢ���⨬�� ����
  FLOAT>DATA SWAP
;
: _double> ( d -- ; f: -- f )
  SWAP DATA>FLOAT
;
