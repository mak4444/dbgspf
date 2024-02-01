\ ����������� ������ CASE
\ � ������ ��������� ����������� ���������� CASE


DECIMAL
VARIABLE   CSP    \ ��������� ����� ��������
6 CONSTANT L-CAS# \ ���������� ������� �����������
CREATE     S-CSP   L-CAS# CELLS ALLOT \ ���� ��������
S-CSP CSP !

: +CSP ( -> P)    \ �������� �������
  CSP @ DUP CELL+ CSP !
;
: -CSP ( -> )     \ ������ �������
  CSP @ 1 CELLS - CSP !
;

: !CSP ( -> )     \ ���������������� �������
  SP@ +CSP !
;

: CSP@ ( -> A)
  CSP @ 1 CELLS - @
;
: ?CSP ( -> )     \ ��������� ������������� �����
  SP@ CSP@ <> 37 ?ERROR ( ABORT" ���� ����� �� CSP !")
  -CSP
;
: CASE ( -> )
  !CSP
; IMMEDIATE
: OF
  POSTPONE OVER POSTPONE =
  [COMPILE] IF POSTPONE DROP
; IMMEDIATE
: ENDOF
  [COMPILE] ELSE
; IMMEDIATE
: ENDCASE
  POSTPONE DROP 
BEGIN
 SP@
 CSP@
 =
[ 0 TO TTTT ]
  0=  WHILE  [COMPILE] THEN  REPEAT -CSP
[ 1 TO TTTT ]
; IMMEDIATE

\EOF

 SP@
55EA20 8945FC		MOV     FC [EBP] , EAX 
55EA23 8D6DFC		LEA     EBP , FC [EBP] 
55EA26 8BC5		MOV     EAX , EBP 
 CSP@
55EA28 8945FC		MOV     FC [EBP] , EAX 
55EA2B 8B40FC		MOV     EAX , FC [EAX] 
55EA2E 3345FC		XOR     EAX , FC [EBP] 
55EA31 8B4500		MOV     EAX , 0 [EBP] 
55EA34 8D6D04		LEA     EBP , 4 [EBP] 
[ 0 TO TTTT ]
