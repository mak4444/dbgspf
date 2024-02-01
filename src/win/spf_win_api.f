( ��������� ������� ���������� ��� WINAPI � WNDPROC
  Windows-��������� �����.
  Copyright [C] 1992-1999 A.Cherezov ac@forth.org
  �������������� �� 16-���������� � 32-��������� ��� - 1995-96��
  ������� - �������� 1999
)


VARIABLE AOLL
VARIABLE AOGPA
0 VALUE ST-RES

CODE AO_INI

      PUSH EBX
      PUSH EDI
      PUSH EBP
      MOV  EAX, 4 [EBX]
      PUSH EAX
      A; 0xA1 C,  AddrOfLoadLibrary
      ALSO FORTH , PREVIOUS \   MOV  EAX, AddrOfLoadLibrary
A; HERE 4 - ' AOLL EXECUTE !
      CALL EAX
      OR   EAX, EAX
      POP EBP
      POP EDI
      POP EBX
      JZ  SHORT @@2

      PUSH EDI
      PUSH EBP
      MOV  ECX, 8 [EBX]
      PUSH ECX
      PUSH EAX
      A; 0xA1 C,  AddrOfGetProcAddress
      ALSO FORTH , PREVIOUS \    MOV  EAX, AddrOfGetProcAddress
A; HERE 4 - ' AOGPA EXECUTE !
      CALL EAX
      OR   EAX, EAX
      POP EBP
      POP EDI

@@2:	RET
END-CODE


CODE _WINAPI-CODE

      POP  EBX
      MOV  -4 [EBP], EAX
      MOV  EAX, [EBX]
      OR   EAX, EAX
      LEA  EBP, -4 [EBP]
      JNZ  SHORT @@1
      CALL  ' AO_INI
      JZ  SHORT @@3
      MOV [EBX], EAX
 
@@1:  MOV  ECX, 12 [EBX]
      OR   ECX, ECX
      JZ   SHORT @@2
      LEA  EBX, [ECX*4]
      SUB  ESP, EBX
      MOV  EDX, EDI
      MOV  EDI, ESP
      MOV  ESI, EBP
      CLD
      REP MOVS DWORD
      ADD  EBP, EBX
      MOV  EDI, EDX
      CALL EAX
      RET

@@2:  PUSH EDI
      PUSH EBP
      SUB  ESP, # 64
      MOV  EDI, ESP
      MOV  ESI, EBP
      MOV  ECX, # 16
      CLD
      REP MOVS DWORD
      MOV  EBP, ESP
      CALL EAX
      MOV  ECX, ESP
      SUB  ECX, EBP
      MOV  ESP, EBP
      ADD  ESP, # 64
      POP  EBP
      ADD  EBP, ECX
      SAR  ECX, # 2
      MOV  12 [EBX], ECX
      POP  EDI

@@3:  RET

END-CODE

' _WINAPI-CODE TO WINAPI-CODE

CODE _RWIN-CODE

     POP     EBX 
     POP     EDX 
     MOV     EAX, [EBX] 
     OR      EAX, EAX 
     MOV     -4 [EBP], EDX 
     JNZ  SHORT @@1

     CALL    ' AO_INI

     JZ  SHORT @@2

     MOV     [EBX], EAX 

@@1: MOV     -8 [EBP], EDI 
     CALL    EAX 
     MOV     EDI, -8 [EBP] 
@@2: JMP     -4 [EBP]

END-CODE

' _RWIN-CODE TO RWIN-CODE


CODE _0WIN-CODE

     POP  EBX 

     MOV  -4 [EBP], EAX
     MOV  EAX, [EBX]
     LEA  EBP, -4 [EBP]

     OR   EAX, EAX 
     JNZ  SHORT @@1

     CALL    ' AO_INI

     JZ  SHORT @@2

     MOV     [EBX], EAX 

@@1: MOV     -8 [EBP], EDI 
     CALL    EAX 
     MOV     EDI, -8 [EBP] 
@@2: RET

END-CODE

' _0WIN-CODE TO 0WIN-CODE


CODE _1WIN-CODE

     POP  EBX

     PUSH  EAX

     MOV  EAX, [EBX] 

     OR   EAX, EAX 
     JNZ  SHORT @@1

     CALL    ' AO_INI

     JZ  SHORT @@2

     MOV     [EBX], EAX 

@@1: MOV     -8 [EBP], EDI 
     CALL    EAX 
     MOV     EDI, -8 [EBP] 
@@2: RET

END-CODE

' _1WIN-CODE TO 1WIN-CODE

CODE API-CALL ( ... extern-addr -- x )
\ ����� ������� ������� (API ��� ������ ������� ����� COM)

      PUSH EDI
      PUSH EBP
      SUB  ESP, # 64
      MOV  EDI, ESP
      MOV  ESI, EBP
      MOV  ECX, # 16
      CLD
      REP MOVS DWORD
      MOV  EBP, ESP
      CALL EAX
      MOV  EBX, EBP
      SUB  EBX, ESP
      MOV  ESP, EBP
      ADD  ESP, # 64
      POP EBP
      SUB EBP, EBX
      POP EDI
      RET
END-CODE

CODE _WNDPROC-CODE
     MOV  EAX, ESP
     SUB  ESP, # 3968
A;   HERE 4 - ' ST-RES 9 + EXECUTE
     PUSH EBP
     MOV  EBP, 4 [EAX] ( ����� �������� �� CALLBACK )
     PUSH EBP
     MOV  EBP, EAX
     ADD  EBP, # 12
     PUSH EBX
     PUSH ECX
     PUSH EDX
     PUSH ESI
     PUSH EDI
     MOV  EAX, [EAX] ( ����� ������ ����-��������� )
     MOV  EBX, [EAX]
     MOV  EAX, -4 [EBP]
     CALL EBX
     LEA  EBP, -4 [EBP]
     MOV  [EBP], EAX
     POP  EDI
     POP  ESI
     POP  EDX
     POP  ECX
     POP  EBX
     MOV  EAX, ESP
     MOV  ESP, EBP
     MOV  EBP, 4 [EAX] \ ����������� EBP
     MOV  EAX, [EAX]   \ ����� �������� �� CALLBACK
     XCHG EAX, [ESP]
     RET
END-CODE


' _WNDPROC-CODE TO WNDPROC-CODE

VECT FORTH-INSTANCE>  \ ��� ��������� ����� ����������� �� �����
VECT <FORTH-INSTANCE  \ � ������ � WNDPROC-��������� (������������� TlsIndex)

' FORTH-INSTANCE> TO TC-FORTH-INSTANCE>
' <FORTH-INSTANCE TO TC-<FORTH-INSTANCE

