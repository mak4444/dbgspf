( ��������� ���������� ���������� [������� �� ����, ���������
  �� ������������ �������, � �.�.] - ����� �������� � THROW.
  Copyright [C] 1992-1999 A.Cherezov ac@forth.org
  C������� 1999
)

   H-STDIN  VALUE  H-STDIN    \ ����� ����� - ������������ �����
   H-STDOUT VALUE  H-STDOUT   \ ����� ����� - ������������ ������
   H-STDERR VALUE  H-STDERR   \ ����� ����� - ������������ ������ ������
          0 VALUE  H-STDLOG


USER EXC-HANDLER  \ ���������� ���������� (������������� � �����������)
VECT <EXC-DUMP>

: (EXC) ( DispatcherContext ContextRecord EstablisherFrame ExceptionRecord -- flag )
  (ENTER) \ ����� ��� ����� ������
  0 FS@ @ \ ����� ������ ������ ��������� ���������� ��� ����� �������� �������
  DUP 0 FS! \ ��������������� ��� ����� ���������� ������ exceptions � �������
  CELL+ CELL+ @ TlsIndex! \ ��������� �� USER-������ ����������� ������

\  2DROP 2DROP
\  0 (LEAVE)               \ ��� ���� ����� �������� ��������� ����

  DUP <EXC-DUMP>

  HANDLER @ 0=
  IF \ ���������� � ������, ��� CATCH, ������ ����� � ��������� (~day)
     DESTROY-HEAP
     -1 ExitThread
  THEN

  FINIT \ ���� float ����������, ���������������

  @ THROW  \ ���������� ���������� � ������ �������� :)
  R> DROP   \ ���� ��� �� ���������, �� �������� ������� �� callback
;

: DROP-EXC-HANDLER
  R> 0 FS! RDROP RDROP
;
: SET-EXC-HANDLER
  R> R>
  TlsIndex@ >R
  ['] (EXC) >R
  0 FS@ >R
  RP@ 0 FS!
  RP@ EXC-HANDLER !
  ['] DROP-EXC-HANDLER >R \ ������������� ����� ����� ��������.����������
  >R >R
;
' SET-EXC-HANDLER (TO) <SET-EXC-HANDLER>

: AT-PROCESS-FINISHING ( -- ) ... ;

: HALT ( ERRNUM -> ) \ ����� � ����� ������
  AT-PROCESS-FINISHING
  ExitProcess
;
