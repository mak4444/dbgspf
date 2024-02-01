~mak\OptTr.f
~mak\dop.f 

HEX
 : QQQQ
 [ ' DoDTST TO DTST ]
  $30 + DROP
 ;


SEE QQQQ

\ see test

\EOF

ROOT_SET
WRITE-ROOT
FORMATG:
G:MOUNT

S" QWERTY" S" UUU.TXT" SAVE-FILE

S" SSSS" MAKE-DIR
\ S" UUU.TXT" >FFM PAD .>* R/O_ *OP-FILE 

 COPY UUU.TXT UU.TXT

\EOF
S" FORMAT.F" R/W PF_OPEN . . \EOF
~mak\ARM9\PROTOOS\RUN\DISK\disk.f \EOF
HERE 99 2008 #R-FILE >PF >DPF >DPF
' #R-FILE H.


C:\Program Files\Notepad++\notepad++.exe -lnormal -n77 D:\masm32\asmspf\src\macroopt.f
\ S" C:\Program Files\Notepad++\notepad++.exe -lnormal -n77 D:\masm32\asmspf\src\macroopt.f"  SYSCALL0 .
 S" notepad++" SYSCALL0 .
\ S" notepad++ -lnormal -n77 D:\masm32\asmspf\src\macroopt.f" SYSCALL0 .
\EOF

\EOF
