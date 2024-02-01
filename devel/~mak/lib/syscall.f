WINAPI: CreateProcessA KERNEL32.DLL
WINAPI: WaitForSingleObject KERNEL32.DLL

0
4 -- cb
4 -- lpReserved
4 -- lpDesktop
4 -- lpTitle
4 -- dwX
4 -- dwY
4 -- dwXSize
4 -- dwYSize
4 -- dwXCountChars
4 -- dwYCountChars
4 -- dwFillAttribute
4 -- dwFlags
2 -- wShowWindow
2 -- cbReserved2
4 -- lpReserved2
4 -- hStdInput
4 -- hStdOutput
4 -- hStdError
CONSTANT /STARTUPINFO

HEX 00000100 CONSTANT STARTF_USESTDHANDLES DECIMAL
CREATE STARTUPINFO /STARTUPINFO ALLOT STARTUPINFO /STARTUPINFO ERASE

/STARTUPINFO STARTUPINFO cb !
STARTF_USESTDHANDLES STARTUPINFO dwFlags !
H-STDIN STARTUPINFO hStdInput !
H-STDOUT STARTUPINFO hStdOutput !
H-STDERR STARTUPINFO hStdError !

: SYSCALL0 ( S" application.exe" -- flag )
  OVER + 0 SWAP C! >R
  PAD \ process information
  STARTUPINFO \ startup info
  0    \ current dir
  0    \ environment
  0    \ creation flags
  FALSE \ inherit handles
  0 0  \ process & thread security
  R>   \ command line
  0    \ application
  CreateProcessA
DROP \ DUP	  IF -1 HERE @ WaitForSingleObject DROP THEN
;


\ EOF


