\ Добавление к DECODE-ERROR

WINAPI: FormatMessageA   KERNEL32.DLL
WINAPI: SetLastError     KERNEL32.DLL

1 CONSTANT WIN_ERROR

..: DECODE-ERROR ( n u -- c-addr u )
     DUP WIN_ERROR =
     IF DROP
        >R 0 1024 PAD 0 
        R> 0 0x1000
        FormatMessageA PAD SWAP
        ANSI>OEM
        EXIT
     THEN
;..

\ Example
(
' ANSI>OEM TO ANSI><OEM
GetLastError WIN_ERROR   DECODE-ERROR TYPE
0xC00000AAL  FORTH_ERROR DECODE-ERROR TYPE
)