( »спользуемые €дром функции Windows.
  Copyright [C] 1992-1999 A.Cherezov ac@forth.org
  –евизи€ - сент€брь 1999
)

  1WIN: GetStdHandle                  KERNEL32.DLL
  0WIN: GetLastError                  KERNEL32.DLL
WINAPI: GetFileAttributesA            KERNEL32.DLL
  1WIN: CloseHandle                   KERNEL32.DLL
WINAPI: CreateFileA                   KERNEL32.DLL
  1WIN: DeleteFileA                   KERNEL32.DLL
WINAPI: SetFilePointer                KERNEL32.DLL
WINAPI: GetFileSize                   KERNEL32.DLL
WINAPI: ReadFile                      KERNEL32.DLL
WINAPI: WriteFile                     KERNEL32.DLL
  1WIN: SetEndOfFile                  KERNEL32.DLL
  1WIN: ExitProcess                   KERNEL32.DLL
  1WIN: ExitThread                    KERNEL32.DLL     
WINAPI: GetNumberOfConsoleInputEvents KERNEL32.DLL
WINAPI: ReadConsoleInputA             KERNEL32.DLL
  0WIN: GetProcessHeap                KERNEL32.DLL
WINAPI: HeapCreate                    KERNEL32.DLL
  1WIN: HeapDestroy                   KERNEL32.DLL
WINAPI: HeapAlloc                     KERNEL32.DLL
WINAPI: HeapFree                      KERNEL32.DLL
WINAPI: HeapReAlloc                   KERNEL32.DLL
  0WIN: GetCommandLineA               KERNEL32.DLL
  1WIN: LoadLibraryA                  KERNEL32.DLL
WINAPI: GetProcAddress                KERNEL32.DLL
WINAPI: CharToOemBuffA                USER32.DLL
WINAPI: OemToCharBuffA                USER32.DLL
WINAPI: GetModuleFileNameA            KERNEL32.DLL
WINAPI: GetEnvironmentVariableA       KERNEL32.DLL
WINAPI: CreateThread                  KERNEL32.DLL
  1WIN: SuspendThread                 KERNEL32.DLL
  1WIN: ResumeThread                  KERNEL32.DLL
WINAPI: TerminateThread               KERNEL32.DLL
  1WIN: Sleep                         KERNEL32.DLL
  1WIN: FlushFileBuffers              KERNEL32.DLL
WINAPI: GetCurrentDirectoryA          KERNEL32.DLL