\ .( CE console ver 0.012 ... )
\ 1001bytes, APR-2000

WINAPI: SendMessageA     USER32.DLL
WINAPI: SetTextColor     GDI32.DLL
WINAPI: SetBkColor       GDI32.DLL
WINAPI: GetKeyState      USER32.DLL

-01 ( TRUE)  VALUE ?CON

     0 VALUE KEY_EVENT_GUI
     0 VALUE START_EVENT
    00 VALUE CON_BUFFER_PREPARED
    00 VALUE tib  \ доп.буфер для ACCEPT
    00 VALUE >in


      00 VALUE  MAINHWND

WINAPI: CreateEventA        KERNEL32.DLL
WINAPI: SetEvent            KERNEL32.DLL
WINAPI: ResetEvent          KERNEL32.DLL
WINAPI: WaitForSingleObject KERNEL32.DLL

HEX
: WAIT_OBJECT_0 0 ;
: WAIT_FAILED  -1 ;
102 CONSTANT WAIT_TIMEOUT
 80 CONSTANT WAIT_ABANDONED
DECIMAL

255 VALUE bSize

: CREATE-AUTOEVENT ( -- handle ior )
\ создает объект event
  0 0 0 0 CreateEventA DUP
  0= IF GetLastError ELSE 0 THEN
;
: SET-EVENT ( handle -- ior )
\ освобождает объект  event
  SetEvent 0= IF GetLastError ELSE 0 THEN
;

: WAIT ( handle timeout -- flag ior )
\ возвращает истину, если объект освобожден другим потоком
\ (либо он освободился сам собой при завершении др.потока)
\ и после этого занят текущим
  SWAP WaitForSingleObject DUP WAIT_FAILED =
  IF GetLastError ELSE DUP WAIT_OBJECT_0 = SWAP WAIT_ABANDONED = OR 0 THEN
;
: INFINITE
 -1  ;

WINAPI: RegisterClassExA USER32.DLL
WINAPI: CreateWindowExA  USER32.DLL
WINAPI: CreateWindowExW  USER32.DLL
WINAPI: GetClassInfoExA  USER32.DLL
WINAPI: UnregisterClassA  USER32.DLL

WINAPI: SetWindowTextA   USER32.DLL
WINAPI: UpdateWindow     USER32.DLL
WINAPI: BeginPaint       USER32.DLL
WINAPI: EndPaint         USER32.DLL
WINAPI: GetClassNameA    USER32.DLL
WINAPI: DefWindowProcA   USER32.DLL
WINAPI: LoadIconA        USER32.DLL
WINAPI: LoadCursorA      USER32.DLL
WINAPI: DrawIcon         USER32.DLL
WINAPI: ShowWindow       USER32.DLL
WINAPI: GetMessageA      USER32.DLL
WINAPI: DispatchMessageA USER32.DLL
WINAPI: TranslateMessage USER32.DLL
WINAPI: GetClientRect    USER32.DLL
WINAPI: InvalidateRect   USER32.DLL
WINAPI: DrawTextA        USER32.DLL
WINAPI: PostQuitMessage  USER32.DLL
WINAPI: GetDC            USER32.DLL
WINAPI: ReleaseDC        USER32.DLL
WINAPI: MessageBoxA      USER32.DLL
WINAPI: GetFocus         USER32.DLL
WINAPI: SetCaretPos      USER32.DLL
WINAPI: CreateCaret      USER32.DLL
WINAPI: ShowCaret        USER32.DLL
WINAPI: HideCaret        USER32.DLL
WINAPI: DestroyCaret     USER32.DLL

\ grafics
WINAPI: SelectObject      GDI32.DLL
WINAPI: TextOutA          GDI32.DLL
WINAPI: GetStockObject    GDI32.DLL
WINAPI: GetTextMetricsA   GDI32.DLL


S" lib\ext\case.f"   INCLUDED  \ оператор case
S" lib\windows\spfgui\DTyps.f"  INCLUDED  \ константы Windows и данные

MODULE: GUI-CONSOLE \ -------------------------------------------


DECIMAL
0 VALUE Myhwnd
100 VALUE cxClient
100 VALUE cyClient
80 VALUE cxBuffer
24 VALUE cyBuffer
0 VALUE cxChar
0 VALUE cyChar
0 VALUE pBuffer
0 VALUE xCaret
0 VALUE yCaret
\ 0 VALUE >in
\ 0 VALUE tib
\ 0 VALUE GO_INTERPRET
0 VALUE ?in

WINAPI: ScrollWindow     USER32.DLL
WINAPI: MoveWindow       USER32.DLL
WINAPI: GetSystemMetrics USER32.DLL

DECIMAL

57 CONSTANT SM_CXMINIMIZED
58 CONSTANT SM_CYMINIMIZED
4  CONSTANT SM_CYCAPTION
6  CONSTANT SM_CYBORDER

\ access to heap allocated 2d array pBuffer
: BUFFER ( y x -- addr )
    SWAP cxBuffer * + pBuffer + ;

: OBJECT  ( длина  -- адр.нач )
  HERE \ len here
  OVER \ len here len
  ALLOT \ len here
  DUP ROT  ERASE CONSTANT ;

\ paint structure
0
4 -- PS.hdc
4 -- PS.fErase
4 -- PS.rcPaint
4 -- PS.fRestore
4 -- PS.fIncUpdate
4 -- PS.rgbReserved
\ 31 +
48 +
CONSTANT /PS


\  WNDCLASS
0
4 -- окна.размер_структ
4 -- окна.стиль
4 -- окна.процедура
4 -- окна.класс+
4 -- окна.окно+
4 -- окна.экземпляр
4 -- окна.икон
4 -- окна.курсор
4 -- окна.фон
4 -- окна.меню
4 -- окна.имя
4 -- окна.икон+
CONSTANT /winclass

0
 4 -- tmHeight
 4 -- tmAscent
 4 -- tmDescent
 4 -- tmInternalLeading
 4 -- tmExternalLeading
 4 -- tmAveCharWidth
 4 -- tmMaxCharWidth
 4 -- tmWeight
 4 -- tmOverhang
 4 -- tmDigitizedAspectX
 4 -- tmDigitizedAspectY
 1 -- tmFirstChar
 1 -- tmLastChar
 1 -- tmDefaultChar
 1 -- tmBreakChar
 1 -- tmItalic
 1 -- tmUnderlined
 1 -- tmStruckOut
 1 -- tmPitchAndFamily
 1 -- tmCharSet
CONSTANT /TEXTMETRIC

0
4 -- MSG.hwnd
4 -- MSG.message
4 -- MSG.wParam
4 -- MSG.lParam
4 -- MSG.time
4 -- MSG.pt
4 -- MSG.ex
CONSTANT /MSG

0
CELL -- par.hwnd
CELL -- par.cxClient
CELL -- par.cyClient
CELL -- par.cyChar
CELL -- par.bKill
CELL -- par.tid
CONSTANT /PARAMS

 /MSG        OBJECT MSG1
 /TEXTMETRIC OBJECT tm
 /winclass   OBJECT ТЕСТ_
 /PS         OBJECT ps
 /PARAMS     OBJECT params

: LOWORD ( lpar -- loword ) 0xFFFF AND ;
: HIWORD ( lpar -- hiword ) 16 RSHIFT ;

\ *\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\***
\ **\\\\\ оконная функция \\\\\\\\\\\\\**
\ ***\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\*
0 VALUE lpar
0 VALUE wpar
0 VALUE msg
0 VALUE hwnd
0 VALUE hdc

:NONAME     ( lpar wpar msg hwnd \ hdc -- )

 TO hwnd TO msg TO wpar TO lpar

   msg CASE

   WM_CREATE OF
      hwnd GetDC TO hdc
      0x4DCD5C 0x2000000 OR hdc SetTextColor DROP
      0 hdc SetBkColor DROP
      SYSTEM_FIXED_FONT GetStockObject  hdc SelectObject  DROP
      tm hdc GetTextMetricsA DROP
      tm tmAveCharWidth @ TO cxChar
      tm tmHeight       @ TO cyChar
      hdc hwnd ReleaseDC DROP
      cxChar cxBuffer 1+ * TO cxClient
      cyChar cyBuffer 1+ * TO cyClient
      0
   ENDOF \ 1

   WM_SIZE OF
    \ obtain window size in pixels
    \   lpar LOWORD TO cxClient
    \   lpar HIWORD TO cyClient
    \ calculate window size in characters
    \   cxClient cxChar / 1 MAX TO cxBuffer
    \   cyClient cyChar / 1- 1 MAX TO cyBuffer
    \ set caret to upper left corner

\     0 TO xCaret
\     0 TO yCaret
     GetFocus hwnd  = IF
          yCaret cyChar *  xCaret cxChar * SetCaretPos DROP
     THEN
     0
   ENDOF    \ 2

   WM_KEYDOWN OF

      wpar CASE

        VK_RETURN OF   \ Enter
             yCaret  0 BUFFER tib  xCaret CMOVE
             xCaret TO >in
             CON_BUFFER_PREPARED SET-EVENT THROW
        ENDOF

        VK_CANCEL OF
             BYE
        ENDOF
                
        [CHAR] C OF
             VK_CONTROL GetKeyState 15 RSHIFT
             IF BYE THEN
        ENDOF
             

     ENDCASE

     0
   ENDOF  \ 5

   WM_CHAR OF

      lpar LOWORD 0
      DO

      wpar CASE

        0x08 OF  \ backspace
           xCaret 0 > IF
              xCaret 1- TO xCaret
           THEN
           cxBuffer 1- xCaret   \
           DO   yCaret I 1+ BUFFER C@ yCaret I BUFFER C!  LOOP
           BL   yCaret cxBuffer 1- BUFFER C!
           hwnd HideCaret DROP
           hwnd GetDC TO  hdc
           SYSTEM_FIXED_FONT GetStockObject hdc  SelectObject  DROP
           cxBuffer xCaret -  yCaret xCaret BUFFER
           yCaret cyChar *  xCaret cxChar * hdc  TextOutA DROP
           cxBuffer yCaret 0 BUFFER yCaret cyChar * 0 hdc TextOutA DROP
           hwnd  ShowCaret DROP
        ENDOF

        0x09 OF  \ tab
           BEGIN
              1  BL WM_CHAR  hwnd SendMessageA DROP
              xCaret 8 MOD 0=
           UNTIL
        ENDOF
        
        0x0A OF
        ENDOF

        0x0D OF  \ line feed
            0 TO xCaret
            yCaret 1+ TO yCaret
            yCaret cyBuffer >
            IF
               cyBuffer  TO yCaret
               1 0 BUFFER
               0 0 BUFFER
               cxBuffer 1+ cyBuffer 1+ * CMOVE
               cyBuffer 0 BUFFER cxBuffer BL FILL
               hwnd UpdateWindow DROP
               0 0 cyChar -1 * 0 hwnd ScrollWindow DROP
               hwnd UpdateWindow DROP
            THEN
        ENDOF

        0x1B OF \  escape
             pBuffer bSize DUP * BL FILL
             0 TO xCaret 0 TO  yCaret
             FALSE 0 hwnd InvalidateRect DROP
        ENDOF

        \ default: character codes
         wpar 0= IF BL TO wpar THEN
         wpar yCaret xCaret  BUFFER C!
         hwnd  HideCaret DROP
         hwnd  GetDC  TO hdc
         SYSTEM_FIXED_FONT GetStockObject hdc SelectObject DROP
         1 yCaret xCaret BUFFER yCaret cyChar * xCaret cxChar * hdc
          TextOutA DROP
         hwnd  ShowCaret DROP
         hdc hwnd  ReleaseDC  DROP
         xCaret 1+ DUP TO xCaret
         cxBuffer =
         IF 0 TO xCaret
             yCaret 1+ DUP TO yCaret
             cyBuffer =
             IF 0 TO yCaret  THEN
         THEN

      ENDCASE
      yCaret cyChar * xCaret cxChar * SetCaretPos DROP
      KEY_EVENT_GUI SetEvent DROP
      LOOP
      0 
   ENDOF

   WM_SETFOCUS OF
   \ create and show the caret
      cyChar cxChar 0 hwnd CreateCaret DROP
      yCaret cyChar *  xCaret cxChar * SetCaretPos DROP
      hwnd ShowCaret DROP
      0
   ENDOF   \ 3

   WM_KILLFOCUS OF
   \ hide and destroy the caret
      hwnd HideCaret DROP
      DestroyCaret DROP
      0
   ENDOF   \ 4

   WM_PAINT  OF
     ps hwnd BeginPaint TO  hdc
     SYSTEM_FIXED_FONT  GetStockObject hdc SelectObject DROP
     cyBuffer 1+ 0 DO
      cxBuffer I 0 BUFFER I cyChar * 0 hdc TextOutA DROP
     LOOP
     ps hwnd EndPaint DROP
     0
   ENDOF


   WM_DESTROY OF
      0 PostQuitMessage
      BYE
   ENDOF
     \ не обработано
     lpar wpar msg hwnd DefWindowProcA   SWAP
 ENDCASE
;

WNDPROC: ConsoleWndProc

DECIMAL

: MessageLoop
  BEGIN
    0 0 0 MSG1 GetMessageA
  WHILE

    MSG1 TranslateMessage DROP
    MSG1 DispatchMessageA DROP

  REPEAT
;

: ACCEPT-GUI ( addr u1 -- u2 )
    CON_BUFFER_PREPARED ResetEvent DROP
    CON_BUFFER_PREPARED INFINITE WAIT THROW DROP
    >in MIN TUCK tib SWAP ROT SWAP CMOVE  
;

: TYPE-GUI ( addr u -- )
    H-STDOUT 0<> IF TYPE1 EXIT THEN \ Если пишем в файл например...
    ANSI><OEM
    OVER + SWAP
    ?DO
       1 I C@ WM_CHAR  Myhwnd SendMessageA DROP
    LOOP
;

: KEY-GUI ( -- u )
    KEY_EVENT_GUI ResetEvent DROP
    KEY_EVENT_GUI INFINITE WAIT THROW DROP
    yCaret xCaret 1- BUFFER C@
;


EXPORT \ ---------------------------------------


: CE-CON-MAIN
  C/L 2+ ALLOCATE THROW TO tib
 \ заполнение структуры
  /winclass                   ТЕСТ_ окна.размер_структ !
  CS_HREDRAW CS_VREDRAW OR
  CS_OWNDC OR  
                              ТЕСТ_ окна.стиль         !
  [']  ConsoleWndProc         ТЕСТ_ окна.процедура     !
  0                           ТЕСТ_ окна.класс+        !
  0                           ТЕСТ_ окна.окно+         !
  HINST                       ТЕСТ_ окна.экземпляр     !
  1 HINST LoadIconA           ТЕСТ_ окна.икон          !
  IDC_ARROW 0 LoadCursorA     ТЕСТ_ окна.курсор        !
  BLACK_BRUSH GetStockObject  ТЕСТ_ окна.фон           !
  0                           ТЕСТ_ окна.меню          !
  S" SP-FORTH 4.0 GUI console" DROP       ТЕСТ_ окна.имя           !
  1 HINST  LoadIconA          ТЕСТ_ окна.икон+         !

  ТЕСТ_  RegisterClassExA  0= ABORT" #Class was not registered!"

  0                             \ параметры создания
  HINST                         \ описатель экземпляра программы
  0                             \ описатель меню
  0                             \ описатель родительского окна
  0 0                           \ window height, width
  0 0                       \ vertical, horizontal position
  WS_OVERLAPPEDWINDOW   \ WS_VISIBLE OR      \ style
                                \ address of window name
  ТЕСТ_     окна.имя  @  DUP    \ address of registered class name
  0                             \ extended window style

  CreateWindowExA
  DUP 0= ABORT" Window not created..."
  TO Myhwnd   Myhwnd TO MAINHWND

  bSize DUP *  ALLOCATE THROW TO pBuffer
  pBuffer 0= IF
       MB_ICONEXCLAMATION  0 S" Out of memory..." DROP
       Myhwnd MessageBoxA DROP
  ELSE
       pBuffer bSize DUP * BL FILL
  THEN

  SM_CYMINIMIZED     GetSystemMetrics
  \ SM_CYBORDER        GetSystemMetrics
  cyClient + 3 + TO cyClient
  0 cyClient cxClient 100 100 hwnd MoveWindow DROP

  Myhwnd UpdateWindow  DROP
  5 Myhwnd ShowWindow  DROP        \ вывести окно на экран
  TITLE
  ." Use ESC to clear the window, Ctrl-c or Ctrl-break to exit" CR 
  START_EVENT SET-EVENT
  MessageLoop                      \ войти в цикл обработки сообщений
;

' CE-CON-MAIN TASK: Thread1

: CECONSOLE
       ['] TYPE-GUI   TO TYPE
       ['] ACCEPT-GUI TO ACCEPT
       ['] KEY-GUI    TO KEY
       ['] OEM>ANSI   TO ANSI><OEM
       CREATE-AUTOEVENT THROW TO CON_BUFFER_PREPARED
       CREATE-AUTOEVENT THROW TO KEY_EVENT_GUI
       CREATE-AUTOEVENT THROW TO START_EVENT
       FALSE TO ?CON 0 TO SOURCE-ID
       START_EVENT ResetEvent DROP
       params Thread1 START  params par.tid !
       START_EVENT INFINITE WAIT THROW DROP
       S" OPTIONS ' SPF-INI ERR-EXIT" DUP #TIB ! \ формируем первоначальный TIB
       TIB SWAP CMOVE >IN 0!
       QUIT
;

;MODULE \ ---------------------------


\ CECONSOLE

   TRUE TO ?GUI
     ' CECONSOLE MAINX !
         S" spf4gui.exe"  SAVE
     BYE
