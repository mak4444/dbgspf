REQUIRE E> samples\~mak\WIN\EDIT\ed1.f
: EDMAIN
  CONSOLE \ N++_ERR_SET
 ['] EDIT_ERROR TO ERROR
 CC_INIT 0!
 GUI-EDIT-INIT ;

' EDMAIN  MAINX !

 CC_INIT 0!

0 TO MAIN_HWND
0 TO hRFLMenu

ALSO GUI-CONSOLE

 TBWndProc
 TBhwnd
 edhwnd
 clhwnd
 SBhwnd
 USBhwnd
splithwnd
Myhwnd
MainMenu
hFileMenu
hFavoritesMenu
hOptionsMenu
CurFocus
hAccel
hFont

0 TO SBhwnd
0 TO clhwnd
0 TO edhwnd
0 TO TBhwnd
0 TO TBWndProc
0 TO USBhwnd
0 TO splithwnd
0 TO Myhwnd
0 TO MainMenu
0 TO hFileMenu
0 TO hFavoritesMenu
0 TO hOptionsMenu
0 TO CurFocus
0 TO hAccel
0 TO hFont

S" RRR.EXE" MSAVE

 TO hFont
 TO hAccel
 TO CurFocus
 TO hOptionsMenu
 TO hFavoritesMenu
 TO hFileMenu
 TO MainMenu
 TO Myhwnd
 TO splithwnd
 TO USBhwnd
 TO SBhwnd
 TO clhwnd
 TO edhwnd
 TO TBhwnd
 TO TBWndProc
PREVIOUS

