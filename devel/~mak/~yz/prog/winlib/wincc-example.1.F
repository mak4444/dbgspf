DIS-OPT \ ��� SPF ���� 4.10 ��� NT
REQUIRE toolbar ~yz/lib/wincc.f
SET-OPT 

0 VALUE t
0 VALUE hd
0 VALUE tv
0 VALUE lv
0 VALUE old-grid

CREATE grids 3 CELLS ALLOT

: newtab { tab -- }
  old-grid ?DUP IF hide-grid THEN
  grids tab CELLS@ DUP >R TO old-grid
  10 40 390 360 R@ map-grid
  R> show-grid ;

PROC: change-tab
  t -selected@ newtab
PROC;

WINAPI: ImageList_Create      COMCTL32.DLL
WINAPI: ImageList_ReplaceIcon COMCTL32.DLL
WINAPI: ImageList_GetImageCount COMCTL32.DLL

: create-il ( size -- il )
  >R 5 5 W: ilc_color8 R> DUP ImageList_Create ;
: add-icon ( resno il -- ) 
  >R IMAGE-BASE LoadIconA -1 R> ImageList_ReplaceIcon DROP ;

WINAPI: FindFirstFileA KERNEL32.DLL
WINAPI: FindNextFileA  KERNEL32.DLL
WINAPI: FindClose      KERNEL32.DLL

: fill-listview { \ no fh [ 400 ] fdata }
  32 create-il DUP 1 SWAP add-icon W: lvsil_normal lv -imagelist!
  16 create-il DUP 1 SWAP add-icon W: lvsil_small  lv -imagelist!
  " ��� �����" 0 0 lv add-column
  " ������" 1 1 lv add-column
  fdata " *.f" FindFirstFileA TO fh
  BEGIN
    fdata 11 CELLS + 0 0 lv add-item
    \ ��������� ����� � ����� ������� ��������� ������������� �� ��������, 
    \ ����� �������� ��������������: ������ ���������� ������ ��� ����� ��������
    \ ����� ���� ���������� ���������� param � ������������ lv-param>i
    fdata 8 CELLS@ S>D <# 0 HOLD #S #> DROP 0 1 lv -isubitem!
  fdata fh FindNextFileA 0= UNTIL
  fh FindClose DROP ;

: make-grids 

  \ ������� 1 ===========================
  GRID
  GRID; DUP grids ! FALSE SWAP winmain add-grid-to-window

  \ ������� 2 ==========================
  GRID
($    0 header DUP TO hd -xspan -yfixed |
    ===
    (* tvs_haslines tvs_linesatroot tvs_hasbuttons *) treeview DUP TO tv -xspan -yspan | 
    0 calendar -xfixed |
$)
  GRID; DUP grids 1 CELLS! FALSE SWAP winmain add-grid-to-window
($  \ 

  0 " ���" none 0 0 hd add-item
  0 " �������" none 0 1 hd add-item
  0 " ��������" none 0 2 hd add-item
  90 0 hd -iwidth!
  100 1 hd -iwidth!
  100 2 hd -iwidth!
  \ 
  16 create-il DUP 1 SWAP add-icon 0 tv -imagelist!
  " yz" 0 0 0 W: tvi_first 0 tv add-item >R
  " prog" 0 0 0 W: tvi_first R> tv add-item >R
  " winlib" 0 0 0 W: tvi_first R> tv add-item >R
  " winlib-example" 0 0 0 W: tvi_last R@ tv add-item
  " winctl-example" 0 0 0 W: tvi_last R@ tv add-item
  " wincc-example" 0 0 0 W: tvi_last R> tv add-item
$)
  \ ������� 3 ========================
  GRID
    0 listview DUP TO lv -xspan -yspan |
  GRID; DUP grids 2 CELLS! FALSE SWAP winmain add-grid-to-window
  fill-listview ;

: make-tabs
  0 tabcontrol TO t
  16 create-il DUP 1 SWAP add-icon 0 t -imagelist!
  " ������" 0 0 0 t add-item
  " ������" 0 0 1 t add-item
  " ������" 0 0 2 t add-item
  change-tab t -command!
  400 400 t ctlresize
  5 5 t place ;

PROC: tbutt
  " ������ �� ������ ������������!" msg
PROC;

PROC: mode1  lv icon-view       PROC;
PROC: mode2  lv smallicon-view  PROC;
PROC: mode3  lv list-view       PROC;
PROC: mode4  lv report-view     PROC;

: make-toolbar 
  0 winmain create-toolbar
  W: idb_view_small_color winmain -toolbar@ add-std-bitmap DROP
  none 0 W: btns_button mode1 0 winmain -toolbar@ add-item
  none 1 W: btns_button mode2 1 winmain -toolbar@ add-item
  none 2 W: btns_button mode3 2 winmain -toolbar@ add-item
  none 3 W: btns_button mode4 3 winmain -toolbar@ add-item
  winmain -toolbar@ separate
  none 4 W: btns_wholedropdown tbutt 4 winmain -toolbar@ add-item
  none 5 W: btns_button tbutt 5 winmain -toolbar@ add-item
;

: run
  WINDOWS...
  0 dialog-window TO winmain
  " ����� �������� ����������" winmain -text!
  0 create-tooltip
  make-toolbar
  make-tabs
  make-grids
  410 410 winmain winresize
  winmain wincenter
  winmain winshow
  0 TO old-grid
  0 newtab
  ...WINDOWS
  grids @ del-grid
  grids 1 CELLS@ del-grid
  grids 2 CELLS@ del-grid
  BYE
;

\ 0 TO SPF-INIT?
\ ' ANSI>OEM TO ANSI><OEM
\ TRUE TO ?GUI
\ ' run MAINX !
\ S" winc�-example.exe" SAVE  
run
BYE
