\ $Id: spf4wc.f,v 1.4 2003/10/08 13:28:40 anfilat Exp $
\ Andrey Filatkin, af@forth.org.ru

 0xF TO MM_SIZE

REQUIRE [IF]         ~mak\CompIF3.f 
REQUIRE [DEFINED] ~nn\lib\ifdef.f 
REQUIRE =OF    ~mak\~yz\lib\mycase.f 
REQUIRE CODE	lib\ext\spf-asm.f
REQUIRE ((	~af\lib\c\prefixfun.f
REQUIRE AT	~af\lib\var.f
REQUIRE {	~mak\locals4.f
REQUIRE USES	~mak\~af\lib\api-func.f
REQUIRE USES_C	~af\lib\c\capi-func.f
REQUIRE FStream	~af\lib\stream_io.f
REQUIRE WINCONST	lib\win\const.f
REQUIRE GetIniString	~mak\~af\lib\ini.f
REQUIRE SaveTlsIndex	~af\lib\QuickWNDPROC.f
REQUIRE MENUITEM	~mak\~af\lib\menu.f
REQUIRE STRUCT:	~af\lib\struct-t.f
REQUIRE :M	~mak\~af\lib\nwordlist.f
REQUIRE #define	~af\lib\c\define.f
REQUIRE WAIT	wait.f
REQUIRE SAVE> ~MAK\LIB\save2.f 


[UNDEFINED] vocLocalsSupport
[IF] : vocLocalsSupport vocLocalsSupport_M  ;
[THEN]
REQUIRE INCLUDED-STRINGS	~af\lib\langstrings.f
REQUIRE CASE-INS	lib\ext\caseins.f
0 VALUE MAIN_HWND

CASE-INS OFF

 REQUIRE  Z" ~mak/~yz/lib/common.f
S" adds.f"	INCLUDED
REQUIRE AddToRFL	~af\lib\rfl.f


REQUIRE STR@	~ac\lib\str2.f
REQUIRE CASE	lib\ext\case.f
REQUIRE Z\"	~mak\~af\lib\c\zstring.f
0 VALUE MMTST

DECIMAL

ALSO WINCONST

MODULE: GUI-CONSOLE_

S" spf4wc.h.f"	INCLUDED  \ constants & data

;MODULE

REMOVE-ALL-CONSTANTS PREVIOUS


CASE-INS ON

 S" spf4MM.exe" S" " ~mak\~af\lib\save.f

\ [THEN]

\ S" spf4mc.exe" S" spf4wc.fres " ~mak\~af\lib\save.f
\ S" spf4pw.exe" SAVE
 BYE
