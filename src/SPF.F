\ $Id: spf.f,v 1.28 2009/01/21 17:09:35 ygreks Exp $

420 CONSTANT SPF-KERNEL-VERSION

WARNING 0! \ чтобы не было сообщений isn't unique

: _FLIT-CODE10 ;
: _FLIT-CODE8 ;

\ S" lib\ext\disasm.f"             INCLUDED

WARNING 0! 

: PARSE-NAME NextWord ;
: UMIN 2DUP U< IF DROP EXIT THEN NIP ;

REQUIRE [IF] ~mak/CompIF1.f
REQUIRE [IFNDEF] ~nn\lib\ifdef.f
REQUIRE CASE  ~mak\case.f
S" lib/ext/spf-asm.f"            INCLUDED
S" lib/include/tools.f"          INCLUDED
S" src/spf_compileoptions.f"     INCLUDED

ALSO ASSEMBLER DEFINITIONS
PREVIOUS DEFINITIONS

: CS-DUP 2DUP ;

' NOOP CONSTANT 'MNOOP

C" M_WL" FIND NIP 0=
[IF] : M_WL  CS-DUP POSTPONE WHILE ; IMMEDIATE
[THEN]

C" LAST-HERE" FIND NIP 0= VALUE INLINEVAR


: ," ( addr u -- )
    DUP C, CHARS HERE OVER ALLOT
    SWAP CMOVE 0 C, ;

512 1024 * TO IMAGE-SIZE
0x8050000 CONSTANT IMAGE-START 

0 VALUE .forth
0 VALUE .forth#


S" src/spf_date.f"                INCLUDED
S" src/spf_xmlhelp.f"             INCLUDED
S" src/tc_spf.F"                  INCLUDED

WARNING 0! \ чтобы не было сообщений isn't unique

\ ==============================================================
\ Начало двоичного образа Форт-системы
\ в начале команда CALL подпрограммы инициализации.
\ Возврата из подпрограммы не будет - адрес на стеке
\ возвратов может использоваться для fixups.


HERE  DUP HEX .( Base address of the image 0x) U.
HERE TC-CALL,

\ ==============================================================
\ Основные низкоуровневые слова Форта,
\ независимые от операционной системы
0x20 TO MM_SIZE
\ HERE 0x79C80 + DUP TC-YDP0 ! TC-YDP ! 1 TO YDP_FL
\ HERE 0x89C80 + DUP TC-YDP0 ! TC-YDP ! 1 TO YDP_FL
HERE 1024 1024 * 4 * + DUP TC-YDP0 ! TC-YDP ! 1 TO YDP_FL
S" src/spf_defkern.f"                INCLUDED
S" src/spf_forthproc.f"              INCLUDED
S" src/spf_floatkern.f"              INCLUDED
DBG\ VECT DBG_DO
DBG\  ' DBG_DO  TO  'DBG_DO  
DBG\  ' 2DROP  (TO) DBG_DO
S" src/spf_forthproc_hl.f"           INCLUDED
\ ==============================================================
\ Вектора, значения которых будут определено позже

VECT TYPE

\ ==============================================================
\ Средства вызова функций Win32 и импорт
\ функций Windows, используемых ядром SP-Forth

\ Средства вызова внешних динамических библиотек
\ и константы ОС

S" src/win/spf_win_api.f"            INCLUDED
S" src/win/spf_win_proc.f"           INCLUDED
S" src/win/spf_win_const.f"          INCLUDED

\ ==============================================================
\ Управление памятью

S" src/win/spf_win_memory.f"         INCLUDED

\ ==============================================================
\ Структурированная обработка исключений (см.также init)

S" src/spf_except.f"                 INCLUDED
S" src/win/spf_win_except.f"         INCLUDED

\ ==============================================================
\ Файловый и консольный ввод-вывод (OC-зависимые)

S" src\win\spf_win_io.f"             INCLUDED
S" src\win\spf_win_conv.f"           INCLUDED

S" src/spf_con_io.f"                 INCLUDED

\ ==============================================================
\ Печать чисел
\ Имя модуля.

S" src/spf_print.f"                  INCLUDED
S" src/spf_module.f"                 INCLUDED

\ ==============================================================
\ Парсер исходного текста форт-программ
S" src/compiler/spf_parser.f"        INCLUDED
S" src/compiler/spf_read_source.f"   INCLUDED

\ ==============================================================
\ Компиляция чисел и строк в словарь.
\ Создание словарных статей.
\ Поиск слов в словарях.
\ Печать словарей.
\ Слова, к-е нельзя инлайнить.

S" src/compiler/spf_nonopt.f"        INCLUDED
S" src/compiler/spf_compile0.f"      INCLUDED

: [>T]  ; IMMEDIATE
:  >T   ; IMMEDIATE
\  Макроподстановщик-оптимизатор
TRUE TO INLINEVAR
BUILD-OPTIMIZER [IF]
S" src/macroopt.f"                   INCLUDED
[ELSE]
S" src/noopt.f"                      INCLUDED
[THEN]
M\ ' DROP ' DTST TC-VECT!

S" src/compiler/spf_compile.f"       INCLUDED
S" src/compiler/spf_wordlist.f"      INCLUDED
S" src/compiler/spf_find.f"          INCLUDED
S" src/compiler/spf_words.f"         INCLUDED


\ ==============================================================
\ Трансляция исходных текстов.
\ Обработка ошибок.
\ Определяющие слова.
\ Числовые литералы.
\ Управление компиляцией.
\ Компиляция управляющих структур.
\ Работа с модулями

S" src\compiler\spf_error.f"         INCLUDED
S" devel\~mak\place.f"               INCLUDED
S" src\compiler\spf_navigator.f"     INCLUDED
S" src/compiler/spf_translate.f"     INCLUDED
S" src/compiler/spf_defwords.f"      INCLUDED
S" src/compiler/spf_immed_transl.f"  INCLUDED
S" src/compiler/spf_immed_lit.f"     INCLUDED
S" src/compiler/spf_literal.f"       INCLUDED_L
S" src/compiler/spf_immed_control.f" INCLUDED
S" src/compiler/spf_immed_loop.f"    INCLUDED
S" src/compiler/spf_modules.f"       INCLUDED
S" src/compiler/spf_inline.f"        INCLUDED

\ ==============================================================
\ Окружение (environment).
\ Определяющие слова для Windows.
\ Многозадачность.
\ CGI

S" src\win\spf_win_envir.f"          INCLUDED
S" src\win\spf_win_defwords.f"       INCLUDED
S" src\win\spf_win_mtask.f"          INCLUDED
S" src\win\spf_win_cgi.f"            INCLUDED

\ ==============================================================
\ Сохранение системы в exe-файле.

S" src\win\spf_pe_save.f"            INCLUDED


DBG\  'MNOOP TO <PRE> 
: DONE 
  CR ." DONE"
  S" src/done.f" INCLUDED
;

\ ==============================================================
\ Инициализация переменных, startup
S" src/spf_init.f"                   INCLUDED


\ ==============================================================

0x10  ALIGN-NOP
CR .( Dummy B, B@ B! and /CHAR )
: B, C, ; : B@ C@ ; : B! C! ; : /CHAR 1 ;

CR .( =============================================================)
CR .( Done. Saving the system.)
CR .( =============================================================)



TC-LATEST-> FORTH-WORDLIST
HERE ' (DP) ( целевой DP) EXECUTE !
TC-WINAPLINK @ ' WINAPLINK EXECUTE !


TC-YDP  @ CR .( Y=) DUP U. ' YDP EXECUTE !
TC-YDP0 @ CR .( Y=) DUP U. ' YDP0  EXECUTE !


CR  
 HERE U.
\ DUP  HERE OVER - S" spf.bin" R/W CREATE-FILE THROW WRITE-FILE THROW

\ записываем "DONE" в командную строку
S"  DONE " GetCommandLineA ASCIIZ> S"  " SEARCH 2DROP SWAP 1+ MOVE


\ на стеке - token слова INIT целевой системы, запускаем её для
\ того чтобы она сама себя сохранила в spf37x.exe выполнением слова DONE,
\ переданного ей в командной строке

\ SEE CATCH

HERE 0!

-4900 TC-YDP0 @ + 0!
	
EXECUTE

