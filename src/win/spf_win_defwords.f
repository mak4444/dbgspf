( Интерфейсы с Windows - определения в словаре импортируемых 
  функций Windows и экспортируемых функций [callback, wndproc и т.п.]
  Windows-зависимые определения.
  Copyright [C] 1992-1999 A.Cherezov ac@forth.org
  Преобразование из 16-разрядного в 32-разрядный код - 1995-96гг
  Ревизия - сентябрь 1999
)

VARIABLE WINAPLINK
0  VALUE NEW-WINAPI?

: __WIN:  ( params CFA_INI "ИмяПроцедуры" "ИмяБиблиотеки" -- )

   COMPILE,
  HERE >R
  0 , \ address of winproc
  0 , \ address of library name
  0 , \ address of function name
  , \ # of parameters
  IS-TEMP-WL 0=
  IF
    HERE WINAPLINK @ , WINAPLINK ! ( связь )
  THEN
  HERE DUP R@ CELL+ CELL+ !
  PARSE-NAME HERE SWAP DUP ALLOT MOVE 0 C, \ имя функции
  HERE DUP R> CELL+ !
  PARSE-NAME HERE SWAP DUP ALLOT MOVE 0 C, \ имя библиотеки
  LoadLibraryA DUP 0= IF -2009 THROW THEN \ ABORT" Library not found"
  GetProcAddress 0= IF -2010 THROW THEN \ ABORT" Procedure not found"
;

: WHEADER ( "name" -- )
  YDP_FL >R  0 TO  YDP_FL  HEADER
  R> TO  YDP_FL
;

: _WIN: ( CFA_INI "ИмяПроцедуры" "ИмяБиблиотеки" -- )
     >IN @   WHEADER   >IN ! __WIN: ;


: RWIN:  ( "RИмяПроцедуры" "ИмяБиблиотеки" -- )
 ( Аналог WINAPI: передача параметров через стек
 возвратов на вершину стека кладется буферное значение
 куда дудет возращон результат. К имени процедуры
 нущно добавить префикс 'R' )
  DUP
 ['] _RWIN-CODE SkipDelimiters  >IN @  WHEADER
 1+  \ обход префикса обозначающего функцию, параметры которой
\ передаются через стек возвратов
    >IN !   __WIN: ;

: 0WIN:  ( "ИмяПроцедуры" "ИмяБиблиотеки" -- )
 ( Аналог WINAPI: для процедуры без параметров )
 0 ['] _0WIN-CODE _WIN: ;


: 1WIN:  ( "ИмяПроцедуры" "ИмяБиблиотеки" -- )
 ( Аналог WINAPI: для процедуры с 1-м параметром )
 1 ['] _1WIN-CODE _WIN: ;

: WINAPI: ( "ИмяПроцедуры" "ИмяБиблиотеки" -- )
  ( Используется для импорта WIN32-процедур.
    Полученное определение будет иметь имя "ИмяПроцедуры".
    Поле address of winproc будет заполнено в момент первого
    выполнения полученной словарной статьи.
    Для вызова полученной "импортной" процедуры параметры
    помещаются на стек данных в порядке, обратном описанному
    в Си-вызове этой процедуры. Результат выполнения функции
    будет положен на стек.
  )
  NEW-WINAPI?
  IF WHEADER
  ELSE
     0
     >IN @
     WHEADER
     >IN !
  THEN  ['] _WINAPI-CODE __WIN:
;


: EXTERN ( xt1 n -- xt2 )
  HERE
  SWAP LIT,
  ['] FORTH-INSTANCE> COMPILE,
  SWAP COMPILE,
  ['] <FORTH-INSTANCE COMPILE,
  RET,
;

: CALLBACK: ( xt n "name" -- )
\ Здесь n в байтах!
  EXTERN
  WHEADER
  ['] _WNDPROC-CODE COMPILE,
  ,
;

: WNDPROC: ( xt "name" -- )
  4 CELLS CALLBACK:
;

: TASK ( xt1 -- xt2 )
  CELL EXTERN
  HERE SWAP
  ['] _WNDPROC-CODE COMPILE,
  ,
;
: TASK: ( xt "name" -- )
  TASK CONSTANT
;

: ERASE-IMPORTS
  \ обнуление адресов импортируемых процедур
  \ и числа параметров
  WINAPLINK
  BEGIN
    @ DUP
  WHILE
    DUP 4 CELLS - 0!
  REPEAT DROP
;
