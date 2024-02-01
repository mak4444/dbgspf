call compile.bat 
call spf4p.BAT 
cd samples\~mak\win\SPFWC 
call compile.bat
cd ..\..\..\..
copy  samples\~mak\win\SPFWC\spf4mwc.exe spf4mwc.exe
