@echo off 

call scripts\buildlib.bat clean
call scripts\buildserial.bat clean
call scripts\buildtbb.bat clean
call scripts\buildopenmp.bat clean
if ""%1"" ==  ""clean"" exit /b


if ""%1"" == ""solution"" goto build_solution
if ""%1"" == ""buildserial"" goto buildserial
if ""%1"" == ""buildtbb"" goto buildtbb
if ""%1"" == ""buildopenmp"" goto buildopenmp

if ""%1"" == ""openmp_solution"" goto openmp_solution
if ""%1"" == ""tbb_solution"" goto tbb_solution

:build_start
call scripts\buildlib.bat
call scripts\buildserial.bat
call scripts\buildtbb.bat
call scripts\buildopenmp.bat
exit /b

:buildserial
call scripts\buildlib.bat
call scripts\buildserial.bat
exit /b

:buildtbb
call scripts\buildlib.bat
call scripts\buildtbb.bat
exit /b

:buildopenmp
call scripts\buildlib.bat
call scripts\buildopenmp.bat
exit /b

:openmp_solution
call scripts\buildlib.bat
call scripts\buildopenmp.bat solution
exit /b

:tbb_solution
call scripts\buildlib.bat
call scripts\buildtbb.bat solution
exit /b

:tbb_solution
call scripts\buildlib.bat
call scripts\buildtbb.bat solution
exit /b

:build_solution
echo Building Solutions
call scripts\buildlib.bat
call scripts\buildserial.bat
call scripts\buildtbb.bat solution
call scripts\buildopenmp.bat solution
