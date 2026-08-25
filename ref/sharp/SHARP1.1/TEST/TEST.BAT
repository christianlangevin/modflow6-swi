@echo off
rem  test.bat - batch program to run SHARP tests
rem
rem  Usage: test [start [stop]]
rem
rem         where: start = starting test number
rem                 stop = ending test number (may be same as start)
rem
rem         (Note that start and stop are assumed to be within the valid
rem          test range and are assumed to be in ascending order--the
rem          absence of numeric variables in batch files makes error checking
rem          awkward.)
rem
rem  History: 99/05/07 mcrouse
rem
set topdir=..
set data=%topdir%\data
set prog=%topdir%\bin\sharp.exe
set prognm=sharp
set divd=---------------------------------------

rem  assign valid test range values as default values
set start=1
set stop=2
rem  if values supplied for start and stop, use those
if not "%1"=="" set start=%1
if not "%2"=="" set stop=%2

echo.
echo %divd%%divd%
echo Begin processing %prognm% test runs %start% to %stop%
echo.
echo NOTE:  No %prognm% output will be displayed to the screen
echo        during execution of the tests.
goto test%start%

:test1
set n=1
for %%f in (output results heads leak) do  if exist %%f.t%n% del %%f.t%n%
echo.
echo.
echo %divd%%divd%
echo Test run number %n%
copy %data%\input.t%n% input
%prog%
for %%f in (output results heads leak) do  ren %%f %%f.t%n%
if "%stop%"=="%n%" goto end

:test2
set n=2
for %%f in (output results heads leak) do  if exist %%f.t%n% del %%f.t%n%
echo.
echo.
echo %divd%%divd%
echo Test run number %n%
copy %data%\input.t%n% input
%prog%
for %%f in (output results heads leak) do  ren %%f %%f.t%n%
if "%stop%"=="%n%" goto end

:end
call %topdir%\test\check %start% %stop%

rem  clear the variables used from the environment
set topdir=
set data=
set prog=
set prognm=
set divd=
set start=
set stop=
set n=
