@echo off
rem  check.bat - batch program to compare original output files
rem              against newly created output files
rem
rem  called from test.bat; not run independently
rem
rem  Usage: check [start [stop]]
rem
rem         where: start = starting test number
rem                 stop = ending test number (may be same as start)
rem
rem  History: 99/03/03 mcrouse
rem

set start=%1
set stop=%2
if exist check.log del check.log

echo.
echo.
echo %divd%%divd%
echo Comparing test output with expected output...
echo.
echo.
goto test%start%

:test1
set n=1
for %%f in (*.t%n%) do  fc %data%\%%f %%f >> check.log
if "%stop%"=="%n%" goto end

:test2
set n=2
for %%f in (*.t%n%) do  fc %data%\%%f %%f >> check.log
if "%stop%"=="%n%" goto end

:end
echo Comparisons completed.  Examine file check.log for any differences.
echo.
echo Differences may indicate a problem with %prognm% installation.
echo.
