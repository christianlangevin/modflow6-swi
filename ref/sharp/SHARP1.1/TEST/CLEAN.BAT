@echo off
rem  clean.bat - clean up files after test runs
rem  Usage:  clean
rem  History: 99/05/06 mcrouse

rem  remove input file
if exist input del input

rem  remove output from each test
for %%f in (output results heads leak) do  if exist %%f.t1 del %%f.t1
for %%f in (output results heads leak) do  if exist %%f.t2 del %%f.t2

if exist check.log del check.log
