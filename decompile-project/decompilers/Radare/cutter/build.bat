@ECHO off
SETLOCAL ENABLEDELAYEDEXPANSION
SETLOCAL ENABLEEXTENSIONS

IF "%VisualStudioVersion%" == "14.0" ( IF NOT DEFINED Platform SET "Platform=X86" )
FOR /F %%i IN ('powershell -c "\"%Platform%\".toLower()"') DO SET PLATFORM=%%i
powershell -c "if ('%PLATFORM%' -notin ('x86', 'x64')) {Exit 1}"
IF !ERRORLEVEL! NEQ 0 (
    ECHO Unknown platform: %PLATFORM%
    EXIT /B 1
)

SET "R2DIST=r2_dist"
SET "BUILDDIR=build_%PLATFORM%"
SET "BREAKPAD_SOURCE_DIR=%CD%\src\breakpad\src\src"

ECHO Preparing directory
RMDIR /S /Q %BUILDDIR%
MKDIR %BUILDDIR%

ECHO Prepare translations
FOR %%i in (src\translations\*.ts) DO lrelease %%i

CD %BUILDDIR%

IF NOT DEFINED CUTTER_ENABLE_CRASH_REPORTS (
SET "CUTTER_ENABLE_CRASH_REPORTS=false"
)

ECHO Building cutter
qmake BREAKPAD_SOURCE_DIR=%BREAKPAD_SOURCE_DIR% CUTTER_ENABLE_CRASH_REPORTS=%CUTTER_ENABLE_CRASH_REPORTS% %* ..\src\cutter.pro -config release
IF !ERRORLEVEL! NEQ 0 EXIT /B 1
nmake
IF !ERRORLEVEL! NEQ 0 EXIT /B 1

ECHO Deploying cutter
MKDIR cutter
COPY release\cutter.exe cutter\cutter.exe
XCOPY /S /I ..\%R2DIST%\share cutter\share
XCOPY /S /I ..\%R2DIST%\lib cutter\lib
DEL cutter\lib\*.lib
COPY ..\%R2DIST%\bin\*.dll cutter\
windeployqt cutter\cutter.exe
FOR %%i in (..\src\translations\*.qm) DO MOVE "%%~fi" cutter\translations

ENDLOCAL
