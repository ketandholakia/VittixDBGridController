@echo off
rem Builds and nothing else; run VittixDBGridTests.exe afterwards.
rem Override the Delphi installation root with VITTIX_DELPHI_ROOT if your
rem studio version differs from the default (Delphi 12 Athens = 23.0).
if "%VITTIX_DELPHI_ROOT%"=="" set "VITTIX_DELPHI_ROOT=C:\Program Files (x86)\Embarcadero\Studio\23.0"
call "%VITTIX_DELPHI_ROOT%\bin\rsvars.bat" >nul 2>&1
cd /d "%~dp0"
msbuild VittixDBGridTests.dproj /p:Config=Debug /p:Platform=Win32 /v:minimal /nologo
exit /b %ERRORLEVEL%
