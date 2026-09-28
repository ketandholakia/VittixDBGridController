@echo off
call "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat" >nul 2>&1
cd /d "%~dp0"
msbuild VittixDBGridTests.dproj /p:Config=Debug /p:Platform=Win32 /v:minimal /nologo
exit /b %ERRORLEVEL%
