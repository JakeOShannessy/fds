@echo off
set PATH=%PATH%;"%~dp0bin"
call "%~dp0bin\fds-verify.exe" %*
