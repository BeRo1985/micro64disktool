@echo off
rem Tested with Borland Delphi 7 and Delphi XE3
rem You must adjust the path to your dcc32.exe of your Delphi installation
del *.dcu
"c:\Program Files (x86)\Borland\Delphi7\bin\dcc32.exe" -b micro64disktool.dpr
del *.dcu
