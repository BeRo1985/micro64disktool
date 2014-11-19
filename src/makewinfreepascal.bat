@echo off
rem Tested with FreePascal 2.6.0
rem fpc.exe must be in your %PATH% environment variable
del *.o
del *.ppu
fpc -B -Sd -O3 micro64disktool.dpr
del *.o
del *.ppu
