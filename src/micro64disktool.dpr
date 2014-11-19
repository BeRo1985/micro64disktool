(*
** This file is part of the Micro64 Disk Tool.
** Copyright (C) 2002-2013 by Benjamin Rosseaux
**
** The source code of the Micro64 Disk Tool and helper tools are
** distributed under the Library GNU General Public License
** (see the file COPYING) with the following modification:
**
** As a special exception, the copyright holders of this software give you
** permission to link this software with independent modules to produce 
** an executable, regardless of the license terms of these independent modules,
** and to copy and distribute the resulting executable under terms of your 
** choice, provided that you also meet, for each linked independent module,
** the terms and conditions of the license of that module. An independent 
** module is a module which is not derived from or based on this software. If 
** you modify this software, you may extend this exception to your  version of
** the software, but you are not obligated to do so. If you do not wish to do 
** so, delete this exception statement from your version.
**
** If you didn't receive a copy of the file COPYING, contact:
**      Free Software Foundation
**      675 Mass Ave
**      Cambridge, MA  02139
**      USA
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
**
*)
program micro64disktool;
{-$DEFINE DEBUG}
{$IFDEF FPC}
 {$MODE DELPHI}
 {$WARNINGS OFF}
 {$HINTS OFF}
 {$OVERFLOWCHECKS OFF}
 {$RANGECHECKS OFF}
 {$IFDEF CPUI386}         
  {$DEFINE CPU386}
  {$ASMMODE INTEL}
 {$ENDIF}
 {$IFDEF FPC_LITTLE_ENDIAN}
  {$DEFINE LITTLE_ENDIAN}
 {$ELSE}
  {$IFDEF FPC_BIG_ENDIAN}
   {$DEFINE BIG_ENDIAN}
  {$ENDIF}
 {$ENDIF}
{$ELSE}
 {$DEFINE LITTLE_ENDIAN}
 {$IFNDEF CPU64}
  {$DEFINE CPU32}
 {$ENDIF}
 {$OPTIMIZATION ON}
 {$setpeoptflags $140}
{$ENDIF}
{$ifdef WIN32}
 {$IMAGEBASE $500000}
 {$SETPEFLAGS $20}
 {-$APPTYPE GUI}
 {$APPTYPE CONSOLE}
 {$define windows}
{$ENDIF}
{$ifdef WIN64}
 {$APPTYPE GUI}
 {$APPTYPE CONSOLE}
 {$define windows}
{$ENDIF}

uses
  SysUtils,
  BeRoStream in 'BeRoStream.pas',
  BeRoUtils in 'BeRoUtils.pas',
  ChecksumUtils in 'ChecksumUtils.pas',
  DiskImageD64 in 'DiskImageD64.pas',
  DiskImageFDI in 'DiskImageFDI.pas',
  DiskImageG64 in 'DiskImageG64.pas',
  DiskImageKryofluxStream in 'DiskImageKryofluxStream.pas',
  DiskImageNIB in 'DiskImageNIB.pas',
  DiskImageP64 in 'DiskImageP64.pas',
  GCR in 'GCR.pas',
  Globals in 'Globals.pas',
  RangeCoder in 'RangeCoder.pas';

begin
 writeln('Micro64 Disk Tool 20130113 - Copyright (C) 2012-2013, Benjamin ''BeRo'' Rosseaux');
 writeln('http://www.micro64.de/');
 if ParamCount=0 then begin
  writeln('Usage: ',ChangeFileExt(ExtractFileName(ParamStr(0)),''),' [options]');
  writeln('Options: +newd64 [filename.d64]');
  writeln('         +g642d64 [input.g64] [output.d64]');
  writeln('         +d642g64 [input.d64] [output.g64]');
  writeln('         +g642p64 [input.g64] [output.p64]');
  writeln('         +p642g64 [input.p64] [output.g64]');
  writeln('         +fdi2p64 [input.fdi] [output.p64]');
  writeln('         +p642fdi [input.p64] [output.fdi]');
  writeln('         +nib2g64 [input.nib] [output.g64]');
  writeln('         +dumpfdi [input.fdi] [output.log]');
  writeln('         +dumpp64 [input.p64] [output.log]');
  writeln('         +dumpp64halftrack [input.p64] [output.log] ([halftrack])');
  writeln('         +kryofluxstream2p64 [inputpathprefix] [output.p64] ([side(0/1)]) ([rpm(180-360)]) ([doublewidetrack(0/1)])');
  writeln('         +kryofluxstream2fdi [inputpathprefix] [output.fdi] ([side(0/1)]) ([rpm(180-360)]) ([doublewidetrack(0/1)])');
 end else begin
  ParseParameter;
 end;
end.


