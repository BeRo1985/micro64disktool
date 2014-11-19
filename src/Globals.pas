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
unit Globals;
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
{$ENDIF}
{$writeableconst on}

interface

uses SysUtils,{$ifdef win32}ShellApi,{$endif}BeRoStream;

type PByteArray=^TByteArray;
     TByteArray=array[0..($7fffffff div sizeof(byte))-1] of byte;

     PBooleanArray=^TBooleanArray;
     TBooleanArray=array[0..($7fffffff div sizeof(boolean))-1] of boolean;

     PLongWordArray=^TLongWordArray;
     TLongWordArray=array[0..($7fffffff div sizeof(longword))-1] of longword;

     pbyte=^byte;

procedure ParseParameter;
function ASCIIToPETSCIIChar(const C:ansichar):ansichar;
function ASCIIToPETSCII(const StringValue:ansistring):ansistring;
function PETSCIIToASCIIChar(const C:ansichar):ansichar;
function PETSCIIToASCII(const StringValue:ansistring):ansistring;
function MatchFilePattern(Pattern,ToTest:pansichar;Len:longint):boolean;

implementation

uses BeRoUtils,BeRoStringToDouble,DiskImageD64,DiskImageP64,DiskImageFDI,DiskImageNIB,DiskImageKryofluxStream;

procedure ParseParameter;
var I,J:longint;
    Side,DoubleWideTracks:boolean;
    S,L:ansistring;
    Stream:TBeRoStream;
    FileStream:TBeRoFileStream;
    DstFileStream:TBeRoFileStream;
    RPM:double;
    p,OK:longbool;
begin
 J:=0;
 if PARAMCOUNT<>0 then begin
  for I:=1 to PARAMCOUNT do begin
   S:=AnsiString(PARAMSTR(I));
   if length(S)>0 then begin
    if not (S[1] in ['-','+','/']) then begin
     inc(J);
    end else begin
     case S[1] of
      '+','/':P:=true;
      else {'-':}P:=false;
     end;
     if p then begin
     end;
     L:=COPY(S,2,length(S)-1);
     S:=UPPERCASE(L);
     if S='NEWD64' then begin
      if (i+1)<=ParamCount then begin
       try
        Stream:=TBeRoStream.Create;
        try
         if CreateEmptyD64(Stream,'DISK') then begin
          DstFileStream:=TBeRoFileStream.CreateNew(AnsiString(ParamStr(i+1)));
          try
           DstFileStream.Assign(Stream);
          finally
           DstFileStream.Free;
          end;
         end;
        finally
         Stream.Free;
        end;
       except
       end;
      end;
      halt;

     end else if S='G642D64' then begin
      if ((i+2)<=ParamCount) and FileExists(AnsiString(ParamStr(i+1))) then begin
       try
        FileStream:=TBeRoFileStream.Create(AnsiString(ParamStr(i+1)));
        try
         Stream:=TBeRoStream.Create;
         try
          if ConvertG64toD64(FileStream,Stream) then begin
           DstFileStream:=TBeRoFileStream.CreateNew(AnsiString(ParamStr(i+2)));
           try
            DstFileStream.Assign(Stream);
           finally
            DstFileStream.Free;
           end;
          end;
         finally
          Stream.Free;
         end;
        finally
         FileStream.Free;
        end;
       except
       end;
      end;
      halt;

     end else if S='D642G64' then begin
      if ((i+2)<=ParamCount) and FileExists(AnsiString(ParamStr(i+1))) then begin
       try
        FileStream:=TBeRoFileStream.Create(AnsiString(ParamStr(i+1)));
        try
         Stream:=TBeRoStream.Create;
         try
          if ConvertD64toG64(FileStream,Stream) then begin
           DstFileStream:=TBeRoFileStream.CreateNew(AnsiString(ParamStr(i+2)));
           try
            DstFileStream.Assign(Stream);
           finally
            DstFileStream.Free;
           end;
          end;
         finally
          Stream.Free;
         end;
        finally
         FileStream.Free;
        end;
       except
       end;
      end;
      halt;

     end else if S='G642P64' then begin
      if ((i+2)<=ParamCount) and FileExists(AnsiString(ParamStr(i+1))) then begin
       try
        FileStream:=TBeRoFileStream.Create(AnsiString(ParamStr(i+1)));
        try
         Stream:=TBeRoStream.Create;
         try
          if ConvertG64toP64(FileStream,Stream) then begin
           DstFileStream:=TBeRoFileStream.CreateNew(AnsiString(ParamStr(i+2)));
           try
            DstFileStream.Assign(Stream);
           finally
            DstFileStream.Free;
           end;
          end;
         finally
          Stream.Free;
         end;
        finally
         FileStream.Free;
        end;
       except
       end;
      end;
      halt;

     end else if S='P642G64' then begin
      if ((i+2)<=ParamCount) and FileExists(AnsiString(ParamStr(i+1))) then begin
       try
        FileStream:=TBeRoFileStream.Create(AnsiString(ParamStr(i+1)));
        try
         Stream:=TBeRoStream.Create;
         try
          if ConvertP64toG64(FileStream,Stream,true) then begin
           DstFileStream:=TBeRoFileStream.CreateNew(AnsiString(ParamStr(i+2)));
           try
            DstFileStream.Assign(Stream);
           finally
            DstFileStream.Free;
           end;
          end;
         finally
          Stream.Free;
         end;
        finally
         FileStream.Free;
        end;
       except
       end;
      end;
      halt;

     end else if S='FDI2P64' then begin
      if ((i+2)<=ParamCount) and FileExists(AnsiString(ParamStr(i+1))) then begin
       try
        FileStream:=TBeRoFileStream.Create(AnsiString(ParamStr(i+1)));
        try
         Stream:=TBeRoStream.Create;
         try
          if ConvertFDItoP64(FileStream,Stream) then begin
           DstFileStream:=TBeRoFileStream.CreateNew(AnsiString(ParamStr(i+2)));
           try
            DstFileStream.Assign(Stream);
           finally
            DstFileStream.Free;
           end;
          end;
         finally
          Stream.Free;
         end;
        finally
         FileStream.Free;
        end;
       except
       end;
      end;
      halt;

     end else if S='P642FDI' then begin
      if ((i+2)<=ParamCount) and FileExists(AnsiString(ParamStr(i+1))) then begin
       try
        FileStream:=TBeRoFileStream.Create(AnsiString(ParamStr(i+1)));
        try
         Stream:=TBeRoStream.Create;
         try
          if ConvertP64toFDI(FileStream,Stream) then begin
           DstFileStream:=TBeRoFileStream.CreateNew(AnsiString(ParamStr(i+2)));
           try
            DstFileStream.Assign(Stream);
           finally
            DstFileStream.Free;
           end;
          end;
         finally
          Stream.Free;
         end;
        finally
         FileStream.Free;
        end;
       except
       end;
      end;
      halt;

     end else if S='NIB2G64' then begin
      if ((i+2)<=ParamCount) and FileExists(AnsiString(ParamStr(i+1))) then begin
       try
        FileStream:=TBeRoFileStream.Create(AnsiString(ParamStr(i+1)));
        try
         Stream:=TBeRoStream.Create;
         try
          if ConvertNIBtoG64(FileStream,Stream) then begin
           DstFileStream:=TBeRoFileStream.CreateNew(AnsiString(ParamStr(i+2)));
           try
            DstFileStream.Assign(Stream);
           finally
            DstFileStream.Free;
           end;
          end;
         finally
          Stream.Free;
         end;
        finally
         FileStream.Free;
        end;
       except
       end;
      end;
      halt;

     end else if S='DUMPFDI' then begin
      if ((i+2)<=ParamCount) and FileExists(AnsiString(ParamStr(i+1))) then begin
       try
        FileStream:=TBeRoFileStream.Create(AnsiString(ParamStr(i+1)));
        try
         Stream:=TBeRoFileStream.CreateNew(AnsiString(ParamStr(i+2)));
         try
          if DumpFDI(FileStream,Stream) then begin
          end;
         finally
          Stream.Free;
         end;
        finally
         FileStream.Free;
        end;
       except
       end;
      end;
      halt;

     end else if S='DUMPP64' then begin
      if ((i+2)<=ParamCount) and FileExists(AnsiString(ParamStr(i+1))) then begin
       try
        FileStream:=TBeRoFileStream.Create(AnsiString(ParamStr(i+1)));
        try
         Stream:=TBeRoFileStream.CreateNew(AnsiString(ParamStr(i+2)));
         try
          if DumpP64(FileStream,Stream) then begin
          end;
         finally
          Stream.Free;
         end;
        finally
         FileStream.Free;
        end;
       except
       end;
      end;
      halt;

     end else if S='DUMPP64HALFTRACK' then begin
      if ((i+3)<=ParamCount) and FileExists(AnsiString(ParamStr(i+1))) then begin
       try
        FileStream:=TBeRoFileStream.Create(AnsiString(ParamStr(i+1)));
        try
         Stream:=TBeRoFileStream.CreateNew(AnsiString(ParamStr(i+2)));
         try
          if DumpP64HalfTrack(FileStream,Stream,StrToIntDef(ParamStr(i+3),36)) then begin
          end;
         finally
          Stream.Free;
         end;
        finally
         FileStream.Free;
        end;
       except
       end;
      end;
      halt;

     end else if (S='KRYOFLUXSTREAM2P64') or (S='KRYOFLUXSTREAM2FDI') then begin
      if (i+2)<=ParamCount then begin
       if (i+3)<=ParamCount then begin
        Side:=ParamStr(i+3)='1';
       end else begin
        Side:=false;
       end;
       if (i+4)<=ParamCount then begin
        OK:=false;
        RPM:=BeRoConvertStringToDouble(AnsiString(ParamStr(i+4)),bstd_ROUND_TO_NEAREST,@OK);
        if (not OK) or (RPM<1) then begin
         RPM:=300;
        end;
       end else begin
        RPM:=300;
       end;
       if (i+5)<=ParamCount then begin
        DoubleWideTracks:=ParamStr(i+5)='1';
       end else begin
        DoubleWideTracks:=false;
       end;
       if ConvertKryofluxStream(AnsiString(ParamStr(i+1)),AnsiString(ParamStr(i+2)),Side,S='KRYOFLUXSTREAM2FDI',DoubleWideTracks,RPM) then begin
       end;
      end;
      halt;

     end;

    end;
   end;
  end;
 end;
 if j<>0 then begin
 end;
end;

function ASCIIToPETSCIIChar(const C:ansichar):ansichar;
begin
 case C of
  'A'..'Z','a'..'z':result:=ansichar(byte(byte(C) xor $20));
  else result:=C;
 end;
end;

function ASCIIToPETSCII(const StringValue:ansistring):ansistring;
var Counter:longint;
begin
 result:=StringValue;
 for Counter:=1 to length(result) do begin
  result[Counter]:=ASCIIToPETSCIIChar(result[Counter]);
 end;
end;

function PETSCIIToASCIIChar(const C:ansichar):ansichar;
begin
 case C of
  'A'..'Z','a'..'z':result:=ansichar(byte(byte(C) xor $20));
  #$c1..#$da:result:=ansichar(byte(byte(C) xor $80));
  else result:=C;
 end;
end;

function PETSCIIToASCII(const StringValue:ansistring):ansistring;
var Counter:longint;
begin
 result:=StringValue;
 for Counter:=1 to length(result) do begin
  result[Counter]:=PETSCIIToASCIIChar(result[Counter]);
 end;
end;

function MatchFilePattern(Pattern,ToTest:pansichar;Len:longint):boolean;
var Counter:longint;
begin
 result:=true;
 if Len>16 then Len:=16;
 for Counter:=1 to Len do begin
  if Pattern^='*' then begin
   break;
  end else if (Pattern^<>'?') and (Pattern^<>ToTest^) then begin
   result:=false;
   break;
  end;
  inc(Pattern);
  inc(ToTest);
 end;
end;

initialization
end.


