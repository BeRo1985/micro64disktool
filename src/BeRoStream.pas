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
unit BeRoStream;
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
{$ifdef win32}
 {$define win}
{$endif}
{$ifdef win64}
 {$define win}
{$endif}

interface

uses {$ifdef win}Windows,{$endif}SysUtils,Classes;

const bsoFromBeginning=0;
      bsoFromCurrent=1;
      bsoFromEnd=2;

type PBeRoStreamData=^TBeRoStreamData;
     TBeRoStreamData=packed array[0..$7ffffffe] of byte;

     PBeRoStreamBuffer=^TBeRoStreamBuffer;
     TBeRoStreamBuffer=packed array[1..4096] of byte;

     PBeRoStream=^TBeRoStream;
     TBeRoStream=class
      private
       fPosition,fSize,fInMemorySize:longint;
       fData:PBeRoStreamData;
       fBitBuffer:longword;
       fBitBufferSize:byte;
       procedure Realloc(NewInMemorySize:longint);
       procedure Resize(NewSize:longint);
       function GetString:ansistring;
       procedure SetString(Value:ansistring);
       function GetByte(BytePosition:longint):byte;
       procedure SetByte(BytePosition:longint;Value:byte);
      public
       constructor Create;
       destructor Destroy; override;
       function ReadFromStream(Stream:TStream):longint;
       function WriteToStream(Stream:TStream):longint;
       function Assign(Src:TBeRoStream):longint;
       function Append(Src:TBeRoStream):longint;
       function AppendFrom(Src:TBeRoStream;Counter:longint):longint;
       procedure Clear; virtual;
       function Read(var Buf;Count:longint):longint; virtual;
       function ReadAt(Position:longint;var Buf;Count:longint):longint; virtual;
       function Write(const Buf;Count:longint):longint; virtual;
       function SeekEx(APosition:longint):longint; virtual;
       function Seek(APosition:longint):longint; overload;
       function Seek(APosition,Origin:longint):longint; overload;
       function Position:longint; virtual;
       function Size:longint; virtual;
       procedure SetSize(NewSize:longint);
       function ReadByte:byte;
       function ReadWord:word;
       function ReadDWord:longword;
       function ReadLine:ansistring;
       function ReadString:ansistring;
       procedure WriteByte(Value:byte);
       function WriteByteCount(Value:byte;Count:longint):longint;
       procedure WriteWord(Value:word);
       procedure WriteDWord(Value:longword);
       procedure WriteShortInt(Value:shortint);
       procedure WriteSmallInt(Value:smallint);
       procedure WriteLongInt(Value:longint);
       procedure WriteInt64(Value:int64);
       procedure WriteBoolean(Value:boolean);
       procedure WriteLine(Line:ansistring);
       procedure WriteString(S:ansistring);
       procedure WriteDataString(S:ansistring);
       procedure ResetBits;
       function ReadBit:boolean;
       function ReadBits(BitsCount:byte):longword;
       function ReadBitsSigned(BitsCount:byte):longint;
       procedure WriteBit(Value:boolean);
       procedure WriteBits(Value:longword;BitsCount:byte);
       procedure WriteBitsSigned(Value:longint;BitsCount:byte);
       procedure FlushBits;
       property Text:ansistring read GetString write SetString;
       property Data:PBeRoStreamData read fData;
       property Bytes[BytePosition:longint]:byte read GetByte write SetByte; default;
       property BitsInBuffer:byte read fBitBufferSize;
     end;

     PBeRoDatenStream=^TBeRoDatenStream;
     TBeRoDatenStream=TBeRoStream;

     PBeRoMemoryStream=^TBeRoMemoryStream;
     TBeRoMemoryStream=TBeRoStream;

     PBeRoFileStream=^TBeRoFileStream;
     TBeRoFileStream=class(TBeRoStream)
      private
{$ifdef win}
       fFile:longword;
{$else}
       fFile:file;
{$endif}
      public
       ReadOnly:boolean;
       constructor Create(FileName:ansistring);
       constructor CreateNew(FileName:ansistring);
       destructor Destroy; override;
       function Read(var Buf;Count:longint):longint; override;
       function Write(const Buf;Count:longint):longint; override;
       function SeekEx(APosition:longint):longint; override;
       function Position:longint; override;
       function Size:longint; override;
     end;

implementation

type pbyte=^byte;

const MemoryDelta=1 shl 16;
      MemoryDeltaMask=MemoryDelta-1;

constructor TBeRoStream.Create;
begin
 inherited Create;
 fData:=nil;
 REALLOCMEM(fData,0);
 fPosition:=0;
 fSize:=0;
 fInMemorySize:=0;
 ResetBits;
end;

destructor TBeRoStream.Destroy;
begin
 REALLOCMEM(fData,0);
 fPosition:=0;
 fSize:=0;
 fInMemorySize:=0;
 inherited Destroy;
end;

function TBeRoStream.ReadFromStream(Stream:TStream):longint;
var Remain,ToDo:longint;
    Buf:TBeRoStreamBuffer;
begin
 Clear;
 result:=0;
 if (Seek(0)=0) and (Stream.Seek(0,soBeginning)=0) then begin
  Remain:=Stream.Size;
  while Remain>0 do begin
   ToDo:=sizeof(TBeRoStreamBuffer);
   if ToDo>Remain then begin
    ToDo:=Remain;
   end;
   if Stream.Read(Buf,ToDo)<>ToDo then begin
    break;
   end;
   if Write(Buf,ToDo)<>ToDo then begin
    break;
   end;
   inc(result,ToDo);
   dec(Remain,ToDo);
  end;
 end;
end;

function TBeRoStream.WriteToStream(Stream:TStream):longint;
var Remain,ToDo:longint;
    Buf:TBeRoStreamBuffer;
begin
 result:=0;
 if (Seek(0)=0) and (Stream.Seek(0,soBeginning)=0) then begin
  Remain:=Size;
  while Remain>0 do begin
   ToDo:=sizeof(TBeRoStreamBuffer);
   if ToDo>Remain then begin
    ToDo:=Remain;
   end;
   if Read(Buf,ToDo)<>ToDo then begin
    break;
   end;
   if Stream.Write(Buf,ToDo)<>ToDo then begin
    break;
   end;
   inc(result,ToDo);
   dec(Remain,ToDo);
  end;
  if Stream.Seek(0,soBeginning)<>0 then begin
   result:=0;
  end;
 end; 
end;

function TBeRoStream.Assign(Src:TBeRoStream):longint;
var Remain,Count:longint;
    Buf:TBeRoStreamBuffer;
begin
 Clear;
 result:=0;
 Remain:=Src.Size;
 if (Seek(0)<>0) or (Src.Seek(0)<>0) then exit;
 while Remain>=sizeof(TBeRoStreamBuffer) do begin
  Count:=Src.Read(Buf,sizeof(TBeRoStreamBuffer));
  Write(Buf,Count);
  inc(result,Count);
  dec(Remain,sizeof(TBeRoStreamBuffer));
 end;
 if Remain>0 then begin
  Count:=Src.Read(Buf,Remain);
  Write(Buf,Count);
  inc(result,Count);
 end;
end;

function TBeRoStream.Append(Src:TBeRoStream):longint;
var Remain,Count:longint;
    Buf:TBeRoStreamBuffer;
begin
 result:=0;
 Remain:=Src.Size;
 if Src.Seek(0)<>0 then exit;
 while Remain>=sizeof(TBeRoStreamBuffer) do begin
  Count:=Src.Read(Buf,sizeof(TBeRoStreamBuffer));
  Write(Buf,Count);
  inc(result,Count);
  dec(Remain,sizeof(TBeRoStreamBuffer));
 end;
 if Remain>0 then begin
  Count:=Src.Read(Buf,Remain);
  Write(Buf,Count);
  inc(result,Count);
 end;
end;

function TBeRoStream.AppendFrom(Src:TBeRoStream;Counter:longint):longint;
var Remain,Count:longint;
    Buf:TBeRoStreamBuffer;
begin
 result:=0;
 Remain:=Counter;
 while Remain>=sizeof(TBeRoStreamBuffer) do begin
  Count:=Src.Read(Buf,sizeof(TBeRoStreamBuffer));
  Write(Buf,Count);
  inc(result,Count);
  dec(Remain,sizeof(TBeRoStreamBuffer));
 end;
 if Remain>0 then begin
  Count:=Src.Read(Buf,Remain);
  Write(Buf,Count);
  inc(result,Count);
 end;
end;

procedure TBeRoStream.Clear;
begin
 ReallocMem(fData,0);
 fPosition:=0;
 fSize:=0;
 fInMemorySize:=0;
end;

procedure TBeRoStream.Realloc(NewInMemorySize:longint);
var OldInMemorySize,Count:longint;
begin
 if NewInMemorySize>0 then begin
  NewInMemorySize:=(NewInMemorySize+MemoryDeltaMask) and not MemoryDeltaMask;
 end;
 if fInMemorySize<>NewInMemorySize then begin
  OldInMemorySize:=fInMemorySize;
  fInMemorySize:=NewInMemorySize;
  ReallocMem(fData,fInMemorySize);
  Count:=NewInMemorySize-OldInMemorySize;
  if Count>0 then begin
   FillChar(fData^[OldInMemorySize],Count,#0);
  end;
 end;
end;

procedure TBeRoStream.Resize(NewSize:longint);
begin
 fSize:=NewSize;
 if fPosition>fSize then begin
  fPosition:=fSize;
 end;
 Realloc(fSize);
end;

function TBeRoStream.Read(var Buf;Count:longint):longint;
begin
 if (fPosition>=0) and (Count>0) then begin
  result:=fSize-fPosition;
  if result>0 then begin
   if result>Count then begin
    result:=Count;
   end;
   Move(fData^[fPosition],Buf,result);
   inc(fPosition,result);
  end else begin
   result:=0;
  end;
 end else begin
  result:=0;
 end;
end;

function TBeRoStream.ReadAt(Position:longint;var Buf;Count:longint):longint;
begin
 if Seek(Position)=Position then begin
  result:=Read(Buf,Count);
 end else begin
  result:=0;
 end;
end;

function TBeRoStream.Write(const Buf;Count:longint):longint;
var EndPosition:longint;
begin
 if (fPosition>=0) and (Count>0) then begin
  EndPosition:=fPosition+Count;
  if EndPosition>fSize then begin
   Resize(EndPosition);
  end;
  Move(Buf,fData^[fPosition],Count);
  fPosition:=EndPosition;
  result:=Count;
 end else begin
  result:=0;
 end;
end;

function TBeRoStream.SeekEx(APosition:longint):longint;
var AltePos,RemainSize:longint;
begin
 fPosition:=APosition;
 if fPosition<0 then fPosition:=0;
 if fPosition>fSize then begin
  AltePos:=fSize;
  RemainSize:=fPosition-fSize;
  if RemainSize>0 then begin
   Resize(fSize+RemainSize);
   FILLCHAR(fData^[AltePos],RemainSize,#0);
  end;
  result:=fPosition;
 end else begin
  result:=fPosition;
 end;
end;

function TBeRoStream.Seek(APosition:longint):longint;
begin
 result:=SeekEx(APosition);
end;

function TBeRoStream.Seek(APosition,Origin:longint):longint;
begin
 case Origin of
  bsoFromBeginning:result:=SeekEx(APosition);
  bsoFromCurrent:result:=SeekEx(Position+APosition);
  bsoFromEnd:result:=SeekEx(Size-APosition);
  else result:=SeekEx(APosition);
 end;
end;

function TBeRoStream.Position:longint;
begin
 result:=fPosition;
end;

function TBeRoStream.Size:longint;
begin
 result:=fSize;
end;

procedure TBeRoStream.SetSize(NewSize:longint);
begin
 fSize:=NewSize;
 if fPosition>fSize then fPosition:=fSize;
 REALLOCMEM(fData,fSize);
end;

function TBeRoStream.ReadByte:byte;
var B:byte;
begin
 if Read(B,1)<>1 then begin
  result:=0;
 end else begin
  result:=B;
 end;
end;

function TBeRoStream.ReadWord:word;
begin
 result:=ReadByte or (ReadByte shl 8);
end;

function TBeRoStream.ReadDWord:longword;
begin
 result:=ReadWord or (ReadWord shl 16);
end;

function TBeRoStream.ReadLine:ansistring;
var C:ansichar;
begin
 result:='';
 while Position<Size do begin
  Read(C,1);
  case C of
   #10,#13:begin
    if (Position<Size) and (((C=#13) and (Bytes[Position]=10)) or
                            ((C=#10) and (Bytes[Position]=13))) then begin
     Read(C,1);
    end;
    break;
   end;
   else begin
    result:=result+C;
   end;
  end;
 end;
end;

function TBeRoStream.ReadString:ansistring;
var L:longword;
begin
 L:=ReadDWord;
 setlength(result,L);
 if L>0 then begin
  Read(result[1],L);
 end;
end;

procedure TBeRoStream.WriteByte(Value:byte);
begin
 Write(Value,sizeof(byte));
end;

function TBeRoStream.WriteByteCount(Value:byte;Count:longint):longint;
var Counter:longint;
begin
 result:=0;
 for Counter:=1 to Count do begin
  inc(result,Write(Value,sizeof(byte)));
 end;
end;

procedure TBeRoStream.WriteWord(Value:word);
begin
 Write(Value,sizeof(word));
end;

procedure TBeRoStream.WriteDWord(Value:longword);
begin
 Write(Value,sizeof(longword));
end;

procedure TBeRoStream.WriteShortInt(Value:shortint);
begin
 Write(Value,sizeof(shortint));
end;

procedure TBeRoStream.WriteSmallInt(Value:smallint);
begin
 Write(Value,sizeof(smallint));
end;

procedure TBeRoStream.WriteLongInt(Value:longint);
begin
 Write(Value,sizeof(longint));
end;

procedure TBeRoStream.WriteInt64(Value:int64);
begin
 Write(Value,sizeof(int64));
end;

procedure TBeRoStream.WriteBoolean(Value:boolean);
begin
 if Value then begin
  WriteByte(1);
 end else begin
  WriteByte(0);
 end;
end;

procedure TBeRoStream.WriteLine(Line:ansistring);
const CRLF:array[1..2] of ansichar=#13#10;
begin
 if length(Line)>0 then Write(Line[1],length(Line));
 Write(CRLF,2);
end;

procedure TBeRoStream.WriteString(S:ansistring);
var L:longword;
begin
 L:=length(S);
 if L>0 then Write(S[1],L);
end;

procedure TBeRoStream.WriteDataString(S:ansistring);
var L:longword;
begin
 L:=length(S);
 WriteDWord(L);
 if L>0 then Write(S[1],L);
end;

procedure TBeRoStream.ResetBits;
begin
 fBitBuffer:=0;
 fBitBufferSize:=0;
end;

function TBeRoStream.ReadBit:boolean;
begin
 result:=(ReadBits(1)<>0);
end;

function TBeRoStream.ReadBits(BitsCount:byte):longword;
begin
 while fBitBufferSize<BitsCount do begin
  fBitBuffer:=(fBitBuffer shl 8) or ReadByte;
  inc(fBitBufferSize,8);
 end;
 result:=(fBitBuffer shr (fBitBufferSize-BitsCount)) and ((1 shl BitsCount)-1);
 dec(fBitBufferSize,BitsCount);
end;

function TBeRoStream.ReadBitsSigned(BitsCount:byte):longint;
begin
 result:=0;
 if BitsCount>1 then begin
  if ReadBits(1)<>0 then begin
   result:=-ReadBits(BitsCount-1);
  end else begin
   result:=ReadBits(BitsCount-1);
  end;
 end;
end;

procedure TBeRoStream.WriteBit(Value:boolean);
begin
 if Value then begin
  WriteBits(1,1);
 end else begin
  WriteBits(0,1);
 end;
end;

procedure TBeRoStream.WriteBits(Value:longword;BitsCount:byte);
begin
 fBitBuffer:=(fBitBuffer shl BitsCount) or Value;
 inc(fBitBufferSize,BitsCount);
 while fBitBufferSize>=8 do begin
  WriteByte((fBitBuffer shr (fBitBufferSize-8)) and $ff);
  dec(fBitBufferSize,8);
 end;
end;

procedure TBeRoStream.WriteBitsSigned(Value:longint;BitsCount:byte);
begin
 if BitsCount>1 then begin
  if Value<0 then begin
   WriteBits(1,1);
   WriteBits(longword(0-Value),BitsCount-1);
  end else begin
   WriteBits(0,1);
   WriteBits(longword(Value),BitsCount-1);
  end;
 end;
end;

procedure TBeRoStream.FlushBits;
begin
 if fBitBufferSize>0 then begin
  WriteByte(fBitBuffer shl (8-fBitBufferSize));
 end;
 fBitBuffer:=0;
 fBitBufferSize:=0;
end;

function TBeRoStream.GetString:ansistring;
begin
 Seek(0);
 if Size>0 then begin
  setlength(result,Size);
  Read(result[1],Size);
 end else begin
  result:='';
 end;
end;

procedure TBeRoStream.SetString(Value:ansistring);
begin
 Clear;
 if length(Value)>0 then begin
  Write(Value[1],length(Value));
 end;
end;

function TBeRoStream.GetByte(BytePosition:longint):byte;
var AltePosition:longint;
begin
 AltePosition:=Position;
 Seek(BytePosition);
 Read(result,sizeof(byte));
 Seek(AltePosition);
end;

procedure TBeRoStream.SetByte(BytePosition:longint;Value:byte);
var AltePosition:longint;
begin
 AltePosition:=Position;
 Seek(BytePosition);
 Write(Value,sizeof(byte));
 Seek(AltePosition);
end;

constructor TBeRoFileStream.Create(FileName:ansistring);
{$ifndef win}
var Alt:byte;
{$endif}
begin
 inherited Create;
 ReadOnly:=false;
{$ifdef win}
 if (FileGetAttr(String(FileName)) and faReadOnly)<>0 then begin
  fFile:=CreateFile(pchar(String(FileName)),GENERIC_READ,FILE_SHARE_READ,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL or FILE_FLAG_RANDOM_ACCESS or FILE_FLAG_WRITE_THROUGH,0);
  if fFile<>0 then begin
   ReadOnly:=true;
  end;
 end else begin
  fFile:=CreateFile(pchar(String(FileName)),GENERIC_READ	or GENERIC_WRITE,FILE_SHARE_READ,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL or FILE_FLAG_RANDOM_ACCESS or FILE_FLAG_WRITE_THROUGH,0);
  if fFile=0 then begin
   fFile:=CreateFile(pchar(String(FileName)),GENERIC_READ,FILE_SHARE_READ,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL or FILE_FLAG_RANDOM_ACCESS or FILE_FLAG_WRITE_THROUGH,0);
   if fFile<>0 then begin
    ReadOnly:=true;
   end else begin
    fFile:=CreateFile(pchar(String(FileName)),GENERIC_READ or GENERIC_WRITE,FILE_SHARE_READ,nil,CREATE_ALWAYS,FILE_ATTRIBUTE_NORMAL or FILE_FLAG_RANDOM_ACCESS or FILE_FLAG_WRITE_THROUGH,0);
   end;
  end;
 end;
{$else}
 Alt:=FileMode;
 if (FileGetAttr(FileName) and faReadOnly)<>0 then begin
  FileMode:=0;
  ReadOnly:=true;
 end else begin
  FileMode:=2;
 end;
 assignfile(fFile,FileName);
 {$i-}reset(fFile,1);{$i+}
 if IOResult<>0 then begin
  FileMode:=0;
  ReadOnly:=true;
  assignfile(fFile,FileName);
  {$i-}reset(fFile,1);{$i+}
  if IOResult<>0 then begin
   FileMode:=2;
   assignfile(fFile,FileName);
   {$i-}rewrite(fFile,1);{$i+}
  end;
 end;
 FileMode:=Alt;
 if IOResult<>0 then begin
 end;
{$endif}
end;

constructor TBeRoFileStream.CreateNew(FileName:ansistring);
{$ifndef win}
var Alt:byte;
{$endif}
begin
 inherited Create;
 ReadOnly:=false;
{$ifdef win}
 fFile:=CreateFile(pchar(string(FileName)),GENERIC_READ or GENERIC_WRITE,FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,nil,CREATE_ALWAYS,FILE_ATTRIBUTE_NORMAL or FILE_FLAG_RANDOM_ACCESS or FILE_FLAG_WRITE_THROUGH,0);
{$else}
 Alt:=FileMode;
 FileMode:=2;
 assignfile(fFile,FileName);
 {$i-}rewrite(fFile,1);{$i+}
 FileMode:=Alt;
 if IOResult<>0 then begin
 end;
{$endif}
end;

destructor TBeRoFileStream.Destroy;
begin
{$ifdef win}
 CloseHandle(fFile);
{$else}
 {$i-}closefile(fFile);{$i+}
 if IOResult<>0 then begin
 end;
{$endif}
 inherited Destroy;
end;

function TBeRoFileStream.Read(var Buf;Count:longint):longint;
{$ifdef win}
var l:longword;
{$else}
var i:longint;
{$endif}
begin
{$ifdef win}
 ReadFile(fFile,Buf,Count,l,nil);
 result:=l;
{$else}
 {$i-}blockread(fFile,Buf,Count,i);{$i+}
 if IOResult<>0 then begin
  result:=0;
  exit;
 end;
 {$i-}fPosition:=filepos(fFile);{$i+}
 if IOResult<>0 then begin
  result:=0;
  exit;
 end;
 result:=i;
{$endif}
end;

function TBeRoFileStream.Write(const Buf;Count:longint):longint;
{$ifdef win}
var l:longword;
{$else}
var i:longint;
{$endif}
begin
{$ifdef win}
 WriteFile(fFile,Buf,Count,l,nil);
 result:=l;
{$else}
 {$i-}blockwrite(fFile,Buf,Count,i);{$i+}
 if IOResult<>0 then begin
  result:=0;
  exit;
 end;
 {$i-}fPosition:=filepos(fFile);{$i+}
 if IOResult<>0 then begin
  result:=0;
  exit
 end;
 result:=i;
{$endif}
end;

function TBeRoFileStream.SeekEx(APosition:longint):longint;
begin
{$ifdef win}
 if APosition<=Size then begin
  SetFilePointer(fFile,APosition,nil,FILE_BEGIN);
  if IOResult<>0 then begin
   result:=0;
   exit;
  end;
 end;
 result:=SetFilePointer(fFile,0,nil,FILE_CURRENT);
{$else}
 if APosition<=Size then begin
  {$i-}System.Seek(fFile,APosition);{$i+}
  if IOResult<>0 then begin
   result:=0;
   exit;
  end;
 end;
 {$i-}result:=filepos(fFile);{$i+}
 if IOResult<>0 then begin
  result:=0;
 end;
{$endif}
end;

function TBeRoFileStream.Position:longint;
begin
{$ifdef win}
 result:=SetFilePointer(fFile,0,nil,FILE_CURRENT);
{$else}
 {$i-}result:=filepos(fFile);{$i+}
 if IOResult<>0 then begin
  result:=0;
 end;
{$endif}
end;

function TBeRoFileStream.Size:longint;
{$ifdef win}
var Old:longint;
{$endif}
begin
{$ifdef win}
 Old:=SetFilePointer(fFile,0,nil,FILE_CURRENT);
 result:=SetFilePointer(fFile,0,nil,FILE_END);
 SetFilePointer(fFile,Old,nil,FILE_BEGIN);
{$else}
 {$i-}result:=filesize(fFile);{$i+}
 if IOResult<>0 then begin
  result:=0;
 end;
{$endif}
end;

end.
