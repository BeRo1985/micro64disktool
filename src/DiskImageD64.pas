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
unit DiskImageD64;
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

interface

uses BeRoStream,BeRoUtils;

type TBlock=array [0..255] of byte;

     TDirItemKind=string[3];

     TDirectory=record
      FirstLine:ansistring;
      Lines:array of ansistring;
      LastLine:ansistring;
     end;

const DirLink:array[0..18] of longint=(0,1,4,7,10,13,16,2,5,8,11,14,17,3,6,9,12,15,18);

      KindName:array[0..15] of TDirItemKind=('DEL','SEQ','PRG',
                                             'USR','REL','EL?',
                                             'EQ?','RG?','SR?',
                                             'EL?','L??','Q??',
                                             'G??','R??','L??',
                                             '???');

function MatchFilePattern(Pattern,ToTest:pansichar):boolean;
function HeaderOffset(Stream:TBeRoStream):longint;
function SectorOffset(Track,Sector:longint):longint;
function SectorNumber(Track,Sector:longint):longint;
function ReadSector(Stream:TBeRoStream;Track,Sector:byte;var Data):boolean;
function WriteSector(Stream:TBeRoStream;Track,Sector:byte;const Data):boolean;
function AllocateBlock(Stream:TBeRoStream;Track,Sector:longint):boolean;
function FindFreeBlock(Stream:TBeRoStream;var Track,Sector:longint):boolean;
function FindLastFreeBlock(Stream:TBeRoStream;var Track,Sector:longint):boolean;
function FindFreeDirectoryItem(Stream:TBeRoStream;var Track,Sector,Position:longint):boolean;
function FreeBlocks(Stream:TBeRoStream):longint;
function CreateEmptyD64(Stream:TBeRoStream;Name:ansistring):boolean;
function CopyToD64(SrcStream:TBeRoStream;SrcFileName:ansistring;DstStream:TBeRoStream):boolean;
function ConvertPRGP00toD64(SrcStream,DstStream:TBeRoStream;SrcFileName:ansistring):boolean;
function ConvertT64toD64(SrcStream,DstStream:TBeRoStream):boolean;
function ConvertG64toD64(SrcStream,DstStream:TBeRoStream):boolean;
function ConvertD64toG64(SrcStream,DstStream:TBeRoStream):boolean;
function GetDirectory(Stream:TBeRoStream;var Directory:TDirectory):boolean;
function GetFile(SrcStream,DstStream:TBeRoStream;FileName:ansistring):boolean;

implementation

uses GCR,DiskImageG64,DiskImageP64,DiskImageFDI,DiskImageNIB,Globals;

{procedure CreateDirLink;
var DirLink:array[0..18] of byte;
    i:longint;
begin
 DirLink[0]:=0;
 for i:=0 to 5 do begin
  DirLink[i+1]:=(i*3)+1;
  DirLink[i+7]:=(i*3)+2;
  DirLink[i+13]:=(i*3)+3;
 end;
 for i:=0 to 18 do begin
  writeln(DirLink[i]);
 end;
end;}

function MatchFilePattern(Pattern,ToTest:pansichar):boolean;
var Counter,Len:longint;
begin
 result:=true;
 if length(Pattern)<length(ToTest) then begin
  len:=length(Pattern);
 end else begin
  len:=length(ToTest);
 end;
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

function HeaderOffset(Stream:TBeRoStream):longint;
var OldPosition:longint;
    X64Header:array[0..63] of byte;
begin
 result:=0;
 if not assigned(Stream) then begin
  exit;
 end;
 OldPosition:=Stream.Position;
 if Stream.Seek(0,bsoFromBeginning)=0 then begin
  if Stream.Read(X64Header,sizeof(X64Header))=sizeof(X64Header) then begin
   if (X64Header[0]=$43) and (X64Header[1]=$15) and (X64Header[2]=$41) and (X64Header[3]=$64) then begin
    result:=64;
   end;
  end;
 end;
 Stream.Seek(OldPosition);
end;

function SectorOffset(Track,Sector:longint):longint;
begin
 if (Track<1) or (Track>40) or (Sector<0) or (Sector>=SectorCount[Track]) then begin
  result:=-1;
 end else begin
  result:=(SectorOfs[Track]+Sector) shl 8;
 end;
end;

function SectorNumber(Track,Sector:longint):longint;
var t,s:longint;
begin
 result:=0;
 if (Track<1) or (Track>40) or (Sector<0) or (Sector>=SectorCount[Track]) then begin
  exit;
 end;
 for t:=1 to 35 do begin
  for s:=0 to SectorCount[Track]-1 do begin
   inc(result);
   if (t=Track) and (s=Sector) then begin
    exit;
   end;
  end;
 end;
end;

function ReadSector(Stream:TBeRoStream;Track,Sector:byte;var Data):boolean;
var Offset:longint;
begin
 if not assigned(Stream) then begin
  result:=false;
  exit;
 end;
 Offset:=SectorOffset(Track,Sector)+HeaderOffset(Stream);
 result:=Offset>=0;
 if result then begin
  result:=Stream.Seek(Offset,bsoFromBeginning)=Offset;
  if result then begin
   result:=Stream.Read(Data,256)=256;
  end;
 end;
end;

function WriteSector(Stream:TBeRoStream;Track,Sector:byte;const Data):boolean;
var Offset:longint;
begin
 if not assigned(Stream) then begin
  result:=false;
  exit;
 end;
 Offset:=SectorOffset(Track,Sector)+HeaderOffset(Stream);
 result:=Offset>=0;
 if result then begin
  result:=Stream.Seek(Offset,bsoFromBeginning)=Offset;
  if result then begin
   result:=Stream.write(Data,256)=256;
  end;
 end;
end;

function AllocateBlock(Stream:TBeRoStream;Track,Sector:longint):boolean;
var Block:TBlock;
begin
 result:=false;
 if not assigned(Stream) then begin
  exit;
 end;
 if not ReadSector(Stream,18,0,Block) then begin
  exit;
 end;
 if (Block[(Track shl 2)+1+(Sector shr 3)] and (1 shl (Sector and 7)))=0 then begin
  exit;
 end;
 Block[(Track shl 2)+1+(Sector shr 3)]:=Block[(Track shl 2)+1+(Sector shr 3)] and not (1 shl (Sector and 7));
 dec(Block[Track shl 2]);
 if not WriteSector(Stream,18,0,Block) then begin
  exit;
 end;
 result:=true;
end;

function FindFreeBlock(Stream:TBeRoStream;var Track,Sector:longint):boolean;
var Block:TBlock;
    t,s:longint;
begin
 result:=false;
 Track:=0;
 Sector:=0;
 if not assigned(Stream) then begin
  exit;
 end;
 if not ReadSector(Stream,18,0,Block) then begin
  exit;
 end;
 for t:=1 to 35 do begin
  if t=18 then continue;
  for s:=0 to SectorCount[t]-1 do begin
   if (Block[(t shl 2)+1+(s shr 3)] and (1 shl (s and 7)))<>0 then begin
    Track:=t;
    Sector:=s;
    result:=true;
    exit;
   end;
  end;
 end;
end;

function FindLastFreeBlock(Stream:TBeRoStream;var Track,Sector:longint):boolean;
var Block:TBlock;
    t,s:longint;
begin
 result:=false;
 Track:=0;
 Sector:=0;
 if not assigned(Stream) then begin
  exit;
 end;
 if not ReadSector(Stream,18,0,Block) then begin
  exit;
 end;
 for t:=35 downto 1 do begin
  if t=18 then continue;
  for s:=SectorCount[t]-1 downto 0 do begin
   if (Block[(t shl 2)+1+(s shr 3)] and (1 shl (s and 7)))<>0 then begin
    Track:=t;
    Sector:=s;
    result:=true;
    exit;
   end;
  end;
 end;
end;

function FindFreeDirectoryItem(Stream:TBeRoStream;var Track,Sector,Position:longint):boolean;
var Block:TBlock;
    i,j:longint;
begin
 result:=false;
 Track:=18;
 Sector:=0;
 Position:=0;
 if not assigned(Stream) then begin
  exit;
 end;
 for i:=1 to SectorCount[18]-1 do begin
  if not ReadSector(Stream,18,DirLink[i],Block) then begin
   exit;
  end;
  for j:=0 to 7 do begin
   if Block[(j shl 5)+2]=0 then begin
    Sector:=DirLink[i];
    Position:=j;
    result:=true;
    exit;
   end;
  end;
 end;
end;

function FreeBlocks(Stream:TBeRoStream):longint;
var Block:TBlock;
    i:longint;
begin
 result:=0;
 if not assigned(Stream) then begin
  exit;
 end;
 if not ReadSector(Stream,18,0,Block) then begin
  exit;
 end;
 for i:=1 to 35 do begin
  if i=18 then begin
   continue;
  end;
  inc(result,Block[(i-1)*4+4]);
 end;
end;

function CreateEmptyD64(Stream:TBeRoStream;Name:ansistring):boolean;
var i,Track,Sector:longint;
    Block,BAM,EmptyBlock:TBlock;
begin
 result:=false;
 if not assigned(Stream) then begin
  exit;
 end;
 FillChar(Block,sizeof(TBlock),#$1);
 Block[0]:=$4b;
 FillChar(BAM,sizeof(TBlock),#$0);
 BAM[0]:=18;
 BAM[1]:=1;
 BAM[2]:=65;
 FillChar(BAM[144],27,#$a0);
 for i:=1 to length(Name) do begin
  if i>16 then begin
   break;
  end;
  bAM[144+i-1]:=ord(Name[i]);
 end;
 FillChar(BAM[162],2,#$20);
 BAM[165]:=50;
 BAM[166]:=65;
 for Track:=1 to 35 do begin
  bAM[Track shl 2]:=$15;
  for Sector:=0 to SectorCount[Track]-1 do begin
   BAM[(Track shl 2)+1+(Sector shr 3)]:=$ff;
  end;
 end;
 FillChar(EmptyBlock,sizeof(TBlock),#$0);
 Stream.Clear;
 Stream.Seek(0);
 for Track:=1 to 17 do begin
  for Sector:=0 to SectorCount[Track]-1 do begin
   if not WriteSector(Stream,Track,Sector,Block) then begin
    exit;
   end;
  end;
 end;
 if not WriteSector(Stream,18,0,BAM) then begin
  exit;
 end;
 if not WriteSector(Stream,18,1,EmptyBlock) then begin
  exit;
 end;
 for Sector:=2 to SectorCount[18]-1 do begin
  if not WriteSector(Stream,18,Sector,EmptyBlock) then begin
   exit;
  end;
 end;
 for Track:=19 to 35 do begin
  for Sector:=0 to SectorCount[Track]-1 do begin
   if not WriteSector(Stream,Track,Sector,Block) then begin
    exit;
   end;
  end;
 end;
 Stream.Seek(Stream.Size);
 result:=true;
end;

function CopyToD64(SrcStream:TBeRoStream;SrcFileName:ansistring;DstStream:TBeRoStream):boolean;
var i,Track,Sector,Position,Track2,Sector2:longint;
    e:array[0..31] of byte;
    Block:TBlock;
begin
 result:=false;
 if not (assigned(SrcStream) and assigned(DstStream)) then begin
  exit;
 end;
 if ((SrcStream.Size+253) div 254)>FreeBlocks(DstStream) then begin
  exit;
 end;
 if not FindFreeDirectoryItem(DstStream,Track,Sector,Position) then begin
  exit;
 end;
 FillChar(e,sizeof(e),#$0);
 FillChar(e[$05],16,#$a0);
 e[$02]:=$82;
 if not FindLastFreeBlock(DstStream,Track2,Sector2) then begin
  exit;
 end;
 e[$03]:=Track2;
 e[$04]:=Sector2;
 if length(SrcFileName)=0 then begin
  Str(SectorNumber(Track2,Sector2),SrcFileName);
 end;
 for i:=1 to length(SrcFileName) do begin
  if i>16 then break;
  e[$04+i]:=ord(SrcFileName[i]);
 end;
 i:=(SrcStream.Size+253) div 254;
 e[30]:=i and $ff;
 e[31]:=(i shr 8) and $ff;
 if not ReadSector(DstStream,Track,Sector,Block) then begin
  exit;
 end;
 Move(e[2],Block[(Position shl 5)+2],30);
 if not WriteSector(DstStream,Track,Sector,Block) then begin
  exit;
 end;
 Position:=0;
 while (Position<=18) and (DirLink[Position]<>Sector) do begin
  inc(Position);
 end;
 if Position>0 then begin
  dec(Position);
 end;
 if not ReadSector(DstStream,Track,DirLink[Position],Block) then begin
  exit;
 end;
 Block[0]:=Track;
 Block[1]:=Sector;
 if not WriteSector(DstStream,Track,DirLink[Position],Block) then begin
  exit;
 end;
 SrcStream.Seek(0);
 while SrcStream.Position<SrcStream.Size do begin
  FillChar(Block,sizeof(TBlock),#$1);
  i:=SrcStream.Read(Block[2],254);
  if i=0 then begin
   exit;
  end;
  Track:=Track2;
  Sector:=Sector2;
  if not AllocateBlock(DstStream,Track,Sector) then begin
   exit;
  end;
  if SrcStream.Position<SrcStream.Size then begin
   if not FindLastFreeBlock(DstStream,Track2,Sector2) then begin
    exit;
   end;
   Block[0]:=Track2;
   Block[1]:=Sector2;
  end else begin
   Block[0]:=0;
   Block[1]:=i+1;
  end;
  if not WriteSector(DstStream,Track,Sector,Block) then begin
   exit;
  end;
 end;
 result:=true;
end;

function ConvertPRGP00toD64(SrcStream,DstStream:TBeRoStream;SrcFileName:ansistring):boolean;
type TP00HeaderSignature=array[1..8] of ansichar;
const P00HeaderSignatureMagic:TP00HeaderSignature='C64File'#0;
      P00HeaderSize=26;
      P00HeaderNameOffset=8;
      P00HeaderNameSize=17;
      P00HeaderRecordOffset=25;
      P00HeaderRecordSize=1;
var Stream,TempStream:TBeRoStream;
    Size:longint;
    HeaderSignature:TP00HeaderSignature;
begin
 result:=false;
 if assigned(SrcStream) then begin
  Stream:=TBeRoStream.Create;
  Stream.Append(SrcStream);
  Stream.Seek(0);

  Size:=Stream.Size;

  if Stream.Read(HeaderSignature,sizeof(TP00HeaderSignature))<>sizeof(TP00HeaderSignature) then begin
   Stream.Destroy;
   exit;
  end;

  if P00HeaderSignatureMagic=HeaderSignature then begin
   Stream.Seek(P00HeaderSize);
   dec(Size,P00HeaderSize);
  end else begin
   Stream.Seek(0);
  end;

  CreateEmptyD64(DstStream,'DISK');
  TempStream:=TBeRoStream.Create;
  TempStream.AppendFrom(Stream,Size);
  CopyToD64(TempStream,trim(SrcFileName),DstStream);
  TempStream.Destroy;

  result:=true;
 end;
end;

function ConvertT64toD64(SrcStream,DstStream:TBeRoStream):boolean;
type TT64Header=packed record
      Signature:array[1..3] of ansichar;
      Text:array[1..29] of ansichar;
      Version:array[0..1] of byte;
      Entries:array[0..1] of byte;
      Used:array[0..1] of byte;
      Dummy:array[0..1] of byte;
      Title:array[1..24] of ansichar;
     end;
     TT64Entry=packed record
      EntryFlag:byte;
      C64Type:byte;
      LoadAddr:array[0..1] of byte;
      EndAddr:array[0..1] of byte;
      Dummy1:array[0..1] of byte;
      DataOfs:array[0..3] of byte;
      Dummy2:array[0..3] of byte;
      C64Name:array[1..16] of ansichar;
     end;
var T64Header:TT64Header;
    T64Entry:TT64Entry;
    i,j,OldPos:longint;
    TempStream:TBeRoStream;
begin
 result:=false;
 if not (assigned(SrcStream) and assigned(DstStream)) then begin
  exit;
 end;
 SrcStream.Seek(0);
 if SrcStream.read(T64Header,sizeof(TT64Header))<>sizeof(TT64Header) then begin
  exit;
 end;
 if (T64Header.Signature[1]<>'C') or (T64Header.Signature[2]<>'6') or (T64Header.Signature[3]<>'4') then begin
  exit;
 end;
 CreateEmptyD64(DstStream,trim(T64Header.Title));
 for i:=0 to (T64Header.Entries[0] or (T64Header.Entries[1] shl 8))-1 do begin
  if SrcStream.read(T64Entry,sizeof(TT64Entry))<>sizeof(TT64Entry) then begin
   exit;
  end;
  OldPos:=SrcStream.Position;
  SrcStream.Seek(T64Entry.DataOfs[0] or (T64Entry.DataOfs[1] shl 8) or (T64Entry.DataOfs[2] shl 16) or (T64Entry.DataOfs[3] shl 24));
  TempStream:=TBeRoStream.Create;
  if TempStream.write(T64Entry.LoadAddr,2)<>2 then exit;
  j:=(T64Entry.EndAddr[0] or (T64Entry.EndAddr[1] shl 8))-(T64Entry.LoadAddr[0] or (T64Entry.LoadAddr[1] shl 8))+1;
  TempStream.AppendFrom(SrcStream,j);
  CopyToD64(TempStream,trim(T64Entry.C64Name),DstStream);
  TempStream.Destroy;
  SrcStream.Seek(OldPos);
 end;
 SrcStream.Seek(SrcStream.Size);
 result:=true;
end;

function D64Read(var GCR:TGCR;SrcStream:TBeRoStream):boolean;
var Track,Sector,MaxSector,ImageHalfTracks,HeaderSize:longint;
    X64Header:array[0..63] of byte;
    Buffer:array[0..259] of byte;
    Ptr:pbyte;
    RC:byte;
    G64Signature:array[1..8] of ansichar;
    Image:TBeRoStream;
    DiskID1,DiskID2:byte;
    OK:boolean;
    FileFDISignature:TFDISignature;
    FileNIBSignature:TNIBSignature;
begin
 result:=false;
 FillChar(GCR.Data,SizeOf(GCR.Data),#$00);
 FillChar(GCR.SpeedZone,SizeOf(GCR.SpeedZone),#$00);
 GCR.MaximumHalfTrackSize:=FileMinMaxBytesPerGCRHalfTrack;
 for Track:=FirstHalfTrack to LastHalfTrack do begin
  GCR.HalfTrackSize[Track]:=RawTrackSize[SpeedMap[Track shr 1]];
  FillChar(GCR.SpeedZone[Track*MaxBytesPerGCRHalfTrack],MaxBytesPerGCRHalfTrack,SpeedMap[Track shr 1]);
 end;
 Image:=SrcStream;
 if assigned(Image) then begin
  ImageHalfTracks:=NumTracks1541*2;
  OK:=false;
  Image.Seek(0);
  if Image.Read(G64Signature,sizeof(G64Signature))=sizeof(G64Signature) then begin
   if G64Signature<>'P64-1541' then begin
    if G64Signature='GCR-1541' then begin
     if Image.Read(Buffer,4)=4 then begin
      ImageHalfTracks:=Buffer[1];
      if (ImageHalfTracks in [(NumTracks1541*2)-1..MaxHalfTracks1541]) then begin
       if (Buffer[2] or (Buffer[3] shl 8))<>$1ef8 then begin
        OK:=true;
       end;
      end else begin
       OK:=true;
      end;
     end else begin
      OK:=true;
     end;
    end else begin
     OK:=true;
    end;
   end else begin
    OK:=true;
   end;
  end;
  if OK then begin
   Image.Seek(0);
   if Image.Read(FileFDISignature,sizeof(TFDISignature))=sizeof(TFDISignature) then begin
    if FileFDISignature=FDISignature then begin
     OK:=false;
    end;
   end;
   if OK then begin
    Image.Seek(0);
    if Image.Read(FileNIBSignature,sizeof(TNIBSignature))=sizeof(TNIBSignature) then begin
     if FileNIBSignature=NIBSignature then begin
      OK:=false;
     end;
    end;
   end;
  end;
  if OK then begin
   Image.Seek(0);
   for Track:=FirstHalfTrack to LastHalfTrack do begin
    GCR.HalfTrackSize[Track]:=RawTrackSize[SpeedMap[Track shr 1]];
    FillChar(GCR.SpeedZone[Track*MaxBytesPerGCRHalfTrack],MaxBytesPerGCRHalfTrack,SpeedMap[Track shr 1]);
   end;
   HeaderSize:=0;
   if Image.Seek(0,bsoFromBeginning)=0 then begin
    if Image.Read(X64Header,sizeof(X64Header))=sizeof(X64Header) then begin
     if (X64Header[0]=$43) and (X64Header[1]=$15) and (X64Header[2]=$41) and (X64Header[3]=$64) then begin
      HeaderSize:=64;
     end;
    end;
   end;
   OK:=false;
   if ((Image.Size-HeaderSize)=(Sectors35*256)) or ((Image.Size-HeaderSize)=(Sectors35*257)) then begin
    ImageHalfTracks:=NumTracks1541*2;
    OK:=true;
   end else if ((Image.Size-HeaderSize)=(Sectors40*256)) or ((Image.Size-HeaderSize)=(Sectors40*257)) then begin
    ImageHalfTracks:=ExtTracks1541*2;
    OK:=true;
   end else if HeaderSize=64 then begin
    ImageHalfTracks:=X64Header[7]*2;
    OK:=true;
   end;
   if OK then begin
    ReadSector(Image,18,0,Buffer);
    DiskID1:=Buffer[$a2];
    DiskID2:=Buffer[$a3];
    GCR.HalfTracks:=ImageHalfTracks;
    for Track:=1 to (ImageHalfTracks+1) shr 1 do begin
     Ptr:=@GCR.Data[(Track shl 1)*MaxBytesPerGCRHalfTrack];
     FillChar(Ptr^,MaxBytesPerGCRHalfTrack,#$ff);
     MaxSector:=SectorCount[Track];
     for Sector:=0 to MaxSector-1 do begin
      ReadSector(Image,Track,Sector,Buffer[1]);
      Ptr:=@GCR.Data[((Track shl 1)*MaxBytesPerGCRHalfTrack)+((GCR.HalfTrackSize[Track shl 1]*Sector) div MaxSector)];
      RC:=0;
      if RC=21 then begin
       FillChar(Ptr^,MaxBytesPerGCRHalfTrack,#$0);
       break;
      end else begin
       GCRConvertSectorToGCR(@Buffer,Ptr,Track,Sector,DiskID1,DiskID2,RC);
      end;
     end;
    end;
    result:=true;
   end;
  end;
 end;
end;

function D64Check(SrcStream:TBeRoStream):boolean;
var GCR:PGCR;
begin
 New(GCR);
 try
  result:=D64Read(GCR^,SrcStream);
 finally
  Dispose(GCR);
 end;
end;

function D64Write(const GCR:TGCR;DstStream:TBeRoStream):boolean;
var Track,HalfTrack,ImageHalfTracks,ImageTracks,TrackLen,Sector,MaxSector:longint;
    G64TrackData:pbyte;
    Buffer:array[0..259] of byte;
begin
 result:=false;
 ImageHalfTracks:=G64CountHalfTracks(GCR);
 if ImageHalfTracks>NumTracks1541*2 then begin
  ImageHalfTracks:=ExtTracks1541*2;
 end;
 ImageTracks:=(ImageHalfTracks+1) shr 1;
 DstStream.Clear;
 if DstStream.Seek(0)<>0 then begin
  exit;
 end;
 for Track:=1 to MaxTracks1541 do begin
  if Track<=ImageTracks then begin
   HalfTrack:=Track shl 1;
   G64TrackData:=@GCR.Data[HalfTrack*MaxBytesPerGCRHalfTrack];
   TrackLen:=GCR.HalfTrackSize[HalfTrack];
   MaxSector:=SectorCount[Track];
   for Sector:=0 to MaxSector-1 do begin
    if GCRReadSector(G64TrackData,TrackLen,Buffer,Track,Sector) then begin
     WriteSector(DstStream,Track,Sector,Buffer[1]);
    end else if GCRBitsReadSector(G64TrackData,TrackLen,Buffer,Track,Sector) then begin
     WriteSector(DstStream,Track,Sector,Buffer[1]);
    end else begin
     FillChar(Buffer,Sizeof(Buffer),#0);
     WriteSector(DstStream,Track,Sector,Buffer[1]);
    end;
   end;
  end;
 end;
 result:=true;
end;

function ConvertG64toD64(SrcStream,DstStream:TBeRoStream):boolean;
var GCR:PGCR;
begin
 New(GCR);
 try
  result:=G64Read(GCR^,SrcStream);
  if result then begin
   D64Write(GCR^,DstStream);
  end else begin
   DstStream.Assign(SrcStream);
  end;
 finally
  if assigned(GCR) then begin
   Dispose(GCR);
  end;
 end;
end;

function ConvertD64toG64(SrcStream,DstStream:TBeRoStream):boolean;
var GCR:PGCR;
begin
 New(GCR);
 try
  result:=D64Read(GCR^,SrcStream);
  if result then begin
   G64Write(GCR^,DstStream);
  end else begin
   DstStream.Assign(SrcStream);
  end;
 finally
  if assigned(GCR) then begin
   Dispose(GCR);
  end;
 end;
end;

function GetDirectory(Stream:TBeRoStream;var Directory:TDirectory):boolean;
var SrcStreamEx:TBeRoStream;
    SrcStreamEx2:TBeRoStream;
    SrcStreamEx3:TBeRoStream;
 function DoIt:boolean;
 type TBit16=packed record
       case boolean of
 {$IFDEF LITTLE_ENDIAN}
        true:(L,H:byte);
 {$ELSE}
        true:(H,L:byte);
 {$ENDIF}
        false:(W:word);
      end;
      TBAM=packed record
       Track:byte;
       Sector:byte;
       Format:ansichar;
       Zero:byte;
       SectorAllocationTable:packed array[1..35,1..4] of byte;
       Name:packed array[1..18] of ansichar;
       ID:TBit16;
       Space:byte;
       Chars:TBit16;
       Spaces:packed array[1..4] of byte;
       Zeros:packed array[1..85] of byte;
      end;
      TDirName=packed array[1..16] of ansichar;
      TDirEntry=packed record
       Kind:byte;
       Track:byte;
       Sector:byte;
       Name:TDirName;
       SideTrack:byte;
       SideSector:byte;
       Len:byte;
       Padding:longword;
       OverwriteTrack:byte;
       OverwriteSector:byte;
       Blocks:TBit16;
       Padding2:word;
      end;
      TDir=packed record
       NextTrack:byte;
       NextSector:byte;
       Entry:packed array[0..7] of TDirEntry;
      end;
 var Counter,Value,CharCounter,i,Lines,Size:longint;
     NumDirBlocks:longint;
     BAM:TBAM;
     Dir:TDir;
     NewLine:ansistring;
     EntryLineName:TDirName;
     Q:ansichar;
 begin
  result:=false;
  Directory.FirstLine:='';
  Size:=0;
  Lines:=0;
  setlength(Directory.Lines,0);
  Directory.LastLine:='';
  if not assigned(Stream) then begin
   exit;
  end;
  if not ReadSector(SrcStreamEx,18,0,BAM) then begin
   exit;
  end;
  NewLine:='';
  for I:=1 to length(BAM.Name) do begin
   if BAM.Name[I]=#$a0 then begin
    break;
   end;
   NewLine:=NewLine+BAM.Name[I];
  end;
  Directory.FirstLine:='0 "'+NewLine+'" '+AnsiChar(CHR(BAM.ID.L))+AnsiChar(CHR(BAM.ID.H))+AnsiChar(CHR(BAM.Space))+AnsiChar(CHR(BAM.CHARS.L))+AnsiChar(CHR(BAM.CHARS.H));
  for i:=1 to length(Directory.FirstLine) do begin
   if Directory.FirstLine[i]=#$a0 then begin
    Directory.FirstLine[i]:=' ';
   end;
  end;
  Dir.NextTrack:=BAM.Track;
  Dir.NextSector:=BAM.Sector;
  if (Dir.NextTrack>42) or (Dir.NextSector>SectorCount[((Dir.NextTrack-1) mod 42)+1]) then begin
   Dir.NextTrack:=18;
   Dir.NextSector:=1;
  end;
  NumDirBlocks:=0;
  while (Dir.NextTrack<>0) and (NumDirBlocks<SectorCount[18]) do begin
   if not ReadSector(SrcStreamEx,Dir.NextTrack,Dir.NextSector,Dir) then begin
    SrcStreamEx.Seek(SrcStreamEx.Size);
    break;
   end;
   inc(NumDirBlocks);
   for Counter:=0 to 7 do begin
    if Dir.Entry[Counter].Kind<>0 then begin
     NewLine:=inttostr(Dir.Entry[Counter].Blocks.W);
     while length(NewLine)<6 do begin
      NewLine:=NewLine+' ';
     end;
     NewLine:=NewLine+'"';
     EntryLineName:=Dir.Entry[Counter].Name;
     Q:='"';
     for CharCounter:=1 to sizeof(TDirName) do begin
      if EntryLineName[CharCounter]=#$a0 then begin
       NewLine:=NewLine+Q;
       Q:=' ';
      end else begin
       NewLine:=NewLine+EntryLineName[CharCounter];
      end;
     end;
     NewLine:=NewLine+Q;
     if (Dir.Entry[Counter].Kind and $80)=0 then begin
      NewLine:=NewLine+'*';
     end else begin
      NewLine:=NewLine+' ';
     end;               
     NewLine:=NewLine+KindName[Dir.Entry[Counter].Kind and $f];
     if (Dir.Entry[Counter].Kind and $40)<>0 then begin
      NewLine:=NewLine+'<';
     end;
     if Lines>=Size then begin
      if Size<16 then begin
       Size:=16;
      end;
      while Lines>=Size do begin
       inc(Size,Size);
      end;
      SetLength(Directory.Lines,Size);
     end;
     Directory.Lines[Lines]:=NewLine;
     inc(Lines);
    end;
   end;
  end;
  SetLength(Directory.Lines,Lines);
  Value:=FreeBlocks(SrcStreamEx);
  Directory.LastLine:=INTTOSTR(Value)+' BLOCKS FREE.';
  result:=Lines>0;
 end;
begin
 result:=false;
 try
  if assigned(Stream) then begin
   SrcStreamEx3:=TBeRoStream.Create;
   SrcStreamEx2:=TBeRoStream.Create;
   SrcStreamEx:=TBeRoStream.Create;
   try
    if ConvertFDItoP64(Stream,SrcStreamEx3) then begin
     if ConvertP64toG64(SrcStreamEx3,SrcStreamEx2,true) then begin
      if not ConvertG64toD64(SrcStreamEx2,SrcStreamEx) then begin
       SrcStreamEx.Assign(Stream);
      end;
     end else begin
      SrcStreamEx.Assign(Stream);
     end;
    end else begin
     SrcStreamEx2.Clear;
     if ConvertNIBtoG64(Stream,SrcStreamEx2) then begin
      if not ConvertG64toD64(SrcStreamEx2,SrcStreamEx) then begin
       SrcStreamEx.Assign(Stream);
      end;
     end else begin
      SrcStreamEx2.Clear;
      if ConvertP64toG64(Stream,SrcStreamEx2,true) then begin
       if not ConvertG64toD64(SrcStreamEx2,SrcStreamEx) then begin
        SrcStreamEx.Assign(Stream);
       end;
      end else if not ConvertG64toD64(Stream,SrcStreamEx) then begin
       SrcStreamEx.Assign(Stream);
      end;
     end;
    end;
    if D64Check(SrcStreamEx) then begin
     result:=DoIt;
    end;
   finally
    SrcStreamEx.Destroy;
    SrcStreamEx2.Destroy;
    SrcStreamEx3.Destroy;
   end;
  end;
 except
  FillChar(Directory,SizeOf(TDirectory),#0);
  result:=false;
 end;
end;

function GetFile(SrcStream,DstStream:TBeRoStream;FileName:ansistring):boolean;
var SrcStreamEx:TBeRoStream;
    SrcStreamEx2:TBeRoStream;
    SrcStreamEx3:TBeRoStream;
 function DoIt:boolean;
 type TBit16=packed record
       case boolean of
 {$IFDEF LITTLE_ENDIAN}
        true:(L,H:byte);
 {$ELSE}
        true:(H,L:byte);
 {$ENDIF}
        false:(W:word);
      end;
      TBAM=packed record
       Track:byte;
       Sector:byte;
       Format:ansichar;
       Zero:byte;
       SectorAllocationTable:packed array[1..35,1..4] of byte;
       Name:packed array[1..18] of ansichar;
       ID:TBit16;
       Space:byte;
       Chars:TBit16;
       Spaces:packed array[1..4] of byte;
       Zeros:packed array[1..85] of byte;
      end;
      TDirName=packed array[1..16] of ansichar;
      TDirEntry=packed record
       Kind:byte;
       Track:byte;
       Sector:byte;
       Name:TDirName;
       SideTrack:byte;
       SideSector:byte;
       Len:byte;
       Padding:longword;
       OverwriteTrack:byte;
       OverwriteSector:byte;
       Blocks:TBit16;
       Padding2:word;
      end;
      TDir=packed record
       NextTrack:byte;
       NextSector:byte;
       Entry:packed array[0..7] of TDirEntry;
      end;
 var Counter,CharCounter,Track,Sector:longint;
     NumDirBlocks:longint;
     BAM:TBAM;
     Dir:TDir;
     ThisFileName:ansistring;
     EntryLineName:TDirName;
     Block:TBlock;
 begin
  result:=false;
  if not assigned(SrcStream) then begin
   exit;
  end;
  if not assigned(DstStream) then begin
   exit;
  end;
  if not ReadSector(SrcStreamEx,18,0,BAM) then begin
   exit;
  end;
  Dir.NextTrack:=BAM.Track;
  Dir.NextSector:=BAM.Sector;
  if (Dir.NextTrack>42) or (Dir.NextSector>SectorCount[((Dir.NextTrack-1) mod 42)+1]) then begin
   Dir.NextTrack:=18;
   Dir.NextSector:=1;
  end;
  NumDirBlocks:=0;
  Track:=-1;
  Sector:=-1;
  while (Dir.NextTrack<>0) and (NumDirBlocks<SectorCount[18]) and (Track<0) and (Sector<0) do begin
   if not ReadSector(SrcStreamEx,Dir.NextTrack,Dir.NextSector,Dir) then begin
    SrcStreamEx.Seek(SrcStreamEx.Size);
    result:=false;
    exit;
   end;
   inc(NumDirBlocks);
   for Counter:=0 to 7 do begin
    if Dir.Entry[Counter].Kind<>0 then begin
     ThisFileName:='';
     EntryLineName:=Dir.Entry[Counter].Name;
     for CharCounter:=1 to sizeof(TDirName) do begin
      if EntryLineName[CharCounter]=#$a0 then begin
       break;
      end else begin
       ThisFileName:=ThisFileName+EntryLineName[CharCounter];
      end;
     end;
     if (ThisFileName=FileName) or (((((Pos('?',String(FileName))+Pos('*',String(FileName)))>0) and MatchFilePattern(pansichar(FileName),pansichar(ThisFileName))) or (FileName=':*')) and ((Dir.Entry[Counter].Kind and $f)=2)) then begin
      Track:=Dir.Entry[Counter].Track;
      Sector:=Dir.Entry[Counter].Sector;
      break;
     end;
    end;
   end;
  end;
  if (Track>0) and (Sector>=0) then begin
   while (Track>0) and (Sector>=0) do begin
    if not ReadSector(SrcStreamEx,Track,Sector,Block) then begin
     result:=false;
     exit;
    end;
    Track:=Block[0];
    Sector:=Block[1];
    if Track>0 then begin
     if DstStream.Write(Block[2],254)<>254 then begin
      result:=false;
      exit;
     end;
    end else begin
     if Sector>0 then begin
      dec(Sector);
      if DstStream.Write(Block[2],Sector)<>Sector then begin
       result:=false;
       exit;
      end;
     end else begin
      result:=false;
      exit;
     end;
     break;
    end;
   end;
   result:=true;
  end;
 end;
begin
 result:=false;
 try
  if assigned(SrcStream) then begin
   SrcStreamEx3:=TBeRoStream.Create;
   SrcStreamEx2:=TBeRoStream.Create;
   SrcStreamEx:=TBeRoStream.Create;
   try
    if ConvertFDItoP64(SrcStream,SrcStreamEx3) then begin
     if ConvertP64toG64(SrcStreamEx3,SrcStreamEx2,true) then begin
      if not ConvertG64toD64(SrcStreamEx2,SrcStreamEx) then begin
       SrcStreamEx.Assign(SrcStream);
      end;
     end else begin
      SrcStreamEx.Assign(SrcStream);
     end;
    end else begin
     SrcStreamEx2.Clear;
     if ConvertNIBtoG64(SrcStream,SrcStreamEx2) then begin
      if not ConvertG64toD64(SrcStreamEx2,SrcStreamEx) then begin
       SrcStreamEx.Assign(SrcStream);
      end;
     end else begin
      SrcStreamEx2.Clear;
      if ConvertP64toG64(SrcStream,SrcStreamEx2,true) then begin
       if not ConvertG64toD64(SrcStreamEx2,SrcStreamEx) then begin
        SrcStreamEx.Assign(SrcStream);
       end;
      end else if not ConvertG64toD64(SrcStream,SrcStreamEx) then begin
       SrcStreamEx.Assign(SrcStream);
      end;
     end;
    end;
    if D64Check(SrcStreamEx) then begin
     result:=DoIt;
    end;
   finally
    SrcStreamEx.Destroy;
    SrcStreamEx2.Destroy;
    SrcStreamEx3.Destroy;
   end;
  end;
 except
  FileName:='';
  result:=false;
 end;
end;

end.
