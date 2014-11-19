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
unit GCR;
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
{$J+}

interface

uses Globals,BeRoStream;

const MaxGCRHalfTracks=84;
      MaxBytesPerGCRHalfTrack=65536;
      MaxBytesPerGCRHalfTrackMask=MaxBytesPerGCRHalfTrack-1;

      FileMinMaxBytesPerGCRHalfTrack=7928;

      MaxTracks1541=42;

      MaxHalfTracks1541=MaxTracks1541*2;

      FirstHalfTrack=2;
      LastHalfTrack=FirstHalfTrack+MaxHalfTracks1541-1;

      NumTracks1541=35;
      ExtTracks1541=40;

      Sectors35=683;
      Sectors40=768;

      RawTrackSize:array[0..3] of word=(6250,6666,7142,7692);
      
      SectorCount:array[0..42] of byte=(0,
                                        21,21,21,21,21,21,21,21,21,21,
                                        21,21,21,21,21,21,21,19,19,19,
                                        19,19,19,19,18,18,18,18,18,18,
                                        17,17,17,17,17,17,17,17,17,17,
                                        17,17);

      SectorOfs:array[0..40] of integer=(0,
                                         0,21,42,63,84,105,126,147,168,189,210,
                                         231,252,273,294,315,336,357,376,395,
                                         414,433,452,471,490,508,526,544,562,
                                         580,598,615,632,649,666,683,700,717,
                                         734,751);

      TrackSpeedTable:array[0..43] of integer=(13,13,13,13,13,13,13,13,
                                               13,13,13,13,13,13,13,13,
                                               13,13,14,14,14,14,14,14,
                                               14,15,15,15,15,15,15,16,
                                               16,16,16,16,16,16,16,16,
                                               16,16,16,16);

      SpeedMap:array[0..MaxTracks1541+2] of byte=(3,
                                                  3,3,3,3,3,3,3,3,3,3,
                                                  3,3,3,3,3,3,3,2,2,2,
                                                  2,2,2,2,1,1,1,1,1,1,
                                                  0,0,0,0,0,
                                                  2,2,2,2,2,2,2{,0,0,0,0,0,0,0},
                                                  0,0);

      GCRConvData:array[0..$f] of byte=($0a,$0b,$12,$13,$0e,$0f,$16,$17,$09,$19,$1a,$1b,$0d,$1d,$1e,$15);
      FromGCRConvData:array[0..$1f] of byte=(0,0,0,0,0,0,0,0,0,8,0,1,0,12,4,5,0,0,2,3,0,15,6,7,0,9,10,11,0,13,14,0);

type TGCRHalfTrackSizes=array[0..MaxGCRHalfTracks+2] of longint;

     PGCR=^TGCR;
     TGCR=packed record
      Data:array[0..((MaxGCRHalfTracks+2)*MaxBytesPerGCRHalfTrack)-1] of byte;
      SpeedZone:array[0..((MaxGCRHalfTracks+2)*MaxBytesPerGCRHalfTrack)-1] of byte;
      HalfTrackSize:TGCRHalfTrackSizes;
      MaximumHalfTrackSize:longint;
      HalfTracks:byte;
     end;

     TGCRBytes=array of byte;

     PGCRSpeedMap=^TGCRSpeedMap;
     TGCRSpeedMap=array[0..((MaxBytesPerGCRHalfTrack+3) shr 2)-1] of byte;

     TGCRHeaderSignature=packed array[0..7] of ansichar;

     PGCRHeader=^TGCRHeader;
     TGCRHeader=packed record
      Signature:TGCRHeaderSignature;
      Version:byte;
      HalfTracks:byte;
      TrackSize:word;
     end;

     PGCRFile=^TGCRFile;
     TGCRFile=packed record
      Header:TGCRHeader;
      Data:TGCR;
     end;

const GCRHeaderSignature:TGCRHeaderSignature='GCR-1541';

procedure GCRConvert4BytesToGCR(var Buffer,Ptr);
procedure GCRConvertGCRTo4Bytes(var Buffer,Ptr);
procedure GCRConvertSectorToGCR(Buffer,Ptr:pbyte;Track,Sector:longword;DiskID1,DiskID2,ErrorCode:byte);
procedure GCRConvertGCRToSector(Buffer,Ptr,GCRTrackPointer:pbyte;GCRTrackSize:longword);
procedure GCRByteRealign(GCRTrackPointer:pbyte;GCRTrackSize:longword);
function GCRBitsReadSector(GCRTrackPointer:pbyte;GCRTrackSize:longword;var ReadData;Track,Sector:longword):boolean;
function GCRFindSectorHeader(Track,Sector:longword;GCRTrackPointer:pbyte;GCRTrackSize:longword):pbyte;
function GCRFindSectorData(Offset,GCRTrackPointer:pbyte;GCRTrackSize:longword):pbyte;
function GCRReadSector(GCRTrackPointer:pbyte;GCRTrackSize:longword;var ReadData;Track,Sector:longword):boolean;
function GCRWriteSector(GCRTrackPointer:pbyte;GCRTrackSize:longword;var WriteData;Track,Sector:longword):boolean;

implementation

const RandomSeed:longword=$c6c2de31;
      RandomSeedA:longword=$7b4d3ce3;
      RandomSeedB:longword=$8e37ab31;
      RandomSeedX:longword=$a4254e16;
      RandomSeedY:longword=$8b24f787;
      RandomSeedZ:longword=$d719ca32;
      RandomSeedW:longword=$f784d312;

function GenerateRandomValue:longword;
begin
 // My own variant of a "kiss it stupid simple" PRNG
 // A mixture of XorShift128 + LCG + MWC
 RandomSeed:=((RandomSeed*1664525)+1013904223)*134775813;
 RandomSeedA:=(65184*(RandomSeedA and 65535))+((RandomSeedA shr 16));
 RandomSeedB:=(64860*(RandomSeedB and 65535))+((RandomSeedB shr 16));
 result:=(RandomSeedX xor (RandomSeedX shl 11));
 RandomSeedX:=RandomSeedY;
 RandomSeedY:=RandomSeedZ;
 RandomSeedZ:=RandomSeedW;
 RandomSeedW:=(RandomSeedW xor (RandomSeedW shr 19)) xor (result xor (result shr 8));
 result:=(RandomSeedW+RandomSeed) xor (RandomSeedA+(RandomSeedB shl 16));
end;

procedure GCRConvert4BytesToGCR(var Buffer,Ptr);
var BufferByte:pbyte;
    PtrByte:pbyte;
    i:integer;
    td:longword;
begin
 BufferByte:=@Buffer;
 PtrByte:=@Ptr;
 td:=0;
 i:=2;
 while i<10 do begin
  td:=(td shl 5) or GCRConvData[BufferByte^ shr 4];
  td:=(td shl 5) or GCRConvData[BufferByte^ and $f];
  PtrByte^:=(td shr i) and $ff;
  inc(BufferByte);
  inc(PtrByte);
  inc(i,2);
 end;
 PtrByte^:=td and $ff;
end;

procedure GCRConvertGCRTo4Bytes(var Buffer,Ptr);
var BufferByte:pbyte;
    PtrByte:pbyte;
    i:integer;
    td:longword;
begin
 BufferByte:=@Buffer;
 PtrByte:=@Ptr;
 td:=BufferByte^ shl 13;
 i:=5;
 while i<13 do begin
  inc(BufferByte);
  td:=td or (BufferByte^ shl i);
  PtrByte^:=FromGCRConvData[(td shr 16) and $1f] shl 4;
  td:=td shl 5;
  PtrByte^:=PtrByte^ or FromGCRConvData[(td shr 16) and $1f];
  td:=td shl 5;
  inc(PtrByte);
  inc(i,2);
 end;
end;

procedure GCRConvertSectorToGCR(Buffer,Ptr:pbyte;Track,Sector:longword;DiskID1,DiskID2,ErrorCode:byte);
var Buf:array[0..3] of byte;
    HeaderID,Checksum:byte;
    Counter:integer;
begin
 if ErrorCode=29 then begin
  HeaderID:=DiskID1 xor $ff;
 end else begin
  HeaderID:=DiskID1;
 end;
 FillChar(Ptr^,5,#$ff);
 inc(Ptr,5);
 if ErrorCode=20 then begin
  Buf[0]:=$ff;
 end else begin
  Buf[0]:=$08;
 end;
 Buf[1]:=Sector xor Track xor DiskID2 xor HeaderID;
 Buf[2]:=Sector;
 Buf[3]:=Track;
 if ErrorCode=27 then begin
  Buf[1]:=Buf[1] xor $ff;
 end;
 GCRConvert4BytesToGCR(Buf,Ptr^);
 inc(Ptr,5);
 Buf[0]:=DiskID2;
 Buf[1]:=HeaderID;
 Buf[2]:=$0f;
 Buf[3]:=$0f;
 GCRConvert4BytesToGCR(Buf,Ptr^);
 inc(Ptr,5);
 FillChar(Ptr^,9,#$55);
 inc(Ptr,9);
 FillChar(Ptr^,5,#$ff);
 inc(Ptr,5);
 if ErrorCode=22 then begin
  PByteArray(Buffer)^[0]:=$ff;
 end else begin
  PByteArray(Buffer)^[0]:=$7;
 end;
 Checksum:=PByteArray(Buffer)^[1];
 for Counter:=2 to 256 do begin
  Checksum:=Checksum xor PByteArray(Buffer)^[Counter];
 end;
 if ErrorCode=23 then begin
  PByteArray(Buffer)^[257]:=Checksum xor $ff;
 end else begin
  PByteArray(Buffer)^[257]:=Checksum;
 end;
 PByteArray(Buffer)^[258]:=0;
 PByteArray(Buffer)^[259]:=0;
 for Counter:=0 to 64 do begin
  GCRConvert4BytesToGCR(Buffer^,Ptr^);
  inc(Buffer,4);
  inc(Ptr,5);
 end;
 FillChar(Ptr^,6,#$55);
end;

procedure GCRConvertGCRToSector(Buffer,Ptr,GCRTrackPointer:pbyte;GCRTrackSize:longword);
var Offset:pbyte;
    GCRTrackEnd:pbyte;
    GCRHeader:array[0..4] of byte;
    Counter,SubCounter:integer;
begin
 Offset:=Ptr;
 GCRTrackEnd:=GCRTrackPointer;
 inc(GCRTrackEnd,GCRTrackSize);
 for Counter:=0 to 64 do begin
  for SubCounter:=0 to 4 do begin
   GCRHeader[SubCounter]:=Offset^;
   inc(Offset);
   if longword(Offset)>=longword(GCRTrackEnd) then Offset:=GCRTrackPointer;
  end;
  GCRConvertGCRTo4Bytes(GCRHeader,Buffer^);
  inc(Buffer,4);
 end;
end;

procedure GCRByteRealign(GCRTrackPointer:pbyte;GCRTrackSize:longword);
type PBuffer=^TBuffer;
     TBuffer=array[0..$3fff] of byte;
var HeadBitLength,RemainHeadBits,HeadBitOffset,ReadShiftRegister,LowBitCount,BitCounter,BufferOffset,SyncMark,
    WrittenHighBits,HighBits:longword;
    Buffer:PBuffer;
    LastWasSyncMark,SyncMarkReadyForWrite,InSync:boolean;
begin
 New(Buffer);
 try
  FillChar(Buffer^,SizeOf(TBuffer),AnsiChar(#0));
  HeadBitLength:=GCRTrackSize shl 3;
  LowBitCount:=0;
  BitCounter:=0;
  BufferOffset:=0;
  ReadShiftRegister:=0;
  RemainHeadBits:=HeadBitLength shl 1;
  HeadBitOffset:=0;
  LastWasSyncMark:=false;
  SyncMarkReadyForWrite:=false;
  InSync:=false;
  SyncMark:=0;
  HighBits:=0;
  WrittenHighBits:=0;
  while (RemainHeadBits>0) and (BufferOffset<=high(TBuffer)) do begin
   ReadShiftRegister:=((ReadShiftRegister shl 1) and $3fe) or (PByteArray(GCRTrackPointer)^[HeadBitOffset shr 3] shr ((not HeadBitOffset) and 7));
   LowBitCount:=(LowBitCount and longword($ffffffff+(ReadShiftRegister and 1)))+1;
   InSync:=InSync and (LowBitCount<8);
   if (LowBitCount>8) and ((ReadShiftRegister and $3f)=8) and (GenerateRandomValue>=$c0000000) then begin
    // Too many low bits confuse the electronics, so return garbage random low and high bits,
    // but never more than three low bits in a row.
    ReadShiftRegister:=ReadShiftRegister or 1;
    if (BitCounter<7) and (GenerateRandomValue<$80000000) then begin
     inc(BitCounter);
     ReadShiftRegister:=(ReadShiftRegister shl 1) and $3fe;
    end;
   end else if (ReadShiftRegister and $f)=0 then begin
    ReadShiftRegister:=ReadShiftRegister or 1;
   end;
   if (ReadShiftRegister and 1)<>0 then begin
    inc(HighBits);
   end else begin
    HighBits:=0;
   end;
   if ReadShiftRegister=$3ff then begin
    // At least 10 high bits found -> so sync mark found!
    // That will remain so until a low bit is found, then -> so end of sync mark found!
    // Reset byte shift register bit counter for perfect synchronizing reading/writing
    BitCounter:=0;
    if LastWasSyncMark then begin
     inc(SyncMark);
    end else begin
     if WrittenHighBits<=HighBits then begin
      SyncMark:=HighBits-WrittenHighBits;
     end else begin
      SyncMark:=10;
     end;
     WrittenHighBits:=0;
    end;
    LastWasSyncMark:=true;
    SyncMarkReadyForWrite:=true;
    InSync:=true;
   end else begin
    if InSync then begin
     if (ReadShiftRegister and 1)<>0 then begin
      inc(WrittenHighBits);
     end else begin
      WrittenHighBits:=0;
     end;
    end;
    LastWasSyncMark:=false;
    inc(BitCounter);
    if BitCounter=8 then begin
     if InSync then begin
      if SyncMarkReadyForWrite then begin
       SyncMarkReadyForWrite:=false;
       SyncMark:=(SyncMark+7) shr 3;
       if SyncMark>0 then begin
        FillChar(Buffer^[BufferOffset],SyncMark,AnsiChar(#$ff));
        inc(BufferOffset,SyncMark);
        SyncMark:=0;
       end;
      end;
      Buffer^[BufferOffset]:=ReadShiftRegister and $ff;
      inc(BufferOffset);
     end;
     BitCounter:=0;
    end;
   end;
   inc(HeadBitOffset);
   while HeadBitOffset>=HeadBitLength do begin
    dec(HeadBitOffset,HeadBitLength);
   end;
   dec(RemainHeadBits);
  end;
  Move(Buffer[0],GCRTrackPointer^,GCRTrackSize);
 finally
  Dispose(Buffer);
 end;
end;

function GCRBitsReadSector(GCRTrackPointer:pbyte;GCRTrackSize:longword;var ReadData;Track,Sector:longword):boolean;
type PBuffer=^TBuffer;
     TBuffer=array[0..$3fff] of byte;
var HeadBitLength,RemainHeadBits,HeadBitOffset,ReadShiftRegister,LowBitCount,BitCounter,BufferOffset,SyncMark,
    WrittenHighBits,HighBits:longword;
    Buffer:PBuffer;
    LastWasSyncMark,SyncMarkReadyForWrite,InSync:boolean;
begin
 try
  New(Buffer);
  try
   FillChar(Buffer^,SizeOf(TBuffer),AnsiChar(#0));
   HeadBitLength:=GCRTrackSize shl 3;
   LowBitCount:=0;
   BitCounter:=0;
   BufferOffset:=0;
   ReadShiftRegister:=0;
   RemainHeadBits:=HeadBitLength shl 1;
   HeadBitOffset:=0;
   LastWasSyncMark:=false;
   SyncMarkReadyForWrite:=false;
   InSync:=false;
   SyncMark:=0;
   HighBits:=0;
   WrittenHighBits:=0;
   while (RemainHeadBits>0) and (BufferOffset<=high(TBuffer)) do begin
    ReadShiftRegister:=((ReadShiftRegister shl 1) and $3fe) or (PByteArray(GCRTrackPointer)^[HeadBitOffset shr 3] shr ((not HeadBitOffset) and 7));
    LowBitCount:=(LowBitCount and longword($ffffffff+(ReadShiftRegister and 1)))+1;
    InSync:=InSync and (LowBitCount<8);
    if (LowBitCount>8) and ((ReadShiftRegister and $3f)=8) and (GenerateRandomValue>=$c0000000) then begin
     // Too many low bits confuse the electronics, so return garbage random low and high bits,
     // but never more than three low bits in a row.
     ReadShiftRegister:=ReadShiftRegister or 1;
     if (BitCounter<7) and (GenerateRandomValue<$80000000) then begin
      inc(BitCounter);
      ReadShiftRegister:=(ReadShiftRegister shl 1) and $3fe;
     end;
    end else if (ReadShiftRegister and $f)=0 then begin
     ReadShiftRegister:=ReadShiftRegister or 1;
    end;
    if (ReadShiftRegister and 1)<>0 then begin
     inc(HighBits);
    end else begin
     HighBits:=0;
    end;
    if ReadShiftRegister=$3ff then begin
     // At least 10 high bits found -> so sync mark found!
     // That will remain so until a low bit is found, then -> so end of sync mark found!
     // Reset byte shift register bit counter for perfect synchronizing reading/writing
     BitCounter:=0;
     if LastWasSyncMark then begin
      inc(SyncMark);
     end else begin
      if WrittenHighBits<=HighBits then begin
       SyncMark:=HighBits-WrittenHighBits;
      end else begin
       SyncMark:=10;
      end;
      WrittenHighBits:=0;
     end;
     LastWasSyncMark:=true;
     SyncMarkReadyForWrite:=true;
     InSync:=true;
    end else begin
     if InSync then begin
      if (ReadShiftRegister and 1)<>0 then begin
       inc(WrittenHighBits);
      end else begin
       WrittenHighBits:=0;
      end;
     end;
     LastWasSyncMark:=false;
     inc(BitCounter);
     if BitCounter=8 then begin
      if InSync then begin
       if SyncMarkReadyForWrite then begin
        SyncMarkReadyForWrite:=false;
        SyncMark:=(SyncMark+7) shr 3;
        if SyncMark>0 then begin
         FillChar(Buffer^[BufferOffset],SyncMark,AnsiChar(#$ff));
         inc(BufferOffset,SyncMark);
         SyncMark:=0;
        end;
       end;
       Buffer^[BufferOffset]:=ReadShiftRegister and $ff;
       inc(BufferOffset);
      end;
      BitCounter:=0;
     end;
    end;
    inc(HeadBitOffset);
    while HeadBitOffset>=HeadBitLength do begin
     dec(HeadBitOffset,HeadBitLength);
    end;
    dec(RemainHeadBits);
   end;
   result:=GCRReadSector(@Buffer[0],BufferOffset,ReadData,Track,Sector);
  finally
   Dispose(Buffer);
  end;
 except
  result:=false;
 end;
end;

function GCRFindSectorHeader(Track,Sector:longword;GCRTrackPointer:pbyte;GCRTrackSize:longword):pbyte;
var Offset:pbyte;
    GCRTrackEnd:pbyte;
    GCRHeader:array[0..4] of byte;
    HeaderData:array[0..3] of byte;
    Counter:integer;
    SyncCount:longword;
    WrapOver:boolean;
begin
 Offset:=GCRTrackPointer;
 GCRTrackEnd:=GCRTrackPointer;
 inc(GCRTrackEnd,GCRTrackSize);
 WrapOver:=false;
 SyncCount:=0;
 while (longword(Offset)<longword(GCRTrackEnd)) and not WrapOver do begin
  while Offset^<>$ff do begin
   inc(Offset);
   if longword(Offset)>=longword(GCRTrackEnd) then begin
    result:=nil;
    exit;
   end;
  end;
  while Offset^=$ff do begin
   inc(Offset);
   if longword(Offset)=longword(GCRTrackEnd) then begin
    Offset:=GCRTrackPointer;
    WrapOver:=true;
   end;
   inc(SyncCount);
   if SyncCount>=GCRTrackSize then begin
    result:=nil;
    exit;
   end;
  end;
  for Counter:=0 to 4 do begin
   GCRHeader[Counter]:=Offset^;
   inc(Offset);
   if longword(Offset)>=longword(GCRTrackEnd) then begin
    Offset:=GCRTrackPointer;
    WrapOver:=true;
   end;
  end;
  GCRConvertGCRTo4Bytes(GCRHeader,HeaderData);
  if HeaderData[0]=$08 then begin
   if (HeaderData[2]=Sector) and (HeaderData[3]=Track) then begin
    result:=Offset;
    exit;
   end;
  end;
 end;
 result:=nil;
end;

function GCRFindSectorData(Offset,GCRTrackPointer:pbyte;GCRTrackSize:longword):pbyte;
var GCRTrackEnd:pbyte;
    Header:integer;
begin
 GCRTrackEnd:=GCRTrackPointer;
 inc(GCRTrackEnd,GCRTrackSize);
 Header:=0;
 while Offset^<>$ff do begin
  inc(Offset);
  if longword(Offset)>=longword(GCRTrackEnd) then begin
   Offset:=GCRTrackPointer;
  end;
  inc(Header);
  if Header>=500 then begin
   result:=nil;
   exit;
  end;
 end;
 while Offset^=$ff do begin
  inc(Offset);
  if longword(Offset)=longword(GCRTrackEnd) then begin
   Offset:=GCRTrackPointer;
  end;
 end;
 result:=Offset;
end;

function GCRReadSector(GCRTrackPointer:pbyte;GCRTrackSize:longword;var ReadData;Track,Sector:longword):boolean;
var Offset:pbyte;
    Buffer:array[0..259] of byte;
begin
 Offset:=GCRFindSectorHeader(Track,Sector,GCRTrackPointer,GCRTrackSize);
 if not assigned(Offset) then begin
  result:=false;
  exit;
 end;
 Offset:=GCRFindSectorData(Offset,GCRTrackPointer,GCRTrackSize);
 if not assigned(Offset) then begin
  result:=false;
  exit;
 end;
 GCRConvertGCRToSector(@Buffer,Offset,GCRTrackPointer,GCRTrackSize);
 if Buffer[0]<>$7 then begin
  result:=false;
  exit;
 end;
 Move(Buffer[0],ReadData,260);
 result:=true;
end;

function GCRWriteSector(GCRTrackPointer:pbyte;GCRTrackSize:longword;var WriteData;Track,Sector:longword):boolean;
var GCRTrackEnd,Offset,Buf,GCRData:pbyte;
    Buffer:array[0..259] of byte;
    GCRBuffer:array[0..324] of byte;
    Checksum:byte;
    Counter:integer;
begin
 Offset:=GCRFindSectorHeader(Track,Sector,GCRTrackPointer,GCRTrackSize);
 if not assigned(Offset) then begin
  result:=false;
  exit;
 end;
 Offset:=GCRFindSectorData(Offset,GCRTrackPointer,GCRTrackSize);
 if not assigned(Offset) then begin
  result:=false;
  exit;
 end;
 Buffer[0]:=$7;
 Move(WriteData,Buffer[1],256);
 Checksum:=Buffer[1];
 for Counter:=2 to 256 do begin
  Checksum:=Checksum xor Buffer[Counter];
 end;
 Buffer[257]:=Checksum;
 Buffer[258]:=0;
 Buffer[259]:=0;
 Buf:=@Buffer;
 GCRData:=@GCRBuffer;
 for Counter:=0 to 64 do begin
  GCRConvert4BytesToGCR(Buf^,GCRData^);
  inc(Buf,4);
  inc(GCRData,5);
 end;
 GCRTrackEnd:=GCRTrackPointer;
 inc(GCRTrackEnd,GCRTrackSize);
 for Counter:=0 to 324 do begin
  Offset^:=GCRBuffer[Counter];
  inc(Offset);
  if longword(Offset)>=longword(GCRTrackEnd) then begin
   Offset:=GCRTrackPointer;
  end;
 end;
 result:=true;
end;

end.
