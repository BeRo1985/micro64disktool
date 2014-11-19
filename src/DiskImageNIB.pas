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
unit DiskImageNIB;
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
{$define use64}

interface

uses SysUtils,BeRoStream,BeRoUtils,Classes,GCR;

type TNIBSignature=array[0..12] of ansichar;

const NIBSignature:TNIBSignature='MNIB-1541-RAW';

      MNIB_TRACK_LENGTH=$2000;
      MIN_TRACK_LENGTH=$1780;
      MATCH_LENGTH=7;

function NIBRead(var GCR:TGCR;Stream:TBeRoStream):boolean;

function ConvertNIBtoG64(SrcStream,DstStream:TBeRoStream):boolean;

implementation

uses DiskImageG64;

function Equals(Data:pansichar;FromPos,ToPos,Len:longint):boolean;
var i:longint;
begin
 result:=true;
 for i:=0 to Len-1 do begin
  if Data[FromPos+i]<>Data[ToPos+i] then begin
   result:=false;
   break;
  end;
 end;
end;

function FindSync(Data:pansichar;Pos,GCREnd:longint):longint;
begin
 while true do begin
  if (Pos+1)>=GCREnd then begin
   result:=-1;
   exit;
  end;
  if ((byte(Data[Pos+0]) and 3)=3) and (byte(Data[Pos+1])=$ff) then begin
   break;
  end;
  inc(Pos);
 end;
 inc(Pos);
 while (Pos<GCREnd) and (Data[Pos+1]=#$ff) do begin
  inc(Pos);
 end;
 if Pos<GCREnd then begin
  result:=Pos;
 end else begin
  result:=-1;
 end;
end;

function FindSector0(Data:pansichar;TrackLen:longint):longint;
var Pos,BufferEnd:longint;
begin
 Pos:=0;
 BufferEnd:=(TrackLen shl 1)-10;

 while Pos<BufferEnd do begin
  Pos:=FindSync(Data,Pos,BufferEnd);
  if Pos<0 then begin
   break;
  end;
  if (Data[Pos+0]=#$52) and ((byte(Data[Pos+1]) and $c0)=$40) and ((byte(Data[Pos+2]) and $0f)=$05) and ((byte(Data[Pos+3]) and $fc)=$28) then begin
   break;
  end;
 end;

 repeat
  dec(Pos);
  if Pos<=0 then begin
   inc(Pos,TrackLen);
  end;
 until Data[Pos]<>#$ff;

 inc(Pos);
 while Pos>=TrackLen do begin
  dec(Pos,TrackLen);
 end;

 result:=Pos;
end;

function FindSectorGap(Data:pansichar;TrackLen:longint):longint;
var SyncMax,Pos,BufferEnd,SyncLast,MaxGap,Gap:longint;
begin
 SyncMax:=0;
 Pos:=0;
 BufferEnd:=(TrackLen shl 1)-10;

 Pos:=FindSync(Data,Pos,BufferEnd);
 if Pos<0 then begin
  result:=-1;
  exit;
 end;

 SyncLast:=Pos;
 MaxGap:=0;
 while Pos<BufferEnd do begin
  Pos:=FindSync(Data,Pos,BufferEnd);
  if Pos<0 then begin
   break;
  end;
  Gap:=Pos-SyncLast;
  if MaxGap<Gap then begin
   MaxGap:=Gap;
   SyncMax:=Pos;
  end;
  SyncLast:=Pos;
 end;

 if MaxGap=0 then begin
  result:=-1;
  exit;
 end;

 Pos:=SyncMax;
 repeat
  dec(Pos);
  if Pos<=0 then begin
   inc(Pos,TrackLen);
  end;
 until Data[Pos]<>#$ff;

 inc(Pos);
 while Pos>=TrackLen do begin
  dec(Pos,TrackLen);
 end;

 result:=Pos;
end;

function FindTrackCycle(Data:pansichar;var CycleStart,CycleStop:longint):longint;
var NIBTrack,StopPos,StartPos,SyncPos,p1,CyclePos,p2:longint;
begin
 NIBTrack:=CycleStart;
 StopPos:=(NIBTrack+MNIB_TRACK_LENGTH)-MATCH_LENGTH;
 StartPos:=NIBTrack;
 while true do begin
  SyncPos:=StartPos+MIN_TRACK_LENGTH;
  if SyncPos>=StopPos then begin
   CycleStop:=CycleStart;
   result:=0;
   exit;
  end;
  while true do begin
   SyncPos:=FindSync(Data,SyncPos,StopPos);
   if SyncPos<0 then begin
    break;
   end;
   p1:=StartPos;
   CyclePos:=SyncPos;
   p2:=CyclePos;
   while p2<StopPos do begin
    if not Equals(Data,p1,p2,MATCH_LENGTH) then begin
     CyclePos:=-1;
     break;
    end;
    p1:=FindSync(Data,p1,StopPos);
    if p1<0 then begin
     break;
    end;
    p2:=FindSync(Data,p2,StopPos);
    if p2<0 then begin
     break;
    end;
   end;
   if CyclePos>=0 then begin
    CycleStart:=StartPos;
    CycleStop:=CyclePos;
    result:=CyclePos-StartPos;
    exit;
   end;
  end;
  StartPos:=FindSync(Data,StartPos,StopPos);
  if StartPos<0 then begin
   StartPos:=StopPos;
  end;
 end;
end;

function NIBRead(var GCR:TGCR;Stream:TBeRoStream):boolean;
var FileNIBSignature:TNIBSignature;
    HalfTrack,CycleStart,CycleStop,Sector0Pos,SectorGapPos,TrackLen,HeaderOffset:longint;
    Header:array[0..$ff] of byte;
    Buffer,OtherBuffer,G64TrackData,G64ZoneData:pointer;
begin
 result:=false;

 if Stream.Seek(0)<>0 then begin
  exit;
 end;

 if Stream.Read(FileNIBSignature,SizeOf(TNIBSignature))<>SizeOf(TNIBSignature) then begin
  exit;
 end;

 if FileNIBSignature<>NIBSignature then begin
  exit;
 end;

 if Stream.Seek(0)<>0 then begin
  exit;
 end;

 if Stream.Read(Header,SizeOf(Header))<>SizeOf(Header) then begin
  exit;
 end;

 GetMem(Buffer,$2000+MaxBytesPerGCRHalfTrack);
 GetMem(OtherBuffer,$8000+MaxBytesPerGCRHalfTrack);

 FillChar(GCR.Data,SizeOf(GCR.Data),#$00);
 FillChar(GCR.SpeedZone,SizeOf(GCR.SpeedZone),#$00);
 GCR.MaximumHalfTrackSize:=FileMinMaxBytesPerGCRHalfTrack;

 HeaderOffset:=16;
 for HalfTrack:=2 to 84 do begin
  G64TrackData:=@GCR.Data[HalfTrack*MaxBytesPerGCRHalfTrack];
  G64ZoneData:=@GCR.SpeedZone[HalfTrack*MaxBytesPerGCRHalfTrack];
  GCR.HalfTrackSize[HalfTrack]:=RawTrackSize[SpeedMap[HalfTrack shr 1]];
  if (Header[HeaderOffset]=HalfTrack) and (Stream.Read(Buffer^,$2000)=$2000) then begin
   inc(HeaderOffset,2);
   CycleStart:=0;
   CycleStop:=0;
   TrackLen:=FindTrackCycle(Buffer,CycleStart,CycleStop);
   if TrackLen>0 then begin
    Move(PAnsiChar(Buffer)[CycleStart],PAnsiChar(OtherBuffer)[0],TrackLen);
    Move(PAnsiChar(Buffer)[CycleStart],PAnsiChar(OtherBuffer)[TrackLen],TrackLen);

    GCR.HalfTrackSize[HalfTrack]:=TrackLen;
    FillChar(G64TrackData^,MaxBytesPerGCRHalfTrack,$ff);
    FillChar(G64ZoneData^,MaxBytesPerGCRHalfTrack,ansichar(byte(Header[17+(HalfTrack-2)] and 3)));

    SectorGapPos:=FindSectorGap(OtherBuffer,TrackLen);
    if SectorGapPos>=0 then begin
     Move(PAnsiChar(OtherBuffer)[SectorGapPos],G64TrackData^,TrackLen);
    end else begin
     Sector0Pos:=FindSector0(OtherBuffer,TrackLen);
     if Sector0Pos>=0 then begin
      Move(PAnsiChar(OtherBuffer)[Sector0Pos],G64TrackData^,TrackLen);
     end else begin
      Move(PAnsiChar(OtherBuffer)[0],G64TrackData^,TrackLen);
     end;
    end;
   end;
  end else begin
   TrackLen:=0;
  end;
  if TrackLen=0 then begin
   FillChar(G64TrackData^,MaxBytesPerGCRHalfTrack,$55);
   FillChar(Buffer^,$2000,$55);
   byte(Buffer^):=$ff;
   Move(Buffer^,G64TrackData^,$2000);
   FillChar(G64ZoneData^,MaxBytesPerGCRHalfTrack,ansichar(byte(SpeedMap[HalfTrack shr 1])));
  end;
  if GCR.MaximumHalfTrackSize<TrackLen then begin
   GCR.MaximumHalfTrackSize:=TrackLen;
  end;
 end;

 FreeMem(Buffer);
 FreeMem(OtherBuffer);

 result:=true;
end;

function ConvertNIBtoG64(SrcStream,DstStream:TBeRoStream):boolean;
var GCR:PGCR;
begin
 New(GCR);
 try
  result:=NIBRead(GCR^,SrcStream);
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

end.
