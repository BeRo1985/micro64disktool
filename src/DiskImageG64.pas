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
unit DiskImageG64;
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

uses Globals,BeRoStream,GCR;

function G64IsEmpty(const Data;Len,Threshold:longint;Value:byte):boolean;
function G64IsEmptyEx(const Data;Len,Threshold:longint):boolean;
function G64CountHalfTracks(const GCR:TGCR):longint;
function G64Read(var GCR:TGCR;SrcStream:TBeRoStream):boolean;
function G64Write(const GCR:TGCR;DstStream:TBeRoStream):boolean;

implementation

function G64IsEmpty(const Data;Len,Threshold:longint;Value:byte):boolean;
var p:pbyte;
begin
 result:=true;
 p:=@Data;
 while Len>0 do begin
  dec(Len);
  if p^<>Value then begin
   dec(Threshold);
   if Threshold<0 then begin
    result:=false;
    exit;
   end;
  end;
  inc(p);
 end;
end;

function G64IsEmptyEx(const Data;Len,Threshold:longint):boolean;
var p:pbyte;
    Value:byte;
begin
 result:=true;
 if Len>0 then begin
  p:=@Data;
  Value:=p^;
  while Len>0 do begin
   dec(Len);
   if p^<>Value then begin
    dec(Threshold);
    if Threshold<0 then begin
     result:=false;
     exit;
    end;
   end;
   inc(p);
  end;
 end;
end;

function G64CountHalfTracks(const GCR:TGCR):longint;
var Track:longint;
begin
 result:=0;
 for Track:=FirstHalfTrack to LastHalfTrack do begin
  if not G64IsEmpty(GCR.Data[Track*MaxBytesPerGCRHalfTrack],GCR.HalfTrackSize[Track],2,$00) then begin
   result:=(Track-FirstHalfTrack)+1;
  end;
 end;
 if (result and 1)<>0 then begin
  result:=result+1;
 end;
 if result<70 then begin
  result:=70;
 end else if result>84 then begin
  result:=84;
 end;
end;

function G64Read(var GCR:TGCR;SrcStream:TBeRoStream):boolean;
var ImageHalfTracks,HalfTrack,TrackLen,ZoneLen,i:longint;
    G64Signature:array[1..8] of ansichar;
    GCRHalfTracks,GCRHalfTrackSpeeds:array[0..MaxHalfTracks1541] of longword;
    G64TrackData,G64ZoneData:pbyte;
    Len:array[0..1] of byte;
    CompSpeed:PGCRSpeedMap;
    Buffer:array[0..259] of byte;
begin
 result:=false;

 if SrcStream.Seek(0)<>0 then begin
  exit;
 end;

 if SrcStream.Read(G64Signature,sizeof(G64Signature))<>sizeof(G64Signature) then begin
  exit;
 end;
 if G64Signature<>'GCR-1541' then begin
  exit;
 end;

 if SrcStream.Read(Buffer,4)<>4 then begin
  exit;
 end;
 if not (Buffer[1] in [(NumTracks1541*2)-1..MaxHalfTracks1541]) then begin
  exit;
 end;
 ImageHalfTracks:=Buffer[1];
 GCR.HalfTracks:=Buffer[1];
 GCR.MaximumHalfTrackSize:=Buffer[2] or (Buffer[3] shl 8);

 if SrcStream.Seek(sizeof(TGCRHeader))<>sizeof(TGCRHeader) then begin
  exit;
 end;

 FillChar(GCRHalfTracks,SizeOf(GCRHalfTracks),#0);
 FillChar(GCRHalfTrackSpeeds,SizeOf(GCRHalfTrackSpeeds),#0);
 if SrcStream.Read(GCRHalfTracks,ImageHalfTracks*sizeof(longword))<>(ImageHalfTracks*sizeof(longword)) then begin
  exit;
 end;
 if SrcStream.Read(GCRHalfTrackSpeeds,ImageHalfTracks*sizeof(longword))<>(ImageHalfTracks*sizeof(longword)) then begin
  exit;
 end;

 FillChar(GCR.Data,SizeOf(GCR.Data),#$00);
 FillChar(GCR.SpeedZone,SizeOf(GCR.SpeedZone),#$00);

 for HalfTrack:=FirstHalfTrack to LastHalfTrack do begin
  G64TrackData:=@GCR.Data[HalfTrack*MaxBytesPerGCRHalfTrack];
  G64ZoneData:=@GCR.SpeedZone[HalfTrack*MaxBytesPerGCRHalfTrack];
  GCR.HalfTrackSize[HalfTrack]:=6250;
  if (HalfTrack<=ImageHalfTracks) and (GCRHalfTracks[HalfTrack-FirstHalfTrack]<>0) then begin
   if SrcStream.Seek(GCRHalfTracks[HalfTrack-FirstHalfTrack])<>longint(GCRHalfTracks[HalfTrack-FirstHalfTrack]) then begin
    exit;
   end;
   if SrcStream.Read(Len,sizeof(Len))<>sizeof(Len) then begin
    exit;
   end;
   TrackLen:=Len[0] or (Len[1] shl 8);
   if (TrackLen<1) or (TrackLen>MaxBytesPerGCRHalfTrack) then begin
    exit;
   end;
   GCR.HalfTrackSize[HalfTrack]:=TrackLen;
   if SrcStream.Read(G64TrackData^,TrackLen)<>TrackLen then begin
    exit;
   end;
   ZoneLen:=(TrackLen+3) div 4;
   if GCRHalfTrackSpeeds[HalfTrack-FirstHalfTrack]>3 then begin
    if SrcStream.Seek(GCRHalfTrackSpeeds[HalfTrack-FirstHalfTrack])<>longint(GCRHalfTrackSpeeds[HalfTrack-FirstHalfTrack]) then begin
     exit;
    end;
    GetMem(CompSpeed,SizeOf(TGCRSpeedMap));
    FillChar(CompSpeed^,SizeOf(TGCRSpeedMap),AnsiChar(#0));
    if SrcStream.Read(CompSpeed^,ZoneLen)<>ZoneLen then begin
     FreeMem(CompSpeed);
     exit;
    end;
    try
     for i:=0 to ZoneLen-1 do begin
      PByteArray(G64ZoneData)^[(i*4)+3]:=CompSpeed^[i] and 3;
      PByteArray(G64ZoneData)^[(i*4)+2]:=(CompSpeed^[i] shr 2) and 3;
      PByteArray(G64ZoneData)^[(i*4)+1]:=(CompSpeed^[i] shr 4) and 3;
      PByteArray(G64ZoneData)^[(i*4)+0]:=(CompSpeed^[i] shr 6) and 3;
     end;
    finally
     FreeMem(CompSpeed);
    end;
   end else begin
    FillChar(G64ZoneData^,MaxBytesPerGCRHalfTrack,chr(GCRHalfTrackSpeeds[HalfTrack-FirstHalfTrack]));
   end;
  end;
 end;

 result:=true;
end;

function G64Write(const GCR:TGCR;DstStream:TBeRoStream):boolean;
var HalfTrack,ImageHalfTracks,DataOffset,TrackLen,ZoneLen,Counter,MaximumHalfTrackSize:longint;
    G64Signature:array[1..8] of ansichar;
    Buffer:array[0..4] of byte;
    GCRHalfTracks,GCRHalfTrackSpeeds:array[0..MaxHalfTracks1541] of longword;
    G64TrackData,G64ZoneData:pbyte;
    CompSpeed:PGCRSpeedMap;
    Len:array[0..1] of byte;
    Zone:byte;
    Different:boolean;
begin
 result:=false;

 DstStream.Clear;

 if DstStream.Seek(0)<>0 then begin
  exit;
 end;

 MaximumHalfTrackSize:=GCR.MaximumHalfTrackSize;

 if MaximumHalfTrackSize=0 then begin
  for HalfTrack:=FirstHalfTrack to LastHalfTrack do begin
   G64TrackData:=@GCR.Data[HalfTrack*MaxBytesPerGCRHalfTrack];
   TrackLen:=GCR.HalfTrackSize[HalfTrack];
   if (TrackLen<>0) and not G64IsEmpty(G64TrackData^,TrackLen,0,$00) then begin
    if MaximumHalfTrackSize<TrackLen then begin
     MaximumHalfTrackSize:=TrackLen;
    end;
   end;
  end;
 end;

 G64Signature:='GCR-1541';
 if DstStream.Write(G64Signature,sizeof(G64Signature))<>sizeof(G64Signature) then begin
  exit;
 end;

 ImageHalfTracks:=G64CountHalfTracks(GCR);
 Buffer[0]:=0;
 Buffer[1]:=ImageHalfTracks;
 Buffer[2]:=MaximumHalfTrackSize and $ff;
 Buffer[3]:=(MaximumHalfTrackSize shr 8) and $ff;
 if DstStream.Write(Buffer,4)<>4 then begin
  exit;
 end;

 FillChar(GCRHalfTracks,SizeOf(GCRHalfTracks),#0);
 FillChar(GCRHalfTrackSpeeds,SizeOf(GCRHalfTrackSpeeds),#0);

 DataOffset:=DstStream.Position;
 if DstStream.Write(GCRHalfTracks,ImageHalfTracks*sizeof(longword))<>(ImageHalfTracks*sizeof(longword)) then begin
  exit;
 end;
 if DstStream.Write(GCRHalfTrackSpeeds,ImageHalfTracks*sizeof(longword))<>(ImageHalfTracks*sizeof(longword)) then begin
  exit;
 end;

 for HalfTrack:=FirstHalfTrack to LastHalfTrack do begin
  G64TrackData:=@GCR.Data[HalfTrack*MaxBytesPerGCRHalfTrack];
  G64ZoneData:=@GCR.SpeedZone[HalfTrack*MaxBytesPerGCRHalfTrack];
  TrackLen:=GCR.HalfTrackSize[HalfTrack];
  if (TrackLen<>0) and not G64IsEmpty(G64TrackData^,TrackLen,0,$00) then begin
   Len[0]:=TrackLen and $ff;
   Len[1]:=(TrackLen shr 8) and $ff;
   GCRHalfTracks[HalfTrack-FirstHalfTrack]:=DstStream.Position;
   if DstStream.Write(Len,sizeof(Len))<>sizeof(Len) then begin
    exit;
   end;
   if DstStream.Write(G64TrackData^,TrackLen)<>TrackLen then begin
    exit;
   end;
   if TrackLen<MaximumHalfTrackSize then begin
    if DstStream.WriteByteCount($00,MaximumHalfTrackSize-TrackLen)<>(MaximumHalfTrackSize-TrackLen) then begin
     exit;      
    end;
   end;
   begin
    Zone:=byte(pansichar(G64ZoneData)[0]) and 3;
    Different:=false;
    for Counter:=1 to TrackLen-1 do begin
     if (byte(pansichar(G64ZoneData)[Counter]) and 3)<>Zone then begin
      Different:=true;
      break;
     end;
    end;
    if Different then begin
     ZoneLen:=(TrackLen+3) div 4;
     GetMem(CompSpeed,SizeOf(TGCRSpeedMap));
     FillChar(CompSpeed^,SizeOf(TGCRSpeedMap),AnsiChar(#0));
     try
      for Counter:=0 to ZoneLen-1 do begin
       CompSpeed^[Counter]:=((PByteArray(G64ZoneData)^[(Counter*4)+3] and 3) shl 0) or
                            ((PByteArray(G64ZoneData)^[(Counter*4)+2] and 3) shl 2) or
                            ((PByteArray(G64ZoneData)^[(Counter*4)+1] and 3) shl 4) or
                            ((PByteArray(G64ZoneData)^[(Counter*4)+0] and 3) shl 6);
      end;
      GCRHalfTrackSpeeds[HalfTrack-FirstHalfTrack]:=DstStream.Position;
      result:=DstStream.Write(CompSpeed^,ZoneLen)=ZoneLen;
     finally
      FreeMem(CompSpeed);
     end;
     if not result then begin
      exit;
     end;
    end else begin
     GCRHalfTrackSpeeds[HalfTrack-FirstHalfTrack]:=Zone;
    end;
   end;
  end;
 end;

 if DstStream.Seek(DataOffset)<>DataOffset then begin
  exit;
 end;
 if DstStream.Write(GCRHalfTracks,ImageHalfTracks*sizeof(longword))<>(ImageHalfTracks*sizeof(longword)) then begin
  exit;
 end;
 if DstStream.Write(GCRHalfTrackSpeeds,ImageHalfTracks*sizeof(longword))<>(ImageHalfTracks*sizeof(longword)) then begin
  exit;
 end;
 if DstStream.Seek(DstStream.Size)<>DstStream.Size then begin
  exit;
 end;

 result:=true;
end;

end.
