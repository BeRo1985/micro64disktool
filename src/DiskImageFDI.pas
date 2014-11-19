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
unit DiskImageFDI;
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

uses SysUtils,BeRoStream,BeRoUtils,Classes,GCR,DiskImageP64,CheckSumUtils;

type TFDISignature=array[0..26] of ansichar;

     TFDICreator=array[0..29] of ansichar;

     TFDICRLF=array[0..1] of ansichar;

     TFDIComment=array[0..79] of ansichar;

     TFDIVersion=array[0..1] of byte;

     TFDITrackDescription=packed record
      TrackType:byte;
      TrackSize:byte;
     end;

     TFDITrackDescriptions=array of TFDITrackDescription;

     TFDIFirstTrackDescriptions=array[0..179] of TFDITrackDescription;

     TFDIHeader=packed record
      case boolean of
       false:(
         Signature:TFDISignature;
         Creator:TFDICreator;
         CRLF:TFDICRLF;
         Comment:TFDIComment;
         EOFByte:byte;
         Version:TFDIVersion;
         LastTrackHi:byte;
         LastTrackLo:byte;
         LastHead:byte;
         DiskImage:byte;
         RotationSpeed:byte;
         Flags:byte;
         TPI:byte;
         HeadWidth:byte;
         Reserved:word;
         TrackDescriptions:TFDIFirstTrackDescriptions;
        );
       true:(
         Alignment:array[0..503] of byte;
         DataCRC32:longword;
         HeaderCRC32:longword;
        );
     end;

     TFDIDataTrack=packed record
      Size:longword;
      IndexPos:longword;
     end;

const FDISignature:TFDISignature='Formatted Disk Image file'#$0d#$0a;

function FDIRead(P64Image:TP64Image;Stream:TBeRoStream):boolean;
function FDIWrite(P64Image:TP64Image;Stream:TBeRoStream):boolean;

function DumpFDI(SrcStream,DstStream:TBeRoStream):boolean;

function ConvertFDItoP64(SrcStream,DstStream:TBeRoStream):boolean;
function ConvertP64toFDI(SrcStream,DstStream:TBeRoStream):boolean;

implementation

type TFDIPulseTrackHeader=record
      NumPulses:array[0..3] of byte;
      AverageSize:array[0..2] of byte;
      MinSize:array[0..2] of byte;
      MaxSize:array[0..2] of byte;
      IndexSize:array[0..2] of byte;
     end;

const TPIScales:array[0..7] of longint=(48,67,96,100,135,192,192,192);

function FDIReadPulseTrack(P64Image:TP64Image;Stream:TBeRoStream;HalfTrack,HeadWidthScale:longint;IndexSynchronized:boolean):boolean;
type TLongwords=array of longword;
     TInt64s=array of int64;
var PulseTrackHeader:TFDIPulseTrackHeader;
    NumPulses,AverageSize,MinSize,MaxSize,IndexSize:longword;
    AverageStream:TLongwords;
    MinStream:TLongwords;
    MaxStream:TLongwords;
    IndexStream:TLongwords;
    AbsolutePulses:TInt64s;
    AbsoluteStrengths:TInt64s;
 function ExpandHuffman(InputBuffer:PAnsiChar;InputLen,RealInputLen:longword;var OutputBuffer:array of longword):boolean;
 type PNode=^TNode;
      TNode=record
       Left,Right:PNode;
       Value:word;
      end;
 var Root:TNode;
     Bits,BitMask:byte;
     SignExtend,SixteenBits:boolean;
  procedure ExtractTree(Node:PNode);
  var Value:byte;
  begin
   if BitMask=0 then begin
    Bits:=byte(ansichar(InputBuffer^));
    inc(InputBuffer);
    BitMask:=$80;
   end;
   Value:=Bits and BitMask;
   BitMask:=BitMask shr 1;
   if Value=0 then begin
    New(Node^.Left);
    FillChar(Node^.Left^,SizeOf(TNode),#0);
    ExtractTree(Node^.Left);

    New(Node^.Right);
    FillChar(Node^.Right^,SizeOf(TNode),#0);
    ExtractTree(Node^.Right);
   end;
  end;
  procedure ExtractValues16(Node:PNode);
  begin
   if assigned(Node^.Left) then begin
    ExtractValues16(Node^.Left);
    ExtractValues16(Node^.Right);
   end else begin
    Node^.Value:=byte(ansichar(InputBuffer^)) shl 8;
    inc(InputBuffer);
    Node^.Value:=Node^.Value or byte(ansichar(InputBuffer^));
    inc(InputBuffer);
   end;
  end;
  procedure ExtractValues8(Node:PNode);
  begin
   if assigned(Node^.Left) then begin
    ExtractValues8(Node^.Left);
    ExtractValues8(Node^.Right);
   end else begin
    Node^.Value:=byte(ansichar(InputBuffer^));
    inc(InputBuffer);
   end;
  end;
  procedure FreeNode(Node:PNode);
  begin
   if assigned(Node) then begin
    FreeNode(Node^.Left);
    FreeNode(Node^.Right);
    Dispose(Node);
   end;
  end;
  function DoIt:boolean;
  var i,SubStreamShift:longint;
      OriginalInputBuffer:PAnsiChar;
      OutVal:Longword;
      Current:PNode;
  begin
   result:=false;

   OriginalInputBuffer:=InputBuffer;

   SubStreamShift:=1;
   while SubStreamShift<>0 do begin
    if longword(InputBuffer-OriginalInputBuffer)>=InputLen then begin
     exit;
    end;

    Bits:=byte(ansichar(InputBuffer^));
    inc(InputBuffer);
    SubStreamShift:=Bits and $7f;
    SignExtend:=(Bits and $80)<>0;

    Bits:=byte(ansichar(InputBuffer^));
    inc(InputBuffer);
    SixteenBits:=(Bits and $80)<>0;

    Root.Left:=nil;
    Root.Right:=nil;
    BitMask:=0;
    ExtractTree(@Root);

    if SixteenBits then begin
     ExtractValues16(@Root);
    end else begin
     ExtractValues8(@Root);
    end;

    if longword(InputBuffer-OriginalInputBuffer)>=InputLen then begin
     exit;
    end;

    BitMask:=0;
    for i:=0 to NumPulses-1 do begin
     Current:=@Root;

     while assigned(Current^.Left) do begin
      BitMask:=BitMask shr 1;
      if BitMask=0 then begin
       if longword(InputBuffer-OriginalInputBuffer)>=InputLen then begin
        Bits:=0;
       end else begin
        Bits:=byte(ansichar(InputBuffer^));
       end;
       inc(InputBuffer);
       BitMask:=$80;
      end;
      if (Bits and BitMask)<>0 then begin
       Current:=Current^.Right;
      end else begin
       Current:=Current^.Left;
      end;
     end;

     OutVal:=OutputBuffer[i];
     if SignExtend then begin
      if SixteenBits then begin
       if (Current^.Value and $8000)<>0 then begin
        OutVal:=OutVal or (($ffff0000 or Current^.Value) shl SubStreamShift);
       end else begin
        OutVal:=OutVal or (Current^.Value shl SubStreamShift);
       end;
      end else begin
       if (Current^.Value and $80)<>0 then begin
        OutVal:=OutVal or (($ffffff00 or Current^.Value) shl SubStreamShift);
       end else begin
        OutVal:=OutVal or (Current^.Value shl SubStreamShift);
       end;
      end;
     end else begin
      OutVal:=OutVal or (Current^.Value shl SubStreamShift);
     end;
     OutputBuffer[i]:=OutVal;
    end;

    FreeNode(Root.Left);
    FreeNode(Root.Right);
    Root.Left:=nil;
    Root.Right:=nil;

   end;

{  if longword(InputBuffer-OriginalInputBuffer)<>InputLen then begin
    exit;
   end;}

   result:=true;
  end;
 begin
  FillChar(Root,SizeOf(TNode),#0);
  try
   result:=DoIt;
  finally
   FreeNode(Root.Left);
   FreeNode(Root.Right);
  end;
 end;
 function UncompressStream(var DataStream:array of longword;Size,CompressionType,Width:longword):boolean;
 var i,lw,RealSize:longword;
     w:word;
     Buffer:pointer;
 begin
  result:=false;
  if Size=0 then begin
   result:=true;
  end else begin
   case CompressionType of
    0:begin
     if (Width*NumPulses)=Size then begin
      case Width of
       2:begin
        for i:=1 to NumPulses do begin
         if Stream.Read(w,sizeof(word))<>sizeof(word) then begin
          exit;
         end;
         DataStream[i-1]:=SwapWordBigEndian(w);
        end;
        result:=true;
       end;
       4:begin
        for i:=1 to NumPulses do begin
         if Stream.Read(lw,sizeof(longword))<>sizeof(longword) then begin
          exit;
         end;
         DataStream[i-1]:=SwapDWordBigEndian(lw);
        end;
        result:=true;
       end;
      end;
     end;
    end;
    1:begin
     GetMem(Buffer,Size);
     try
      RealSize:=longword(Stream.Read(Buffer^,Size));
      if (RealSize>0) and (RealSize<=Size) then begin
       for i:=1 to NumPulses do begin
        DataStream[i-1]:=0;
       end;
       result:=ExpandHuffman(Buffer,RealSize,Size,DataStream);
      end;
     finally
      FreeMem(Buffer);
     end;
    end;
   end;
  end;
 end;
 function DoIt:boolean;
 var i,j:longint;
     Sum,MaxIndex,Strength:longword;
     TotalLen,Offset,PulsePosition:int64;
 begin
  result:=false;

  SetLength(AverageStream,NumPulses);
  SetLength(MinStream,NumPulses);
  SetLength(MaxStream,NumPulses);
  SetLength(IndexStream,NumPulses);
  SetLength(AbsolutePulses,NumPulses);
  SetLength(AbsoluteStrengths,NumPulses);

  if not UncompressStream(AverageStream,AverageSize and $3fffff,(AverageSize and $c00000) shr 22,4) then begin
   exit;
  end;
  if not UncompressStream(MinStream,MinSize and $3fffff,(MinSize and $c00000) shr 22,4) then begin
   exit;
  end;
  if not UncompressStream(MaxStream,MaxSize and $3fffff,(MaxSize and $c00000) shr 22,4) then begin
   exit;
  end;
  if not UncompressStream(IndexStream,IndexSize and $3fffff,(IndexSize and $c00000) shr 22,4) then begin
   IndexSize:=0;
   //exit;
  end;

  if ((MinSize and $3fffff)<>0) and ((MaxSize and $3fffff)<>0) then begin
   for i:=0 to NumPulses-1 do begin
    MaxStream[i]:=(AverageStream[i]+MinStream[i])-MaxStream[i];
    MinStream[i]:=AverageStream[i]-MinStream[i];
   end;
  end else begin
   MinStream:=copy(AverageStream);
   MaxStream:=copy(AverageStream);
  end;

  if (IndexSize and $3fffff)=0 then begin
   for i:=0 to NumPulses-1 do begin
    IndexStream[i]:=$0200;
   end;
  end;

  MaxIndex:=0;
  for i:=0 to NumPulses-1 do begin
   Sum:=((IndexStream[i] and $ff00) shr 8)+(IndexStream[i] and $ff);
   if MaxIndex<Sum then begin
    MaxIndex:=Sum;
   end;
  end;

  TotalLen:=0;
  PulsePosition:=0;
  for i:=0 to NumPulses-1 do begin
   Sum:=((IndexStream[i] and $ff00) shr 8)+(IndexStream[i] and $ff);
   if MaxIndex=Sum then begin
    inc(TotalLen,AverageStream[i]);
    inc(PulsePosition,AverageStream[i]);
    AbsolutePulses[i]:=PulsePosition;
    AbsoluteStrengths[i]:=$ffffffff;
   end else begin
    AbsolutePulses[i]:=PulsePosition+AverageStream[i];
    if MaxIndex>0 then begin
     AbsoluteStrengths[i]:=(Sum*int64($ffffffff)) div MaxIndex;
    end else begin
     AbsoluteStrengths[i]:=$80000000;
    end;
   end;
  end;

  if TotalLen>0 then begin

   Offset:=0;
   if IndexSynchronized then begin
    i:=0;
    while (i<longint(NumPulses)) and ((IndexStream[i] and $ff)<>0) do begin
     inc(i);
    end;
    if i<longint(NumPulses) then begin
     j:=i;
     repeat
      inc(i);
      if i>=longint(NumPulses) then begin
       i:=0;
      end;
     until (i=j) or ((IndexStream[i] and $ff)<>0);
     if i<>j then begin
      while (i<>j) and (((IndexStream[i] shr 8) and $ff)>(IndexStream[i] and $ff)) do begin
       inc(i);
       if i>=longint(NumPulses) then begin
        i:=0;
       end;
      end;
      j:=i;
      Offset:=0;
      for i:=0 to j do begin
       inc(Offset,AverageStream[i]);
      end;
      Offset:=TotalLen-Offset;
     end;
    end;
   end;

   for i:=0 to NumPulses-1 do begin
    Strength:=AbsoluteStrengths[i];
    if Strength<>0 then begin
     PulsePosition:=AbsolutePulses[i]+Offset;
     while PulsePosition>=TotalLen do begin
      dec(PulsePosition,TotalLen);
     end;
     PulsePosition:=(PulsePosition*P64PulseSamplesPerRotation) div TotalLen;
     while PulsePosition>=P64PulseSamplesPerRotation do begin
      dec(PulsePosition,P64PulseSamplesPerRotation);
     end;
     P64Image.PulseStreams[HalfTrack].AddPulse(PulsePosition,Strength);
    end;
   end;

  end;

//writeln(TotalLen);

  result:=true;
 end;
begin
 result:=false;

 if Stream.Read(PulseTrackHeader,SizeOf(TFDIPulseTrackHeader))<>SizeOf(TFDIPulseTrackHeader) then begin
  exit;
 end;

 NumPulses:=(PulseTrackHeader.NumPulses[0] shl 24) or (PulseTrackHeader.NumPulses[1] shl 16) or (PulseTrackHeader.NumPulses[2] shl 8) or (PulseTrackHeader.NumPulses[3] shl 0);
 AverageSize:=(PulseTrackHeader.AverageSize[0] shl 16) or (PulseTrackHeader.AverageSize[1] shl 8) or (PulseTrackHeader.AverageSize[2] shl 0);
 MinSize:=(PulseTrackHeader.MinSize[0] shl 16) or (PulseTrackHeader.MinSize[1] shl 8) or (PulseTrackHeader.MinSize[2] shl 0);
 MaxSize:=(PulseTrackHeader.MaxSize[0] shl 16) or (PulseTrackHeader.MaxSize[1] shl 8) or (PulseTrackHeader.MaxSize[2] shl 0);
 IndexSize:=(PulseTrackHeader.IndexSize[0] shl 16) or (PulseTrackHeader.IndexSize[1] shl 8) or (PulseTrackHeader.IndexSize[2] shl 0);

 AverageStream:=nil;
 MinStream:=nil;
 MaxStream:=nil;
 IndexStream:=nil;
 AbsolutePulses:=nil;
 AbsoluteStrengths:=nil;
 try
  result:=DoIt;
 finally
  SetLength(AverageStream,0);
  SetLength(MinStream,0);
  SetLength(MaxStream,0);
  SetLength(IndexStream,0);
  SetLength(AbsolutePulses,0);
  SetLength(AbsoluteStrengths,0);
 end;

end;

function FDIRead(P64Image:TP64Image;Stream:TBeRoStream):boolean;
var FDIHeader:TFDIHeader;
    FDITrack,Size,HalfTrack,Sector,MaxSector,RC,Count:longint;
    TrackStream:TBeRoStream;
    OK,IndexSynchronized:boolean;
    GCR:PGCR;
    Ptr:pointer;
    Buffer:array[0..260] of byte;
    DataTrack:TFDIDataTrack;
    TrackDescriptions:TFDITrackDescriptions;
begin
 result:=false;

 TrackDescriptions:=nil;

 FillChar(FDIHeader,SizeOf(TFDIHeader),#0);

 if Stream.Seek(0)<>0 then begin
  exit;
 end;
 if Stream.Read(FDIHeader,SizeOf(TFDIHeader))<>SizeOf(TFDIHeader) then begin
  exit;
 end;
 if FDIHeader.Signature<>FDISignature then begin
  exit;
 end;
 if (FDIHeader.CRLF[0]<>#$0d) or (FDIHeader.CRLF[1]<>#$0a) then begin
  exit;
 end;
 if FDIHeader.EOFByte<>$1a then begin
  exit;
 end;
 if (FDIHeader.Version[0]<>$02) or not (FDIHeader.Version[1] in [$00,$01]) then begin
  exit;
 end;
 if (FDIHeader.LastTrackHi=0) and (FDIHeader.LastTrackLo=0) then begin
  exit;
 end;
 if FDIHeader.LastHead<>$00 then begin
  exit;
 end;
 if FDIHeader.DiskImage<>$01 then begin
  exit;
 end;

 SetLength(TrackDescriptions,longint(FDIHeader.LastTrackLo or (FDIHeader.LastTrackHi shl 8))+1);

 Count:=length(TrackDescriptions);
 if Count>180 then begin
  Count:=180;
 end;
 Move(FDIHeader.TrackDescriptions[0],TrackDescriptions[0],Count*SizeOf(TFDITrackDescription));
 if length(TrackDescriptions)>180 then begin
  Count:=length(TrackDescriptions)-180;
  if Stream.Read(TrackDescriptions[180],Count*SizeOf(TFDITrackDescription))<>(Count*SizeOf(TFDITrackDescription)) then begin
   SetLength(TrackDescriptions,0);
   exit;
  end;
  if (Stream.Position and 511)<>0 then begin
   Count:=Stream.Position-(512-(Stream.Position and 511));
   if Stream.Seek(Count)<>Count then begin
    SetLength(TrackDescriptions,0);
    exit;
   end;
  end;
 end;

 P64Image.Clear;
 P64Image.WriteProtected:=(FDIHeader.Flags and 1)<>0;
 IndexSynchronized:=(FDIHeader.Flags and 2)<>0;

 for FDITrack:=0 to length(TrackDescriptions)-1 do begin
  HalfTrack:=((FDITrack*96) div TPIScales[FDIHeader.TPI and 7])+2;
  if (TrackDescriptions[FDITrack].TrackSize>0) and ((HalfTrack>=0) and (HalfTrack<=MaxGCRHalfTracks)) then begin
   OK:=false;

   TrackStream:=TBeRoMemoryStream.Create;
   try

    case TrackDescriptions[FDITrack].TrackType of
     $80..$bf:begin
      Size:=((TrackDescriptions[FDITrack].TrackType and $3f) shl 8) or TrackDescriptions[FDITrack].TrackSize;
     end;
     else begin
      Size:=TrackDescriptions[FDITrack].TrackSize;
     end;
    end;

    Size:=Size shl 8;

    if TrackStream.AppendFrom(Stream,Size)=Size then begin
     if TrackStream.Seek(0)=0 then begin

      case TrackDescriptions[FDITrack].TrackType of
       $00:begin
        OK:=true;
       end;
       $0a:begin
        New(GCR);
        try
         FillChar(GCR^.Data,SizeOf(GCR^.Data),#$00);
         FillChar(GCR^.SpeedZone,SizeOf(GCR^.SpeedZone),#$00);
         GCR.MaximumHalfTrackSize:=$ffff;
         GCR^.HalfTrackSize[HalfTrack]:=RawTrackSize[SpeedMap[HalfTrack shr 1]];
         FillChar(GCR^.SpeedZone[HalfTrack*MaxBytesPerGCRHalfTrack],MaxBytesPerGCRHalfTrack,SpeedMap[HalfTrack shr 1]);
         Ptr:=@GCR^.Data[HalfTrack*MaxBytesPerGCRHalfTrack];
         FillChar(Ptr^,MaxBytesPerGCRHalfTrack,#$ff);
         MaxSector:=SectorCount[HalfTrack shr 1];
         for Sector:=0 to MaxSector-1 do begin
          TrackStream.Read(Buffer[1],256);
          Ptr:=@GCR.Data[(HalfTrack*MaxBytesPerGCRHalfTrack)+((GCR.HalfTrackSize[HalfTrack]*Sector) div MaxSector)];
          RC:=0;
          if RC=21 then begin
           FillChar(Ptr^,MaxBytesPerGCRHalfTrack,#$0);
           break;
          end else begin
           GCRConvertSectorToGCR(@Buffer,Ptr,HalfTrack shr 1,Sector,0,0,RC);
          end;
         end;
         P64Image.PulseStreams[HalfTrack].ConvertFromGCR(Ptr,GCR^.HalfTrackSize[HalfTrack] shl 3);
         OK:=true;
        finally
         Dispose(GCR);
        end;
       end;
       $c9..$cb:begin
       end;
       $d9..$db:begin
        New(GCR);
        try
         FillChar(GCR^.Data,SizeOf(GCR^.Data),#$ff);
         GCR.MaximumHalfTrackSize:=$ffff;
         OK:=TrackStream.Read(DataTrack,SizeOf(TFDIDataTrack))=SizeOf(TFDIDataTrack);
         DataTrack.Size:=SwapDWordBigEndian(DataTrack.Size);
         DataTrack.IndexPos:=SwapDWordBigEndian(DataTrack.IndexPos);
         if OK then begin
          OK:=((TrackStream.Position+longint((DataTrack.Size+7) shr 3))<=TrackStream.Size) and ((TrackStream.Size-TrackStream.Position)<=SizeOf(GCR^.Data));
          if OK Then begin
           TrackStream.Read(GCR^.Data,TrackStream.Size-TrackStream.Position);
           P64Image.PulseStreams[HalfTrack].ConvertFromGCR(@GCR^.Data,DataTrack.Size);
          end;
         end;
        finally
         Dispose(GCR);
        end;
       end;
       $80..$bf:begin
        OK:=FDIReadPulseTrack(P64Image,TrackStream,HalfTrack,TPIScales[FDIHeader.HeadWidth and 7],IndexSynchronized);
       end;
      end;

     end;

    end;

   finally
    TrackStream.Free;
   end;

   if not OK then begin
    SetLength(TrackDescriptions,0);
    exit;
   end;

  end;

 end;

 SetLength(TrackDescriptions,0);

 result:=true;
end;

procedure HuffManEncode(var InputData:array of longword;Stream:TBeRoStream);
var SubStreamShiftIndex,SubStreamShift,SubStreamShiftCount8,SubStreamShiftCount16,MaxValue,SubStreamShiftCount,SubStreamMask:longword;
    Counter:longint;
    SixteenBits,SignExtend:boolean;
 procedure DoIt;
 type TNode=record
       Frequency:longint;
       Value:longint;
       LeftNode:longint;
       RightNode:longint;
       ParentNode:longint;
       BitCode:ansistring;
      end;
 var Nodes:array of TNode;
     NodeValues:array of longint;
     NodeCount,NodeIndex,NodeIndexLeft,NodeIndexRight,i,NodeFrequencyLeft,NodeFrequencyRight,Value,Counter:longint;
     ByteValue:byte;
     BitBuffer,BitBufferCount:byte;

  procedure ResetBitBuffer;
  begin
   BitBuffer:=0;
   BitBufferCount:=0;
  end;

  procedure FlushBitBuffer;
  begin
   if BitBufferCount<>0 then begin
    Stream.Write(BitBuffer,SizeOf(byte));
    BitBufferCount:=0;
   end;
  end;

  procedure WriteBit(Bit:byte);
  begin
   BitBuffer:=BitBuffer or (Bit shl ((not BitBufferCount) and 7));
   inc(BitBufferCount);
   if BitBufferCount=8 then begin
    Stream.Write(BitBuffer,SizeOf(byte));
    BitBuffer:=0;
    BitBufferCount:=0;
   end;
  end;

  procedure EncodeNodeTree(Node:longint);
  begin
   if Node>=0 then begin
    if Nodes[Node].Value<0 then begin
     WriteBit(0);
     EncodeNodeTree(Nodes[Node].LeftNode);
     EncodeNodeTree(Nodes[Node].RightNode);
    end else begin
     WriteBit(1);
    end;
   end else begin
    WriteBit(1);
   end;
  end;

  procedure EncodeNodeValues(Node:longint);
  var Value:longword;
      ByteValue:byte;
  begin
   if Node>=0 then begin
    if Nodes[Node].Value<0 then begin
     EncodeNodeValues(Nodes[Node].LeftNode);
     EncodeNodeValues(Nodes[Node].RightNode);
    end else begin
     Value:=Nodes[Node].Value;
     if SixteenBits then begin
      ByteValue:=(Value shr 8) and $ff;
      Stream.Write(ByteValue,SizeOf(byte));
     end;
     ByteValue:=Value and $ff;
     Stream.Write(ByteValue,SizeOf(byte));
    end;
   end;
  end;

  procedure CreateBitSequences(NodeIndex:longint;const BitCode:ansistring);
  begin
   if Nodes[NodeIndex].Value>=0 Then begin
    Nodes[NodeIndex].BitCode:=BitCode;
    exit;
   end;
   if Nodes[NodeIndex].LeftNode>=0 Then begin
    CreateBitSequences(Nodes[NodeIndex].LeftNode,BitCode+#0);
   end;
   if Nodes[NodeIndex].RightNode>=0 Then begin
    CreateBitSequences(Nodes[NodeIndex].RightNode,BitCode+#1);
   end;
  end;

  procedure EncodeNodeTreeCode(Value:longint);
  var NodeIndex,Counter:longint;
      s:ansistring;
  begin
   NodeIndex:=NodeValues[Value and $ffff];
   if NodeIndex>=0 then begin
    s:=Nodes[NodeIndex].BitCode;
    for Counter:=1 to length(s) do begin
     if s[Counter]=#1 then begin
      WriteBit(1);
     end else begin
      WriteBit(0);
     end;
    end;
   end;
  end;

 begin
  Nodes:=nil;
  NodeValues:=nil;
  SetLength(Nodes,262144);
  SetLength(NodeValues,65536);
  try

   for i:=0 to 65535 do begin
    NodeValues[i]:=-1;
   end;

   NodeCount:=0;
   for Counter:=0 to length(InputData)-1 do begin
    Value:=(InputData[Counter] shr SubStreamShift) and SubStreamMask;
    NodeIndex:=NodeValues[Value and $ffff];
    if NodeIndex<0 then begin
     NodeValues[Value and $ffff]:=NodeCount;
     Nodes[NodeCount].Value:=(InputData[Counter] shr SubStreamShift) and SubStreamMask;
     Nodes[NodeCount].Frequency:=0;
     Nodes[NodeCount].LeftNode:=-1;
     Nodes[NodeCount].RightNode:=-1;
     Nodes[NodeCount].ParentNode:=-1;
     Nodes[NodeCount].BitCode:='';
     inc(NodeCount);
    end else begin
     inc(Nodes[NodeIndex].Frequency);
    end;
   end;

   NodeFrequencyLeft:=0;
   NodeFrequencyRight:=0;

   for NodeIndex:=NodeCount downto 2 do begin
    NodeIndexLeft:=-1;
    NodeIndexRight:=-1;
    for i:=0 to NodeCount-1 do begin
     if Nodes[i].ParentNode<0 then begin
      if NodeIndexLeft<0 then begin
       NodeFrequencyLeft:=Nodes[i].Frequency;
       NodeIndexLeft:=i;
      end else if NodeIndexRight<0 Then begin
       NodeFrequencyRight:=Nodes[i].Frequency;
       NodeIndexRight:=i;
      end else if Nodes[i].Frequency<NodeFrequencyLeft Then begin
       if Nodes[i].Frequency<NodeFrequencyRight Then begin
        if NodeFrequencyLeft<NodeFrequencyRight Then begin
         NodeFrequencyRight:=Nodes[i].Frequency;
         NodeIndexRight:=i;
        end else begin
         NodeFrequencyLeft:=Nodes[i].Frequency;
         NodeIndexLeft:=i;
        end;
       end else begin
        NodeFrequencyLeft:=Nodes[i].Frequency;
        NodeIndexLeft:=i;
       end;
      end else if Nodes[i].Frequency<NodeFrequencyRight Then begin
       NodeFrequencyRight:=Nodes[i].Frequency;
       NodeIndexRight:=i;
      end;
     end;
    end;

    Nodes[NodeCount].Frequency:=NodeFrequencyLeft+NodeFrequencyRight;
    Nodes[NodeCount].LeftNode:=NodeIndexLeft;
    Nodes[NodeCount].RightNode:=NodeIndexRight;
    Nodes[NodeCount].ParentNode:=-1;
    Nodes[NodeCount].Value:=-1;

    Nodes[NodeIndexLeft].ParentNode:=NodeCount;
    Nodes[NodeIndexRight].ParentNode:=NodeCount;

    inc(NodeCount);
   end;

   ByteValue:=SubStreamShift and $7f;
   if SignExtend then begin
    ByteValue:=ByteValue or $80;
   end;
   Stream.Write(ByteValue,SizeOf(byte));

   if SixteenBits then begin
    ByteValue:=(SubStreamShift+15) and $7f;
    ByteValue:=ByteValue or $80;
   end else begin
    ByteValue:=(SubStreamShift+7) and $7f;
   end;
   Stream.Write(ByteValue,SizeOf(byte));

   ResetBitBuffer;
   EncodeNodeTree(NodeCount-1);
   FlushBitBuffer;

   EncodeNodeValues(NodeCount-1);

   CreateBitSequences(NodeCount-1,'');

   ResetBitBuffer;
   for Counter:=0 to length(InputData)-1 do begin
    EncodeNodeTreeCode(longint((InputData[Counter] shr SubStreamShift) and SubStreamMask));
   end;
   FlushBitBuffer;

  finally
   SetLength(Nodes,0);
   SetLength(NodeValues,0);
  end;
 end;
begin
 MaxValue:=0;
 for Counter:=0 to length(InputData)-1 do begin
  if MaxValue<InputData[Counter] then begin
   MaxValue:=InputData[Counter];
  end;
 end;

 if MaxValue<256 then begin
  SubStreamShift:=0;
  SixteenBits:=false;
  SignExtend:=false;
  SubStreamMask:=$ff;
  DoIt;
 end else if MaxValue<65536 then begin
  SubStreamShift:=0;
  SixteenBits:=true;
  SignExtend:=false;
  SubStreamMask:=$ffff;
  DoIt;
 end else begin

  SignExtend:=false;

  SubStreamShiftCount8:=0;
  for SubStreamShiftIndex:=3 downto 0 do begin
   SubStreamShift:=SubStreamShiftIndex*8;
   for Counter:=0 to length(InputData)-1 do begin
    if ((InputData[Counter] shr SubStreamShift) and $ff)<>0 then begin
     inc(SubStreamShiftCount8);
    end;
   end;
  end;

  SubStreamShiftCount16:=0;
  for SubStreamShiftIndex:=1 downto 0 do begin
   SubStreamShift:=SubStreamShiftIndex*16;
   for Counter:=0 to length(InputData)-1 do begin
    if ((InputData[Counter] shr SubStreamShift) and $ffff)<>0 then begin
     inc(SubStreamShiftCount16);
    end;
   end;
  end;

  if SubStreamShiftCount8<=SubStreamShiftCount16 then begin

   SubStreamMask:=$ff;
   SixteenBits:=false;

   for SubStreamShiftIndex:=3 downto 0 do begin
    SubStreamShift:=SubStreamShiftIndex*8;

    SubStreamShiftCount:=0;
    for Counter:=0 to length(InputData)-1 do begin
     if ((InputData[Counter] shr SubStreamShift) and $ff)<>0 then begin
      inc(SubStreamShiftCount);
     end;
    end;
    if SubStreamShiftCount>0 then begin
     DoIt;
    end;

   end;

  end else begin

   SubStreamMask:=$ffff;
   SixteenBits:=true;

   for SubStreamShiftIndex:=1 downto 0 do begin
    SubStreamShift:=SubStreamShiftIndex*16;

    SubStreamShiftCount:=0;
    for Counter:=0 to length(InputData)-1 do begin
     if ((InputData[Counter] shr SubStreamShift) and $ff)<>0 then begin
      inc(SubStreamShiftCount);
     end;
    end;
    if SubStreamShiftCount>0 then begin
     DoIt;
    end;

   end;

  end;

 end;
end;

function FDIWrite(P64Image:TP64Image;Stream:TBeRoStream):boolean;
var MemoryStream:TBeRoMemoryStream;
 function DoIt:boolean;
 var FDIHeader:TFDIHeader;
     TrackStream:TBeRoStream;
     AverageStream:TBeRoStream;
     IndexStream:TBeRoStream;
     FDITrack,HalfTrack,Current,Count:longint;
     PulseTrackHeader:TFDIPulseTrackHeader;
     NumPulses,CountPulses,PulseIndex:longword;
     Last:int64;
     Pulses:array of longword;
     HasWeakPulses:boolean;
     TrackDescriptions:TFDITrackDescriptions;
 begin
  result:=false;

  TrackDescriptions:=nil;

  FillChar(FDIHeader,SizeOf(TFDIHeader),#0);
  FDIHeader.Signature:=FDISignature;
  FDIHeader.Creator:='Micro64 1541 FDI save         ';
  FDIHeader.CRLF[0]:=#$0d;
  FDIHeader.CRLF[1]:=#$0a;
  FillChar(FDIHeader.Comment,SizeOf(FDIHeader.Comment),#$1a);
  FDIHeader.EOFByte:=$1a;
  FDIHeader.Version[0]:=$02;
  FDIHeader.Version[1]:=$01;
  FDIHeader.Flags:=0;
  if P64Image.WriteProtected then begin
   FDIHeader.Flags:=FDIHeader.Flags or 1;
  end;
  FDIHeader.LastTrackHi:=0;
  FDIHeader.LastTrackLo:=83;
  FDIHeader.LastHead:=$00;
  FDIHeader.DiskImage:=$01;
  FDIHeader.RotationSpeed:=$ac;
  FDIHeader.TPI:=$02;
  FDIHeader.HeadWidth:=$02;

  MemoryStream.Clear;

  if MemoryStream.Seek(0)<>0 then begin
   exit;
  end;

  SetLength(TrackDescriptions,FDIHeader.LastTrackLo+1);

  if MemoryStream.Write(FDIHeader,SizeOf(TFDIHeader))<>SizeOf(TFDIHeader) then begin
   SetLength(TrackDescriptions,0);
   exit;
  end;

  for FDITrack:=0 to length(TrackDescriptions)-1 do begin
   HalfTrack:=FDITrack+2;

   NumPulses:=P64Image.PulseStreams[HalfTrack].GetPulseCount;
   if NumPulses>0 then begin

    TrackStream:=TBeRoStream.Create;
    AverageStream:=TBeRoStream.Create;
    IndexStream:=TBeRoStream.Create;
    Pulses:=nil;
    try
     SetLength(Pulses,NumPulses);

     Last:=0;
     Current:=P64Image.PulseStreams[HalfTrack].UsedLast;
     while Current>=0 do begin
      if P64Image.PulseStreams[HalfTrack].Pulses^[Current].Strength=$ffffffff then begin
       Last:=int64(P64Image.PulseStreams[HalfTrack].Pulses^[Current].Position)-P64PulseSamplesPerRotation;
       break;
      end else begin
       if Current=P64Image.PulseStreams[HalfTrack].UsedFirst then begin
        break;
       end else begin
        Current:=P64Image.PulseStreams[HalfTrack].Pulses^[Current].Previous;
       end;
      end;
     end;

     HasWeakPulses:=false;
     PulseIndex:=0;
     Current:=P64Image.PulseStreams[HalfTrack].UsedFirst;
     while Current>=0 do begin
      Pulses[PulseIndex]:=P64Image.PulseStreams[HalfTrack].Pulses^[Current].Position-Last;
      if P64Image.PulseStreams[HalfTrack].Pulses^[Current].Strength=$ffffffff then begin
       Last:=P64Image.PulseStreams[HalfTrack].Pulses^[Current].Position;
      end else begin
       HasWeakPulses:=true;
      end;
      inc(PulseIndex);
      Current:=P64Image.PulseStreams[HalfTrack].Pulses^[Current].Next;
     end;
     SetLength(Pulses,NumPulses);
     CountPulses:=NumPulses;
     HuffManEncode(Pulses,AverageStream);

     if HasWeakPulses then begin
      PulseIndex:=0;
      Current:=P64Image.PulseStreams[HalfTrack].UsedFirst;
      while Current>=0 do begin
       if P64Image.PulseStreams[HalfTrack].Pulses^[Current].Strength<>$ffffffff then begin
        Pulses[PulseIndex]:=P64Image.PulseStreams[HalfTrack].Pulses^[Current].Strength shr 16;
       end else begin
        Pulses[PulseIndex]:=$ffff;
       end;
       inc(PulseIndex);
       Current:=P64Image.PulseStreams[HalfTrack].Pulses^[Current].Next;
      end;
      HuffManEncode(Pulses,IndexStream);
     enD;

     FillChar(PulseTrackHeader,SizeOf(TFDIPulseTrackHeader),#0);
     PulseTrackHeader.NumPulses[0]:=(CountPulses shr 24) and $ff;
     PulseTrackHeader.NumPulses[1]:=(CountPulses shr 16) and $ff;
     PulseTrackHeader.NumPulses[2]:=(CountPulses shr 8) and $ff;
     PulseTrackHeader.NumPulses[3]:=(CountPulses shr 0) and $ff;
     PulseTrackHeader.AverageSize[0]:=(((1 shl 22) or (AverageStream.Size and $3fffff)) shr 16) and $ff;
     PulseTrackHeader.AverageSize[1]:=(AverageStream.Size shr 8) and $ff;
     PulseTrackHeader.AverageSize[2]:=(AverageStream.Size shr 0) and $ff;
     if HasWeakPulses then begin
      PulseTrackHeader.IndexSize[0]:=(((1 shl 22) or (IndexStream.Size and $3fffff)) shr 16) and $ff;
      PulseTrackHeader.IndexSize[1]:=(IndexStream.Size shr 8) and $ff;
      PulseTrackHeader.IndexSize[2]:=(IndexStream.Size shr 0) and $ff;
     end;

     TrackStream.Write(PulseTrackHeader,sizeof(PulseTrackHeader));
     AverageStream.Seek(0);
     TrackStream.AppendFrom(AverageStream,AverageStream.Size);
     if HasWeakPulses then begin
      IndexStream.Seek(0);
      TrackStream.AppendFrom(IndexStream,IndexStream.Size);
     end;
     if (TrackStream.Size and $ff)<>0 then begin
      TrackStream.WriteByteCount(0,256-(TrackStream.Size and $ff));
     end;

     TrackDescriptions[FDITrack].TrackType:=$80 or ((TrackStream.Size shr 16) and $3f);
     TrackDescriptions[FDITrack].TrackSize:=(TrackStream.Size shr 8) and $ff;

     TrackStream.Seek(0);

     MemoryStream.Append(TrackStream);

    finally
     IndexStream.Destroy;
     AverageStream.Destroy;
     TrackStream.Destroy;
     SetLength(Pulses,0);
    end;

   end;
  end;

  if MemoryStream.Seek(0)<>0 then begin
   SetLength(TrackDescriptions,0);
   exit;
  end;
  Count:=length(TrackDescriptions);
  if Count>180 then begin
   Count:=180;
  end;
  Move(TrackDescriptions[0],FDIHeader.TrackDescriptions[0],Count*SizeOf(TFDITrackDescription));
  FDIHeader.DataCRC32:=SwapDWordBigEndian(CRC32(@pansichar(MemoryStream.Data)[SizeOf(TFDIHeader)],MemoryStream.Size-SizeOf(TFDIHeader)));
  FDIHeader.HeaderCRC32:=SwapDWordBigEndian(CRC32(@FDIHeader,508));
  if MemoryStream.Write(FDIHeader,SizeOf(TFDIHeader))<>SizeOf(TFDIHeader) then begin
   SetLength(TrackDescriptions,0);
   exit;
  end;

  if MemoryStream.Seek(MemoryStream.Size)<>MemoryStream.Size then begin
   SetLength(TrackDescriptions,0);
   exit;
  end;

  SetLength(TrackDescriptions,0);

  result:=Stream.Assign(MemoryStream)=MemoryStream.Size;
 end;
begin
 MemoryStream:=TBeRoMemoryStream.Create;
 try
  result:=DoIt;
 finally
  MemoryStream.Free;
 end;

end;

function DumpFDI(SrcStream,DstStream:TBeRoStream):boolean;
var P64Image:TP64Image;
 procedure WriteString(s:ansistring);
 begin
  s:=s+#13#10;
  if length(s)>0 then begin
   DstStream.Write(s[1],length(s));
  end;
 end;
var HalfTrack,Pulse:longint;
begin
 result:=false;
 P64Image:=TP64Image.Create;
 try
  SrcStream.Seek(0);
  if FDIRead(P64Image,SrcStream) then begin
   DstStream.Clear;
   if P64Image.WriteProtected then begin
    WriteString('Write-protected');
   end else begin
    WriteString('Non-write-protected');
   end;
   WriteString('');
   for HalfTrack:=FirstHalfTrack to LastHalfTrack do begin
    WriteString('========================================================================================================');
    WriteString('Half track '+IntToStr(HalfTrack));
    WriteString('--------------------------------------------------------------------------------------------------------');
    WriteString('Count of pulses '+IntToStr(P64Image.PulseStreams[HalfTrack].PulsesCount));
    WriteString('');
    for Pulse:=0 to P64Image.PulseStreams[HalfTrack].PulsesCount-1 do begin
     WriteString('************************************');
     WriteString('Number '+IntToStr(Pulse));
     WriteString('Position '+IntToStr(P64Image.PulseStreams[HalfTrack].Pulses[Pulse].Position));
     WriteString('Strength 0x'+AnsiString(lowercase(AnsiString(IntToHex(P64Image.PulseStreams[HalfTrack].Pulses[Pulse].Strength,8)))));
    end;
    WriteString('************************************');
    WriteString('');
   end;
   result:=true;
  end;
 finally
  P64Image.Free;
 end;
end;

function ConvertFDItoP64(SrcStream,DstStream:TBeRoStream):boolean;
var P64Image:TP64Image;
    MemoryStream:TMemoryStream;
begin
 result:=false;
 P64Image:=TP64Image.Create;
 try
  SrcStream.Seek(0);
  if FDIRead(P64Image,SrcStream) then begin
   DstStream.Clear;
   MemoryStream:=TMemoryStream.Create;
   try
    result:=P64Image.WriteToStream(MemoryStream);
    if result then begin
     result:=DstStream.Write(MemoryStream.Memory^,MemoryStream.Size)=MemoryStream.Size;
    end;
   finally
    MemoryStream.Free;
   end;
  end;
 finally
  P64Image.Free;
 end;
end;

function ConvertP64toFDI(SrcStream,DstStream:TBeRoStream):boolean;
var P64Image:TP64Image;
    MemoryStream:TMemoryStream;
    Data:pointer;
    Size:longint;
begin
 result:=false;

 Size:=SrcStream.Size;
 if Size>0 then begin

  GetMem(Data,Size);
  try

   if SrcStream.Seek(0)=0 then begin
    if SrcStream.Read(Data^,Size)=Size then begin

     MemoryStream:=TMemoryStream.Create;
     try

      if MemoryStream.Write(Data^,Size)=Size then begin
       if MemoryStream.Seek(0,soBeginning)=0 then begin

        P64Image:=TP64Image.Create;
        try

         if P64Image.ReadFromStream(MemoryStream) then begin

          result:=FDIWrite(P64Image,DstStream);

         end;

        finally
         P64Image.Free;
        end;

       end;
      end;

     finally
      MemoryStream.Free;
     end;

    end;
   end;

  finally
   FreeMem(Data);
  end;

 end;

end;

end.
