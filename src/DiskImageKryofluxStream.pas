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
unit DiskImageKryofluxStream;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpui386}
  {$define cpu386}
 {$endif}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
 {$ifdef FPC_LITTLE_ENDIAN}
  {$define LITTLE_ENDIAN}
 {$else}
  {$ifdef FPC_BIG_ENDIAN}
   {$define BIG_ENDIAN}
  {$endif}
 {$endif}
 {-$pic off}
 {$define caninline}
 {$ifdef FPC_HAS_TYPE_EXTENDED}
  {$define HAS_TYPE_EXTENDED}
 {$else}
  {$undef HAS_TYPE_EXTENDED}
 {$endif}
 {$ifdef FPC_HAS_TYPE_DOUBLE}
  {$define HAS_TYPE_DOUBLE}
 {$else}
  {$undef HAS_TYPE_DOUBLE}
 {$endif}
 {$ifdef FPC_HAS_TYPE_SINGLE}
  {$define HAS_TYPE_SINGLE}
 {$else}
  {$undef HAS_TYPE_SINGLE}
 {$endif}
{$else}
 {$realcompatibility off}
 {$localsymbols on}
 {$define LITTLE_ENDIAN}
 {$ifndef cpu64}
  {$define cpu32}
 {$endif}
 {$define HAS_TYPE_EXTENDED}
 {$define HAS_TYPE_DOUBLE}
 {$define HAS_TYPE_SINGLE}
{$endif}
{$ifdef win32}
 {$define windows}
{$endif}
{$ifdef win64}
 {$define windows}
{$endif}
{$ifdef wince}
 {$define windows}
{$endif}
{$rangechecks off}
{$extendedsyntax on}
{$writeableconst on}
{$hints off}
{$booleval off}
{$typedaddress off}
{$stackframes off}
{$varstringchecks on}
{$typeinfo on}
{$overflowchecks off}
{$longstrings on}
{$openstrings on}

interface

uses SysUtils,Classes,BeRoStream,Math;

function ConvertKryofluxStream(Prefix,OutputFileName:ansistring;Side,FDI,DoubleWideTracks:boolean;TargetRPM:double):boolean;

implementation

uses DiskImageP64,DiskImageFDI;

{$ifdef fpc}
 {$undef OldDelphi}
{$else}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=23.0}
   {$undef OldDelphi}
type qword=uint64;
     ptruint=NativeUInt;
     ptrint=NativeInt;
  {$else}
   {$define OldDelphi}
  {$ifend}
 {$else}
  {$define OldDelphi}
 {$endif}
{$endif}
{$ifdef OldDelphi}
type qword=int64;
{$ifdef cpu64}
     ptruint=qword;
     ptrint=int64;
{$else}
     ptruint=longword;
     ptrint=longint;
{$endif}
{$endif}

function RoundUpToPowerOfTwo(x:ptruint):ptruint; {$ifdef caninline}inline;{$endif}
begin
 dec(x);
 x:=x or (x shr 1);
 x:=x or (x shr 2);
 x:=x or (x shr 4);
 x:=x or (x shr 8);
 x:=x or (x shr 16);
{$ifdef cpu64}
 x:=x or (x shr 32);
{$endif}
 result:=x+1;
end;

type TPulses=array of longword;

procedure QuickSortPulses(var Pulses:TPulses;PulsesLeft,PulsesRight:longint);
var Left,Right:longint;
    Pivot,Pulse:longword;
begin
 Left:=PulsesLeft;
 Right:=PulsesRight;
 Pivot:=Pulses[(Left+Right) div 2];
 repeat
  while Pulses[Left]<Pivot do begin
   inc(Left);
  end;
  while Pulses[Right]>Pivot do begin
   dec(Right);
  end;
  if Left<=Right then begin
   Pulse:=Pulses[Left];
   Pulses[Left]:=Pulses[Right];
   Pulses[Right]:=Pulse;
   inc(Left);
   dec(Right);
  end;
 until Left>Right;
 if Right>PulsesLeft then begin
  QuickSortPulses(Pulses,PulsesLeft,Right);
 end;
 if Left<PulsesRight then begin
  QuickSortPulses(Pulses,Left,PulsesRight);
 end;
end;

procedure LoadRAW(Stream:TStream;Backward:boolean;var PulseValues:TPulses;TargetRPM:double);
const kryo_mck=((18432000*73)/14)/2;
      kryo_sck=kryo_mck/2;
      kryo_ick=kryo_mck/16;
type TFloat={$ifdef HAS_TYPE_EXTENDED}extended{$else}double{$endif};
     TIndex=record
      StreamPosition:longword;
      Timer:longword;
      SystemTimer:longword;
      CellPosition:longint;
      IndexTime:longword;
      PreIndexCellTime:longword;
      PostIndexCellTime:longword;
      FluxIndex:longint;
      PreIndexCellTimePercent:TFloat;
      PostIndexCellTimePercent:TFloat;
      RPM:TFloat;
      BitIndex:longword;
      ByteIndex:longword;
      PreBitCount:longword;
      PostBitCount:longword;
      DeltaSinceIndexSignal:int64;
     end;
     TCell=record
      CyclePosition:longword;
      StreamPosition:longword;
      Delta:qword;
     end;
var OBBStream:TMemoryStream;
    OBBType:byte;
    OBBLen:longint;
    CellValues:array of TCell;
    CellCount:longint;
    PulseCount:longint;
    IndexValues:array of TIndex;
    IndexCount:longint;
 procedure ReadStream;
 var Cmd:byte;
     Done:boolean;
     NextCellValue,NewCellValue,StreamOffset:longword;
     Counter:longint;
  function ReadByte:byte;
  begin
   Stream.Read(result,sizeof(byte));
  end;
  procedure Skip(Bytes:longint);
  begin
   Stream.Seek(Bytes,soFromCurrent);
  end;
  procedure OutputCellValue(Value:longword);
  begin
   if CellCount>=length(CellValues) then begin
    SetLength(CellValues,RoundUpToPowerOfTwo(CellCount+16));
   end;
   CellValues[CellCount].CyclePosition:=Value;
   CellValues[CellCount].StreamPosition:=StreamOffset;
   inc(CellCount);
  end;
  function ReadDWordFromOBB:longword;
  var b:byte;
  begin
   OBBStream.Read(b,sizeof(byte));
   result:=b;
   OBBStream.Read(b,sizeof(byte));
   result:=result or (b shl 8);
   OBBStream.Read(b,sizeof(byte));
   result:=result or (b shl 16);
   OBBStream.Read(b,sizeof(byte));
   result:=result or (b shl 24);
  end;
 begin
  Done:=false;
  NextCellValue:=0;
  StreamOffset:=0;
  while (Stream.Position<Stream.Size) and not Done do begin
   if StreamOffset<>0 then begin
    if (IndexCount>0) and (IndexValues[IndexCount-1].StreamPosition=StreamOffset) then begin
     IndexValues[IndexCount-1].FluxIndex:=CellCount;
    end else begin
     for Counter:=0 to IndexCount-1 do begin
      if (IndexValues[Counter].FluxIndex<0) and (IndexValues[Counter].StreamPosition=StreamOffset) then begin
       IndexValues[Counter].FluxIndex:=CellCount;
      end;
     end;
    end;
   end;
   Cmd:=ReadByte;
   case Cmd of
    $00:begin
     NewCellValue:=ReadByte+NextCellValue;
     NextCellValue:=0;
     OutputCellValue(NewCellValue);
     inc(StreamOffset,2);
    end;
    $01..$07:begin
     NewCellValue:=((Cmd shl 8) or ReadByte)+NextCellValue;
     NextCellValue:=0;
     OutputCellValue(NewCellValue);
     inc(StreamOffset,2);
    end;
    $0e..$ff:begin
     NewCellValue:=Cmd+NextCellValue;
     NextCellValue:=0;
     OutputCellValue(NewCellValue);
     inc(StreamOffset);
    end;
    $08:begin
     inc(StreamOffset);
    end;
    $09:begin
     ReadByte;
     inc(StreamOffset,2);
    end;
    $0a:begin
     ReadByte;
     ReadByte;
     inc(StreamOffset,3);
    end;
    $0b:begin
     inc(NextCellValue,$10000);
     inc(StreamOffset);
    end;
    $0c:begin
     NewCellValue:=ReadByte shl 8;
     NewCellValue:=(NewCellValue or ReadByte)+NextCellValue;
     NextCellValue:=0;
     OutputCellValue(NewCellValue);
     inc(StreamOffset,3);
    end;
    $0d:begin
     OBBType:=ReadByte;
     OBBLen:=ReadByte;
     OBBLen:=OBBLen or (ReadByte shl 8);
     OBBStream:=TMemoryStream.Create;
     try
      if OBBType<>$0d then begin
       OBBStream.CopyFrom(Stream,OBBLen);
      end;
      OBBStream.Seek(0,soFromBeginning);
      case OBBType of
       $00:begin
       end;
       $01:begin
        // OBB:Stream Read
        ReadDWordFromOBB;
        ReadDWordFromOBB;
       end;
       $02:begin
        // OBB:Index
        if IndexCount>=length(IndexValues) then begin
         SetLength(IndexValues,RoundUpToPowerOfTwo(IndexCount+16));
        end;
        IndexValues[IndexCount].StreamPosition:=ReadDWordFromOBB;
        IndexValues[IndexCount].Timer:=ReadDWordFromOBB;
        IndexValues[IndexCount].SystemTimer:=ReadDWordFromOBB;
        IndexValues[IndexCount].CellPosition:=0;
        IndexValues[IndexCount].IndexTime:=0;
        IndexValues[IndexCount].PreIndexCellTime:=0;
        IndexValues[IndexCount].PostIndexCellTime:=0;
        IndexValues[IndexCount].FluxIndex:=-1;
        IndexValues[IndexCount].RPM:=0;
        inc(IndexCount);
       end;
       $03:begin
        // OBB:Stream End
        ReadDWordFromOBB;
        ReadDWordFromOBB;
       end;
       $04:begin
        // OBB:Comment
       end;
       $0d:begin
        // OBB:EOF
        Done:=true;
       end;
      end;
     finally
      OBBStream.Free;
     end;
    end;
   end;
  end;
  if CellCount>=length(CellValues) then begin
   SetLength(CellValues,RoundUpToPowerOfTwo(CellCount+16));
  end;
  CellValues[CellCount].CyclePosition:=NextCellValue;
  CellValues[CellCount].StreamPosition:=StreamOffset;
  SetLength(CellValues,CellCount+1);
 end;
 function DecodeIndex:boolean;
 var IndexPosition,CellPosition,NextCellPosition:longint;
     IndexTime,NextStreamPosition,IndexCellTime,IndexCellOverflowCount,PreCellOverflowCount,PreIndexCellTime:longword;
 begin
  if (IndexCount<3) or (CellCount<1) then begin
   result:=false;
   exit;
  end;
  IndexTime:=0;
  IndexPosition:=0;
  NextStreamPosition:=IndexValues[IndexPosition].StreamPosition;
  for CellPosition:=0 to CellCount-1 do begin
   inc(IndexTime,CellValues[CellPosition].CyclePosition);
   NextCellPosition:=CellPosition+1;
   if CellValues[NextCellPosition].StreamPosition<NextStreamPosition then begin
    continue;
   end;
   if (CellPosition=0) and (CellValues[NextCellPosition].StreamPosition>=NextStreamPosition) then begin
    NextCellPosition:=0;
   end;
   if IndexPosition<IndexCount then begin
    IndexValues[IndexPosition].CellPosition:=NextCellPosition;
    IndexCellTime:=CellValues[NextCellPosition].CyclePosition;
    if IndexValues[IndexPosition].Timer=0 then begin
     IndexValues[IndexPosition].Timer:=IndexCellTime;
    end;
    if (NextCellPosition>=CellCount) and (CellValues[NextCellPosition].StreamPosition=NextStreamPosition) then begin
     inc(IndexCellTime,IndexValues[IndexPosition].Timer);
     CellValues[NextCellPosition].CyclePosition:=IndexCellTime;
    end;
    IndexCellOverflowCount:=IndexCellTime shr 16;
    PreCellOverflowCount:=CellValues[NextCellPosition].StreamPosition-NextStreamPosition;
    if IndexCellOverflowCount<PreCellOverflowCount then begin
     result:=false;
     exit;
    end;
    PreIndexCellTime:=((IndexCellOverflowCount-PreCellOverflowCount) shl 16)+IndexValues[IndexPosition].Timer;
    IndexValues[IndexPosition].PreIndexCellTime:=PreIndexCellTime;
    IndexValues[IndexPosition].PostIndexCellTime:=IndexCellTime-PreIndexCellTime;
    if IndexPosition<>0 then begin
     dec(IndexTime,IndexValues[IndexPosition-1].PreIndexCellTime);
    end;
    if NextCellPosition<>0 then begin
     IndexValues[IndexPosition].IndexTime:=IndexTime+PreIndexCellTime;
    end else begin
     IndexValues[IndexPosition].IndexTime:=PreIndexCellTime;
    end;
    inc(IndexPosition);
    if IndexPosition<IndexCount then begin
     NextStreamPosition:=IndexValues[IndexPosition].StreamPosition;
    end else begin
     NextStreamPosition:=0;
    end;
    if NextCellPosition<>0 then begin
     IndexTime:=0;
    end;
   end;
  end;
  if IndexPosition<IndexCount then begin
   result:=false;
   exit;
  end;
  if IndexValues[IndexPosition-1].CellPosition>=CellCount then begin
   if CellCount>=length(CellValues) then begin
    SetLength(CellValues,RoundUpToPowerOfTwo(CellCount+16));
   end;
   CellValues[CellCount].CyclePosition:=IndexTime;
   CellValues[CellCount].StreamPosition:=NextStreamPosition;
   inc(CellCount);
  end;
  result:=true;
 end;
 procedure ConvertIndex;
 var IndexPosition:longint;
     Delta:longword;
     PreCycle,PostCycle,AllCycle,PreIndexCellPercent,RPM,FloatDelta:TFloat;
 begin
  RPM:=360;
  for IndexPosition:=0 to IndexCount-1 do begin
   PreCycle:=IndexValues[IndexPosition].PreIndexCellTime;
   PostCycle:=IndexValues[IndexPosition].PostIndexCellTime;
   AllCycle:=PreCycle+PostCycle;
   PreIndexCellPercent:=PreCycle/AllCycle;
   IndexValues[IndexPosition].PreIndexCellTimePercent:=PreIndexCellPercent;
   IndexValues[IndexPosition].PostIndexCellTimePercent:=1.0-PreIndexCellPercent;
   if IndexPosition<>0 then begin
    Delta:=IndexValues[IndexPosition].SystemTimer-IndexValues[IndexPosition-1].SystemTimer;
    if Delta<>0 then begin
     RPM:=(kryo_ick*60)/Delta;
    end;
    IndexValues[IndexPosition].RPM:=RPM;
   end;
   IndexValues[IndexPosition].BitIndex:=0;
   IndexValues[IndexPosition].ByteIndex:=0;
   IndexValues[IndexPosition].PreBitCount:=0;
   IndexValues[IndexPosition].PostBitCount:=0;
  end;
  if IndexCount<2 then begin
   IndexValues[0].RPM:=0;
  end else begin
   IndexValues[0].RPM:=IndexValues[1].RPM;
  end;
  for IndexPosition:=0 to IndexCount-1 do begin
   FloatDelta:=((IndexValues[IndexPosition].PostIndexCellTime/kryo_sck)*(IndexValues[IndexPosition].RPM/TargetRPM))*16000000;
   IndexValues[IndexPosition].DeltaSinceIndexSignal:=(qword(int64(trunc(FloatDelta))) shl 32)+trunc(frac(FloatDelta)*$100000000);
  end;
 end;
 procedure ConvertCellTime;
 var IndexPosition,CellPosition,RPMPosition:longint;
     RPM,RPMOffset,RPMStep,Delta:TFloat;
 begin
  IndexPosition:=0;
  RPMOffset:=IndexValues[IndexPosition].RPM;
  RPMStep:=0;
  RPMPosition:=0;
  for CellPosition:=0 to CellCount-1 do begin
   if (CellPosition=IndexValues[IndexPosition].CellPosition) and ((IndexPosition+1)<IndexCount) then begin
    RPMOffset:=IndexValues[IndexPosition].RPM;
    RPMStep:=(IndexValues[IndexPosition+1].RPM-IndexValues[IndexPosition].RPM)/(IndexValues[IndexPosition+1].CellPosition-IndexValues[IndexPosition].CellPosition);
    RPMPosition:=0;
   end;
   RPM:=RPMOffset+(RPMStep*RPMPosition);
   inc(RPMPosition);
   Delta:=((CellValues[CellPosition].CyclePosition/kryo_sck)*(RPM/TargetRPM))*16000000;
   CellValues[CellPosition].Delta:=(qword(int64(trunc(Delta))) shl 32)+trunc(frac(Delta)*$100000000);
  end;
 end;
 procedure DoCollect(WhichIndex:longint);
 var CellPosition,PulsePosition:longint;
     Position:int64;
     MustSort:boolean;
 begin
  while (WhichIndex+1)>=IndexCount do begin
   dec(WhichIndex);
  end;
  while WhichIndex<0 do begin
   inc(WhichIndex);
  end;
  MustSort:=false;
  PulseCount:=0;
  if IndexValues[WhichIndex].CellPosition<IndexValues[WhichIndex+1].CellPosition then begin
   SetLength(PulseValues,(IndexValues[WhichIndex+1].CellPosition-IndexValues[WhichIndex].CellPosition)+1);
   Position:=0;
   for CellPosition:=IndexValues[WhichIndex].CellPosition to IndexValues[WhichIndex+1].CellPosition do begin
    inc(Position,CellValues[CellPosition].Delta);
    if Position>=0 then begin
     if (Position shr 32)>=P64PulseSamplesPerRotation then begin
      break;
     end else begin
      PulsePosition:=(Position+IndexValues[WhichIndex].DeltaSinceIndexSignal) shr 32;
      if PulsePosition>=P64PulseSamplesPerRotation then begin
       repeat
        dec(PulsePosition,P64PulseSamplesPerRotation);
       until PulsePosition<P64PulseSamplesPerRotation;
       MustSort:=true;
      end;
      PulseValues[PulseCount]:=PulsePosition;
      inc(PulseCount);
     end;
    end;
   end;
  end;
  SetLength(PulseValues,PulseCount);
  if MustSort and (PulseCount>1) then begin
   QuickSortPulses(PulseValues,0,PulseCount-1);
  end;
 end;
const NewFPUExceptionMask:TFPUExceptionMask=[exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision];
      NewFPURoundingMode:TFPURoundingMode=rmNEAREST;
      NewFPUPrecisionMode:TFPUPrecisionMode={$ifdef HAS_TYPE_EXTENDED}pmEXTENDED{$else}pmDOUBLE{$endif};
var Counter,WhichIndex:longint;
    OldFPUExceptionMask:TFPUExceptionMask;
    OldFPURoundingMode:TFPURoundingMode;
    OldFPUPrecisionMode:TFPUPrecisionMode;
begin
 OldFPUExceptionMask:=GetExceptionMask;
 OldFPURoundingMode:=GetRoundMode;
 OldFPUPrecisionMode:=GetPrecisionMode;
 try
  if OldFPUExceptionMask<>NewFPUExceptionMask then begin
   SetExceptionMask(NewFPUExceptionMask);
  end;
  if OldFPURoundingMode<>NewFPURoundingMode then begin
   SetRoundMode(NewFPURoundingMode);
  end;
  if OldFPUPrecisionMode<>NewFPUPrecisionMode then begin
   SetPrecisionMode(NewFPUPrecisionMode);
  end;
  CellValues:=nil;
  CellCount:=0;
  PulseValues:=nil;
  PulseCount:=0;
  IndexValues:=nil;
  IndexCount:=0;
  try
   ReadStream;

   if DecodeIndex then begin

    ConvertIndex;
    ConvertCellTime;

    // Try first the middle index
    WhichIndex:=(IndexCount+1) div 2;
    while (WhichIndex+1)>=(IndexCount-1) do begin
     dec(WhichIndex);
    end;
    while WhichIndex<1 do begin
     inc(WhichIndex);
    end;

    DoCollect(WhichIndex);

    if Backward then begin
     for Counter:=0 to PulseCount-1 do begin
      PulseValues[Counter]:=P64PulseSamplesPerRotation-(PulseValues[Counter]+1);
     end;
    end;

   end;

  finally
   SetLength(CellValues,0);
   SetLength(IndexValues,0);
  end;
 finally
  if OldFPUExceptionMask<>NewFPUExceptionMask then begin
   SetExceptionMask(OldFPUExceptionMask);
  end;
  if OldFPURoundingMode<>NewFPURoundingMode then begin
   SetRoundMode(OldFPURoundingMode);
  end;
  if OldFPUPrecisionMode<>NewFPUPrecisionMode then begin
   SetPrecisionMode(OldFPUPrecisionMode);
  end;
 end;
end;

function ConvertKryofluxStream(Prefix,OutputFileName:ansistring;Side,FDI,DoubleWideTracks:boolean;TargetRPM:double):boolean;
var Stream:TStream;
    MemoryStream:TMemoryStream;
    StreamEx:TBeRoStream;
    Pulses:array[0..85] of TPulses;
    Counter,SubCounter:longint;
    FileName:ansistring;
    P64Image:TP64Image;
begin
 FillChar(Pulses,SizeOf(TPulses),#0);
 for Counter:=2 to 85 do begin
  FileName:=AnsiString(IntToStr(Counter-2));
  if length(FileName)<2 then begin
   FileName:='0'+FileName;
  end;
  if Side then begin
   FileName:=FileName+'.1';
  end else begin
   FileName:=FileName+'.0';
  end;
  FileName:=Prefix+FileName+'.raw';
  if FileExists(String(FileName)) then begin
   Stream:=TFileStream.Create(String(FileName),fmOpenRead);
   try
    MemoryStream:=TMemoryStream.Create;
    try
     MemoryStream.LoadFromStream(Stream);
     MemoryStream.Seek(0,soFromBeginning);
     LoadRAW(MemoryStream,Side,Pulses[Counter],TargetRPM);
    finally
     MemoryStream.Free;
    end;
   finally
    Stream.Free;
   end;
  end;
 end;
 P64Image:=TP64Image.Create;
 try
  for Counter:=0 to 85 do begin
   for SubCounter:=0 to length(Pulses[Counter])-1 do begin
    if Pulses[Counter,SubCounter]<P64PulseSamplesPerRotation then begin
     P64Image.PulseStreams[Counter].AddPulse(Pulses[Counter,SubCounter],$ffffffff);
     if DoubleWideTracks and ((Counter and 1)=0) and (Counter<85) then begin
      P64Image.PulseStreams[Counter+1].AddPulse(Pulses[Counter,SubCounter],$ffffffff);
     end;
    end else begin
     break;
    end;
   end;
  end;
  if FDI then begin
   StreamEx:=TBeRoFileStream.CreateNew(OutputFileName);
   try
    result:=FDIWrite(P64Image,StreamEx);
   finally
    StreamEx.Free;
   end;
  end else begin
   Stream:=TFileStream.Create(String(OutputFileName),fmCreate);
   try
    result:=P64Image.WriteToStream(Stream);
   finally
    Stream.Free;
   end;
  end;
 finally
  P64Image.Free;
 end;
 for Counter:=0 to 85 do begin
  SetLength(Pulses[Counter],0);
 end;
end;

end.
