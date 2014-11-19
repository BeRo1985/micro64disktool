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
unit DiskImageP64;
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

const P64PulseSamplesPerRotation=3200000;

type PP64Signature=^TP64Signature;
     TP64Signature=array[0..7] of ansichar;

     PP64ChunkSignature=^TP64ChunkSignature;
     TP64ChunkSignature=array[0..3] of ansichar;

     PP64ChunkHeader=^TP64ChunkHeader;
     TP64ChunkHeader=packed record
      Signature:TP64ChunkSignature;
      Size:longword;
      Checksum:longword;
     end;

     PP64Header=^TP64Header;
     TP64Header=packed record
      Signature:TP64Signature;
      Version:longword;
      Flags:longword;
      Size:longword;
      Checksum:longword;
     end;

     PP64HalfTrackHeader=^TP64HalfTrackHeader;
     TP64HalfTrackHeader=packed record
      CountPulses:longword;
      Size:longword;
     end;

     PP64Pulse=^TP64Pulse;
     TP64Pulse=record
      Previous:longint;
      Next:longint;
      Position:longword;
      Strength:longword;
     end;

     PP64Pulses=^TP64Pulses;
     TP64Pulses=array[0..($7fffffff div sizeof(TP64Pulse))-1] of TP64Pulse;

//   TP64PulseStreamBitArray=array[0..(P64PulseSamplesPerRotation+7) shr 3] of byte;

     TP64PulseStream=class
      public
       Pulses:PP64Pulses;
       PulsesAllocated:longint;
       PulsesCount:longint;
       UsedFirst:longint;
       UsedLast:longint;
       FreeList:longint;
       CurrentIndex:longint;
//     BitArray:TP64PulseStreamBitArray;
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       function AllocatePulse:longint;
       procedure FreePulse(Index:longint);
       procedure AddPulse(Position,Strength:longword);
       procedure RemovePulses(Position,Count:longword);
       procedure RemovePulse(Position:longword);
       function DeltaPositionToNextPulse(Position:longword):longword;
       function GetNextPulse(Position:longword):longword;
       function GetPulseCount:longword;
       function GetPulse(Position:longword):longword;
       procedure SetPulse(Position,Strength:longword);
       procedure Seek(Position:longword);
       procedure Dump;
       procedure ConvertFromGCR(Bytes:pointer;Len:longint);
       procedure ConvertToGCR(Bytes:pointer;Len:longint);
       function ConvertToGCRWithLogic(Bytes:pointer;Len,SpeedZone:longint):longint;
       function ReadFromStream(Stream:TStream):boolean;
       function WriteToStream(Stream:TStream):boolean;
     end;

     TP64PulseStreams=array[0..LastHalfTrack+1] of TP64PulseStream;

     TP64Image=class
      public
       PulseStreams:TP64PulseStreams;
       WriteProtected:boolean;
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       function ReadFromStream(Stream:TStream):boolean;
       function WriteToStream(Stream:TStream):boolean;
     end;

     TP64LogicTriggerBits=array[0..3,0..(P64PulseSamplesPerRotation+15) shr 4] of word;
     TP64LogicTriggerPositions=array[0..3,0..(P64PulseSamplesPerRotation+15) shr 4] of byte;

const P64Signature:TP64Signature='P64-1541';

var P64LogicTriggerBits:TP64LogicTriggerBits;
    P64WriteLogicTriggerBits:TP64LogicTriggerBits;
    P64LogicTriggerPositions:TP64LogicTriggerPositions;
    P64WriteLogicTriggerPositions:TP64LogicTriggerPositions;

function DumpP64(SrcStream,DstStream:TBeRoStream):boolean;
function DumpP64HalfTrack(SrcStream,dstStream:TBeRoStream;HalfTrack:longint):boolean;

function ConvertG64toP64(SrcStream,DstStream:TBeRoStream):boolean;
function ConvertP64toG64(SrcStream,DstStream:TBeRoStream;WithLogic:boolean):boolean;

implementation

uses Globals,DiskImageD64,DiskImageG64,LZBRA,ChecksumUtils,RangeCoder;

type pbyte=^byte;

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

constructor TP64PulseStream.Create;
begin
 inherited Create;
 Pulses:=nil;
 PulsesAllocated:=0;
 PulsesCount:=0;
 UsedFirst:=-1;
 UsedLast:=-1;
 FreeList:=-1;
 CurrentIndex:=-1;
end;

destructor TP64PulseStream.Destroy;
begin
 Clear;
 inherited Destroy;
end;

procedure TP64PulseStream.Clear;
begin
 if assigned(Pulses) then begin
  FreeMem(Pulses);
  Pulses:=nil;
 end;
// FillChar(BitArray,SizeOf(TP64PulseStreamBitArray),#0);
 PulsesAllocated:=0;
 PulsesCount:=0;
 UsedFirst:=-1;
 UsedLast:=-1;
 FreeList:=-1;
end;

function TP64PulseStream.AllocatePulse:longint;
begin
 if FreeList<0 then begin
  if PulsesCount>=PulsesAllocated then begin
   if PulsesAllocated<16 then begin
    PulsesAllocated:=16;
   end;
   while PulsesCount>=PulsesAllocated do begin
    inc(PulsesAllocated,PulsesAllocated);
   end;
   ReallocMem(Pulses,PulsesAllocated*SizeOf(TP64Pulse));
  end;
  result:=PulsesCount;
  inc(PulsesCount);
 end else begin
  result:=FreeList;
  FreeList:=Pulses^[result].Next;
 end;
 Pulses^[result].Previous:=-1;
 Pulses^[result].Next:=-1;
 Pulses^[result].Position:=0;
 Pulses^[result].Strength:=0;
end;

procedure TP64PulseStream.FreePulse(Index:longint);
begin
 if CurrentIndex=Index then begin
  CurrentIndex:=Pulses^[Index].Next;
 end;
 if Pulses^[Index].Previous<0 then begin
  UsedFirst:=Pulses^[Index].Next;
 end else begin
  Pulses^[Pulses^[Index].Previous].Next:=Pulses^[Index].Next;
 end;
 if Pulses^[Index].Next<0 then begin
  UsedLast:=Pulses^[Index].Previous;
 end else begin
  Pulses^[Pulses^[Index].Next].Previous:=Pulses^[Index].Previous;
 end;
 Pulses^[Index].Previous:=-1;
 Pulses^[Index].Next:=FreeList;
 FreeList:=Index;
end;

procedure TP64PulseStream.AddPulse(Position,Strength:longword);
var Current,Index:longint;
begin
 while Position>=P64PulseSamplesPerRotation do begin
  dec(Position,P64PulseSamplesPerRotation);
 end;
// BitArray[Position shr 3]:=BitArray[Position shr 3] or (1 shl (Position and 7));
 Current:=CurrentIndex;
 if (UsedLast>=0) and (Pulses^[UsedLast].Position<Position) then begin
  Current:=-1;
 end else begin
  if (Current<0) or ((Current<>UsedFirst) and ((Pulses^[Current].Previous>=0) and (Pulses^[Pulses^[Current].Previous].Position>=Position))) then begin
   Current:=UsedFirst;
  end;
  while (Current>=0) and (Pulses^[Current].Position<Position) do begin
   Current:=Pulses^[Current].Next;
  end;
 end;
 if Current<0 then begin
  Index:=AllocatePulse;
  if UsedLast<0 then begin
   UsedFirst:=Index;
  end else begin
   Pulses^[UsedLast].Next:=Index;
   Pulses^[Index].Previous:=UsedLast;
  end;
  UsedLast:=Index;
 end else begin
  if Pulses^[Current].Position=Position then begin
   Index:=Current;
  end else begin
   Index:=AllocatePulse;
   Pulses^[Index].Previous:=Pulses^[Current].Previous;
   Pulses^[Index].Next:=Current;
   Pulses^[Current].Previous:=Index;
   if Pulses^[Index].Previous<0 then begin
    UsedFirst:=Index;
   end else begin
    Pulses^[Pulses^[Index].Previous].Next:=Index;
   end;
  end;
 end;
 Pulses^[Index].Position:=Position;
 Pulses^[Index].Strength:=Strength;
 CurrentIndex:=Index;
end;

procedure TP64PulseStream.RemovePulses(Position,Count:longword);
var ToDo:longword;
    Current,Next:longint;
begin
 while Position>=P64PulseSamplesPerRotation do begin
  dec(Position,P64PulseSamplesPerRotation);
 end;
 while Count>0 do begin

  ToDo:=Count;
  if (Position+ToDo)>P64PulseSamplesPerRotation then begin
   ToDo:=P64PulseSamplesPerRotation-Position;
  end;

  Current:=CurrentIndex;
  if (Current<0) or ((Current<>UsedFirst) and ((Pulses^[Current].Previous>=0) and (Pulses^[Pulses^[Current].Previous].Position>=Position))) then begin
   Current:=UsedFirst;
  end;
  while (Current>=0) and (Pulses^[Current].Position<Position) do begin
   Current:=Pulses^[Current].Next;
  end;
  while (Current>=0) and ((Pulses^[Current].Position>=Position) and (Pulses^[Current].Position<(Position+ToDo))) do begin
// BitArray[Pulses^[Current].Position shr 3]:=BitArray[Pulses^[Current].Position shr 3] and not (1 shl (Pulses^[Current].Position and 7));
   Next:=Pulses^[Current].Next;
   FreePulse(Current);
   Current:=Next;
  end;

  inc(Position,ToDo);
  dec(Count,ToDo);
 end;
end;

procedure TP64PulseStream.RemovePulse(Position:longword);
var Current:longint;
begin
 while Position>=P64PulseSamplesPerRotation do begin
  dec(Position,P64PulseSamplesPerRotation);
 end;
//BitArray[Position shr 3]:=BitArray[Position shr 3] and not (1 shl (Position and 7));
 Current:=CurrentIndex;
 if (Current<0) or ((Current<>UsedFirst) and ((Pulses^[Current].Previous>=0) and (Pulses^[Pulses^[Current].Previous].Position>=Position))) then begin
  Current:=UsedFirst;
 end;
 while (Current>=0) and (Pulses^[Current].Position<Position) do begin
  Current:=Pulses^[Current].Next;
 end;
 if (Current>=0) and (Pulses^[Current].Position=Position) then begin
  FreePulse(Current);
 end;
end;

function TP64PulseStream.DeltaPositionToNextPulse(Position:longword):longword;
var Current:longint;
begin
 while Position>=P64PulseSamplesPerRotation do begin
  dec(Position,P64PulseSamplesPerRotation);
 end;
 Current:=CurrentIndex;
 if (Current<0) or ((Current<>UsedFirst) and ((Pulses^[Current].Previous>=0) and (Pulses^[Pulses^[Current].Previous].Position>=Position))) then begin
  Current:=UsedFirst;
 end;
 while (Current>=0) and (Pulses^[Current].Position<Position) do begin
  Current:=Pulses^[Current].Next;
 end;
 if Current<0 then begin
  if UsedFirst<0 then begin
   result:=P64PulseSamplesPerRotation-Position;
  end else begin
   result:=(P64PulseSamplesPerRotation+Pulses^[UsedFirst].Position)-Position;
  end;
 end else begin
  CurrentIndex:=Current;
  result:=Pulses^[Current].Position-Position;
 end;
end;

function TP64PulseStream.GetNextPulse(Position:longword):longword;
var Current:longint;
begin
 while Position>=P64PulseSamplesPerRotation do begin
  dec(Position,P64PulseSamplesPerRotation);
 end;
 Current:=CurrentIndex;
 if (Current<0) or ((Current<>UsedFirst) and ((Pulses^[Current].Previous>=0) and (Pulses^[Pulses^[Current].Previous].Position>=Position))) then begin
  Current:=UsedFirst;
 end;
 while (Current>=0) and (Pulses^[Current].Position<Position) do begin
  Current:=Pulses^[Current].Next;
 end;
 if Current<0 then begin
  if UsedFirst<0 then begin
   result:=0;
  end else begin
   result:=Pulses^[UsedFirst].Strength;
  end;
 end else begin
  CurrentIndex:=Current;
  result:=Pulses^[Current].Strength;
 end;
end;

function TP64PulseStream.GetPulseCount:longword;
var Current:longint;
begin
 result:=0;
 Current:=UsedFirst;
 while Current>=0 do begin
  inc(result);
  Current:=Pulses^[Current].Next;
 end;
end;

function TP64PulseStream.GetPulse(Position:longword):longword;
var Current:longint;
begin
 while Position>=P64PulseSamplesPerRotation do begin
  dec(Position,P64PulseSamplesPerRotation);
 end;
 Current:=CurrentIndex;
 if (Current<0) or ((Current<>UsedFirst) and ((Pulses^[Current].Previous>=0) and (Pulses^[Pulses^[Current].Previous].Position>=Position))) then begin
  Current:=UsedFirst;
 end;
 while (Current>=0) and (Pulses^[Current].Position<Position) do begin
  Current:=Pulses^[Current].Next;
 end;
 if (Current<0) or (Pulses^[Current].Position<>Position) then begin
  result:=0;
 end else begin
  CurrentIndex:=Current;
  result:=Pulses^[Current].Strength;
 end;
end;

procedure TP64PulseStream.SetPulse(Position,Strength:longword);
begin
 if Strength<>0 then begin
  AddPulse(Position,Strength);
 end else begin
  RemovePulse(Position);
 end;
end;

procedure TP64PulseStream.Seek(Position:longword);
var Current:longint;
begin
 Current:=UsedFirst;
 while (Current>=0) and (Pulses^[Current].Position<Position) do begin
  Current:=Pulses^[Current].Next;
 end;
 CurrentIndex:=Current;
end;

procedure TP64PulseStream.Dump;
var Current:longint;
begin
 Current:=UsedFirst;
 while Current>=0 do begin
  writeln(Pulses^[Current].Position,' ',Pulses^[Current].Strength);
  Current:=Pulses^[Current].Next;
 end;
 writeln;
end;

procedure TP64PulseStream.ConvertFromGCR(Bytes:pointer;Len:longint);
var Position,Increment:int64;
    BitStreamPosition:longint;
begin
 Clear;
 if Len>0 then begin

  // Encoding from GCR bits to NRZI pulses
  Increment:=(int64(P64PulseSamplesPerRotation) shl 32) div int64(Len);
  Position:=Increment shr 1;
  for BitStreamPosition:=0 to Len-1 do begin
   if (byte(PAnsiChar(Bytes)[BitStreamPosition shr 3]) and (1 shl ((not BitStreamPosition) and 7)))<>0 then begin
    AddPulse((Position shr 32) and $ffffffff,$ffffffff);
   end;
   inc(Position,Increment);
  end;
 end;
end;

procedure TP64PulseStream.ConvertToGCR(Bytes:pointer;Len:longint);
const Range=int64(P64PulseSamplesPerRotation) shl 32;
var Position,Increment:int64; {-$68db8bac710}
    BitStreamPosition,BitStreamLen,Current,ShiftRegister,Shift,BitPosition:longint;
    LastWasSyncMark:boolean;
    Temp:pointer;
begin
 if Len>0 then begin

  // Decoding from NRZI pulses to GCR bits
  FillChar(Bytes^,(Len+7) shr 3,AnsiChar(#$00));
  BitStreamLen:=Len;
  Increment:=Range div int64(BitStreamLen);
  Current:=UsedFirst;
  if Current>=0 then begin
   Position:=(int64(Pulses^[Current].Position) shl 32)-1;
  end else begin
   Position:=0;
  end;
  for BitStreamPosition:=0 to BitStreamLen-1 do begin
   inc(Position,Increment);
   while true do begin
    if (Current>=0) and (Pulses^[Current].Position<(Position shr 32)) then begin
     Position:=(int64(Pulses^[Current].Position) shl 32)+(Increment-$400000000); // 1.25 microseconds headroom
     Current:=Pulses^[Current].Next;
     byte(PAnsiChar(Bytes)[BitStreamPosition shr 3]):=byte(PAnsiChar(Bytes)[BitStreamPosition shr 3]) or (1 shl ((not BitStreamPosition) and 7));
    end else if Position>=Range then begin
     dec(Position,Range);
     Current:=UsedFirst;
     continue;
    end;
    break;
   end;
  end;

  // Try simple dirty whole-half-track-at-once-byte-realigning, but only when it is needed (for GCR-bytewise 1541 drive emulations like as in
  // older VICE versions)
  ShiftRegister:=0;
  Shift:=0;
  LastWasSyncMark:=false;
  for BitStreamPosition:=0 to Len-1 do begin
   ShiftRegister:=((ShiftRegister shl 1) and $3fe) or ((byte(PAnsiChar(Bytes)[BitStreamPosition shr 3]) shr ((not BitStreamPosition) and 7)) and 1);
   if ShiftRegister=$3ff then begin
    LastWasSyncMark:=true;
   end else begin
    if LastWasSyncMark then begin
     LastWasSyncMark:=false;
     Shift:=BitStreamPosition and 7;
     if Shift<>0 then begin
      break;
     end;
    end;
   end;
  end;
  if Shift>0 then begin
   GetMem(Temp,(Len+7) shr 3);
   Move(Bytes^,Temp^,(Len+7) shr 3);
   FillChar(Bytes^,(Len+7) shr 3,AnsiChar(#0));
   BitStreamPosition:=Shift;
   for BitPosition:=0 to Len-1 do begin
    byte(PAnsiChar(Bytes)[BitPosition shr 3]):=byte(PAnsiChar(Bytes)[BitPosition shr 3]) or (((byte(PAnsiChar(Temp)[BitStreamPosition shr 3]) shr ((not BitStreamPosition) and 7)) and 1) shl ((not BitPosition) and 7));
    inc(BitStreamPosition);
    if BitStreamPosition>=Len then begin
     BitStreamPosition:=0;
    end;
   end;
   FreeMem(Temp);

   // Try syncmark-position-subdivided half-track-at-once-byte-realigning, but only when it is needed
   ShiftRegister:=0;
   Shift:=0;
   LastWasSyncMark:=false;
   for BitStreamPosition:=0 to Len-1 do begin
    ShiftRegister:=((ShiftRegister shl 1) and $3fe) or ((byte(PAnsiChar(Bytes)[BitStreamPosition shr 3]) shr ((not BitStreamPosition) and 7)) and 1);
    if ShiftRegister=$3ff then begin
     LastWasSyncMark:=true;
    end else begin
     if LastWasSyncMark then begin
      LastWasSyncMark:=false;
      Shift:=BitStreamPosition and 7;
      if Shift<>0 then begin
       break;
      end;
     end;
    end;
   end;
   if Shift>0 then begin
    GCRByteRealign(Bytes,(Len+7) shr 3);
   end;
  end;

 end;
end;

function TP64PulseStream.ConvertToGCRWithLogic(Bytes:pointer;Len,SpeedZone:longint):longint;
var Current,BitStreamPosition:longint;
    FlipFlop,LastFlipFlop:boolean;
    Position,LastPosition,Delta,DelayCounter,Clock,Counter:longword;
begin
 LastPosition:=0;
 FlipFlop:=false;
 LastFlipFlop:=false;
 BitStreamPosition:=0;
 Clock:=SpeedZone;
 Counter:=0;
 Current:=UsedFirst;
 FillChar(Bytes^,(Len+7) shr 3,AnsiChar(#0));
 while (Current>=0) and (BitStreamPosition<Len) do begin
  if Pulses^[Current].Strength>=$80000000 then begin
   Position:=Pulses^[Current].Position;
   Delta:=Position-LastPosition;
   LastPosition:=Position;
   DelayCounter:=0;
   FlipFlop:=not FlipFlop;
   repeat
    if (DelayCounter=40) and (LastFlipFlop<>FlipFlop) then begin
     LastFlipFlop:=FlipFlop;
     Clock:=SpeedZone;
     Counter:=0;
    end;
    if Clock=16 then begin
     Clock:=SpeedZone;
     Counter:=(Counter+1) and $f;
     if (Counter and 3)=2 then begin
      byte(PAnsiChar(Bytes)[BitStreamPosition shr 3]):=byte(PAnsiChar(Bytes)[BitStreamPosition shr 3]) or ((((Counter+$1c) shr 4) and 1) shl ((not BitStreamPosition) and 7));
      inc(BitStreamPosition);
     end;
    end;
    inc(Clock);
    inc(DelayCounter);
   until DelayCounter>=Delta;
  end;
  Current:=Pulses^[Current].Next;
 end;
 if abs(((BitStreamPosition+7) shr 3)-Len)<2 then begin
  result:=Len shl 3;
 end else begin
  result:=BitStreamPosition;
 end;
end;

function TP64PulseStream.ReadFromStream(Stream:TStream):boolean;
const ModelPosition=0;
      ModelStrength=4;
      ModelPositionFlag=8;
      ModelStrengthFlag=9;
const ProbabilityCounts:array[0..9] of longword=(65536,65536,65536,65536,65536,65536,65536,65536,4,4);
var RangeCoderProbabilities:PRangeCoderProbabilities;
    RangeCoderProbabilityOffsets:array[0..high(ProbabilityCounts)] of longword;
    RangeCoderProbabilityStates:array[0..high(ProbabilityCounts)] of longword;
    RangeCoderInstance:TRangeCoder;
    ProbabilityCount,Index,Count,DeltaPosition,Position,Strength:longword;
    Buffer:pointer;
    Header:TP64HalfTrackHeader;

 function ReadDWord(Model:longword):longword;
 var ByteValue,ByteIndex,Context,Bit:longword;
 begin
  result:=0;
  for ByteIndex:=0 to 3 do begin
   Context:=1;
   for Bit:=7 downto 0 do begin
    Context:=(Context shl 1) or RangeCoderDecodeBit(RangeCoderInstance,RangeCoderProbabilities^[RangeCoderProbabilityOffsets[Model+ByteIndex]+(((RangeCoderProbabilityStates[Model+ByteIndex] shl 8) or Context) and $ffff)],4);
   end;
   ByteValue:=Context and $ff;
   RangeCoderProbabilityStates[Model+ByteIndex]:=ByteValue;
   result:=result or ((ByteValue and $ff) shl (ByteIndex shl 3));
  end;
 end;

 function ReadBit(Model:longword):longword;
 begin
  result:=RangeCoderDecodeBit(RangeCoderInstance,RangeCoderProbabilities^[RangeCoderProbabilityOffsets[Model]+RangeCoderProbabilityStates[Model]],4);
  RangeCoderProbabilityStates[Model]:=result;
 end;

begin
 result:=false;
 if Stream.Read(Header,sizeof(TP64HalfTrackHeader))=sizeof(TP64HalfTrackHeader) then begin
  if Header.Size=0 then begin
   result:=true;
  end else begin
   GetMem(Buffer,Header.Size);
   try
    if Stream.Read(Buffer^,Header.Size)=longint(Header.Size) then begin

     ProbabilityCount:=0;
     for Index:=0 to high(ProbabilityCounts) do begin
      RangeCoderProbabilityOffsets[Index]:=ProbabilityCount;
      inc(ProbabilityCount,ProbabilityCounts[Index]);
      RangeCoderProbabilityStates[Index]:=0;
     end;             
     RangeCoderProbabilities:=RangeCoderProbabilitiesAllocate(ProbabilityCount);
     RangeCoderProbabilitiesReset(RangeCoderProbabilities,ProbabilityCount);

     FillChar(RangeCoderInstance,SizeOf(TRangeCoder),AnsiChar(#0));
     RangeCoderInit(RangeCoderInstance);

     RangeCoderInstance.Buffer:=Buffer;
     RangeCoderInstance.BufferSize:=Header.Size;
     RangeCoderInstance.BufferPosition:=0;
     RangeCoderStart(RangeCoderInstance);

     Count:=0;

     Position:=0;
     DeltaPosition:=0;

     Strength:=0;

     while Count<Header.CountPulses do begin

      if ReadBit(ModelPositionFlag)<>0 then begin
       DeltaPosition:=ReadDWord(ModelPosition);
       if DeltaPosition=0 then begin
        break;
       end;
      end;
      inc(Position,DeltaPosition);

      if ReadBit(ModelStrengthFlag)<>0 then begin
       inc(Strength,ReadDWord(ModelStrength));
      end;

      AddPulse(Position,Strength);

      inc(Count);
     end;

     RangeCoderProbabilitiesFree(RangeCoderProbabilities);

     result:=Count=Header.CountPulses;

    end;
   finally
    FreeMem(Buffer);
   end;
  end;
 end;
end;

function TP64PulseStream.WriteToStream(Stream:TStream):boolean;
const ModelPosition=0;
      ModelStrength=4;
      ModelPositionFlag=8;
      ModelStrengthFlag=9;
const ProbabilityCounts:array[0..9] of longword=(65536,65536,65536,65536,65536,65536,65536,65536,2,2);
var RangeCoderProbabilities:PRangeCoderProbabilities;
    RangeCoderProbabilityOffsets:array[0..high(ProbabilityCounts)] of longword;
    RangeCoderProbabilityStates:array[0..high(ProbabilityCounts)] of longword;
    RangeCoderInstance:TRangeCoder;
    Index,Current:longint;
    ProbabilityCount,LastPosition,PreviousDeltaPosition,DeltaPosition,LastStrength:longword;
    Header:TP64HalfTrackHeader;

 procedure WriteDWord(Model,Value:longword);
 var ByteValue,ByteIndex,Context,Bit:longword;
 begin
  for ByteIndex:=0 to 3 do begin
   ByteValue:=(Value shr (ByteIndex shl 3)) and $ff;
   Context:=1;
   for Bit:=7 downto 0 do begin
    Context:=(Context shl 1) or RangeCoderEncodeBit(RangeCoderInstance,RangeCoderProbabilities[RangeCoderProbabilityOffsets[Model+ByteIndex]+(((RangeCoderProbabilityStates[Model+ByteIndex] shl 8) or Context) and $ffff)],4,(ByteValue shr Bit) and 1);
   end;
   RangeCoderProbabilityStates[Model+ByteIndex]:=ByteValue;
  end;
 end;

 procedure WriteBit(Model,Value:longword);
 begin
  RangeCoderProbabilityStates[Model]:=RangeCoderEncodeBit(RangeCoderInstance,RangeCoderProbabilities[RangeCoderProbabilityOffsets[Model]+RangeCoderProbabilityStates[Model]],4,Value and 1);
 end;

begin
 result:=false;

 FillChar(Header,SizeOf(TP64HalfTrackHeader),AnsiChar(#0));
 Header.CountPulses:=0;
 Header.Size:=0;

 ProbabilityCount:=0;
 for Index:=0 to high(ProbabilityCounts) do begin
  RangeCoderProbabilityOffsets[Index]:=ProbabilityCount;
  inc(ProbabilityCount,ProbabilityCounts[Index]);
  RangeCoderProbabilityStates[Index]:=0;
 end;
 RangeCoderProbabilities:=RangeCoderProbabilitiesAllocate(ProbabilityCount);
 RangeCoderProbabilitiesReset(RangeCoderProbabilities,ProbabilityCount);

 FillChar(RangeCoderInstance,SizeOf(TRangeCoder),AnsiChar(#0));
 RangeCoderInit(RangeCoderInstance);

 LastPosition:=0;
 PreviousDeltaPosition:=0;

 LastStrength:=0;

 Current:=UsedFirst;
 while Current>=0 do begin

  DeltaPosition:=Pulses^[Current].Position-LastPosition;
  if PreviousDeltaPosition<>DeltaPosition then begin
   PreviousDeltaPosition:=DeltaPosition;
   WriteBit(ModelPositionFlag,1);
   WriteDWord(ModelPosition,DeltaPosition);
  end else begin
   WriteBit(ModelPositionFlag,0);
  end;
  LastPosition:=Pulses^[Current].Position;

  if LastStrength<>Pulses^[Current].Strength then begin
   WriteBit(ModelStrengthFlag,1);
   WriteDWord(ModelStrength,Pulses^[Current].Strength-LastStrength);
  end else begin
   WriteBit(ModelStrengthFlag,0);
  end;
  LastStrength:=Pulses^[Current].Strength;

  inc(Header.CountPulses);

  Current:=Pulses^[Current].Next;
 end;

 // End code
 WriteBit(ModelPositionFlag,1);
 WriteDWord(ModelPosition,0);

 RangeCoderFlush(RangeCoderInstance);

 RangeCoderProbabilitiesFree(RangeCoderProbabilities);

 if assigned(RangeCoderInstance.Buffer) then begin
  Header.Size:=RangeCoderInstance.BufferPosition;
 end;

 if Stream.Write(Header,sizeof(TP64HalfTrackHeader))=sizeof(TP64HalfTrackHeader) then begin
  if assigned(RangeCoderInstance.Buffer) then begin
   result:=Stream.Write(RangeCoderInstance.Buffer^,RangeCoderInstance.BufferPosition)=longint(RangeCoderInstance.BufferPosition);
  end else begin
   result:=true;
  end;
 end;

 if assigned(RangeCoderInstance.Buffer) then begin
  FreeMem(RangeCoderInstance.Buffer);
 end;
end;

constructor TP64Image.Create;
var HalfTrack:longint;
begin
 inherited Create;
 for HalfTrack:=low(TP64PulseStreams) to high(TP64PulseStreams) do begin
  PulseStreams[HalfTrack]:=TP64PulseStream.Create;
 end;
 Clear;
end;

destructor TP64Image.Destroy;
var HalfTrack:longint;
begin
 for HalfTrack:=low(TP64PulseStreams) to high(TP64PulseStreams) do begin
  PulseStreams[HalfTrack].Destroy;
 end;
 inherited Destroy;
end;

procedure TP64Image.Clear;
var HalfTrack:longint;
begin
 WriteProtected:=false;
 for HalfTrack:=low(TP64PulseStreams) to high(TP64PulseStreams) do begin
  PulseStreams[HalfTrack].Clear;
 end;
end;

function TP64Image.ReadFromStream(Stream:TStream):boolean;
var ChunksMemoryStream,ChunkMemoryStream:TMemoryStream;
    Header:TP64Header;
    HalfTrack:longword;
    ChunkHeader:TP64ChunkHeader;
    OK:boolean;
begin

 result:=false;

 Clear;
 
 if Stream.Read(Header,SizeOf(TP64Header))=SizeOf(TP64Header) then begin
  if (Header.Signature=P64Signature) and (Header.Version=$00000000) then begin

   WriteProtected:=(Header.Flags and 1)<>0;

   ChunksMemoryStream:=TMemoryStream.Create;
   try

    if ChunksMemoryStream.CopyFrom(Stream,Header.Size)=Header.Size then begin
     if CRC32(ChunksMemoryStream.Memory,Header.Size)=Header.Checksum then begin
      if ChunksMemoryStream.Seek(0,soBeginning)=0 then begin

       OK:=true;
       while OK and (ChunksMemoryStream.Position<ChunksMemoryStream.Size) do begin
        if ChunksMemoryStream.Read(ChunkHeader,SizeOf(TP64ChunkHeader))=SizeOf(TP64ChunkHeader) then begin
         OK:=false;
         ChunkMemoryStream:=TMemoryStream.Create;
         try
          if ChunkHeader.Size=0 then begin
           if ChunkHeader.Checksum=0 then begin
            if ChunkHeader.Signature='DONE' then begin
             OK:=true;
            end else begin
             OK:=true;
            end;
           end;
          end else begin
           if ChunkMemoryStream.CopyFrom(ChunksMemoryStream,ChunkHeader.Size)=longint(ChunkHeader.Size) then begin
            if ChunkMemoryStream.Seek(0,soBeginning)=0 then begin
             if CRC32(ChunkMemoryStream.Memory,ChunkHeader.Size)=ChunkHeader.Checksum then begin
              if (ChunkHeader.Signature[0]='H') and (ChunkHeader.Signature[1]='T') and (ChunkHeader.Signature[2]='P') and (ChunkHeader.Signature[3] in [ansichar(byte(FirstHalfTrack))..ansichar(byte(LastHalfTrack))]) then begin
               HalfTrack:=byte(ansichar(ChunkHeader.Signature[3]));
               OK:=PulseStreams[HalfTrack].ReadFromStream(ChunkMemoryStream);
              end else begin
               OK:=true;
              end;
             end;
            end;
           end;
          end;
         finally
          ChunkMemoryStream.Free;
         end;
        end else begin
         break;
        end;
       end;

       result:=OK;

      end;
     end;
    end;

   finally
    ChunksMemoryStream.Free;
   end;

  end;
 end;

end;

function TP64Image.WriteToStream(Stream:TStream):boolean;
var MemoryStream,ChunksMemoryStream,ChunkMemoryStream:TMemoryStream;
    Header:TP64Header;
    HalfTrack:longword;
    ChunkSignature:TP64ChunkSignature;

 function WriteChunk:boolean;
 var ChunkHeader:TP64ChunkHeader;
 begin
  result:=false;
  ChunkHeader.Signature:=ChunkSignature;
  ChunkHeader.Size:=ChunkMemoryStream.Size;
  if ChunkMemoryStream.Size>0 then begin
   ChunkHeader.Checksum:=CRC32(ChunkMemoryStream.Memory,ChunkMemoryStream.Size);
  end else begin
   ChunkHeader.Checksum:=0;
  end;
  if ChunksMemoryStream.Write(ChunkHeader,SizeOf(TP64ChunkHeader))=SizeOf(TP64ChunkHeader) then begin
   if ChunkMemoryStream.Size=0 then begin
    result:=true;
   end else begin
    if ChunkMemoryStream.Seek(0,soBeginning)=0 then begin
     if ChunksMemoryStream.CopyFrom(ChunkMemoryStream,ChunkMemoryStream.Size)=ChunkMemoryStream.Size then begin
      result:=true;
     end;
    end;
   end;
  end;
 end;
 
begin
 FillChar(Header,SizeOf(TP64Header),AnsiChar(#0));
 Header.Signature:=P64Signature;
 Header.Version:=$00000000;

 MemoryStream:=TMemoryStream.Create;
 try

  ChunksMemoryStream:=TMemoryStream.Create;
  try

   result:=true;
   for HalfTrack:=FirstHalfTrack to LastHalfTrack do begin

    ChunkMemoryStream:=TMemoryStream.Create;
    try
     result:=PulseStreams[HalfTrack].WriteToStream(ChunkMemoryStream);
     if result then begin
      ChunkSignature:='HTP'#0;
      ChunkSignature[3]:=ansichar(byte(HalfTrack));
      result:=WriteChunk;
     end;
    finally
     ChunkMemoryStream.Free;
    end;
    if not result then begin
     break;
    end;

   end;

   if result then begin

    ChunkMemoryStream:=TMemoryStream.Create;
    try
     ChunkSignature:='DONE';
     result:=WriteChunk;
    finally
     ChunkMemoryStream.Free;
    end;

    if result then begin
     result:=false;

     Header.Flags:=0;
     if WriteProtected then begin
      Header.Flags:=Header.Flags or 1;
     end;

     Header.Size:=ChunksMemoryStream.Size;
     Header.Checksum:=CRC32(ChunksMemoryStream.Memory,Header.Size);

     if MemoryStream.Write(Header,SizeOf(TP64Header))=SizeOf(TP64Header) then begin
      if ChunksMemoryStream.Seek(0,soBeginning)=0 then begin
       if MemoryStream.CopyFrom(ChunksMemoryStream,ChunksMemoryStream.Size)=ChunksMemoryStream.Size then begin
        if MemoryStream.Seek(0,soBeginning)=0 then begin
         if Stream.CopyFrom(MemoryStream,MemoryStream.Size)=MemoryStream.Size then begin
          result:=true;
         end;
        end;
       end;
      end;
     end;

    end;

   end;

  finally
   ChunksMemoryStream.Free;
  end;

 finally
  MemoryStream.Free;
 end;
end;

function DumpP64(SrcStream,dstStream:TBeRoStream):boolean;
var P64Image:TP64Image;
    MemoryStream:TMemoryStream;
    Data:pointer;
    Size:longint;
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
            WriteString('Strength 0x'+Ansistring(lowercase(Ansistring(IntToHex(P64Image.PulseStreams[HalfTrack].Pulses[Pulse].Strength,8)))));
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

function DumpP64HalfTrack(SrcStream,dstStream:TBeRoStream;HalfTrack:longint):boolean;
var P64Image:TP64Image;
    MemoryStream:TMemoryStream;
    MemoryStreamEx:TBeRoMemoryStream;
    Data:pointer;
    Size:longint;
    Pulse:longint;
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

          DstStream.Clear;

          MemoryStreamEx:=TBeRoMemoryStream.Create;
          try
           if (HalfTrack>=FirstHalfTrack) and (HalfTrack<=LastHalfTrack) then begin
            Pulse:=P64Image.PulseStreams[HalfTrack].UsedFirst;
            while Pulse>=0 do begin
             if P64Image.PulseStreams[HalfTrack].Pulses[Pulse].Strength>0 then begin
              MemoryStreamEx.WriteDWord(P64Image.PulseStreams[HalfTrack].Pulses[Pulse].Position);
              MemoryStreamEx.WriteDWord(P64Image.PulseStreams[HalfTrack].Pulses[Pulse].Strength);
             end;
             Pulse:=P64Image.PulseStreams[HalfTrack].Pulses[Pulse].Next;
            end;
           end;
           MemoryStreamEx.Seek(0,soFromBeginning);
           DstStream.AppendFrom(MemoryStreamEx,MemoryStreamEx.Size);
          finally 
           MemoryStreamEx.Free;
          end;

          result:=true;

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

function ConvertG64toP64(SrcStream,DstStream:TBeRoStream):boolean;
var HalfTrack:longint;
    GCR:^TGCR;
    P64Image:TP64Image;
    MemoryStream:TMemoryStream;
begin
 result:=false;
 New(GCR);
 P64Image:=TP64Image.Create;
 try
  FillChar(GCR^,SizeOf(TGCR),#0);
  FillChar(GCR^.Data,SizeOf(GCR^.Data),#$00);
  FillChar(GCR^.SpeedZone,SizeOf(GCR^.SpeedZone),#$00);
  GCR.MaximumHalfTrackSize:=FileMinMaxBytesPerGCRHalfTrack;
  SrcStream.Seek(0);
  if G64Read(GCR^,SrcStream) then begin
   for HalfTrack:=FirstHalfTrack to LastHalfTrack do begin
    if ((HalfTrack-FirstHalfTrack)<=GCR^.HalfTracks) and (GCR^.HalfTrackSize[HalfTrack]>0) and not G64IsEmpty(GCR^.Data[HalfTrack*MaxBytesPerGCRHalfTrack],GCR^.HalfTrackSize[HalfTrack],2,$00) then begin
     P64Image.PulseStreams[HalfTrack].ConvertFromGCR(@GCR^.Data[HalfTrack*MaxBytesPerGCRHalfTrack],GCR^.HalfTrackSize[HalfTrack] shl 3);
    end;
   end;
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
  if assigned(GCR) then begin
   Dispose(GCR);
  end;
 end;
end;

function ConvertP64toG64(SrcStream,DstStream:TBeRoStream;WithLogic:boolean):boolean;
var P64Image:TP64Image;
    MemoryStream:TMemoryStream;
    Data:pointer;
    Size,HalfTrack,TrackLen:longint;
    GCR:PGCR;
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

          New(GCR);
          try

           FillChar(GCR^,SizeOf(TGCR),#0);
           FillChar(GCR^.Data,SizeOf(GCR^.Data),#$00);
           FillChar(GCR^.SpeedZone,SizeOf(GCR^.SpeedZone),#$00);
           GCR.MaximumHalfTrackSize:=FileMinMaxBytesPerGCRHalfTrack;

           for HalfTrack:=FirstHalfTrack to LastHalfTrack do begin
            if WithLogic then begin
             TrackLen:=(P64Image.PulseStreams[HalfTrack].ConvertToGCRWithLogic(@GCR.Data[HalfTrack*MaxBytesPerGCRHalfTrack],RawTrackSize[SpeedMap[HalfTrack shr 1]] shl 3,SpeedMap[HalfTrack shr 1])+7) shr 3;
             FillChar(GCR.SpeedZone[HalfTrack*MaxBytesPerGCRHalfTrack],TrackLen,ansichar(byte(SpeedMap[HalfTrack shr 1])));
            end else begin
             TrackLen:=RawTrackSize[SpeedMap[HalfTrack shr 1]];
             P64Image.PulseStreams[HalfTrack].ConvertToGCR(@GCR.Data[HalfTrack*MaxBytesPerGCRHalfTrack],RawTrackSize[SpeedMap[HalfTrack shr 1]] shl 3);
             FillChar(GCR.SpeedZone[HalfTrack*MaxBytesPerGCRHalfTrack],TrackLen,ansichar(byte(SpeedMap[HalfTrack shr 1])));
            end;
            if G64IsEmpty(GCR^.Data[HalfTrack*MaxBytesPerGCRHalfTrack],TrackLen,2,0) then begin
             TrackLen:=0;
            end;
            GCR.HalfTrackSize[HalfTrack]:=TrackLen;
            if GCR.MaximumHalfTrackSize<TrackLen then begin
             GCR.MaximumHalfTrackSize:=TrackLen;
            end;
           end;
           result:=G64Write(GCR^,DstStream);

          finally
           Dispose(GCR);
          end;

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

procedure InitializeP64LogicTriggerBits;
const Overflow=$100000000;
var Increment,Accumulator:int64;
    SpeedZone,BitStreamLength,BitStreamPosition,Position,LastPosition:longint;
begin
 FillChar(P64LogicTriggerBits,sizeof(TP64LogicTriggerBits),#0);
 FillChar(P64LogicTriggerPositions,sizeof(TP64LogicTriggerPositions),#$ff);
 for SpeedZone:=0 to 3 do begin
  BitStreamLength:=RawTrackSize[SpeedZone and 3] shl 3;
  Increment:=(int64(P64PulseSamplesPerRotation) shl 32) div int64(BitStreamLength);
  Accumulator:=0;
  Position:=0;
  LastPosition:=0;
  for BitStreamPosition:=1 to BitStreamLength do begin
   inc(Accumulator,Increment);
   while Accumulator>=Overflow do begin
    dec(Accumulator,Overflow);
    LastPosition:=Position;
    inc(Position);
    if Position>=P64PulseSamplesPerRotation then begin
     Position:=0;
    end;
   end;
   P64LogicTriggerBits[SpeedZone,LastPosition shr 4]:=P64LogicTriggerBits[SpeedZone,LastPosition shr 4] or (1 shl (LastPosition and 15));
   P64LogicTriggerPositions[SpeedZone,LastPosition shr 4]:=LastPosition and 15;
  end;
 end;
end;

procedure InitializeP64WriteLogicTriggerBits;
const Overflow=$100000000;
var Increment,HalfIncrement,Accumulator,WriteAccumulator:int64;
    SpeedZone,BitStreamLength,BitStreamPosition,Position,LastPosition:longint;
begin
 FillChar(P64WriteLogicTriggerBits,sizeof(TP64LogicTriggerBits),#$0);
 FillChar(P64WriteLogicTriggerPositions,sizeof(TP64LogicTriggerPositions),#$ff);
 for SpeedZone:=0 to 3 do begin
  BitStreamLength:=RawTrackSize[SpeedZone] shl 3;
  Increment:=(int64(P64PulseSamplesPerRotation) shl 32) div int64(BitStreamLength);
  HalfIncrement:=(Increment+1) shr 1;
  Accumulator:=0;
  Position:=0;
  LastPosition:=0;
  for BitStreamPosition:=1 to BitStreamLength do begin
   WriteAccumulator:=0;
   inc(Accumulator,Increment);
   while Accumulator>=Overflow do begin
    dec(Accumulator,Overflow);
    inc(WriteAccumulator,Overflow);
    if WriteAccumulator<HalfIncrement then begin
     LastPosition:=Position;
    end;
    inc(Position);
    if Position>=P64PulseSamplesPerRotation then begin
     Position:=0;
    end;
   end;
   P64WriteLogicTriggerBits[SpeedZone,LastPosition shr 4]:=P64WriteLogicTriggerBits[SpeedZone,LastPosition shr 4] or (1 shl (LastPosition and 15));
   P64WriteLogicTriggerPositions[SpeedZone,LastPosition shr 4]:=LastPosition and 15;
  end;
 end;
end;

initialization
 InitializeP64LogicTriggerBits;
 InitializeP64WriteLogicTriggerBits;
end.

