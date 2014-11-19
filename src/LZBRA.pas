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
unit LZBRA;
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
 {$pic on}
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
{$assertions off}
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

type TCompressLZBRAStatusHook=function(Current,Total:longint):boolean;

function CompressLZBRA(SourcePointer:pointer;var DestinationPointer:pointer;SourceSize,WindowSize:longword;OptimalMatching:boolean;StatusHook:TCompressLZBRAStatusHook):longword;
function DecompressLZBRA(SourcePointer:pointer;var DestinationPointer:pointer;SourceSize:longword):longword;

implementation

const FlagModel=0;
      PrevMatchModel=2;
      MatchLowModel=3;
      LiteralModel=35;
      Gamma0Model=291;
      Gamma1Model=547;
      SizeModels=803;

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

function CompressLZBRA(SourcePointer:pointer;var DestinationPointer:pointer;SourceSize,WindowSize:longword;OptimalMatching:boolean;StatusHook:TCompressLZBRAStatusHook):longword;
type PNode=^TNode;
     TNode=record
      DataPointer:pointer;
      Previous,Next:PNode;
     end;
     PNodes=^TNodes;
     TNodes=array[0..($7fffffff div sizeof(TNode))-1] of TNode;
     PRecentNodes=^TRecentNodes;
     TRecentNodes=array[byte] of PNode;
var Source,Destination,EndPointer,LastHashed:pansichar;
    DestinationAllocated:longword;
    Nodes:PNodes;
    RecentNodes:PRecentNodes;
    NodePosition:longword;
    Code,Range:longword;
    Model:array[0..SizeModels-1] of longword;
    LastWasMatch:boolean;
    LastPosition:longint;

 procedure IncrementSize(Count:longword);
 begin
  if ((ptruint(Destination)-ptruint(DestinationPointer))+Count)>=DestinationAllocated then begin
   while ((ptruint(Destination)-ptruint(DestinationPointer))+Count)>=DestinationAllocated do begin
    inc(DestinationAllocated,DestinationAllocated);
   end;
   dec(ptruint(Destination),ptruint(DestinationPointer));
   ReAllocMem(DestinationPointer,DestinationAllocated);
   inc(ptruint(Destination),ptruint(DestinationPointer));
  end;
 end;

 function AddNode(Data:pansichar):boolean;
 var Prefix:byte;
     LastNode:PNode;
     NewNode:PNode;
 begin
  result:=NodePosition<(SourceSize-1);
  if result then begin
   Prefix:=byte(pointer(Data)^);
   LastNode:=RecentNodes^[Prefix];
   NewNode:=@Nodes^[NodePosition];
   with NewNode^ do begin
    DataPointer:=Data;
    Previous:=LastNode;
    Next:=nil;
   end;
   if assigned(LastNode) then begin
    LastNode^.Next:=NewNode;
   end;
   RecentNodes^[Prefix]:=NewNode;
   inc(NodePosition);
  end;
 end;

 function RemoveNode(Data:pointer):boolean;
 var Prefix:word;
     Node:PNode;
 begin
  result:=NodePosition<(SourceSize-1);
  if result then begin
   Prefix:=byte(Data^);
   Node:=RecentNodes^[Prefix];
   if assigned(Node) and (Node^.DataPointer=Data) and (Node=@Nodes^[NodePosition-1]) then begin
    RecentNodes^[Prefix]:=Node^.Previous;
    if assigned(Node^.Previous) then begin
     Node^.Previous^.Next:=Node^.Next;
    end;
    if assigned(Node^.Next) then begin
     Node^.Next^.Previous:=Node^.Previous;
    end;
    Node^.Previous:=nil;
    Node^.Next:=nil;
    dec(NodePosition);
   end;
  end;
 end;

 procedure DoHash(Source:pansichar);
 begin
  while LastHashed<Source do begin
   AddNode(LastHashed);
   inc(LastHashed);
  end;
 end;

 procedure DoUnhash(Source:pansichar);
 begin
  while LastHashed>Source do begin
   dec(LastHashed);
   RemoveNode(LastHashed);
  end;
 end;

 function EncodeBit(ModelIndex,Move,Bit:longint):longint;
 var Bound,OldCode:longword;
     p:pansichar;
 begin
  Bound:=(Range shr 12)*Model[ModelIndex];
  if Bit=0 then begin
   Range:=Bound;
   inc(Model[ModelIndex],(4096-Model[ModelIndex]) shr Move);
  end else begin
   OldCode:=Code;
   inc(Code,Bound);
   dec(Range,Bound);
   dec(Model[ModelIndex],Model[ModelIndex] shr Move);
   if Code<OldCode then begin
    p:=@Destination[-1];
    while p^=#$ff do begin
     p^:=#0;
     dec(p);
    end;
    inc(p^);
   end;
  end;
  while Range<$1000000 do begin
   IncrementSize(1);
   byte(pointer(Destination)^):=Code shr 24;
   inc(Destination);
   Code:=Code shl 8;
   Range:=Range shl 8;
  end;
  result:=Bit;
 end;

 procedure EncoderFlush;
 var OldCode,Bytes:longword;
     p:pansichar;
 begin
  OldCode:=Code;
  if Range>$2000000 then begin
   inc(Code,$1000000);
   Range:=$800000;
  end else begin
   inc(Code,$800000);
   Range:=$8000;
  end;
  if Code<OldCode then begin
   p:=@Destination[-1];
   while p^=#$ff do begin
    p^:=#0;
    dec(p);
   end;
   inc(p^);
  end;
  for Bytes:=1 to 4 do begin
   IncrementSize(1);
   byte(pointer(Destination)^):=Code shr 24;
   inc(Destination);
   Code:=Code shl 8;
   Range:=Range shl 8;
  end;
  longword(pointer(@pansichar(DestinationPointer)[4])^):=(byte(pansichar(DestinationPointer)[4]) shl 24) or (byte(pansichar(DestinationPointer)[5]) shl 16) or (byte(pansichar(DestinationPointer)[6]) shl 8) or byte(pansichar(DestinationPointer)[7]);
 end;

 procedure EncodeTree(ModelIndex,Bits,Move,Value:longint);
 var Context:longint;
 begin
  Context:=1;
  while Bits>0 do begin
   dec(Bits);
   Context:=(Context shl 1) or EncodeBit(ModelIndex+Context,Move,(Value shr Bits) and 1);
  end;
 end;

 procedure EncodeGamma(ModelIndex,Value:longword);
 var Mask:longword;
     Context:byte;
 begin
  Context:=1;
  Mask:=Value shr 1;
  while (Mask and (Mask-1))<>0 do begin
   Mask:=Mask and (Mask-1);
  end;
  while Mask<>0 do begin
   Context:=(Context shl 1) or EncodeBit(ModelIndex+Context,5,(0-(Mask shr 1)) shr 31);
   Context:=(Context shl 1) or longword(EncodeBit(ModelIndex+Context,5,(0-(Value and Mask)) shr 31));
   Mask:=Mask shr 1;
  end;
 end;

 procedure EncodeEnd(ModelIndex:longint);
 var Bits:longword;
     Context:byte;
 begin
  Context:=1;
  Bits:=32;
  while Bits>0 do begin
   dec(Bits);
   Context:=(Context shl 1) or EncodeBit(ModelIndex+Context,5,(0-Bits) shr 31);
   EncodeBit(ModelIndex+Context,5,0);
   Context:=Context shl 1;
  end;
 end;

 function CompareBytes(FirstComparePointer,SecondComparePointer:pansichar):longword;
 begin
  result:=0;
  while (SecondComparePointer<EndPointer) and (FirstComparePointer^=SecondComparePointer^) do begin
   inc(result);
   inc(FirstComparePointer);
   inc(SecondComparePointer);
  end;
 end;

 procedure DoSearch(Source:pansichar;var BestPosition,BestFoundLength:longint);
 var SearchPointer:pansichar;
     FoundLength,Position:longint;
     Node:PNode;
 begin
  BestPosition:=0;
  BestFoundLength:=1;
  Node:=RecentNodes^[byte(pointer(Source)^)];
  while assigned(Node) and ((longword(Source)-longword(Node^.DataPointer))<=WindowSize) do begin
   SearchPointer:=Node^.DataPointer;
   FoundLength:=CompareBytes(SearchPointer,Source);
   if FoundLength>1 then begin
    Position:=pansichar(Source)-pansichar(SearchPointer);
    if ((Position>0) and ((Position<96) or ((Position>96) and (FoundLength>3)) or ((Position>2048) and (FoundLength>4)))) and
       ((BestFoundLength<FoundLength) or (((BestFoundLength=FoundLength) and (Position<=BestPosition)))) then begin
     BestFoundLength:=FoundLength;
     BestPosition:=Position;
    end;
   end;
   Node:=Node^.Previous;
  end;
 end;

 procedure PutResult(var Source:pansichar;BestPosition,BestFoundLength:longint); register;
 var Offset:longword;
 begin
  if (BestFoundLength>1) and (BestPosition>0) then begin
   inc(Source,BestFoundLength);
   EncodeBit(FlagModel+byte(boolean(LastWasMatch)),5,1);
   if (not LastWasMatch) and (BestPosition=LastPosition) then begin
    EncodeBit(PrevMatchModel,5,1);
   end else begin
    if not LastWasMatch then begin
     EncodeBit(PrevMatchModel,5,0);
    end;
    Offset:=BestPosition-1;
    EncodeGamma(Gamma0Model,(Offset shr 4)+2);
    EncodeTree(MatchLowModel+(ord((Offset shr 4)<>0) shl 4),4,5,Offset and $f);
    dec(BestFoundLength,ord(BestPosition>=96)+ord(BestPosition>=2048));
   end;
   EncodeGamma(Gamma1Model,BestFoundLength);
   LastWasMatch:=true;
   LastPosition:=BestPosition;
  end else begin
   EncodeBit(FlagModel+byte(boolean(LastWasMatch)),5,0);
   EncodeTree(LiteralModel,8,4,byte(pointer(Source)^));
   inc(Source);
   LastWasMatch:=false;
  end;
 end;

var BestPosition,BestFoundLength:longint;
    LookaheadBestPosition,LookaheadBestFoundLength:longint;
    Lookahead,OldLastHashed:pansichar;
begin
 result:=0;
 if SourceSize>0 then begin
  GetMem(Nodes,SourceSize*sizeof(TNode));
  New(RecentNodes);
  FillChar(Nodes^,SourceSize*sizeof(TNode),#0);
  FillChar(RecentNodes^,sizeof(TRecentNodes),#0);

  NodePosition:=0;
  Source:=SourcePointer;
  LastHashed:=Source;

  DestinationAllocated:=(SourceSize shr 1) or 16;
  GetMem(DestinationPointer,DestinationAllocated);
  Destination:=DestinationPointer;
  EndPointer:=Source;
  inc(EndPointer,SourceSize);

  IncrementSize(4);
  longword(pointer(Destination)^):=0;
  inc(Destination,4);

  Range:=$ffffffff;
  Code:=0;

  LastPosition:=-1;

  for BestPosition:=0 to SizeModels-1 do begin
   Model[BestPosition]:=2048;
  end;

  BestPosition:=0;
  BestFoundLength:=0;

  LookaheadBestPosition:=0;
  LookaheadBestFoundLength:=0;

  EncodeTree(LiteralModel,8,4,byte(pointer(Source)^));
  inc(Source);

  while longword(Source)<longword(EndPointer) do begin
   if assigned(StatusHook) then begin
    StatusHook(pansichar(Source)-pansichar(SourcePointer),SourceSize);
   end;
   DoSearch(Source,BestPosition,BestFoundLength);
   if OptimalMatching and (BestFoundLength>1) then begin
    Lookahead:=Source;
    OldLastHashed:=LastHashed;
    while Lookahead<pansichar(@Source[BestFoundLength]) do begin
     inc(Lookahead);
     DoHash(Lookahead);
     DoSearch(Lookahead,LookaheadBestPosition,LookaheadBestFoundLength);
     if LookaheadBestFoundLength>0 then begin
      if (BestFoundLength+(Lookahead-Source))<=LookaheadBestFoundLength then begin
       BestPosition:=0;
       BestFoundLength:=1;
      end;
      break;
     end;
    end;
    DoUnhash(OldLastHashed);
   end;
   PutResult(Source,BestPosition,BestFoundLength);
   DoHash(Source);
  end;

  EncodeBit(FlagModel+byte(boolean(LastWasMatch)),5,1);
  if not LastWasMatch then begin
   EncodeBit(PrevMatchModel,5,0);
  end;
  EncodeEnd(Gamma0Model);

  EncoderFlush;

  Dispose(RecentNodes);
  FreeMem(Nodes);

  longword(pointer(DestinationPointer)^):=SourceSize;

  result:=Destination-pansichar(DestinationPointer);
 end;

end;

function DecompressLZBRA(SourcePointer:pointer;var DestinationPointer:pointer;SourceSize:longword):longword;
var Code,Range:longword;
    Model:array[0..SizeModels-1] of longword;
    Source:pansichar;
    Abort:boolean;

 function DecodeBit(ModelIndex,Move:longint):longint;
 var Bound:longword;
 begin
  Bound:=(Range shr 12)*Model[ModelIndex];
  if Code<Bound then begin
   Range:=Bound;
   inc(Model[ModelIndex],(4096-Model[ModelIndex]) shr Move);
   result:=0;
  end else begin
   dec(Code,Bound);
   dec(Range,Bound);
   dec(Model[ModelIndex],Model[ModelIndex] shr Move);
   result:=1;
  end;
  while Range<$1000000 do begin
   if ptruint(Source)<(ptruint(SourcePointer)+SourceSize) then begin
    Code:=(Code shl 8) or byte(pointer(Source)^);
    inc(Source);
    Range:=Range shl 8;
   end else begin
    Abort:=true;
    break;
   end;
  end;
 end;

 function DecodeTree(ModelIndex,MaxValue,Move:longint):longint;
 begin
  result:=1;
  while result<MaxValue do begin
   result:=(result shl 1) or DecodeBit(ModelIndex+result,Move);
  end;
  dec(result,MaxValue);
 end;

 function DecodeGamma(ModelIndex:longint):longint;
 var Context:byte;
 begin
  result:=1;
  Context:=1;
  repeat
   Context:=(Context shl 1) or DecodeBit(ModelIndex+Context,5);
   result:=(result shl 1) or DecodeBit(ModelIndex+Context,5);
   Context:=(Context shl 1) or (result and 1);
  until ((Context and 2)=0) or Abort;
 end;

var Len,Offset,LastOffset:longint;
    Flag,LastWasMatch:boolean;
    Destination:pansichar;
    Size:longword;
begin
 if assigned(SourcePointer) and (SourceSize>=8) then begin
  Abort:=false;

  Source:=SourcePointer;

  Size:=longword(pointer(Source)^);
  inc(Source,sizeof(longword));

  GetMem(DestinationPointer,Size);
  Destination:=DestinationPointer;

  Code:=longword(pointer(Source)^);
  inc(Source,sizeof(longword));

  Range:=$ffffffff;

  for Len:=0 to SizeModels-1 do begin
   Model[Len]:=2048;
  end;

  LastOffset:=0;

  LastWasMatch:=false;

  Flag:=false;

  while (ptruint(Source)<(ptruint(SourcePointer)+SourceSize)) and not Abort do begin

   if Flag then begin
    if (not LastWasMatch) and (DecodeBit(PrevMatchModel,5)<>0) then begin
     Offset:=LastOffset;
     Len:=0;
    end else begin
     Offset:=DecodeGamma(Gamma0Model);
     if Offset=0 then begin
      result:=Destination-pansichar(DestinationPointer);
      exit;
     end;
     dec(Offset,2);
     Offset:=((Offset shl 4)+DecodeTree(MatchLowModel+(ord(Offset<>0) shl 4),16,5))+1;
     Len:=ord(Offset>=96)+ord(Offset>=2048);
    end;
    LastOffset:=Offset;
    LastWasMatch:=true;
    inc(Len,DecodeGamma(Gamma1Model));
    if ((ptruint(Destination)+longword(Len))<=(ptruint(DestinationPointer)+longword(Size))) and not Abort then begin
     while Len>0 do begin
      dec(Len);
      Destination^:=Destination[-Offset];
      inc(Destination);
     end;
    end else begin
     break;
    end;
   end else begin
    if (ptruint(Destination)<(ptruint(DestinationPointer)+Size)) and not Abort then begin
     byte(pointer(Destination)^):=DecodeTree(LiteralModel,256,4);
     inc(Destination);
     LastWasMatch:=false;
    end else begin
     break;
    end;
   end;

   Flag:=boolean(byte(DecodeBit(FlagModel+byte(boolean(LastWasMatch)),5)));

  end;

  FreeMem(DestinationPointer);

 end;

 DestinationPointer:=nil;
 result:=0;

end;

end.
