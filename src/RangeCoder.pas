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
unit RangeCoder;
{$ifdef fpc}
 {$mode delphi}
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

type PRangeCoderProbabilities=^TRangeCoderProbabilities;
     TRangeCoderProbabilities=array[0..$3ffffff] of longword;

     PRangeCoder=^TRangeCoder;
     TRangeCoder=record
      Buffer:pointer;
      BufferSize:longword;
      BufferPosition:longword;
      RangeCode:longword;
      RangeLow:longword;
      RangeHigh:longword;
      RangeMiddle:longword;
     end;

function RangeCoderProbabilitiesAllocate(Count:longint):PRangeCoderProbabilities;
procedure RangeCoderProbabilitiesFree(Probabilities:PRangeCoderProbabilities);
procedure RangeCoderProbabilitiesReset(Probabilities:PRangeCoderProbabilities;Count:longint);

function RangeCoderRead(var Instance:TRangeCoder):byte;
procedure RangeCoderWrite(var Instance:TRangeCoder;Value:byte);

procedure RangeCoderInit(var Instance:TRangeCoder);

procedure RangeCoderStart(var Instance:TRangeCoder);

procedure RangeCoderFlush(var Instance:TRangeCoder);

procedure RangeCoderEncodeNormalize(var Instance:TRangeCoder);
function RangeCoderEncodeBit(var Instance:TRangeCoder;var Probability:longword;Shift,BitValue:longword):longword;
function RangeCoderEncodeBitWithoutProbability(var Instance:TRangeCoder;BitValue:longword):longword;

procedure RangeCoderDecodeNormalize(var Instance:TRangeCoder);
function RangeCoderDecodeBit(var Instance:TRangeCoder;var Probability:longword;Shift:longword):longword;
function RangeCoderDecodeBitWithoutProbability(var Instance:TRangeCoder):longword;

function RangeCoderEncodeDirectBits(var Instance:TRangeCoder;Bits,Value:longword):longword;
function RangeCoderDecodeDirectBits(var Instance:TRangeCoder;Bits:longword):longword;

implementation

function RangeCoderProbabilitiesAllocate(Count:longint):PRangeCoderProbabilities;
begin
 GetMem(result,Count*SizeOf(longword));
end;

procedure RangeCoderProbabilitiesFree(Probabilities:PRangeCoderProbabilities);
begin
 FreeMem(Probabilities);
end;

procedure RangeCoderProbabilitiesReset(Probabilities:PRangeCoderProbabilities;Count:longint);
var Index:longint;
begin
 for Index:=0 to Count-1 do begin
  Probabilities^[Index]:=2048;
 end;
end;

function RangeCoderRead(var Instance:TRangeCoder):byte;
begin
 if Instance.BufferPosition<Instance.BufferSize then begin
  result:=byte(PAnsiChar(Instance.Buffer)[Instance.BufferPosition]);
  inc(Instance.BufferPosition);
 end else begin
  result:=0;
 end;
end;

procedure RangeCoderWrite(var Instance:TRangeCoder;Value:byte);
begin
 if Instance.BufferPosition>=Instance.BufferSize then begin
  if Instance.BufferSize<16 then begin
   Instance.BufferSize:=16;
  end;
  while Instance.BufferPosition>=Instance.BufferSize do begin
   inc(Instance.BufferSize,Instance.BufferSize);
  end;
  ReallocMem(Instance.Buffer,Instance.BufferSize);
 end;
 byte(PAnsiChar(Instance.Buffer)[Instance.BufferPosition]):=Value;
 inc(Instance.BufferPosition);
end;

procedure RangeCoderInit(var Instance:TRangeCoder);
begin
 Instance.RangeCode:=0;
 Instance.RangeLow:=0;
 Instance.RangeHigh:=$ffffffff;
end;

procedure RangeCoderStart(var Instance:TRangeCoder);
var Counter:longword;
begin
 for Counter:=1 to 4 do begin
  Instance.RangeCode:=(Instance.RangeCode shl 8) or RangeCoderRead(Instance);
 end;
end;

procedure RangeCoderFlush(var Instance:TRangeCoder);
var Counter:longword;
begin
 for Counter:=1 to 4 do begin
  RangeCoderWrite(Instance,Instance.RangeHigh shr 24);
  Instance.RangeHigh:=Instance.RangeHigh shl 8;
 end;
end;

procedure RangeCoderEncodeNormalize(var Instance:TRangeCoder);
begin
 while ((Instance.RangeLow xor Instance.RangeHigh) and $ff000000)=0 do begin
  RangeCoderWrite(Instance,Instance.RangeHigh shr 24);
  Instance.RangeLow:=Instance.RangeLow shl 8;
  Instance.RangeHigh:=(Instance.RangeHigh shl 8) or $ff;
 end;
end;

function RangeCoderEncodeBit(var Instance:TRangeCoder;var Probability:longword;Shift,BitValue:longword):longword;
begin
 Instance.RangeMiddle:=Instance.RangeLow+(((Instance.RangeHigh-Instance.RangeLow) shr 12)*Probability);
 if BitValue<>0 then begin
  inc(Probability,($fff-Probability) shr Shift);
  Instance.RangeHigh:=Instance.RangeMiddle;
 end else begin
  dec(Probability,Probability shr Shift);
  Instance.RangeLow:=Instance.RangeMiddle+1;
 end;
 RangeCoderEncodeNormalize(Instance);
 result:=BitValue;
end;

function RangeCoderEncodeBitWithoutProbability(var Instance:TRangeCoder;BitValue:longword):longword;
begin
 Instance.RangeMiddle:=Instance.RangeLow+((Instance.RangeHigh-Instance.RangeLow) shr 1);
 if BitValue<>0 then begin
  Instance.RangeHigh:=Instance.RangeMiddle;
 end else begin
  Instance.RangeLow:=Instance.RangeMiddle+1;
 end;
 RangeCoderEncodeNormalize(Instance);
 result:=BitValue;
end;

procedure RangeCoderDecodeNormalize(var Instance:TRangeCoder);
begin
 while ((Instance.RangeLow xor Instance.RangeHigh) and $ff000000)=0 do begin
  Instance.RangeLow:=Instance.RangeLow shl 8;
  Instance.RangeHigh:=(Instance.RangeHigh shl 8) or $ff;
  Instance.RangeCode:=(Instance.RangeCode shl 8) or RangeCoderRead(Instance);
 end;
end;

function RangeCoderDecodeBit(var Instance:TRangeCoder;var Probability:longword;Shift:longword):longword;
begin
 Instance.RangeMiddle:=Instance.RangeLow+(((Instance.RangeHigh-Instance.RangeLow) shr 12)*Probability);
 if Instance.RangeCode<=Instance.RangeMiddle then begin
  inc(Probability,($fff-Probability) shr Shift);
  Instance.RangeHigh:=Instance.RangeMiddle;
  result:=1;
 end else begin
  dec(Probability,Probability shr Shift);
  Instance.RangeLow:=Instance.RangeMiddle+1;
  result:=0;
 end;
 RangeCoderDecodeNormalize(Instance);
end;

function RangeCoderDecodeBitWithoutProbability(var Instance:TRangeCoder):longword;
begin
 Instance.RangeMiddle:=Instance.RangeLow+((Instance.RangeHigh-Instance.RangeLow) shr 1);
 if Instance.RangeCode<=Instance.RangeMiddle then begin
  Instance.RangeHigh:=Instance.RangeMiddle;
  result:=1;
 end else begin
  Instance.RangeLow:=Instance.RangeMiddle+1;
  result:=0;
 end;
 RangeCoderDecodeNormalize(Instance);
end;

function RangeCoderEncodeDirectBits(var Instance:TRangeCoder;Bits,Value:longword):longword;
begin
 while Bits>0 do begin
  dec(Bits);
  RangeCoderEncodeBitWithoutProbability(Instance,(Value shr Bits) and 1);
 end;
 result:=Value;
end;

function RangeCoderDecodeDirectBits(var Instance:TRangeCoder;Bits:longword):longword;
begin
 result:=0;
 while Bits>0 do begin
  dec(Bits);
  inc(result,result+RangeCoderDecodeBitWithoutProbability(Instance));
 end;
end;

end.
