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
unit ChecksumUtils;
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

function CRC32(Data:pointer;Len:longword):longword;

implementation

function CRC32(Data:pointer;Len:longword):longword;
const CRC32Table:array[0..15] of longword=($00000000,$1db71064,$3b6e20c8,$26d930ac,$76dc4190,
                                           $6b6b51f4,$4db26158,$5005713c,$edb88320,$f00f9344,
                                           $d6d6a3e8,$cb61b38c,$9b64c2b0,$86d3d2d4,$a00ae278,
                                           $bdbdf21c);

var b:pansichar;
    i:longword;
begin
 if Len=0 then begin
  result:=0;
 end else begin
  b:=Data;
  result:=$ffffffff;
  for i:=1 to Len do begin
   result:=result xor byte(b^);
   result:=CRC32Table[result and $f] xor (result shr 4);
   result:=CRC32Table[result and $f] xor (result shr 4);
   inc(b);
  end;
  result:=result xor $ffffffff;
 end;
end;


end.
 