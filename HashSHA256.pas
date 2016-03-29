{
  Hash functions
  Copyright (c) 1999-2002 David Barton
  Copyright (C) 2007, Ascher Stefan. All rights reserved.
}

{
  Reimplementation of the hash functions from DCPcrypt
  <http://www.cityinthesky.co.uk/cryptography.html> by David Barton
}

unit HashSHA256;

interface

uses
  SysUtils, Classes, Hash;

type
  THashSHA256 = class(THash)
  private
    procedure Compress;
  protected
    fLenHi, fLenLo: Cardinal;
    fIndex: Cardinal;
    fCurHash: array[0..7] of Cardinal;
    fHashBuff: array[0..63] of byte;
  public
    procedure Init; override;
    procedure Update(const Buffer; Size: Cardinal); override;
    procedure Final(var Digest: array of Byte); override;
    function SelfTest: boolean; override;
    class function GetSize: integer; override;

    procedure Burn; override;
  end;
  THashSHA256Class = class of THashSHA256;

implementation
{$R-}{$Q-}

function SwapDWord(a: Cardinal): Cardinal;
begin
  Result:= ((a and $FF) shl 24) or ((a and $FF00) shl 8) or ((a and $FF0000) shr 8)
    or ((a and $FF000000) shr 24);
end;

function THashSHA256.SelfTest: boolean;
const
  Test1Out: array[0..31] of byte=
    ($ba,$78,$16,$bf,$8f,$01,$cf,$ea,$41,$41,$40,$de,$5d,$ae,$22,$23,
     $b0,$03,$61,$a3,$96,$17,$7a,$9c,$b4,$10,$ff,$61,$f2,$00,$15,$ad);
  Test2Out: array[0..31] of byte=
    ($24,$8d,$6a,$61,$d2,$06,$38,$b8,$e5,$c0,$26,$93,$0c,$3e,$60,$39,
     $a3,$3c,$e4,$59,$64,$ff,$21,$67,$f6,$ec,$ed,$d4,$19,$db,$06,$c1);
var
  TestOut: array[0..31] of byte;
begin
  Init;
  UpdateStr('abc');
  Final(TestOut);
  Result:= boolean(CompareMem(@TestOut,@Test1Out,Sizeof(Test1Out)));
  Init;
  UpdateStr('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq');
  Final(TestOut);
  Result:= boolean(CompareMem(@TestOut,@Test2Out,Sizeof(Test2Out))) and Result;
end;

procedure THashSHA256.Compress;
var
  a, b, c, d, e, f, g, h, t1, t2: Cardinal;
  W: array[0..63] of Cardinal;
  i: longword;
begin
  fIndex:= 0;
  a:= fCurHash[0]; b:= fCurHash[1]; c:= fCurHash[2]; d:= fCurHash[3];
  e:= fCurHash[4]; f:= fCurHash[5]; g:= fCurHash[6]; h:= fCurHash[7];
  Move(fHashBuff,W,Sizeof(fHashBuff));
  for i:= 0 to 15 do
    W[i]:= SwapDWord(W[i]);
  for i:= 16 to 63 do
    W[i]:= (((W[i-2] shr 17) or (W[i-2] shl 15)) xor ((W[i-2] shr 19) or (W[i-2] shl 13)) xor
      (W[i-2] shr 10)) + W[i-7] + (((W[i-15] shr 7) or (W[i-15] shl 25)) xor
      ((W[i-15] shr 18) or (W[i-15] shl 14)) xor (W[i-15] shr 3)) + W[i-16];

  t1:= h + (((e shr 6) or (e shl 26)) xor ((e shr 11) or (e shl 21)) xor ((e shr 25) or (e shl 7))) + ((e and f) xor (not e and g)) + $428a2f98 + W[0]; t2:= (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19)) xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c)); h:= t1 + t2; d:= d + t1;
  t1:= g + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21)) xor ((d shr 25) or (d shl 7))) + ((d and e) xor (not d and f)) + $71374491 + W[1]; t2:= (((h shr 2) or (h shl 30)) xor ((h shr 13) or (h shl 19)) xor ((h shr 22) xor (h shl 10))) + ((h and a) xor (h and b) xor (a and b)); g:= t1 + t2; c:= c + t1;
  t1:= f + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21)) xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and e)) + $b5c0fbcf + W[2]; t2:= (((g shr 2) or (g shl 30)) xor ((g shr 13) or (g shl 19)) xor ((g shr 22) xor (g shl 10))) + ((g and h) xor (g and a) xor (h and a)); f:= t1 + t2; b:= b + t1;
  t1:= e + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21)) xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) + $e9b5dba5 + W[3]; t2:= (((f shr 2) or (f shl 30)) xor ((f shr 13) or (f shl 19)) xor ((f shr 22) xor (f shl 10))) + ((f and g) xor (f and h) xor (g and h)); e:= t1 + t2; a:= a + t1;
  t1:= d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21)) xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) + $3956c25b + W[4]; t2:= (((e shr 2) or (e shl 30)) xor ((e shr 13) or (e shl 19)) xor ((e shr 22) xor (e shl 10))) + ((e and f) xor (e and g) xor (f and g)); d:= t1 + t2; h:= h + t1;
  t1:= c + (((h shr 6) or (h shl 26)) xor ((h shr 11) or (h shl 21)) xor ((h shr 25) or (h shl 7))) + ((h and a) xor (not h and b)) + $59f111f1 + W[5]; t2:= (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19)) xor ((d shr 22) xor (d shl 10))) + ((d and e) xor (d and f) xor (e and f)); c:= t1 + t2; g:= g + t1;
  t1:= b + (((g shr 6) or (g shl 26)) xor ((g shr 11) or (g shl 21)) xor ((g shr 25) or (g shl 7))) + ((g and h) xor (not g and a)) + $923f82a4 + W[6]; t2:= (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19)) xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and e) xor (d and e)); b:= t1 + t2; f:= f + t1;
  t1:= a + (((f shr 6) or (f shl 26)) xor ((f shr 11) or (f shl 21)) xor ((f shr 25) or (f shl 7))) + ((f and g) xor (not f and h)) + $ab1c5ed5 + W[7]; t2:= (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19)) xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d)); a:= t1 + t2; e:= e + t1;
  t1:= h + (((e shr 6) or (e shl 26)) xor ((e shr 11) or (e shl 21)) xor ((e shr 25) or (e shl 7))) + ((e and f) xor (not e and g)) + $d807aa98 + W[8]; t2:= (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19)) xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c)); h:= t1 + t2; d:= d + t1;
  t1:= g + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21)) xor ((d shr 25) or (d shl 7))) + ((d and e) xor (not d and f)) + $12835b01 + W[9]; t2:= (((h shr 2) or (h shl 30)) xor ((h shr 13) or (h shl 19)) xor ((h shr 22) xor (h shl 10))) + ((h and a) xor (h and b) xor (a and b)); g:= t1 + t2; c:= c + t1;
  t1:= f + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21)) xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and e)) + $243185be + W[10]; t2:= (((g shr 2) or (g shl 30)) xor ((g shr 13) or (g shl 19)) xor ((g shr 22) xor (g shl 10))) + ((g and h) xor (g and a) xor (h and a)); f:= t1 + t2; b:= b + t1;
  t1:= e + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21)) xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) + $550c7dc3 + W[11]; t2:= (((f shr 2) or (f shl 30)) xor ((f shr 13) or (f shl 19)) xor ((f shr 22) xor (f shl 10))) + ((f and g) xor (f and h) xor (g and h)); e:= t1 + t2; a:= a + t1;
  t1:= d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21)) xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) + $72be5d74 + W[12]; t2:= (((e shr 2) or (e shl 30)) xor ((e shr 13) or (e shl 19)) xor ((e shr 22) xor (e shl 10))) + ((e and f) xor (e and g) xor (f and g)); d:= t1 + t2; h:= h + t1;
  t1:= c + (((h shr 6) or (h shl 26)) xor ((h shr 11) or (h shl 21)) xor ((h shr 25) or (h shl 7))) + ((h and a) xor (not h and b)) + $80deb1fe + W[13]; t2:= (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19)) xor ((d shr 22) xor (d shl 10))) + ((d and e) xor (d and f) xor (e and f)); c:= t1 + t2; g:= g + t1;
  t1:= b + (((g shr 6) or (g shl 26)) xor ((g shr 11) or (g shl 21)) xor ((g shr 25) or (g shl 7))) + ((g and h) xor (not g and a)) + $9bdc06a7 + W[14]; t2:= (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19)) xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and e) xor (d and e)); b:= t1 + t2; f:= f + t1;
  t1:= a + (((f shr 6) or (f shl 26)) xor ((f shr 11) or (f shl 21)) xor ((f shr 25) or (f shl 7))) + ((f and g) xor (not f and h)) + $c19bf174 + W[15]; t2:= (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19)) xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d)); a:= t1 + t2; e:= e + t1;
  t1:= h + (((e shr 6) or (e shl 26)) xor ((e shr 11) or (e shl 21)) xor ((e shr 25) or (e shl 7))) + ((e and f) xor (not e and g)) + $e49b69c1 + W[16]; t2:= (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19)) xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c)); h:= t1 + t2; d:= d + t1;
  t1:= g + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21)) xor ((d shr 25) or (d shl 7))) + ((d and e) xor (not d and f)) + $efbe4786 + W[17]; t2:= (((h shr 2) or (h shl 30)) xor ((h shr 13) or (h shl 19)) xor ((h shr 22) xor (h shl 10))) + ((h and a) xor (h and b) xor (a and b)); g:= t1 + t2; c:= c + t1;
  t1:= f + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21)) xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and e)) + $0fc19dc6 + W[18]; t2:= (((g shr 2) or (g shl 30)) xor ((g shr 13) or (g shl 19)) xor ((g shr 22) xor (g shl 10))) + ((g and h) xor (g and a) xor (h and a)); f:= t1 + t2; b:= b + t1;
  t1:= e + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21)) xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) + $240ca1cc + W[19]; t2:= (((f shr 2) or (f shl 30)) xor ((f shr 13) or (f shl 19)) xor ((f shr 22) xor (f shl 10))) + ((f and g) xor (f and h) xor (g and h)); e:= t1 + t2; a:= a + t1;
  t1:= d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21)) xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) + $2de92c6f + W[20]; t2:= (((e shr 2) or (e shl 30)) xor ((e shr 13) or (e shl 19)) xor ((e shr 22) xor (e shl 10))) + ((e and f) xor (e and g) xor (f and g)); d:= t1 + t2; h:= h + t1;
  t1:= c + (((h shr 6) or (h shl 26)) xor ((h shr 11) or (h shl 21)) xor ((h shr 25) or (h shl 7))) + ((h and a) xor (not h and b)) + $4a7484aa + W[21]; t2:= (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19)) xor ((d shr 22) xor (d shl 10))) + ((d and e) xor (d and f) xor (e and f)); c:= t1 + t2; g:= g + t1;
  t1:= b + (((g shr 6) or (g shl 26)) xor ((g shr 11) or (g shl 21)) xor ((g shr 25) or (g shl 7))) + ((g and h) xor (not g and a)) + $5cb0a9dc + W[22]; t2:= (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19)) xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and e) xor (d and e)); b:= t1 + t2; f:= f + t1;
  t1:= a + (((f shr 6) or (f shl 26)) xor ((f shr 11) or (f shl 21)) xor ((f shr 25) or (f shl 7))) + ((f and g) xor (not f and h)) + $76f988da + W[23]; t2:= (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19)) xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d)); a:= t1 + t2; e:= e + t1;
  t1:= h + (((e shr 6) or (e shl 26)) xor ((e shr 11) or (e shl 21)) xor ((e shr 25) or (e shl 7))) + ((e and f) xor (not e and g)) + $983e5152 + W[24]; t2:= (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19)) xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c)); h:= t1 + t2; d:= d + t1;
  t1:= g + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21)) xor ((d shr 25) or (d shl 7))) + ((d and e) xor (not d and f)) + $a831c66d + W[25]; t2:= (((h shr 2) or (h shl 30)) xor ((h shr 13) or (h shl 19)) xor ((h shr 22) xor (h shl 10))) + ((h and a) xor (h and b) xor (a and b)); g:= t1 + t2; c:= c + t1;
  t1:= f + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21)) xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and e)) + $b00327c8 + W[26]; t2:= (((g shr 2) or (g shl 30)) xor ((g shr 13) or (g shl 19)) xor ((g shr 22) xor (g shl 10))) + ((g and h) xor (g and a) xor (h and a)); f:= t1 + t2; b:= b + t1;
  t1:= e + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21)) xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) + $bf597fc7 + W[27]; t2:= (((f shr 2) or (f shl 30)) xor ((f shr 13) or (f shl 19)) xor ((f shr 22) xor (f shl 10))) + ((f and g) xor (f and h) xor (g and h)); e:= t1 + t2; a:= a + t1;
  t1:= d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21)) xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) + $c6e00bf3 + W[28]; t2:= (((e shr 2) or (e shl 30)) xor ((e shr 13) or (e shl 19)) xor ((e shr 22) xor (e shl 10))) + ((e and f) xor (e and g) xor (f and g)); d:= t1 + t2; h:= h + t1;
  t1:= c + (((h shr 6) or (h shl 26)) xor ((h shr 11) or (h shl 21)) xor ((h shr 25) or (h shl 7))) + ((h and a) xor (not h and b)) + $d5a79147 + W[29]; t2:= (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19)) xor ((d shr 22) xor (d shl 10))) + ((d and e) xor (d and f) xor (e and f)); c:= t1 + t2; g:= g + t1;
  t1:= b + (((g shr 6) or (g shl 26)) xor ((g shr 11) or (g shl 21)) xor ((g shr 25) or (g shl 7))) + ((g and h) xor (not g and a)) + $06ca6351 + W[30]; t2:= (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19)) xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and e) xor (d and e)); b:= t1 + t2; f:= f + t1;
  t1:= a + (((f shr 6) or (f shl 26)) xor ((f shr 11) or (f shl 21)) xor ((f shr 25) or (f shl 7))) + ((f and g) xor (not f and h)) + $14292967 + W[31]; t2:= (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19)) xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d)); a:= t1 + t2; e:= e + t1;
  t1:= h + (((e shr 6) or (e shl 26)) xor ((e shr 11) or (e shl 21)) xor ((e shr 25) or (e shl 7))) + ((e and f) xor (not e and g)) + $27b70a85 + W[32]; t2:= (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19)) xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c)); h:= t1 + t2; d:= d + t1;
  t1:= g + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21)) xor ((d shr 25) or (d shl 7))) + ((d and e) xor (not d and f)) + $2e1b2138 + W[33]; t2:= (((h shr 2) or (h shl 30)) xor ((h shr 13) or (h shl 19)) xor ((h shr 22) xor (h shl 10))) + ((h and a) xor (h and b) xor (a and b)); g:= t1 + t2; c:= c + t1;
  t1:= f + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21)) xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and e)) + $4d2c6dfc + W[34]; t2:= (((g shr 2) or (g shl 30)) xor ((g shr 13) or (g shl 19)) xor ((g shr 22) xor (g shl 10))) + ((g and h) xor (g and a) xor (h and a)); f:= t1 + t2; b:= b + t1;
  t1:= e + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21)) xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) + $53380d13 + W[35]; t2:= (((f shr 2) or (f shl 30)) xor ((f shr 13) or (f shl 19)) xor ((f shr 22) xor (f shl 10))) + ((f and g) xor (f and h) xor (g and h)); e:= t1 + t2; a:= a + t1;
  t1:= d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21)) xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) + $650a7354 + W[36]; t2:= (((e shr 2) or (e shl 30)) xor ((e shr 13) or (e shl 19)) xor ((e shr 22) xor (e shl 10))) + ((e and f) xor (e and g) xor (f and g)); d:= t1 + t2; h:= h + t1;
  t1:= c + (((h shr 6) or (h shl 26)) xor ((h shr 11) or (h shl 21)) xor ((h shr 25) or (h shl 7))) + ((h and a) xor (not h and b)) + $766a0abb + W[37]; t2:= (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19)) xor ((d shr 22) xor (d shl 10))) + ((d and e) xor (d and f) xor (e and f)); c:= t1 + t2; g:= g + t1;
  t1:= b + (((g shr 6) or (g shl 26)) xor ((g shr 11) or (g shl 21)) xor ((g shr 25) or (g shl 7))) + ((g and h) xor (not g and a)) + $81c2c92e + W[38]; t2:= (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19)) xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and e) xor (d and e)); b:= t1 + t2; f:= f + t1;
  t1:= a + (((f shr 6) or (f shl 26)) xor ((f shr 11) or (f shl 21)) xor ((f shr 25) or (f shl 7))) + ((f and g) xor (not f and h)) + $92722c85 + W[39]; t2:= (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19)) xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d)); a:= t1 + t2; e:= e + t1;
  t1:= h + (((e shr 6) or (e shl 26)) xor ((e shr 11) or (e shl 21)) xor ((e shr 25) or (e shl 7))) + ((e and f) xor (not e and g)) + $a2bfe8a1 + W[40]; t2:= (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19)) xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c)); h:= t1 + t2; d:= d + t1;
  t1:= g + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21)) xor ((d shr 25) or (d shl 7))) + ((d and e) xor (not d and f)) + $a81a664b + W[41]; t2:= (((h shr 2) or (h shl 30)) xor ((h shr 13) or (h shl 19)) xor ((h shr 22) xor (h shl 10))) + ((h and a) xor (h and b) xor (a and b)); g:= t1 + t2; c:= c + t1;
  t1:= f + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21)) xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and e)) + $c24b8b70 + W[42]; t2:= (((g shr 2) or (g shl 30)) xor ((g shr 13) or (g shl 19)) xor ((g shr 22) xor (g shl 10))) + ((g and h) xor (g and a) xor (h and a)); f:= t1 + t2; b:= b + t1;
  t1:= e + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21)) xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) + $c76c51a3 + W[43]; t2:= (((f shr 2) or (f shl 30)) xor ((f shr 13) or (f shl 19)) xor ((f shr 22) xor (f shl 10))) + ((f and g) xor (f and h) xor (g and h)); e:= t1 + t2; a:= a + t1;
  t1:= d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21)) xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) + $d192e819 + W[44]; t2:= (((e shr 2) or (e shl 30)) xor ((e shr 13) or (e shl 19)) xor ((e shr 22) xor (e shl 10))) + ((e and f) xor (e and g) xor (f and g)); d:= t1 + t2; h:= h + t1;
  t1:= c + (((h shr 6) or (h shl 26)) xor ((h shr 11) or (h shl 21)) xor ((h shr 25) or (h shl 7))) + ((h and a) xor (not h and b)) + $d6990624 + W[45]; t2:= (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19)) xor ((d shr 22) xor (d shl 10))) + ((d and e) xor (d and f) xor (e and f)); c:= t1 + t2; g:= g + t1;
  t1:= b + (((g shr 6) or (g shl 26)) xor ((g shr 11) or (g shl 21)) xor ((g shr 25) or (g shl 7))) + ((g and h) xor (not g and a)) + $f40e3585 + W[46]; t2:= (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19)) xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and e) xor (d and e)); b:= t1 + t2; f:= f + t1;
  t1:= a + (((f shr 6) or (f shl 26)) xor ((f shr 11) or (f shl 21)) xor ((f shr 25) or (f shl 7))) + ((f and g) xor (not f and h)) + $106aa070 + W[47]; t2:= (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19)) xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d)); a:= t1 + t2; e:= e + t1;
  t1:= h + (((e shr 6) or (e shl 26)) xor ((e shr 11) or (e shl 21)) xor ((e shr 25) or (e shl 7))) + ((e and f) xor (not e and g)) + $19a4c116 + W[48]; t2:= (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19)) xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c)); h:= t1 + t2; d:= d + t1;
  t1:= g + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21)) xor ((d shr 25) or (d shl 7))) + ((d and e) xor (not d and f)) + $1e376c08 + W[49]; t2:= (((h shr 2) or (h shl 30)) xor ((h shr 13) or (h shl 19)) xor ((h shr 22) xor (h shl 10))) + ((h and a) xor (h and b) xor (a and b)); g:= t1 + t2; c:= c + t1;
  t1:= f + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21)) xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and e)) + $2748774c + W[50]; t2:= (((g shr 2) or (g shl 30)) xor ((g shr 13) or (g shl 19)) xor ((g shr 22) xor (g shl 10))) + ((g and h) xor (g and a) xor (h and a)); f:= t1 + t2; b:= b + t1;
  t1:= e + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21)) xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) + $34b0bcb5 + W[51]; t2:= (((f shr 2) or (f shl 30)) xor ((f shr 13) or (f shl 19)) xor ((f shr 22) xor (f shl 10))) + ((f and g) xor (f and h) xor (g and h)); e:= t1 + t2; a:= a + t1;
  t1:= d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21)) xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) + $391c0cb3 + W[52]; t2:= (((e shr 2) or (e shl 30)) xor ((e shr 13) or (e shl 19)) xor ((e shr 22) xor (e shl 10))) + ((e and f) xor (e and g) xor (f and g)); d:= t1 + t2; h:= h + t1;
  t1:= c + (((h shr 6) or (h shl 26)) xor ((h shr 11) or (h shl 21)) xor ((h shr 25) or (h shl 7))) + ((h and a) xor (not h and b)) + $4ed8aa4a + W[53]; t2:= (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19)) xor ((d shr 22) xor (d shl 10))) + ((d and e) xor (d and f) xor (e and f)); c:= t1 + t2; g:= g + t1;
  t1:= b + (((g shr 6) or (g shl 26)) xor ((g shr 11) or (g shl 21)) xor ((g shr 25) or (g shl 7))) + ((g and h) xor (not g and a)) + $5b9cca4f + W[54]; t2:= (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19)) xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and e) xor (d and e)); b:= t1 + t2; f:= f + t1;
  t1:= a + (((f shr 6) or (f shl 26)) xor ((f shr 11) or (f shl 21)) xor ((f shr 25) or (f shl 7))) + ((f and g) xor (not f and h)) + $682e6ff3 + W[55]; t2:= (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19)) xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d)); a:= t1 + t2; e:= e + t1;
  t1:= h + (((e shr 6) or (e shl 26)) xor ((e shr 11) or (e shl 21)) xor ((e shr 25) or (e shl 7))) + ((e and f) xor (not e and g)) + $748f82ee + W[56]; t2:= (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19)) xor ((a shr 22) xor (a shl 10))) + ((a and b) xor (a and c) xor (b and c)); h:= t1 + t2; d:= d + t1;
  t1:= g + (((d shr 6) or (d shl 26)) xor ((d shr 11) or (d shl 21)) xor ((d shr 25) or (d shl 7))) + ((d and e) xor (not d and f)) + $78a5636f + W[57]; t2:= (((h shr 2) or (h shl 30)) xor ((h shr 13) or (h shl 19)) xor ((h shr 22) xor (h shl 10))) + ((h and a) xor (h and b) xor (a and b)); g:= t1 + t2; c:= c + t1;
  t1:= f + (((c shr 6) or (c shl 26)) xor ((c shr 11) or (c shl 21)) xor ((c shr 25) or (c shl 7))) + ((c and d) xor (not c and e)) + $84c87814 + W[58]; t2:= (((g shr 2) or (g shl 30)) xor ((g shr 13) or (g shl 19)) xor ((g shr 22) xor (g shl 10))) + ((g and h) xor (g and a) xor (h and a)); f:= t1 + t2; b:= b + t1;
  t1:= e + (((b shr 6) or (b shl 26)) xor ((b shr 11) or (b shl 21)) xor ((b shr 25) or (b shl 7))) + ((b and c) xor (not b and d)) + $8cc70208 + W[59]; t2:= (((f shr 2) or (f shl 30)) xor ((f shr 13) or (f shl 19)) xor ((f shr 22) xor (f shl 10))) + ((f and g) xor (f and h) xor (g and h)); e:= t1 + t2; a:= a + t1;
  t1:= d + (((a shr 6) or (a shl 26)) xor ((a shr 11) or (a shl 21)) xor ((a shr 25) or (a shl 7))) + ((a and b) xor (not a and c)) + $90befffa + W[60]; t2:= (((e shr 2) or (e shl 30)) xor ((e shr 13) or (e shl 19)) xor ((e shr 22) xor (e shl 10))) + ((e and f) xor (e and g) xor (f and g)); d:= t1 + t2; h:= h + t1;
  t1:= c + (((h shr 6) or (h shl 26)) xor ((h shr 11) or (h shl 21)) xor ((h shr 25) or (h shl 7))) + ((h and a) xor (not h and b)) + $a4506ceb + W[61]; t2:= (((d shr 2) or (d shl 30)) xor ((d shr 13) or (d shl 19)) xor ((d shr 22) xor (d shl 10))) + ((d and e) xor (d and f) xor (e and f)); c:= t1 + t2; g:= g + t1;
  t1:= b + (((g shr 6) or (g shl 26)) xor ((g shr 11) or (g shl 21)) xor ((g shr 25) or (g shl 7))) + ((g and h) xor (not g and a)) + $bef9a3f7 + W[62]; t2:= (((c shr 2) or (c shl 30)) xor ((c shr 13) or (c shl 19)) xor ((c shr 22) xor (c shl 10))) + ((c and d) xor (c and e) xor (d and e)); b:= t1 + t2; f:= f + t1;
  t1:= a + (((f shr 6) or (f shl 26)) xor ((f shr 11) or (f shl 21)) xor ((f shr 25) or (f shl 7))) + ((f and g) xor (not f and h)) + $c67178f2 + W[63]; t2:= (((b shr 2) or (b shl 30)) xor ((b shr 13) or (b shl 19)) xor ((b shr 22) xor (b shl 10))) + ((b and c) xor (b and d) xor (c and d)); a:= t1 + t2; e:= e + t1;

  fCurHash[0]:= fCurHash[0] + a;
  fCurHash[1]:= fCurHash[1] + b;
  fCurHash[2]:= fCurHash[2] + c;
  fCurHash[3]:= fCurHash[3] + d;
  fCurHash[4]:= fCurHash[4] + e;
  fCurHash[5]:= fCurHash[5] + f;
  fCurHash[6]:= fCurHash[6] + g;
  fCurHash[7]:= fCurHash[7] + h;
  FillChar(W,Sizeof(W),0);
  FillChar(fHashBuff,Sizeof(fHashBuff),0);
end;

procedure THashSHA256.Init;
begin
  Burn;
  fCurHash[0]:= $6a09e667;
  fCurHash[1]:= $bb67ae85;
  fCurHash[2]:= $3c6ef372;
  fCurHash[3]:= $a54ff53a;
  fCurHash[4]:= $510e527f;
  fCurHash[5]:= $9b05688c;
  fCurHash[6]:= $1f83d9ab;
  fCurHash[7]:= $5be0cd19;
end;

procedure THashSHA256.Update(const Buffer; Size: Cardinal);
var
  PBuf: ^byte;
begin
  Inc(fLenHi,Size shr 29);
  Inc(fLenLo,Size*8);
  if fLenLo< (Size*8) then
    Inc(fLenHi);

  PBuf:= @Buffer;
  while Size> 0 do
  begin
    if (Sizeof(fHashBuff)-fIndex)<= Cardinal(Size) then
    begin
      Move(PBuf^,fHashBuff[fIndex],Sizeof(fHashBuff)-fIndex);
      Dec(Size,Sizeof(fHashBuff)-fIndex);
      Inc(PBuf,Sizeof(fHashBuff)-fIndex);
      Compress;
    end
    else
    begin
      Move(PBuf^,fHashBuff[fIndex],Size);
      Inc(fIndex,Size);
      Size:= 0;
    end;
  end;
end;

procedure THashSHA256.Final(var Digest: array of Byte);
begin
  fHashBuff[fIndex]:= $80;
  if fIndex>= 56 then
    Compress;
  PCardinal(@fHashBuff[56])^:= SwapDWord(fLenHi);
  PCardinal(@fHashBuff[60])^:= SwapDWord(fLenLo);
  Compress;
  fCurHash[0]:= SwapDWord(fCurHash[0]);
  fCurHash[1]:= SwapDWord(fCurHash[1]);
  fCurHash[2]:= SwapDWord(fCurHash[2]);
  fCurHash[3]:= SwapDWord(fCurHash[3]);
  fCurHash[4]:= SwapDWord(fCurHash[4]);
  fCurHash[5]:= SwapDWord(fCurHash[5]);
  fCurHash[6]:= SwapDWord(fCurHash[6]);
  fCurHash[7]:= SwapDWord(fCurHash[7]);
  Move(fCurHash,Digest,Sizeof(fCurHash));
  Burn;
end;

procedure THashSHA256.Burn;
begin
  fLenHi:= 0; fLenLo:= 0;
  fIndex:= 0;
  FillChar(fHashBuff,Sizeof(fHashBuff),0);
  FillChar(fCurHash,Sizeof(fCurHash),0);
end;

class function THashSHA256.GetSize: integer;
begin
  Result := SizeOf(Cardinal) * 8;
end;

initialization
  THash.RegisterHashType('SHA-256', THashSHA256);

end.
