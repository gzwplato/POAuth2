{
  Hash functions
  Copyright (c) 1999-2002 David Barton
  Copyright (C) 2007, Ascher Stefan. All rights reserved.
}

{
  Reimplementation of the hash functions from DCPcrypt
  <http://www.cityinthesky.co.uk/cryptography.html> by David Barton
}

unit HashSHA512Base;

interface

uses
  SysUtils, Classes, Hash;

type
  THashSHA512Base = class(THash)
  private
  protected
    fLenHi, fLenLo: Int64;
    fIndex: DWord;
    fCurHash: array[0..7] of Int64;
    fHashBuff: array[0..127] of byte;
    procedure Compress;
  public
    procedure Update(const Buffer; Size: Cardinal); override;
    procedure Burn; override;
  end;

function SwapDWord(a: int64): int64;
  
implementation
{$R-}{$Q-}

function SwapDWord(a: int64): int64;
begin
  Result:= ((a and $FF) shl 56) or ((a and $FF00) shl 40) or ((a and $FF0000) shl 24) or
    ((a and $FF000000) shl 8) or ((a and $FF00000000) shr 8) or ((a and $FF0000000000) shr 24)
    or ((a and $FF000000000000) shr 40) or ((a and $FF00000000000000) shr 56);
end;

procedure THashSHA512Base.Compress;
var
  a, b, c, d, e, f, g, h, t1, t2: int64;
  W: array[0..79] of int64;
  i: longword;
begin
  fIndex:= 0;
  a:= fCurHash[0]; b:= fCurHash[1]; c:= fCurHash[2]; d:= fCurHash[3];
  e:= fCurHash[4]; f:= fCurHash[5]; g:= fCurHash[6]; h:= fCurHash[7];
  Move(fHashBuff,W,Sizeof(fHashBuff));
  for i:= 0 to 15 do
    W[i]:= SwapDWord(W[i]);
  for i:= 16 to 79 do
    W[i]:= (((W[i-2] shr 19) or (W[i-2] shl 45)) xor ((W[i-2] shr 61) or (W[i-2] shl 3)) xor
      (W[i-2] shr 6)) + W[i-7] + (((W[i-15] shr 1) or (W[i-15] shl 63)) xor ((W[i-15] shr 8) or
      (W[i-15] shl 56)) xor (W[i-15] shr 7)) + W[i-16];

  t1:= h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $428a2f98d728ae22 + W[0]; t2:= (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c)); d:= d + t1; h:= t1 + t2;
  t1:= g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $7137449123ef65cd + W[1]; t2:= (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b)); c:= c + t1; g:= t1 + t2;
  t1:= f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $b5c0fbcfec4d3b2f + W[2]; t2:= (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a)); b:= b + t1; f:= t1 + t2;
  t1:= e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $e9b5dba58189dbbc + W[3]; t2:= (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h)); a:= a + t1; e:= t1 + t2;
  t1:= d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $3956c25bf348b538 + W[4]; t2:= (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g)); h:= h + t1; d:= t1 + t2;
  t1:= c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $59f111f1b605d019 + W[5]; t2:= (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f)); g:= g + t1; c:= t1 + t2;
  t1:= b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $923f82a4af194f9b + W[6]; t2:= (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e)); f:= f + t1; b:= t1 + t2;
  t1:= a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $ab1c5ed5da6d8118 + W[7]; t2:= (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d)); e:= e + t1; a:= t1 + t2;
  t1:= h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $d807aa98a3030242 + W[8]; t2:= (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c)); d:= d + t1; h:= t1 + t2;
  t1:= g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $12835b0145706fbe + W[9]; t2:= (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b)); c:= c + t1; g:= t1 + t2;
  t1:= f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $243185be4ee4b28c + W[10]; t2:= (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a)); b:= b + t1; f:= t1 + t2;
  t1:= e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $550c7dc3d5ffb4e2 + W[11]; t2:= (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h)); a:= a + t1; e:= t1 + t2;
  t1:= d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $72be5d74f27b896f + W[12]; t2:= (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g)); h:= h + t1; d:= t1 + t2;
  t1:= c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $80deb1fe3b1696b1 + W[13]; t2:= (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f)); g:= g + t1; c:= t1 + t2;
  t1:= b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $9bdc06a725c71235 + W[14]; t2:= (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e)); f:= f + t1; b:= t1 + t2;
  t1:= a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $c19bf174cf692694 + W[15]; t2:= (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d)); e:= e + t1; a:= t1 + t2;
  t1:= h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $e49b69c19ef14ad2 + W[16]; t2:= (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c)); d:= d + t1; h:= t1 + t2;
  t1:= g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $efbe4786384f25e3 + W[17]; t2:= (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b)); c:= c + t1; g:= t1 + t2;
  t1:= f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $0fc19dc68b8cd5b5 + W[18]; t2:= (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a)); b:= b + t1; f:= t1 + t2;
  t1:= e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $240ca1cc77ac9c65 + W[19]; t2:= (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h)); a:= a + t1; e:= t1 + t2;
  t1:= d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $2de92c6f592b0275 + W[20]; t2:= (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g)); h:= h + t1; d:= t1 + t2;
  t1:= c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $4a7484aa6ea6e483 + W[21]; t2:= (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f)); g:= g + t1; c:= t1 + t2;
  t1:= b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $5cb0a9dcbd41fbd4 + W[22]; t2:= (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e)); f:= f + t1; b:= t1 + t2;
  t1:= a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $76f988da831153b5 + W[23]; t2:= (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d)); e:= e + t1; a:= t1 + t2;
  t1:= h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $983e5152ee66dfab + W[24]; t2:= (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c)); d:= d + t1; h:= t1 + t2;
  t1:= g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $a831c66d2db43210 + W[25]; t2:= (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b)); c:= c + t1; g:= t1 + t2;
  t1:= f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $b00327c898fb213f + W[26]; t2:= (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a)); b:= b + t1; f:= t1 + t2;
  t1:= e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $bf597fc7beef0ee4 + W[27]; t2:= (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h)); a:= a + t1; e:= t1 + t2;
  t1:= d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $c6e00bf33da88fc2 + W[28]; t2:= (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g)); h:= h + t1; d:= t1 + t2;
  t1:= c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $d5a79147930aa725 + W[29]; t2:= (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f)); g:= g + t1; c:= t1 + t2;
  t1:= b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $06ca6351e003826f + W[30]; t2:= (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e)); f:= f + t1; b:= t1 + t2;
  t1:= a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $142929670a0e6e70 + W[31]; t2:= (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d)); e:= e + t1; a:= t1 + t2;
  t1:= h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $27b70a8546d22ffc + W[32]; t2:= (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c)); d:= d + t1; h:= t1 + t2;
  t1:= g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $2e1b21385c26c926 + W[33]; t2:= (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b)); c:= c + t1; g:= t1 + t2;
  t1:= f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $4d2c6dfc5ac42aed + W[34]; t2:= (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a)); b:= b + t1; f:= t1 + t2;
  t1:= e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $53380d139d95b3df + W[35]; t2:= (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h)); a:= a + t1; e:= t1 + t2;
  t1:= d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $650a73548baf63de + W[36]; t2:= (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g)); h:= h + t1; d:= t1 + t2;
  t1:= c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $766a0abb3c77b2a8 + W[37]; t2:= (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f)); g:= g + t1; c:= t1 + t2;
  t1:= b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $81c2c92e47edaee6 + W[38]; t2:= (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e)); f:= f + t1; b:= t1 + t2;
  t1:= a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $92722c851482353b + W[39]; t2:= (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d)); e:= e + t1; a:= t1 + t2;
  t1:= h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $a2bfe8a14cf10364 + W[40]; t2:= (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c)); d:= d + t1; h:= t1 + t2;
  t1:= g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $a81a664bbc423001 + W[41]; t2:= (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b)); c:= c + t1; g:= t1 + t2;
  t1:= f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $c24b8b70d0f89791 + W[42]; t2:= (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a)); b:= b + t1; f:= t1 + t2;
  t1:= e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $c76c51a30654be30 + W[43]; t2:= (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h)); a:= a + t1; e:= t1 + t2;
  t1:= d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $d192e819d6ef5218 + W[44]; t2:= (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g)); h:= h + t1; d:= t1 + t2;
  t1:= c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $d69906245565a910 + W[45]; t2:= (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f)); g:= g + t1; c:= t1 + t2;
  t1:= b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $f40e35855771202a + W[46]; t2:= (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e)); f:= f + t1; b:= t1 + t2;
  t1:= a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $106aa07032bbd1b8 + W[47]; t2:= (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d)); e:= e + t1; a:= t1 + t2;
  t1:= h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $19a4c116b8d2d0c8 + W[48]; t2:= (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c)); d:= d + t1; h:= t1 + t2;
  t1:= g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $1e376c085141ab53 + W[49]; t2:= (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b)); c:= c + t1; g:= t1 + t2;
  t1:= f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $2748774cdf8eeb99 + W[50]; t2:= (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a)); b:= b + t1; f:= t1 + t2;
  t1:= e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $34b0bcb5e19b48a8 + W[51]; t2:= (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h)); a:= a + t1; e:= t1 + t2;
  t1:= d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $391c0cb3c5c95a63 + W[52]; t2:= (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g)); h:= h + t1; d:= t1 + t2;
  t1:= c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $4ed8aa4ae3418acb + W[53]; t2:= (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f)); g:= g + t1; c:= t1 + t2;
  t1:= b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $5b9cca4f7763e373 + W[54]; t2:= (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e)); f:= f + t1; b:= t1 + t2;
  t1:= a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $682e6ff3d6b2b8a3 + W[55]; t2:= (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d)); e:= e + t1; a:= t1 + t2;
  t1:= h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $748f82ee5defb2fc + W[56]; t2:= (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c)); d:= d + t1; h:= t1 + t2;
  t1:= g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $78a5636f43172f60 + W[57]; t2:= (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b)); c:= c + t1; g:= t1 + t2;
  t1:= f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $84c87814a1f0ab72 + W[58]; t2:= (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a)); b:= b + t1; f:= t1 + t2;
  t1:= e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $8cc702081a6439ec + W[59]; t2:= (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h)); a:= a + t1; e:= t1 + t2;
  t1:= d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $90befffa23631e28 + W[60]; t2:= (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g)); h:= h + t1; d:= t1 + t2;
  t1:= c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $a4506cebde82bde9 + W[61]; t2:= (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f)); g:= g + t1; c:= t1 + t2;
  t1:= b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $bef9a3f7b2c67915 + W[62]; t2:= (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e)); f:= f + t1; b:= t1 + t2;
  t1:= a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $c67178f2e372532b + W[63]; t2:= (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d)); e:= e + t1; a:= t1 + t2;
  t1:= h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $ca273eceea26619c + W[64]; t2:= (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c)); d:= d + t1; h:= t1 + t2;
  t1:= g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $d186b8c721c0c207 + W[65]; t2:= (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b)); c:= c + t1; g:= t1 + t2;
  t1:= f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $eada7dd6cde0eb1e + W[66]; t2:= (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a)); b:= b + t1; f:= t1 + t2;
  t1:= e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $f57d4f7fee6ed178 + W[67]; t2:= (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h)); a:= a + t1; e:= t1 + t2;
  t1:= d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $06f067aa72176fba + W[68]; t2:= (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g)); h:= h + t1; d:= t1 + t2;
  t1:= c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $0a637dc5a2c898a6 + W[69]; t2:= (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f)); g:= g + t1; c:= t1 + t2;
  t1:= b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $113f9804bef90dae + W[70]; t2:= (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e)); f:= f + t1; b:= t1 + t2;
  t1:= a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $1b710b35131c471b + W[71]; t2:= (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d)); e:= e + t1; a:= t1 + t2;
  t1:= h + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23))) + ((e and f) xor (not e and g)) + $28db77f523047d84 + W[72]; t2:= (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25))) + ((a and b) xor (a and c) xor (b and c)); d:= d + t1; h:= t1 + t2;
  t1:= g + (((d shr 14) or (d shl 50)) xor ((d shr 18) or (d shl 46)) xor ((d shr 41) or (d shl 23))) + ((d and e) xor (not d and f)) + $32caab7b40c72493 + W[73]; t2:= (((h shr 28) or (h shl 36)) xor ((h shr 34) or (h shl 30)) xor ((h shr 39) or (h shl 25))) + ((h and a) xor (h and b) xor (a and b)); c:= c + t1; g:= t1 + t2;
  t1:= f + (((c shr 14) or (c shl 50)) xor ((c shr 18) or (c shl 46)) xor ((c shr 41) or (c shl 23))) + ((c and d) xor (not c and e)) + $3c9ebe0a15c9bebc + W[74]; t2:= (((g shr 28) or (g shl 36)) xor ((g shr 34) or (g shl 30)) xor ((g shr 39) or (g shl 25))) + ((g and h) xor (g and a) xor (h and a)); b:= b + t1; f:= t1 + t2;
  t1:= e + (((b shr 14) or (b shl 50)) xor ((b shr 18) or (b shl 46)) xor ((b shr 41) or (b shl 23))) + ((b and c) xor (not b and d)) + $431d67c49c100d4c + W[75]; t2:= (((f shr 28) or (f shl 36)) xor ((f shr 34) or (f shl 30)) xor ((f shr 39) or (f shl 25))) + ((f and g) xor (f and h) xor (g and h)); a:= a + t1; e:= t1 + t2;
  t1:= d + (((a shr 14) or (a shl 50)) xor ((a shr 18) or (a shl 46)) xor ((a shr 41) or (a shl 23))) + ((a and b) xor (not a and c)) + $4cc5d4becb3e42b6 + W[76]; t2:= (((e shr 28) or (e shl 36)) xor ((e shr 34) or (e shl 30)) xor ((e shr 39) or (e shl 25))) + ((e and f) xor (e and g) xor (f and g)); h:= h + t1; d:= t1 + t2;
  t1:= c + (((h shr 14) or (h shl 50)) xor ((h shr 18) or (h shl 46)) xor ((h shr 41) or (h shl 23))) + ((h and a) xor (not h and b)) + $597f299cfc657e2a + W[77]; t2:= (((d shr 28) or (d shl 36)) xor ((d shr 34) or (d shl 30)) xor ((d shr 39) or (d shl 25))) + ((d and e) xor (d and f) xor (e and f)); g:= g + t1; c:= t1 + t2;
  t1:= b + (((g shr 14) or (g shl 50)) xor ((g shr 18) or (g shl 46)) xor ((g shr 41) or (g shl 23))) + ((g and h) xor (not g and a)) + $5fcb6fab3ad6faec + W[78]; t2:= (((c shr 28) or (c shl 36)) xor ((c shr 34) or (c shl 30)) xor ((c shr 39) or (c shl 25))) + ((c and d) xor (c and e) xor (d and e)); f:= f + t1; b:= t1 + t2;
  t1:= a + (((f shr 14) or (f shl 50)) xor ((f shr 18) or (f shl 46)) xor ((f shr 41) or (f shl 23))) + ((f and g) xor (not f and h)) + $6c44198c4a475817 + W[79]; t2:= (((b shr 28) or (b shl 36)) xor ((b shr 34) or (b shl 30)) xor ((b shr 39) or (b shl 25))) + ((b and c) xor (b and d) xor (c and d)); e:= e + t1; a:= t1 + t2;

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

procedure THashSHA512Base.Update(const Buffer; Size: Cardinal);
var
  PBuf: ^byte;
begin
  Inc(fLenLo,Size*8);
  if fLenLo< (Size*8) then
    Inc(fLenHi);

  PBuf:= @Buffer;
  while Size> 0 do
  begin
    if (Sizeof(fHashBuff)-fIndex)<= DWord(Size) then
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

procedure THashSHA512Base.Burn;
begin
  fLenHi:= 0; fLenLo:= 0;
  fIndex:= 0;
  FillChar(fHashBuff,Sizeof(fHashBuff),0);
  FillChar(fCurHash,Sizeof(fCurHash),0);
end;

end.
