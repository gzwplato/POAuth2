{
  Hash functions
  Copyright (c) 1999-2002 David Barton
  Copyright (C) 2007, Ascher Stefan. All rights reserved.
}

{
  Reimplementation of the hash functions from DCPcrypt
  <http://www.cityinthesky.co.uk/cryptography.html> by David Barton
}

unit HashRipeMD128;

interface

uses
  SysUtils, Classes, Windows, Hash;

type
  THashRipeMD128 = class(THash)
  private
    procedure Compress;
  protected
    fLenHi, fLenLo: Cardinal;
    fIndex: DWord;
    fCurHash: array[0..3] of DWord;
    fHashBuff: array[0..63] of byte;
  public
    procedure Init; override;
    procedure Update(const Buffer; Size: Cardinal); override;
    procedure Final(var Digest: array of Byte); override;
    function SelfTest: boolean; override;
    class function GetSize: integer; override;

    procedure Burn; override;
  end;
  THashRipeMD128Class = class of THashRipeMD128;

implementation
{$R-}{$Q-}

function THashRipeMD128.SelfTest: boolean;
const
  Test1Out: array[0..15] of byte=
    ($86,$be,$7a,$fa,$33,$9d,$0f,$c7,$cf,$c7,$85,$e7,$2f,$57,$8d,$33);
  Test2Out: array[0..15] of byte=
    ($fd,$2a,$a6,$07,$f7,$1d,$c8,$f5,$10,$71,$49,$22,$b3,$71,$83,$4e);
var
  TestOut: array[0..15] of byte;
begin
  Init;
  UpdateStr('a');
  Final(TestOut);
  Result:= CompareMem(@TestOut,@Test1Out,Sizeof(Test1Out));
  Init;
  UpdateStr('abcdefghijklmnopqrstuvwxyz');
  Final(TestOut);
  Result:= CompareMem(@TestOut,@Test2Out,Sizeof(Test2Out)) and Result;
end;

procedure THashRipeMD128.Compress;
var
  X: array[0..15] of DWord;
  a, aa, b, bb, c, cc, d, dd, t: dword;
begin
  Move(fHashBuff,X,Sizeof(X));
  a:= fCurHash[0]; aa:= a;
  b:= fCurHash[1]; bb:= b;
  c:= fCurHash[2]; cc:= c;
  d:= fCurHash[3]; dd:= d;

  t:= a + (b xor c xor d) + X[ 0]; a:= (t shl 11) or (t shr (32-11));
  t:= d + (a xor b xor c) + X[ 1]; d:= (t shl 14) or (t shr (32-14));
  t:= c + (d xor a xor b) + X[ 2]; c:= (t shl 15) or (t shr (32-15));
  t:= b + (c xor d xor a) + X[ 3]; b:= (t shl 12) or (t shr (32-12));
  t:= a + (b xor c xor d) + X[ 4]; a:= (t shl 5) or (t shr (32-5));
  t:= d + (a xor b xor c) + X[ 5]; d:= (t shl 8) or (t shr (32-8));
  t:= c + (d xor a xor b) + X[ 6]; c:= (t shl 7) or (t shr (32-7));
  t:= b + (c xor d xor a) + X[ 7]; b:= (t shl 9) or (t shr (32-9));
  t:= a + (b xor c xor d) + X[ 8]; a:= (t shl 11) or (t shr (32-11));
  t:= d + (a xor b xor c) + X[ 9]; d:= (t shl 13) or (t shr (32-13));
  t:= c + (d xor a xor b) + X[10]; c:= (t shl 14) or (t shr (32-14));
  t:= b + (c xor d xor a) + X[11]; b:= (t shl 15) or (t shr (32-15));
  t:= a + (b xor c xor d) + X[12]; a:= (t shl 6) or (t shr (32-6));
  t:= d + (a xor b xor c) + X[13]; d:= (t shl 7) or (t shr (32-7));
  t:= c + (d xor a xor b) + X[14]; c:= (t shl 9) or (t shr (32-9));
  t:= b + (c xor d xor a) + X[15]; b:= (t shl 8) or (t shr (32-8));

  t:= a + ((b and c) or (not b and d)) + X[ 7]+$5A827999; a:= (t shl 7) or (t shr (32-7));
  t:= d + ((a and b) or (not a and c)) + X[ 4]+$5A827999; d:= (t shl 6) or (t shr (32-6));
  t:= c + ((d and a) or (not d and b)) + X[13]+$5A827999; c:= (t shl 8) or (t shr (32-8));
  t:= b + ((c and d) or (not c and a)) + X[ 1]+$5A827999; b:= (t shl 13) or (t shr (32-13));
  t:= a + ((b and c) or (not b and d)) + X[10]+$5A827999; a:= (t shl 11) or (t shr (32-11));
  t:= d + ((a and b) or (not a and c)) + X[ 6]+$5A827999; d:= (t shl 9) or (t shr (32-9));
  t:= c + ((d and a) or (not d and b)) + X[15]+$5A827999; c:= (t shl 7) or (t shr (32-7));
  t:= b + ((c and d) or (not c and a)) + X[ 3]+$5A827999; b:= (t shl 15) or (t shr (32-15));
  t:= a + ((b and c) or (not b and d)) + X[12]+$5A827999; a:= (t shl 7) or (t shr (32-7));
  t:= d + ((a and b) or (not a and c)) + X[ 0]+$5A827999; d:= (t shl 12) or (t shr (32-12));
  t:= c + ((d and a) or (not d and b)) + X[ 9]+$5A827999; c:= (t shl 15) or (t shr (32-15));
  t:= b + ((c and d) or (not c and a)) + X[ 5]+$5A827999; b:= (t shl 9) or (t shr (32-9));
  t:= a + ((b and c) or (not b and d)) + X[ 2]+$5A827999; a:= (t shl 11) or (t shr (32-11));
  t:= d + ((a and b) or (not a and c)) + X[14]+$5A827999; d:= (t shl 7) or (t shr (32-7));
  t:= c + ((d and a) or (not d and b)) + X[11]+$5A827999; c:= (t shl 13) or (t shr (32-13));
  t:= b + ((c and d) or (not c and a)) + X[ 8]+$5A827999; b:= (t shl 12) or (t shr (32-12));

  t:= a + ((b or not c) xor d) + X[ 3]+$6ED9EBA1; a:= (t shl 11) or (t shr (32-11));
  t:= d + ((a or not b) xor c) + X[10]+$6ED9EBA1; d:= (t shl 13) or (t shr (32-13));
  t:= c + ((d or not a) xor b) + X[14]+$6ED9EBA1; c:= (t shl 6) or (t shr (32-6));
  t:= b + ((c or not d) xor a) + X[ 4]+$6ED9EBA1; b:= (t shl 7) or (t shr (32-7));
  t:= a + ((b or not c) xor d) + X[ 9]+$6ED9EBA1; a:= (t shl 14) or (t shr (32-14));
  t:= d + ((a or not b) xor c) + X[15]+$6ED9EBA1; d:= (t shl 9) or (t shr (32-9));
  t:= c + ((d or not a) xor b) + X[ 8]+$6ED9EBA1; c:= (t shl 13) or (t shr (32-13));
  t:= b + ((c or not d) xor a) + X[ 1]+$6ED9EBA1; b:= (t shl 15) or (t shr (32-15));
  t:= a + ((b or not c) xor d) + X[ 2]+$6ED9EBA1; a:= (t shl 14) or (t shr (32-14));
  t:= d + ((a or not b) xor c) + X[ 7]+$6ED9EBA1; d:= (t shl 8) or (t shr (32-8));
  t:= c + ((d or not a) xor b) + X[ 0]+$6ED9EBA1; c:= (t shl 13) or (t shr (32-13));
  t:= b + ((c or not d) xor a) + X[ 6]+$6ED9EBA1; b:= (t shl 6) or (t shr (32-6));
  t:= a + ((b or not c) xor d) + X[13]+$6ED9EBA1; a:= (t shl 5) or (t shr (32-5));
  t:= d + ((a or not b) xor c) + X[11]+$6ED9EBA1; d:= (t shl 12) or (t shr (32-12));
  t:= c + ((d or not a) xor b) + X[ 5]+$6ED9EBA1; c:= (t shl 7) or (t shr (32-7));
  t:= b + ((c or not d) xor a) + X[12]+$6ED9EBA1; b:= (t shl 5) or (t shr (32-5));

  t:= a + ((b and d) or (c and not d)) + X[ 1]+$8F1BBCDC; a:= (t shl 11) or (t shr (32-11));
  t:= d + ((a and c) or (b and not c)) + X[ 9]+$8F1BBCDC; d:= (t shl 12) or (t shr (32-12));
  t:= c + ((d and b) or (a and not b)) + X[11]+$8F1BBCDC; c:= (t shl 14) or (t shr (32-14));
  t:= b + ((c and a) or (d and not a)) + X[10]+$8F1BBCDC; b:= (t shl 15) or (t shr (32-15));
  t:= a + ((b and d) or (c and not d)) + X[ 0]+$8F1BBCDC; a:= (t shl 14) or (t shr (32-14));
  t:= d + ((a and c) or (b and not c)) + X[ 8]+$8F1BBCDC; d:= (t shl 15) or (t shr (32-15));
  t:= c + ((d and b) or (a and not b)) + X[12]+$8F1BBCDC; c:= (t shl 9) or (t shr (32-9));
  t:= b + ((c and a) or (d and not a)) + X[ 4]+$8F1BBCDC; b:= (t shl 8) or (t shr (32-8));
  t:= a + ((b and d) or (c and not d)) + X[13]+$8F1BBCDC; a:= (t shl 9) or (t shr (32-9));
  t:= d + ((a and c) or (b and not c)) + X[ 3]+$8F1BBCDC; d:= (t shl 14) or (t shr (32-14));
  t:= c + ((d and b) or (a and not b)) + X[ 7]+$8F1BBCDC; c:= (t shl 5) or (t shr (32-5));
  t:= b + ((c and a) or (d and not a)) + X[15]+$8F1BBCDC; b:= (t shl 6) or (t shr (32-6));
  t:= a + ((b and d) or (c and not d)) + X[14]+$8F1BBCDC; a:= (t shl 8) or (t shr (32-8));
  t:= d + ((a and c) or (b and not c)) + X[ 5]+$8F1BBCDC; d:= (t shl 6) or (t shr (32-6));
  t:= c + ((d and b) or (a and not b)) + X[ 6]+$8F1BBCDC; c:= (t shl 5) or (t shr (32-5));
  t:= b + ((c and a) or (d and not a)) + X[ 2]+$8F1BBCDC; b:= (t shl 12) or (t shr (32-12));

  t:= aa + ((bb and dd) or (cc and not dd)) + X[ 5]+$50A28BE6; aa:= (t shl 8) or (t shr (32-8));
  t:= dd + ((aa and cc) or (bb and not cc)) + X[14]+$50A28BE6; dd:= (t shl 9) or (t shr (32-9));
  t:= cc + ((dd and bb) or (aa and not bb)) + X[ 7]+$50A28BE6; cc:= (t shl 9) or (t shr (32-9));
  t:= bb + ((cc and aa) or (dd and not aa)) + X[ 0]+$50A28BE6; bb:= (t shl 11) or (t shr (32-11));
  t:= aa + ((bb and dd) or (cc and not dd)) + X[ 9]+$50A28BE6; aa:= (t shl 13) or (t shr (32-13));
  t:= dd + ((aa and cc) or (bb and not cc)) + X[ 2]+$50A28BE6; dd:= (t shl 15) or (t shr (32-15));
  t:= cc + ((dd and bb) or (aa and not bb)) + X[11]+$50A28BE6; cc:= (t shl 15) or (t shr (32-15));
  t:= bb + ((cc and aa) or (dd and not aa)) + X[ 4]+$50A28BE6; bb:= (t shl 5) or (t shr (32-5));
  t:= aa + ((bb and dd) or (cc and not dd)) + X[13]+$50A28BE6; aa:= (t shl 7) or (t shr (32-7));
  t:= dd + ((aa and cc) or (bb and not cc)) + X[ 6]+$50A28BE6; dd:= (t shl 7) or (t shr (32-7));
  t:= cc + ((dd and bb) or (aa and not bb)) + X[15]+$50A28BE6; cc:= (t shl 8) or (t shr (32-8));
  t:= bb + ((cc and aa) or (dd and not aa)) + X[ 8]+$50A28BE6; bb:= (t shl 11) or (t shr (32-11));
  t:= aa + ((bb and dd) or (cc and not dd)) + X[ 1]+$50A28BE6; aa:= (t shl 14) or (t shr (32-14));
  t:= dd + ((aa and cc) or (bb and not cc)) + X[10]+$50A28BE6; dd:= (t shl 14) or (t shr (32-14));
  t:= cc + ((dd and bb) or (aa and not bb)) + X[ 3]+$50A28BE6; cc:= (t shl 12) or (t shr (32-12));
  t:= bb + ((cc and aa) or (dd and not aa)) + X[12]+$50A28BE6; bb:= (t shl 6) or (t shr (32-6));

  t:= aa + ((bb or not cc) xor dd) + X[ 6]+$5C4DD124; aa:= (t shl 9) or (t shr (32-9));
  t:= dd + ((aa or not bb) xor cc) + X[11]+$5C4DD124; dd:= (t shl 13) or (t shr (32-13));
  t:= cc + ((dd or not aa) xor bb) + X[ 3]+$5C4DD124; cc:= (t shl 15) or (t shr (32-15));
  t:= bb + ((cc or not dd) xor aa) + X[ 7]+$5C4DD124; bb:= (t shl 7) or (t shr (32-7));
  t:= aa + ((bb or not cc) xor dd) + X[ 0]+$5C4DD124; aa:= (t shl 12) or (t shr (32-12));
  t:= dd + ((aa or not bb) xor cc) + X[13]+$5C4DD124; dd:= (t shl 8) or (t shr (32-8));
  t:= cc + ((dd or not aa) xor bb) + X[ 5]+$5C4DD124; cc:= (t shl 9) or (t shr (32-9));
  t:= bb + ((cc or not dd) xor aa) + X[10]+$5C4DD124; bb:= (t shl 11) or (t shr (32-11));
  t:= aa + ((bb or not cc) xor dd) + X[14]+$5C4DD124; aa:= (t shl 7) or (t shr (32-7));
  t:= dd + ((aa or not bb) xor cc) + X[15]+$5C4DD124; dd:= (t shl 7) or (t shr (32-7));
  t:= cc + ((dd or not aa) xor bb) + X[ 8]+$5C4DD124; cc:= (t shl 12) or (t shr (32-12));
  t:= bb + ((cc or not dd) xor aa) + X[12]+$5C4DD124; bb:= (t shl 7) or (t shr (32-7));
  t:= aa + ((bb or not cc) xor dd) + X[ 4]+$5C4DD124; aa:= (t shl 6) or (t shr (32-6));
  t:= dd + ((aa or not bb) xor cc) + X[ 9]+$5C4DD124; dd:= (t shl 15) or (t shr (32-15));
  t:= cc + ((dd or not aa) xor bb) + X[ 1]+$5C4DD124; cc:= (t shl 13) or (t shr (32-13));
  t:= bb + ((cc or not dd) xor aa) + X[ 2]+$5C4DD124; bb:= (t shl 11) or (t shr (32-11));

  t:= aa + ((bb and cc) or (not bb and dd)) + X[15]+$6D703EF3; aa:= (t shl 9) or (t shr (32-9));
  t:= dd + ((aa and bb) or (not aa and cc)) + X[ 5]+$6D703EF3; dd:= (t shl 7) or (t shr (32-7));
  t:= cc + ((dd and aa) or (not dd and bb)) + X[ 1]+$6D703EF3; cc:= (t shl 15) or (t shr (32-15));
  t:= bb + ((cc and dd) or (not cc and aa)) + X[ 3]+$6D703EF3; bb:= (t shl 11) or (t shr (32-11));
  t:= aa + ((bb and cc) or (not bb and dd)) + X[ 7]+$6D703EF3; aa:= (t shl 8) or (t shr (32-8));
  t:= dd + ((aa and bb) or (not aa and cc)) + X[14]+$6D703EF3; dd:= (t shl 6) or (t shr (32-6));
  t:= cc + ((dd and aa) or (not dd and bb)) + X[ 6]+$6D703EF3; cc:= (t shl 6) or (t shr (32-6));
  t:= bb + ((cc and dd) or (not cc and aa)) + X[ 9]+$6D703EF3; bb:= (t shl 14) or (t shr (32-14));
  t:= aa + ((bb and cc) or (not bb and dd)) + X[11]+$6D703EF3; aa:= (t shl 12) or (t shr (32-12));
  t:= dd + ((aa and bb) or (not aa and cc)) + X[ 8]+$6D703EF3; dd:= (t shl 13) or (t shr (32-13));
  t:= cc + ((dd and aa) or (not dd and bb)) + X[12]+$6D703EF3; cc:= (t shl 5) or (t shr (32-5));
  t:= bb + ((cc and dd) or (not cc and aa)) + X[ 2]+$6D703EF3; bb:= (t shl 14) or (t shr (32-14));
  t:= aa + ((bb and cc) or (not bb and dd)) + X[10]+$6D703EF3; aa:= (t shl 13) or (t shr (32-13));
  t:= dd + ((aa and bb) or (not aa and cc)) + X[ 0]+$6D703EF3; dd:= (t shl 13) or (t shr (32-13));
  t:= cc + ((dd and aa) or (not dd and bb)) + X[ 4]+$6D703EF3; cc:= (t shl 7) or (t shr (32-7));
  t:= bb + ((cc and dd) or (not cc and aa)) + X[13]+$6D703EF3; bb:= (t shl 5) or (t shr (32-5));

  t:= aa + (bb xor cc xor dd) + X[ 8]; aa:= (t shl 15) or (t shr (32-15));
  t:= dd + (aa xor bb xor cc) + X[ 6]; dd:= (t shl 5) or (t shr (32-5));
  t:= cc + (dd xor aa xor bb) + X[ 4]; cc:= (t shl 8) or (t shr (32-8));
  t:= bb + (cc xor dd xor aa) + X[ 1]; bb:= (t shl 11) or (t shr (32-11));
  t:= aa + (bb xor cc xor dd) + X[ 3]; aa:= (t shl 14) or (t shr (32-14));
  t:= dd + (aa xor bb xor cc) + X[11]; dd:= (t shl 14) or (t shr (32-14));
  t:= cc + (dd xor aa xor bb) + X[15]; cc:= (t shl 6) or (t shr (32-6));
  t:= bb + (cc xor dd xor aa) + X[ 0]; bb:= (t shl 14) or (t shr (32-14));
  t:= aa + (bb xor cc xor dd) + X[ 5]; aa:= (t shl 6) or (t shr (32-6));
  t:= dd + (aa xor bb xor cc) + X[12]; dd:= (t shl 9) or (t shr (32-9));
  t:= cc + (dd xor aa xor bb) + X[ 2]; cc:= (t shl 12) or (t shr (32-12));
  t:= bb + (cc xor dd xor aa) + X[13]; bb:= (t shl 9) or (t shr (32-9));
  t:= aa + (bb xor cc xor dd) + X[ 9]; aa:= (t shl 12) or (t shr (32-12));
  t:= dd + (aa xor bb xor cc) + X[ 7]; dd:= (t shl 5) or (t shr (32-5));
  t:= cc + (dd xor aa xor bb) + X[10]; cc:= (t shl 15) or (t shr (32-15));
  t:= bb + (cc xor dd xor aa) + X[14]; bb:= (t shl 8) or (t shr (32-8));

  Inc(dd,c + fCurHash[1]);
  fCurHash[1]:= fCurHash[2] + d + aa;
  fCurHash[2]:= fCurHash[3] + a + bb;
  fCurHash[3]:= fCurHash[0] + b + cc;
  fCurHash[0]:= dd;

  FillChar(X,Sizeof(X),0);
  fIndex:= 0;
  FillChar(fHashBuff,Sizeof(fHashBuff),0);
end;

procedure THashRipeMD128.Init;
begin
  Burn;
  fCurHash[0]:= $67452301;
  fCurHash[1]:= $efcdab89;
  fCurHash[2]:= $98badcfe;
  fCurHash[3]:= $10325476;
 end;

procedure THashRipeMD128.Update(const Buffer; Size: Cardinal);
var
  PBuf: PByte;
begin
  Inc(fLenHi,Size shr 29);
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

procedure THashRipeMD128.Final(var Digest: array of Byte);
begin
  fHashBuff[fIndex]:= $80;
  if fIndex>= 56 then
    Compress;
  PDWord(@fHashBuff[56])^:= fLenLo;
  PDWord(@fHashBuff[60])^:= fLenHi;
  Compress;
  Move(fCurHash,Digest,Sizeof(fCurHash));
  Burn;
end;

procedure THashRipeMD128.Burn;
begin
  fLenHi := 0; fLenLo := 0;
  fIndex := 0;
  FillChar(fHashBuff,Sizeof(fHashBuff),0);
  FillChar(fCurHash,Sizeof(fCurHash),0);
end;

class function THashRipeMD128.GetSize: integer;
begin
  Result := SizeOf(DWord) * 4;
end;

initialization
  THash.RegisterHashType('RIPEMD-128', THashRipeMD128);

end.
