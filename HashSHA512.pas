{
  Hash functions
  Copyright (c) 1999-2002 David Barton
  Copyright (C) 2007, Ascher Stefan. All rights reserved.
}

{
  Reimplementation of the hash functions from DCPcrypt
  <http://www.cityinthesky.co.uk/cryptography.html> by David Barton
}

unit HashSHA512;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Hash, HashSHA512Base;

type
  THashSHA512 = class(THashSHA512Base)
  public
    procedure Init; override;
    procedure Final(var Digest: array of Byte); override;
    function SelfTest: boolean; override;
    class function GetSize: integer; override;
  end;
  THashSHA512Class = class of THashSHA512;

implementation
{$R-}{$Q-}

function THashSHA512.SelfTest: boolean;
const
  Test1Out: array[0..63] of byte=
    ($dd,$af,$35,$a1,$93,$61,$7a,$ba,$cc,$41,$73,$49,$ae,$20,$41,$31,
     $12,$e6,$fa,$4e,$89,$a9,$7e,$a2,$0a,$9e,$ee,$e6,$4b,$55,$d3,$9a,
     $21,$92,$99,$2a,$27,$4f,$c1,$a8,$36,$ba,$3c,$23,$a3,$fe,$eb,$bd,
     $45,$4d,$44,$23,$64,$3c,$e8,$0e,$2a,$9a,$c9,$4f,$a5,$4c,$a4,$9f);
  Test2Out: array[0..63] of byte=
    ($8e,$95,$9b,$75,$da,$e3,$13,$da,$8c,$f4,$f7,$28,$14,$fc,$14,$3f,
     $8f,$77,$79,$c6,$eb,$9f,$7f,$a1,$72,$99,$ae,$ad,$b6,$88,$90,$18,
     $50,$1d,$28,$9e,$49,$00,$f7,$e4,$33,$1b,$99,$de,$c4,$b5,$43,$3a,
     $c7,$d3,$29,$ee,$b6,$dd,$26,$54,$5e,$96,$e5,$5b,$87,$4b,$e9,$09);
var
  TestOut: array[0..63] of byte;
begin
  Init;
  UpdateStr('abc');
  Final(TestOut);
  Result:= boolean(CompareMem(@TestOut,@Test1Out,Sizeof(Test1Out)));
  Init;
  UpdateStr('abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu');
  Final(TestOut);
  Result:= boolean(CompareMem(@TestOut,@Test2Out,Sizeof(Test2Out))) and Result;
end;

procedure THashSHA512.Init;
begin
  Burn;
  fCurHash[0]:= $6a09e667f3bcc908;
  fCurHash[1]:= $bb67ae8584caa73b;
  fCurHash[2]:= $3c6ef372fe94f82b;
  fCurHash[3]:= $a54ff53a5f1d36f1;
  fCurHash[4]:= $510e527fade682d1;
  fCurHash[5]:= $9b05688c2b3e6c1f;
  fCurHash[6]:= $1f83d9abfb41bd6b;
  fCurHash[7]:= $5be0cd19137e2179;
end;

procedure THashSHA512.Final(var Digest: array of Byte);
begin
  fHashBuff[fIndex]:= $80;
  if fIndex>= 112 then
    Compress;
  Pint64(@fHashBuff[112])^:= SwapDWord(fLenHi);
  Pint64(@fHashBuff[120])^:= SwapDWord(fLenLo);
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

class function THashSHA512.GetSize: integer;
begin
  Result := SizeOf(Int64) * 8;
end;

initialization
  THash.RegisterHashType('SHA-512', THashSHA512);

end.
 
