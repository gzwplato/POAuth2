{
  Hash functions
  Copyright (c) 1999-2002 David Barton
  Copyright (C) 2007, Ascher Stefan. All rights reserved.
}

{
  Reimplementation of the hash functions from DCPcrypt
  <http://www.cityinthesky.co.uk/cryptography.html> by David Barton
}

unit HashSHA384;

interface

uses
  SysUtils, Classes, Hash, HashSHA512Base;

type
  THashSHA384 = class(THashSHA512Base)
  public
    procedure Init; override;
    procedure Final(var Digest: array of Byte); override;
    function SelfTest: boolean; override;
    class function GetSize: integer; override;
  end;
  THashSHA384Class = class of THashSHA384;

implementation
{$R-}{$Q-}

function THashSHA384.SelfTest: boolean;
const
  Test1Out: array[0..47] of byte=
    ($cb,$00,$75,$3f,$45,$a3,$5e,$8b,$b5,$a0,$3d,$69,$9a,$c6,$50,$07,
     $27,$2c,$32,$ab,$0e,$de,$d1,$63,$1a,$8b,$60,$5a,$43,$ff,$5b,$ed,
     $80,$86,$07,$2b,$a1,$e7,$cc,$23,$58,$ba,$ec,$a1,$34,$c8,$25,$a7);
  Test2Out: array[0..47] of byte=
    ($09,$33,$0c,$33,$f7,$11,$47,$e8,$3d,$19,$2f,$c7,$82,$cd,$1b,$47,
     $53,$11,$1b,$17,$3b,$3b,$05,$d2,$2f,$a0,$80,$86,$e3,$b0,$f7,$12,
     $fc,$c7,$c7,$1a,$55,$7e,$2d,$b9,$66,$c3,$e9,$fa,$91,$74,$60,$39);
var
  TestOut: array[0..47] of byte;
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

procedure THashSHA384.Init;
begin
  Burn;
  fCurHash[0]:= $cbbb9d5dc1059ed8;
  fCurHash[1]:= $629a292a367cd507;
  fCurHash[2]:= $9159015a3070dd17;
  fCurHash[3]:= $152fecd8f70e5939;
  fCurHash[4]:= $67332667ffc00b31;
  fCurHash[5]:= $8eb44a8768581511;
  fCurHash[6]:= $db0c2e0d64f98fa7;
  fCurHash[7]:= $47b5481dbefa4fa4;
end;

procedure THashSHA384.Final(var Digest: array of Byte);
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
  Move(fCurHash,Digest,384 div 8);
  Burn;
end;

class function THashSHA384.GetSize: integer;
begin
  Result := 384 div 8;
end;

initialization
  THash.RegisterHashType('SHA-384', THashSHA384);

end.
 
