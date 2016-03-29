{
  Simple OAuth2 client

  (C) 2016, Stefan Ascher
}

{
  Hash-based message authentication code (HMAC)
  
  https://en.wikipedia.org/wiki/Hash-based_message_authentication_code
  https://dzone.com/articles/oauth-20-bearer-token-profile
  http://oauth2.thephpleague.com/token-types/
}
unit uOAuth2Hmac;

{$IFDEF FPC}
  {$mode objfpc}
  {$H+}
{$ENDIF}

interface

uses
  SysUtils, Hash;

function HmacCalc(const AMsg, AKey: AnsiString; AHash: THashClass; const ABlockSize: integer): AnsiString; overload;
function HmacCalc(const AMsg, AKey: AnsiString; const AHashName: AnsiString): AnsiString; overload;
function HashString(const AStr: string; AHash: THashClass): string;

implementation

uses
  HashHaval, HashMD5, HashRipeMD128, HashRipeMD160, HashSHA1, HashSHA256,
  HashSHA384, HashSHA512, HashTiger;

function HashString(const AStr: string; AHash: THashClass): string;
var
  d: array of Byte;
begin
  SetLength(d, AHash.GetSize);
  AHash.CalcString(AStr, AHash, d);
  Result := AHash.DigestToString(d);
end;


function HmacCalc(const AMsg, AKey: AnsiString; AHash: THashClass; const ABlockSize: integer): AnsiString;
var
  key: ShortString;
  k_ipad, k_opad: ShortString;
  i: integer;
  d: array of Byte;
  s_digest: AnsiString;
  hs: integer;
begin
  // if key is longer than 64 bytes reset it to key=sha256(key)
  if Length(AKey) > ABlockSize then
    key := HashString(AKey, AHash)
  else if Length(AKey) < ABlockSize then begin
    // keys shorter than blocksize are zero-padded
    key := AKey;
    SetLength(key, ABlockSize);
    for i := Length(AKey) + 1 to ABlockSize do
      key[i] := #0;
  end else
    key := AKey;

  // SHA256(K XOR opad, SHA256(K XOR ipad, text))
  SetLength(k_ipad, ABlockSize);
  SetLength(k_opad, ABlockSize);
  for i := 1 to ABlockSize do begin
    k_ipad[i] := AnsiChar(Byte(key[i]) xor $36);
    k_opad[i] := AnsiChar(Byte(key[i]) xor $5c);
  end;

  hs := AHash.GetSize;
  SetLength(d, hs);
  // Inner Hash
  AHash.CalcString(k_ipad + AMsg, AHash, d);
  // Outer Hash
  SetString(s_digest, PAnsiChar(@d[0]), hs);
  AHash.CalcString(k_opad + s_digest, AHash, d);

  Result := THash.DigestToString(d);
end;

function HmacCalc(const AMsg, AKey: AnsiString; const AHashName: AnsiString): AnsiString;
var
  ht: PHashType;
  size: integer;
begin
  ht := THash.GetHashType(AHashName);
  if (ht = nil) then begin
    raise Exception.CreateFmt('Unknown/unsupported Hash %s', [AHashName]);
  end;
  // Blocksize is 64 (bytes) when using one of the following hash functions: SHA-1, MD5, RIPEMD-128/160
  if ht^.HashClass.InheritsFrom(THashSHA1) or
    ht^.HashClass.InheritsFrom(THashSHA256) or
    ht^.HashClass.InheritsFrom(THashMD5) or
    ht^.HashClass.InheritsFrom(THashRipeMD128) or
    ht^.HashClass.InheritsFrom(THashRipeMD160) then
    size := 64
  else
    size := ht^.HashClass.GetSize;
  Result := HmacCalc(AMsg, AKey, ht^.HashClass, size);
end;

end.
