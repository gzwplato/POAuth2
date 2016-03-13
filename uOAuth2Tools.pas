{
  Simple OAuth2 client

  (C) 2016, Stefan Ascher
}

unit uOAuth2Tools;

interface

uses
  SysUtils;

type
  TUrlParts = record
    Protocol: string;
    Host: string;
    Path: string;
    Query: string;
    Port: Word;
    User: string;
    Pass: string;
  end;

function ParseUrl(const AUrl: string): TUrlParts;
function BuildUrl(const AParts: TUrlParts): string;

function EncodeBase64(const S: string): AnsiString;
function DecodeBase64(const S: AnsiString): string;

function RemoveLeadingSlash(const S: string): string;
function AddLeadingSlash(const S: string): string;
function RemoveTrailingSlash(const S: string): string;
function AddTrailingSlash(const S: string): string;

implementation

{$IFDEF FPC}
uses
  StrUtils;
{$ENDIF}

const
  BASE64_CODES: AnsiString = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+/';

function RemoveLeadingSlash(const S: string): string;
begin
  Result := S;
  if Result <> '' then begin
    if Result[1] = '/' then
      Delete(Result, 1, 1);
  end;
end;

function AddLeadingSlash(const S: string): string;
begin
  Result := S;
  if Result <> '' then begin
    if Result[1] <> '/' then
      Result := '/' + Result;
  end;
end;

function RemoveTrailingSlash(const S: string): string;
begin
  Result := S;
  if Result <> '' then begin
    if Result[Length(Result)] = '/' then
      Delete(Result, Length(Result), 1);
  end;
end;

function AddTrailingSlash(const S: string): string;
begin
  Result := S;
  if Result <> '' then begin
    if Result[Length(Result)] <> '/' then
      Result := Result + '/';
  end;
end;

function BuildUrl(const AParts: TUrlParts): string;
begin
  Result := AParts.Protocol + '://';
  if AParts.User <> '' then begin
    Result := Result + AParts.User;
    if AParts.Pass <> '' then begin
      Result := Result + ':' + AParts.Pass;
    end;
    Result := Result + '@';
  end;
  Result := Result + AParts.Host;
  if AParts.Port <> 0 then begin
    if ((AParts.Protocol = 'http') and (AParts.Port <> 80)) or
      ((AParts.Protocol = 'https') and (AParts.Port <> 443)) then
    Result := Format('%s:%d', [Result, AParts.Port]);
  end;
  if AParts.Path <> '' then
    Result := Result + AParts.Path
  else
    Result := Result + '/';
  if AParts.Query <> '' then
    Result := Result + '?' + AParts.Query;
end;

{
  https://user:pass@server:1337/path/to/file?query=somevalue
}
function ParseUrl(const AUrl: string): TUrlParts;
var
  p, p2, p3: integer;
  userpass, h, port, fq: string;
begin
  with Result do begin
    Protocol := '';
    Host := '';
    Path := '';
    Query:= '';
    Port := 0;
    User := '';
    Pass := '';
  end;
  p := Pos('://', AUrl);
  if p = 0 then
    Exit;
  Result.Protocol := LowerCase(Copy(AUrl, 1, p - 1));
  Inc(p, 3);
  p2 := Pos('@', AUrl);
  if p2 <> 0 then begin
    userpass := Copy(AUrl, p, p2 - p);
    p3 := Pos(':', userpass);
    if p3 <> 0 then begin
      Result.User := Copy(userpass, 1, p3 - 1);
      Result.Pass := Copy(userpass, p3 + 1, MaxInt);
    end else begin
      Result.User := userpass;
    end;
    p := p2 + 1;
  end;
  p2 := {$IFDEF FPC}PosEx{$ELSE}Pos{$ENDIF}('/', AUrl, p);
  if p2 = 0 then
    Exit;
  h := Copy(AUrl, p, p2 - p);
  p3 := Pos(':', h);
  if (p3 <> 0) then begin
    Result.Host := Copy(h, 1, p3 - 1);
    port := Copy(h, p3 + 1, MaxInt);
    Result.Port := StrToIntDef(port, 0);
  end else begin
    Result.Host := h;
  end;
  if (Result.Port = 0) and (Result.Protocol <> '') then begin
    if Result.Protocol = 'http' then
      Result.Port := 80
    else if Result.Protocol = 'https' then
      Result.Port := 443;
  end;
  p := p2;
  fq := Copy(AUrl, p, MaxInt);
  p2 := Pos('?', fq);
  if p2 <> 0 then begin
    Result.Path := Copy(fq, 1, p2 - 1);
    Result.Query := Copy(fq, p2 + 1, MaxInt);
  end else begin
    Result.Path := fq;
  end;
end;

function EncodeBase64(const S: string): AnsiString;
var
  i: Integer;
  a: Integer;
  x: Integer;
  b: Integer;
begin
  Result := '';
  a := 0;
  b := 0;
  for i := 1 to Length(s) do begin
    x := Ord(S[i]);
    b := b * 256 + x;
    a := a + 8;
    while a >= 6 do begin
      a := a - 6;
      x := b div (1 shl a);
      b := b mod (1 shl a);
      Result := Result + BASE64_CODES[x + 1];
    end;
  end;
  if a > 0 then begin
    x := b shl (6 - a);
    Result := Result + BASE64_CODES[x + 1];
  end;
end;

function DecodeBase64(const S: AnsiString): string;
var
  i: Integer;
  a: Integer;
  x: Integer;
  b: Integer;
begin
  Result := '';
  a := 0;
  b := 0;
  for i := 1 to Length(s) do begin
    x := Pos(s[i], BASE64_CODES) - 1;
    if x >= 0 then begin
      b := b * 64 + x;
      a := a + 6;
      if a >= 8 then begin
        a := a - 8;
        x := b shr a;
        b := b mod (1 shl a);
        x := x mod 256;
        Result := Result + Chr(x);
      end;
    end else
      Exit;
  end;
end;

end.
