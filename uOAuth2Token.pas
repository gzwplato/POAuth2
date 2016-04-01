{
  Simple OAuth2 client

  (C) 2016, Stefan Ascher
}

unit uOAuth2Token;

{$IFDEF FPC}
  {$mode objfpc}
  {$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, uOAuth2Tools;

type
  TOAuth2Token = class
  protected
    FExpiresIn: integer;
    FExpiresAt: TDateTime;
    FTokenType: string;
    FRefreshToken: string;
    FAccessToken: string;
    FMacKey: string;
    FMacAlgorithm: string;
    procedure SetExpiresIn(Value: integer);
    function GetBearerAuthHeader: string;
    function GetMacAuthHeader(const AMethod: string; const AUrl: TUrlParts): string;
    function GetMacSignature(const AMethod: string; const AUrl: TUrlParts;
      const ATs: LongInt; const ANonce: string): string;
  public
    function IsExpired: boolean;
    function GetAuthHeader(const AMethod: string; const AUrl: TUrlParts): string;

    property ExpiresIn: integer read FExpiresIn write SetExpiresIn;
    property ExpiresAt: TDateTime read FExpiresAt write FExpiresAt;
    property TokenType: string read FTokenType write FTokenType;
    property RefreshToken: string read FRefreshToken write FRefreshToken;
    property AccessToken: string read FAccessToken write FAccessToken;
    property MacKey: string read FMacKey write FMacKey;
    property MacAlgorithm: string read FMacAlgorithm write FMacAlgorithm;
  end;

implementation

uses
  DateUtils, uOAuth2Consts, uOAuth2Hmac;

function TOAuth2Token.GetBearerAuthHeader: string;
begin
  Result := Format('%s %s', [OAUTH2_TOKENTYPE_BEARER, FAccessToken])
end;

function TOAuth2Token.GetMacSignature(const AMethod: string; const AUrl: TUrlParts;
  const ATs: LongInt; const ANonce: string): string;
var
  url: string;
begin
  url := BuildUrl(AUrl);
  Result := Format('%d'#10'%s'#10'%s'#10'%s'#10'%s'#10'%d', [
    ATs, ANonce, AMethod, url, AUrl.Host, AUrl.Port
  ]);
  Result := Result;
end;

function TOAuth2Token.GetMacAuthHeader(const AMethod: string; const AUrl: TUrlParts): string;
var
  ts: LongInt;
  nonce: string;
  guid: TGuid;
  mac_sig: string;
  hmac: AnsiString;
  hash_name: AnsiString;
  p: integer;
begin
  // Untested
  // https://dzone.com/articles/oauth-20-bearer-token-profile
  // http://oauth2.thephpleague.com/token-types/

  ts := DateTimeToUnix(Now);
  CreateGUID(guid);
  nonce := GUIDToString(guid);
  nonce := StringReplace(nonce, '-', '', [rfReplaceAll]);
  mac_sig := GetMacSignature(AMethod, AUrl, ts, nonce);

  p := Pos('hmac-', LowerCase(FMacAlgorithm));
  if p = 0 then
    raise Exception.CreateFmt('Unsupported MAC algorithm %s', [FMacAlgorithm]);

  hash_name := Copy(FMacAlgorithm, 6, MaxInt);
  hmac := EncodeBase64(HmacCalc(mac_sig, FMacKey, hash_name));
  Result := Format('MAC id="%s", ts="%d", nonce="%s", mac="%s"', [
    FAccessToken, ts, nonce, hmac
  ]);
end;

function TOAuth2Token.GetAuthHeader(const AMethod: string; const AUrl: TUrlParts): string;
begin
  if SameText(FTokenType, OAUTH2_TOKENTYPE_BEARER) then
    Result := GetBearerAuthHeader
  else if SameText(FTokenType, OAUTH2_TOKENTYPE_MAC) then
    Result := GetMacAuthHeader(AMethod, AUrl)
  else
    raise Exception.CreateFmt('Unsupported Tokentype "%s"', [FTokenType]);
end;

procedure TOAuth2Token.SetExpiresIn(Value: integer);
begin
  FExpiresIn := Value;
  FExpiresAt := IncSecond(Now, Value);
end;

function TOAuth2Token.IsExpired: boolean;
begin
  Result := FExpiresAt < Now;
end;

end.
