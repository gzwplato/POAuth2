{
  Simple OAuth2 client

  (C) 2016, Stefan Ascher
}

unit uOAuth2Token;

interface

uses
  SysUtils, Classes, uOAuth2HttpClient;

type
  TOAuth2Token = class
  protected
    FExpiresIn: integer;
    FExpiresAt: TDateTime;
    FTokenType: string;
    FRefreshToken: string;
    FAccessToken: string;
    procedure SetExpiresIn(Value: integer);
  public
    function IsExpired: boolean;

    property ExpiresIn: integer read FExpiresIn write SetExpiresIn;
    property ExpiresAt: TDateTime read FExpiresAt write FExpiresAt;
    property TokenType: string read FTokenType write FTokenType;
    property RefreshToken: string read FRefreshToken write FRefreshToken;
    property AccessToken: string read FAccessToken write FAccessToken;
  end;

implementation

uses
  DateUtils;

procedure TOAuth2Token.SetExpiresIn(Value: integer);
begin
  FExpiresIn := Value;
  FExpiresAt := IncSecond(Now, Value);
end;

function TOAuth2Token.IsExpired: boolean;
var
  n: TDateTime;
begin
  n := Now;
  Result := FExpiresAt < n;
end;

end.
