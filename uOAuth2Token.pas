unit uOAuth2Token;

interface

uses
	SysUtils, Classes, uOAuth2HttpClient;

type
	TOAuth2Token = class
	private
  	FExpiresIn: integer;
    FExpiresAt: integer;
    FTokenType: string;
    FRefreshToken: string;
    FAccessToken: string;
  public
  	function IsExpired: boolean;

  	property ExpiresIn: integer read FExpiresIn;
    property ExpiresAt: integer read FExpiresAt;
    property TokenType: string read FTokenType;
    property RefreshToken: string read FRefreshToken;
    property AccessToken: string read FAccessToken;
  end;

implementation

function TOAuth2Token.IsExpired: boolean;
begin

end;

end.
