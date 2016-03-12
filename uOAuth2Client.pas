unit uOAuth2Client;

interface

uses
	SysUtils, Classes, uOAuth2Token, uOAuth2HttpClient;

type
	TOAuth2Client = class
  private
  	FUserName: string;
    FPassWord: string;
    FClientId: string;
    FClientSecret: string;
    FSite: string;
    FScope: string;
    FHttpClient: TOAuth2HttpClient;
    FAccessToken: TOAuth2Token;
    FGrantType: string;
    function GetAuthHeaderForAccessToken(const AAccessToken: string): string;
    function GetBasicAuthHeader(const AUsername, APassword: string): string;
    function EncodeCredentials(const AUsername, APassword: string): string;
    procedure RefreshAccessToken(AToken: TOAuth2Token);
  	function GetAccessToken: TOAuth2Token;
  public
  	constructor Create(AClient: TOAuth2HttpClient);
    destructor Destroy; override;
    function GetReosurce(const APath: string): TOAuth2Response;

    property UserName: string read FUserName write FUserName;
    property PassWord: string read FPassWord write FPassWord;
    property ClientId: string read FClientId write FClientId;
    property ClientSecret: string read FClientSecret write FClientSecret;
    property Site: string read FSite write FSite;
    property GrantType: string read FGrantType write FGrantType;
    property AccessToken: TOAuth2Token read FAccessToken;
  end;

implementation

uses
	uOAuth2Consts, uOAuth2Tools;

constructor TOAuth2Client.Create(AClient: TOAuth2HttpClient);
begin
  inherited Create;
  FHttpClient := AClient;
  FAccessToken := nil;
end;

destructor TOAuth2Client.Destroy;
begin
	if Assigned(FAccessToken) then
  	FAccessToken.Free;
  inherited;
end;

function TOAuth2Client.GetAccessToken: TOAuth2Token;
var
  response: TOAuth2Response;
  url: string;
begin
	if Assigned(FAccessToken) then
		FAccessToken.Free;

  FHttpClient.ClearFormFields;
  FHttpClient.AddFormField(OAUTH2_GRANT_TYPE, FGrantType);
  FHttpClient.AddFormField(OAUTH2_USERNAME, FUserName);
  FHttpClient.AddFormField(OAUTH2_PASSWORD, FPassWord);
  if FClientId <> '' then
    FHttpClient.AddFormField(OAUTH2_CLIENT_ID, FClientId);
  if FClientSecret <> '' then
    FHttpClient.AddFormField(OAUTH2_CLIENT_SECRET, FClientSecret);
  if FScope <> '' then
    FHttpClient.AddFormField(OAUTH2_SCOPE, FScope);
  url := FSite + OAUTH2_TOKEN_ENDPOINT;
  response := FHttpClient.Post(url);
  Result := TOAuth2Token.Create;
end;

function TOAuth2Client.GetAuthHeaderForAccessToken(const AAccessToken: string): string;
begin
  Result := Format('%s %s', [OAUTH2_BEARER, AAccessToken]);
end;

function TOAuth2Client.GetBasicAuthHeader(const AUsername, APassword: string): string;
begin
	Result := Format('%s %s', [OATUH2_BASIC, EncodeCredentials(AUsername, APassword)]);
end;

function TOAuth2Client.EncodeCredentials(const AUsername, APassword: string): string;
var
	cred: string;
begin
	cred := Format('%s:%s', [AUsername, APassword]);
  Result := EncodeBase64(cred);
end;

procedure TOAuth2Client.RefreshAccessToken(AToken: TOAuth2Token);
var
	url: string;
  response: TOAuth2Response;
begin
  FHttpClient.ClearFormFields;
  FHttpClient.AddFormField(OAUTH2_GRANT_TYPE, 'refresh_token');
  FHttpClient.AddFormField(OAUTH2_REFRESH_TOKEN, AToken.RefreshToken);
  if FClientId <> '' then
    FHttpClient.AddFormField(OAUTH2_CLIENT_ID, FClientId);
  if FClientSecret <> '' then
    FHttpClient.AddFormField(OAUTH2_CLIENT_SECRET, FClientSecret);
  url := FSite + OAUTH2_TOKEN_ENDPOINT;
  response := FHttpClient.Post(url);
end;

function TOAuth2Client.GetReosurce(const APath: string): TOAuth2Response;
var
	url: string;
  response: TOAuth2Response;
begin
	if not Assigned(FAccessToken) then
  	FAccessToken := GetAccessToken;
  if FAccessToken.IsExpired then
    RefreshAccessToken(FAccessToken);

	url := FSite + APath;
  FHttpClient.AddHeader(OAUTH2_AUTHORIZATION, GetAuthHeaderForAccessToken(FAccessToken.AccessToken));
  response := FHttpClient.Get(url);
  if response.Code <> 200 then begin
    raise Exception.CreateFmt('Server returned %d: %s', [response.Code, response.Body]);
  end;
  Result := response;
end;

end.
