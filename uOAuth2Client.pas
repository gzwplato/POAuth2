{
  Simple OAuth2 client

  (C) 2016, Stefan Ascher
}

unit uOAuth2Client;

{$IFDEF FPC}
  {$mode objfpc}
  {$H+}
{$ENDIF}

{
  Tested with https://github.com/bshaffer/oauth2-server-php

  Limitations:
    * Only the password GrantType (i.e. user credentials) is supported.
    * Server must return JSON
    * Refresh token must be returned with the access token
}

interface

uses
  SysUtils, Classes, uOAuth2Token, uOAuth2HttpClient, uOAuth2Config;

type
  TOAuth2GrantType = (
    gtPassword,                     // Username, Password
    gtAuthCode,                     // AuthCode
    gtClientCredentials             // ClientId, ClientSecret
  );
  TAccessTokenLoc = (atlUnknown, atlHeader, atlFormfield);

  TOAuth2Client = class
  private
    FAccessTokenLoc: TAccessTokenLoc;
    FUserName: string;
    FPassWord: string;
    FClientId: string;
    FClientSecret: string;
    FSite: string;
    FScope: string;
    FRedirectUri: string;
    FAuthCode: string;
    FHttpClient: TOAuth2HttpClient;
    FAccessToken: TOAuth2Token;
    FGrantType: TOAuth2GrantType;
    FConfig: TOAuth2Config;
    procedure RefreshAccessToken(AToken: TOAuth2Token);
    function GetAccessToken: TOAuth2Token;
    procedure SetAccessToken(Value: TOAuth2Token);
    procedure SetUserName(Value: string);
    procedure SetPassWord(Value: string);
    procedure SetClientId(Value: string);
    procedure SetClientSecret(Value: string);
    procedure SetSite(Value: string);
    procedure SetGrantType(Value: TOAuth2GrantType);
  public
    constructor Create(AClient: TOAuth2HttpClient); overload;
    constructor Create(AConfig: TOAuth2Config; AClient: TOAuth2HttpClient); overload;
    destructor Destroy; override;
    function Get(const APath: string): TOAuth2Response;
    function Post(const APath: string; AFormFields: TStringList): TOAuth2Response;
    procedure InvalidateToken;

    property UserName: string read FUserName write SetUserName;
    property PassWord: string read FPassWord write SetPassWord;
    property ClientId: string read FClientId write SetClientId;
    property ClientSecret: string read FClientSecret write SetClientSecret;
    property Site: string read FSite write SetSite;
    property GrantType: TOAuth2GrantType read FGrantType write SetGrantType default gtPassword;
    property AccessToken: TOAuth2Token read FAccessToken write SetAccessToken;
    property RedirectUri: string read FRedirectUri write FRedirectUri;
    // for GrantType = gtAuthCode
    property AuthCode: string read FAuthCode write FAuthCode;
    property Config: TOAuth2Config read FConfig write FConfig;
  end;

implementation

uses
  uOAuth2Consts, uOAuth2Tools, uJson;

const
  GRANT_TYPE_STRINGS: array[TOAuth2GrantType] of string = (
    'password',
    'authorization_code',
    'client_credentials'
  );

constructor TOAuth2Client.Create(AClient: TOAuth2HttpClient);
begin
  inherited Create;
  FHttpClient := AClient;
  FAccessToken := nil;
  FConfig := DEF_OATUH2_CONFIG;
  FGrantType := gtPassword;
  FAccessTokenLoc := atlUnknown;
end;

constructor TOAuth2Client.Create(AConfig: TOAuth2Config; AClient: TOAuth2HttpClient);
begin
  inherited Create;
  FHttpClient := AClient;
  FAccessToken := nil;
  FConfig := AConfig;
  FGrantType := gtPassword;
  FAccessTokenLoc := atlUnknown;
end;

destructor TOAuth2Client.Destroy;
begin
  if Assigned(FAccessToken) then
    FAccessToken.Free;
  inherited;
end;

procedure TOAuth2Client.InvalidateToken;
begin
  if Assigned(FAccessToken) then
    FreeAndNil(FAccessToken);
end;

procedure TOAuth2Client.SetAccessToken(Value: TOAuth2Token);
begin
  if FAccessToken <> Value then begin
    if Assigned(FAccessToken) then
      FAccessToken.Free;
    FAccessToken := Value;
  end;
end;

procedure TOAuth2Client.SetUserName(Value: string);
begin
  if FUserName <> Value then begin
    FUserName := Value;
    InvalidateToken;
  end;
end;

procedure TOAuth2Client.SetPassWord(Value: string);
begin
  if FPassWord <> Value then begin
    FPassWord := Value;
    InvalidateToken;
  end;
end;

procedure TOAuth2Client.SetClientId(Value: string);
begin
  if FClientId <> Value then begin
    FClientId := Value;
    InvalidateToken;
  end;
end;

procedure TOAuth2Client.SetClientSecret(Value: string);
begin
  if FClientSecret <> Value then begin
    FClientSecret := Value;
    InvalidateToken;
  end;
end;

procedure TOAuth2Client.SetSite(Value: string);
begin
  if FSite <> Value then begin
    FSite := Value;
    FAccessTokenLoc := atlUnknown;
    InvalidateToken;
  end;
end;

procedure TOAuth2Client.SetGrantType(Value: TOAuth2GrantType);
begin
  if FGrantType <> Value then begin
    FGrantType := Value;
    InvalidateToken;
  end;
end;

function TOAuth2Client.GetAccessToken: TOAuth2Token;
var
  response: TOAuth2Response;
  url: string;
  json: TJson;
  val: TJsonValue;
begin
  FHttpClient.ClearHeader;
  FHttpClient.ClearFormFields;
  FHttpClient.AddFormField(OAUTH2_GRANT_TYPE, GRANT_TYPE_STRINGS[FGrantType]);
  if (FGrantType = gtPassword) then begin
    FHttpClient.AddFormField(OAUTH2_USERNAME, FUserName);
    FHttpClient.AddFormField(OAUTH2_PASSWORD, FPassWord);
  end else if (FGrantType = gtAuthCode) then begin
    if FAuthCode = '' then
      raise Exception.Create('Missing auth code for authorization_code grant type');
    FHttpClient.AddFormField(OAUTH2_CODE, FAuthCode);
    FHttpClient.AddFormField(OAUTH2_REDIRECT_URI, FRedirectUri);
  end;
  if FClientId <> '' then
    FHttpClient.AddFormField(OAUTH2_CLIENT_ID, FClientId)
  else if FGrantType = gtClientCredentials then
    raise Exception.Create('Missing Client ID for client credentials grant type');
  if FClientSecret <> '' then
    FHttpClient.AddFormField(OAUTH2_CLIENT_SECRET, FClientSecret)
  else if FGrantType = gtClientCredentials then
    raise Exception.Create('Missing Client Secret for client credentials grant type');
  if FScope <> '' then
    FHttpClient.AddFormField(OAUTH2_SCOPE, FScope);
  url := FSite + FConfig.TokenEndPoint;
  response := FHttpClient.Post(url);

  if response.Code <> HTTP_OK then begin
    raise Exception.CreateFmt('Server returned %d: %s', [response.Code, response.Body]);
  end;

  if not IsJson(response.ContentType) then
    raise Exception.CreateFmt('JSON required, got %s', [response.ContentType]);

  Result := TOAuth2Token.Create;
  json := TJson.Create;
  try
    json.Parse(response.Body);
    val := json.GetValue(OATUH2_ACCESS_TOKEN);
    if val.Kind = JVKString then begin
      Result.AccessToken := string(json.Output.Strings[val.Index]);
    end;
    val := json.GetValue(OAUTH2_REFRESH_TOKEN);
    if val.Kind = JVKString then begin
      Result.RefreshToken := string(json.Output.Strings[val.Index]);
    end;
    val := json.GetValue(OAUTH2_EXPIRES_IN);
    if val.Kind = JVKNumber then begin
      Result.ExpiresIn := Trunc(json.Output.Numbers[val.Index]);
    end;
    val := json.GetValue(OAUTH2_TOKEN_TYPE);
    if val.Kind = JVKString then begin
      Result.TokenType := string(json.Output.Strings[val.Index]);
    end;
    val := json.GetValue(OAUTH2_SCOPE);
    if val.Kind = JVKString then begin
      FScope := string(json.Output.Strings[val.Index]);
    end;
  finally
    json.Free;
  end;
end;

procedure TOAuth2Client.RefreshAccessToken(AToken: TOAuth2Token);
var
  url: string;
  response: TOAuth2Response;
  json: TJson;
  val: TJsonValue;
begin
  if AToken.RefreshToken = '' then
    raise Exception.Create('No Refresh token');

  FHttpClient.ClearHeader;
  FHttpClient.ClearFormFields;
  FHttpClient.AddFormField(OAUTH2_GRANT_TYPE, OAUTH2_REFRESH_TOKEN);
  FHttpClient.AddFormField(OAUTH2_REFRESH_TOKEN, AToken.RefreshToken);
  if FClientId <> '' then
    FHttpClient.AddFormField(OAUTH2_CLIENT_ID, FClientId);
  if FClientSecret <> '' then
    FHttpClient.AddFormField(OAUTH2_CLIENT_SECRET, FClientSecret);
  url := FSite + FConfig.TokenEndPoint;
  response := FHttpClient.Post(url);

  if response.Code <> HTTP_OK then begin
    raise Exception.CreateFmt('Server returned %d: %s', [response.Code, response.Body]);
  end;
  if not IsJson(response.ContentType) then
    raise Exception.CreateFmt('JSON required, got %s', [response.ContentType]);

  json := TJson.Create;
  try
    json.Parse(response.Body);
    val := json.GetValue(OATUH2_ACCESS_TOKEN);
    if val.Kind = JVKString then begin
      AToken.AccessToken := string(json.Output.Strings[val.Index]);
    end;
    val := json.GetValue(OAUTH2_REFRESH_TOKEN);
    if val.Kind = JVKString then begin
      AToken.RefreshToken := string(json.Output.Strings[val.Index]);
    end;
    val := json.GetValue(OAUTH2_EXPIRES_IN);
    if val.Kind = JVKNumber then begin
      AToken.ExpiresIn := Trunc(json.Output.Numbers[val.Index]);
    end;
    val := json.GetValue(OAUTH2_TOKEN_TYPE);
    if val.Kind = JVKString then begin
      AToken.TokenType := string(json.Output.Strings[val.Index]);
    end;
    val := json.GetValue(OAUTH2_MACKEY);
    if val.Kind = JVKString then begin
      AToken.MacKey := string(json.Output.Strings[val.Index]);
    end;
    val := json.GetValue(OAUTH2_MAXALGORITHM);
    if val.Kind = JVKString then begin
      AToken.MacAlgorithm := string(json.Output.Strings[val.Index]);
    end;
    val := json.GetValue(OAUTH2_SCOPE);
    if val.Kind = JVKString then begin
      FScope := string(json.Output.Strings[val.Index]);
    end;
  finally
    json.Free;
  end;

end;

function TOAuth2Client.Get(const APath: string): TOAuth2Response;
var
  url: string;
  urlp: TUrlParts;
begin
  if not Assigned(FAccessToken) then
    FAccessToken := GetAccessToken;
  if FAccessToken.IsExpired then begin
    if FAccessToken.RefreshToken <> '' then
      RefreshAccessToken(FAccessToken)
    else begin
      // no refreh token -> get a new access token
      FAccessToken.Free;
      FAccessToken := GetAccessToken;
    end;
  end;

  FHttpClient.ClearHeader;
  FHttpClient.ClearFormFields;
  url := FSite + APath;
  urlp := ParseUrl(url);
  Result.Code := 0;
  case FAccessTokenLoc of
    atlUnknown:
      begin
        FHttpClient.AddHeader(OAUTH2_AUTHORIZATION, FAccessToken.GetAuthHeader('GET', urlp));
        Result := FHttpClient.Get(url);
        if IsAccessDenied(Result.Code) then begin
          // Maybe passing access token as formfield helps
          FHttpClient.RemoveHeader(OAUTH2_AUTHORIZATION);
          FHttpClient.AddFormField(OATUH2_ACCESS_TOKEN, FAccessToken.AccessToken);
          Result := FHttpClient.Get(url);
          if not IsAccessDenied(Result.Code) then
            FAccessTokenLoc := atlFormfield;
        end else
          FAccessTokenLoc := atlHeader;
      end;
    atlHeader:
      begin
        FHttpClient.AddHeader(OAUTH2_AUTHORIZATION, FAccessToken.GetAuthHeader('GET', urlp));
        Result := FHttpClient.Get(url);
      end;
    atlFormfield:
      begin
        FHttpClient.AddFormField(OATUH2_ACCESS_TOKEN, FAccessToken.AccessToken);
        Result := FHttpClient.Get(url);
      end;
  end;
  if Result.Code <> HTTP_OK then begin
    raise Exception.CreateFmt('Server returned %d: %s', [Result.Code, Result.Body]);
  end;
end;

function TOAuth2Client.Post(const APath: string; AFormFields: TStringList): TOAuth2Response;
var
  url: string;
  urlp: TUrlParts;
  i: integer;
  key, value: string;
begin
  if not Assigned(FAccessToken) then
    FAccessToken := GetAccessToken;
  if FAccessToken.IsExpired then
    RefreshAccessToken(FAccessToken);

  FHttpClient.ClearHeader;
  FHttpClient.ClearFormFields;
  for i := 0 to AFormFields.Count - 1 do begin
    key := AFormFields.Names[i];
    value := AFormFields.Values[key];
    FHttpClient.AddFormField(key, value);
  end;
  url := FSite + APath;
  urlp := ParseUrl(url);
  Result.Code := 0;
  case FAccessTokenLoc of
    atlUnknown:
      begin
        FHttpClient.AddHeader(OAUTH2_AUTHORIZATION, FAccessToken.GetAuthHeader('GET', urlp));
        Result := FHttpClient.Post(url);
        if IsAccessDenied(Result.Code) then begin
          // Maybe passing access token as formfield helps
          FHttpClient.RemoveHeader(OAUTH2_AUTHORIZATION);
          FHttpClient.AddFormField(OATUH2_ACCESS_TOKEN, FAccessToken.AccessToken);
          Result := FHttpClient.Post(url);
          if not IsAccessDenied(Result.Code) then
            FAccessTokenLoc := atlFormfield;
        end else
          FAccessTokenLoc := atlHeader;
      end;
    atlHeader:
      begin
        FHttpClient.AddHeader(OAUTH2_AUTHORIZATION, FAccessToken.GetAuthHeader('GET', urlp));
        Result := FHttpClient.Post(url);
      end;
    atlFormfield:
      begin
        FHttpClient.AddFormField(OATUH2_ACCESS_TOKEN, FAccessToken.AccessToken);
        Result := FHttpClient.Post(url);
      end;
  end;
  if Result.Code <> HTTP_OK then begin
    raise Exception.CreateFmt('Server returned %d: %s', [Result.Code, Result.Body]);
  end;
end;

end.
