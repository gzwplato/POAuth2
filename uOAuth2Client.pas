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
    * Refress token must be returned with the access token
}

interface

uses
  SysUtils, Classes, uOAuth2Token, uOAuth2HttpClient, uOAuth2Config;

type
  TOAuth2GrantType = (
    gtPassword
  );

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
    FGrantType: TOAuth2GrantType;
    FConfig: TOAuth2Config;
    function GetAuthHeaderForAccessToken(const AAccessToken: string): string;
    function GetBasicAuthHeader(const AUsername, APassword: string): string;
    function EncodeCredentials(const AUsername, APassword: string): string;
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
  end;

implementation

uses
  uOAuth2Consts, uOAuth2Tools, uJson;

const
  GRANT_TYPE_STRINGS: array[TOAuth2GrantType] of string = ('password');

constructor TOAuth2Client.Create(AClient: TOAuth2HttpClient);
begin
  inherited Create;
  FHttpClient := AClient;
  FAccessToken := nil;
  FConfig := DEF_OATUH2_CONFIG;
  FGrantType := gtPassword;
end;

constructor TOAuth2Client.Create(AConfig: TOAuth2Config; AClient: TOAuth2HttpClient);
begin
  inherited Create;
  FHttpClient := AClient;
  FAccessToken := nil;
  FConfig := AConfig;
  FGrantType := gtPassword;
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
  FHttpClient.AddFormField(OAUTH2_USERNAME, FUserName);
  FHttpClient.AddFormField(OAUTH2_PASSWORD, FPassWord);
  if FClientId <> '' then
    FHttpClient.AddFormField(OAUTH2_CLIENT_ID, FClientId);
  if FClientSecret <> '' then
    FHttpClient.AddFormField(OAUTH2_CLIENT_SECRET, FClientSecret);
  if FScope <> '' then
    FHttpClient.AddFormField(OAUTH2_SCOPE, FScope);
  url := FSite + FConfig.TokenEndPoint;
  response := FHttpClient.Post(url);

  if IsAccessDenied(response.Code) and
    (FUserName <> '') and (FPassWord <> '') then begin
    // Try with Basic Auth
    FHttpClient.AddHeader(OAUTH2_AUTHORIZATION, GetBasicAuthHeader(FUserName, FPassWord));
    response := FHttpClient.Post(url);
    if IsAccessDenied(response.Code) and
      (FClientId <> '') and (FClientSecret <> '') then begin
      // Try with client credentials
      FHttpClient.AddHeader(OAUTH2_AUTHORIZATION, GetBasicAuthHeader(FClientId, FClientSecret));
      response := FHttpClient.Post(url);
    end;
  end;

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
      Result.AccessToken := json.Output.Strings[val.Index];
    end;
    val := json.GetValue(OAUTH2_REFRESH_TOKEN);
    if val.Kind = JVKString then begin
      Result.RefreshToken := json.Output.Strings[val.Index];
    end;
    val := json.GetValue(OAUTH2_EXPIRES_IN);
    if val.Kind = JVKNumber then begin
      Result.ExpiresIn := Trunc(json.Output.Numbers[val.Index]);
    end;
    val := json.GetValue(OAUTH2_TOKEN_TYPE);
    if val.Kind = JVKString then begin
      Result.TokenType := json.Output.Strings[val.Index];
    end;
    val := json.GetValue(OAUTH2_SCOPE);
    if val.Kind = JVKString then begin
      FScope := json.Output.Strings[val.Index];
    end;
  finally
    json.Free;
  end;
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
  Result := string(EncodeBase64(cred));
end;

procedure TOAuth2Client.RefreshAccessToken(AToken: TOAuth2Token);
var
  url: string;
  response: TOAuth2Response;
  json: TJson;
  val: TJsonValue;
begin
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

  if IsAccessDenied(response.Code) and
    (FUserName <> '') and (FPassWord <> '') then begin
    // Try with Basic Auth
    FHttpClient.AddHeader(OAUTH2_AUTHORIZATION, GetBasicAuthHeader(FUserName, FPassWord));
    response := FHttpClient.Post(url);
    if IsAccessDenied(response.Code) and
      (FClientId <> '') and (FClientSecret <> '') then begin
      // Try with client credentials
      FHttpClient.AddHeader(OAUTH2_AUTHORIZATION, GetBasicAuthHeader(FClientId, FClientSecret));
      response := FHttpClient.Post(url);
    end;
  end;

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
      AToken.AccessToken := json.Output.Strings[val.Index];
    end;
    val := json.GetValue(OAUTH2_REFRESH_TOKEN);
    if val.Kind = JVKString then begin
      AToken.RefreshToken := json.Output.Strings[val.Index];
    end;
    val := json.GetValue(OAUTH2_EXPIRES_IN);
    if val.Kind = JVKNumber then begin
      AToken.ExpiresIn := Trunc(json.Output.Numbers[val.Index]);
    end;
    val := json.GetValue(OAUTH2_TOKEN_TYPE);
    if val.Kind = JVKString then begin
      AToken.TokenType := json.Output.Strings[val.Index];
    end;
    val := json.GetValue(OAUTH2_SCOPE);
    if val.Kind = JVKString then begin
      FScope := json.Output.Strings[val.Index];
    end;
  finally
    json.Free;
  end;

end;

function TOAuth2Client.Get(const APath: string): TOAuth2Response;
var
  url: string;
begin
  if not Assigned(FAccessToken) then
    FAccessToken := GetAccessToken;
  if FAccessToken.IsExpired then
    RefreshAccessToken(FAccessToken);

  FHttpClient.ClearHeader;
  FHttpClient.ClearFormFields;
  url := FSite + APath;
  FHttpClient.AddHeader(OAUTH2_AUTHORIZATION, GetAuthHeaderForAccessToken(FAccessToken.AccessToken));
  Result := FHttpClient.Get(url);
  if IsAccessDenied(Result.Code) then begin
    // Maybe passing access token as formfield helps
    FHttpClient.RemoveHeader(OAUTH2_AUTHORIZATION);
    FHttpClient.AddFormField(OATUH2_ACCESS_TOKEN, FAccessToken.AccessToken);
    Result := FHttpClient.Get(url);
  end;
  if Result.Code <> HTTP_OK then begin
    raise Exception.CreateFmt('Server returned %d: %s', [Result.Code, Result.Body]);
  end;
end;

function TOAuth2Client.Post(const APath: string; AFormFields: TStringList): TOAuth2Response;
var
  url: string;
  i: integer;
  key, value: string;
begin
  if not Assigned(FAccessToken) then
    FAccessToken := GetAccessToken;
  if FAccessToken.IsExpired then
    RefreshAccessToken(FAccessToken);

  FHttpClient.ClearHeader;
  FHttpClient.ClearFormFields;
  url := FSite + APath;
  FHttpClient.AddHeader(OAUTH2_AUTHORIZATION, GetAuthHeaderForAccessToken(FAccessToken.AccessToken));
  for i := 0 to AFormFields.Count - 1 do begin
    key := AFormFields.Names[i];
    value := AFormFields.Values[key];
    FHttpClient.AddFormField(key, value);
  end;
  Result := FHttpClient.Post(url);
  if IsAccessDenied(Result.Code) then begin
    // Maybe passing access token as formfield helps
    FHttpClient.RemoveHeader(OAUTH2_AUTHORIZATION);
    FHttpClient.AddFormField(OATUH2_ACCESS_TOKEN, FAccessToken.AccessToken);
    Result := FHttpClient.Post(url);
  end;
  if Result.Code <> HTTP_OK then begin
    raise Exception.CreateFmt('Server returned %d: %s', [Result.Code, Result.Body]);
  end;
end;

end.
