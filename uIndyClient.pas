{
  Simple OAuth2 client

  (C) 2016, Stefan Ascher
}

unit uIndyClient;

{$IFDEF FPC}
  {$mode objfpc}
  {$H+}
{$ENDIF}

{
  HTTP client using Indy
}

interface

uses
  SysUtils, Classes, IdHttp, uOAuth2HttpClient, IdIOHandler, IdAuthentication, IdHeaderList;

type
  TIndyHttpClient = class(TOAuth2HttpClient)
  private
    FHttp: TIdHttp;
    FOwnClient: boolean;
    FIOHandler: TIdIOHandler;
    FSSLIOHandler: TIdIOHandler;
    FUsername: string;
    FPassword: string;
    procedure SetIOHandler(const AProt: string);
  public
    constructor Create(AHttp: TIdHttp);
    destructor Destroy; override;
    function Get(const AUrl: string): TOAuth2Response; override;
    function Post(const AUrl: string): TOAuth2Response; override;

    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
  end;

implementation

uses
  uOAuth2Tools, IdSSLOpenSSL, IdURI, uOAuth2Consts;

constructor TIndyHttpClient.Create(AHttp: TIdHttp);
begin
  inherited Create;
  FSSLIOHandler := nil;
  if Assigned(AHttp) then begin
    FHttp := AHttp;
    FOwnClient := false;
  end else begin
    FHttp := TIdHttp.Create(nil);
    FHttp.Request.UserAgent := 'Mozilla/3.0 (compatible; POAuth2)';
    FOwnClient := true;
  end;
  with FHttp do begin
    AllowCookies := True;
    HandleRedirects := false;
    HTTPOptions := HTTPOptions + [hoKeepOrigProtocol];
  end;
end;

destructor TIndyHttpClient.Destroy;
begin
  if FOwnClient then
    FHttp.Free;
  inherited;
end;

procedure TIndyHttpClient.SetIOHandler(const AProt: string);
begin
  if (AProt = 'https') and not (FHttp.IOHandler is TIdSSLIOHandlerSocketOpenSSL) then begin
    FIOHandler := FHttp.IOHandler;
    if not Assigned(FSSLIOHandler) then
      FSSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(FHttp);
    FHttp.IOHandler := FSSLIOHandler;
  end else if ((AProt = 'http') and (FHttp.IOHandler is TIdSSLIOHandlerSocketOpenSSL)) then begin
    FHttp.IOHandler := FIOHandler;
  end;
end;

function TIndyHttpClient.Get(const AUrl: string): TOAuth2Response;
var
  body: string;
  urlp: TUrlParts;
  url: string;
begin
  urlp := ParseUrl(AUrl);
  try
    SetIOHandler(urlp.Protocol);

    if FFormFields.Count > 0 then begin
      // Append form fileds to the URL
      if urlp.Query <> '' then
        urlp.Query := urlp.Query + '&';
      urlp.Query := urlp.Query + GetQuery;
    end;
    url := TIdURI.URLEncode(BuildUrl(urlp));
    FHttp.Request.CustomHeaders.Clear;
    if FHeaders.Count > 0 then begin
      if FHeaders.IndexOfName(OAUTH2_AUTHORIZATION) = -1 then begin
        FHttp.Request.Username := FUsername;
        FHttp.Request.Password := FPassword;
        FHttp.Request.BasicAuthentication := (FUsername <> '') and (FPassword <> '');
      end else begin
        FHttp.Request.Username := '';
        FHttp.Request.Password := '';
        FHttp.Request.BasicAuthentication := false;
      end;
      FHttp.Request.CustomHeaders.AddStrings(FHeaders);
    end else begin
      FHttp.Request.Username := FUsername;
      FHttp.Request.Password := FPassword;
      FHttp.Request.BasicAuthentication := (FUsername <> '') and (FPassword <> '');
    end;
    body := FHttp.Get(url);
    Result.Code := FHttp.ResponseCode;
    Result.ContentType := FHttp.Response.ContentType;
    Result.Body := body;
  except
    on E: EIdHTTPProtocolException do begin
      Result.Code := E.ErrorCode;
      Result.Body := E.ErrorMessage;
    end;
  end;
end;

function TIndyHttpClient.Post(const AUrl: string): TOAuth2Response;
var
  body: string;
  urlp: TUrlParts;
  url: string;
begin
  urlp := ParseUrl(AUrl);
  try
    SetIOHandler(urlp.Protocol);

    FHttp.Request.CustomHeaders.Clear;
    if FHeaders.Count > 0 then begin
      if FHeaders.IndexOfName(OAUTH2_AUTHORIZATION) = -1 then begin
        FHttp.Request.Username := FUsername;
        FHttp.Request.Password := FPassword;
        FHttp.Request.BasicAuthentication := (FUsername <> '') and (FPassword <> '');
      end else begin
        FHttp.Request.Username := '';
        FHttp.Request.Password := '';
        FHttp.Request.BasicAuthentication := false;
      end;
      FHttp.Request.CustomHeaders.AddStrings(FHeaders);
    end else begin
      FHttp.Request.Username := FUsername;
      FHttp.Request.Password := FPassword;
      FHttp.Request.BasicAuthentication := (FUsername <> '') and (FPassword <> '');
    end;
    url := TIdURI.URLEncode(AUrl);
    body := FHttp.Post(url, FFormFields);
    Result.Code := FHttp.ResponseCode;
    Result.ContentType := FHttp.Response.ContentType;
    Result.Body := body;
  except
    on E: EIdHTTPProtocolException do begin
      Result.Code := E.ErrorCode;
      Result.Body := E.ErrorMessage;
    end;
  end;
end;

end.
