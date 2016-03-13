{
  Simple OAuth2 client

  (C) 2016, Stefan Ascher
}

unit uIndyClient;

{
  HTTP client using Indy
}

interface

uses
  SysUtils, Classes, IdHttp, uOAuth2HttpClient, IdIOHandler;

type
  TIndyHttpClient = class(TOAuth2HttpClient)
  private
    FHttp: TIdHttp;
    FOwnClient: boolean;
    FIOHandler: TIdIOHandler;
    FSSLIOHandler: TIdIOHandler;
    procedure SetIOHandler(const AProt: string);
  public
    constructor Create(AHttp: TIdHttp);
    destructor Destroy; override;
    function Get(const AUrl: string): TOAuth2Response; override;
    function Post(const AUrl: string): TOAuth2Response; override;
  end;

implementation

uses
  uOAuth2Tools, IdSSLOpenSSL, IdURI;

constructor TIndyHttpClient.Create(AHttp: TIdHttp);
begin
  inherited Create;
  FSSLIOHandler := nil;
  if Assigned(AHttp) then begin
    FHttp := AHttp;
    FOwnClient := false;
  end else begin
    FHttp := TIdHttp.Create(nil);
    FOwnClient := true;
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

    if urlp.Query <> '' then
      urlp.Query := urlp.Query + '&';
    urlp.Query := urlp.Query + GetQuery;
    url := TIdURI.URLEncode(BuildUrl(urlp));
    if FHeaders.Count > 0 then
      FHttp.Request.CustomHeaders.AddStrings(FHeaders);
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
begin
  urlp := ParseUrl(AUrl);
  try
    SetIOHandler(urlp.Protocol);

    if FHeaders.Count > 0 then
      FHttp.Request.CustomHeaders.AddStrings(FHeaders);
    body := FHttp.Post(AUrl, FFormFields);
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
