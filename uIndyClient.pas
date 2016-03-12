unit uIndyClient;

// http://www.delphipraxis.net/160152-indy-10-http-s-protokoll.html

interface

uses
	SysUtils, Classes, IdHttp, uOAuth2HttpClient;

type
	TIndyHttpClient = class(TOAuth2HttpClient)
  private
  	FHttp: TIdHttp;
    FOwnClient: boolean;
	public
  	constructor Create(AHttp: TIdHttp);
    destructor Destroy; override;
  	function Get(const AUrl: string): TOAuth2Response; override;
  	function Post(const AUrl: string): TOAuth2Response; override;
  end;

implementation

uses
	uOAuth2Tools, IdSSLOpenSSL;

constructor TIndyHttpClient.Create(AHttp: TIdHttp);
begin
  inherited Create;
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

function TIndyHttpClient.Get(const AUrl: string): TOAuth2Response;
var
	body: string;
  urlp: TUrlParts;
begin
	urlp := ParseUrl(AUrl);
	try
  	if urlp.Protocol = 'https' then begin
//      FHttp.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create;
    end;

	  body := FHttp.Get(AUrl);
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
begin
	try
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
