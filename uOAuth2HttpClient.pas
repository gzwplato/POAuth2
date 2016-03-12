unit uOAuth2HttpClient;

interface

uses
	SysUtils, Classes;

type
	TOAuth2Response = record
    Code: integer;           // Response Code, e.g. 200, 401, 403
    Body: string;
    ContentType: string;
  end;

	TOAuth2HttpClient = class
  protected
  	FHeaders: TStringList;
  	FFormFields: TStringList;
    FBody: string;
	public
  	constructor Create;
    destructor Destroy; override;
  	function Get(const AUrl: string): TOAuth2Response; virtual; abstract;
  	function Post(const AUrl: string): TOAuth2Response; virtual; abstract;

    procedure AddFormField(const AKey, AValue: string); virtual;
    procedure ClearFormFields; virtual;
    procedure AddHeader(const AKey, AValue: string); virtual;
    procedure ClearHeader; virtual;
  end;

implementation

constructor TOAuth2HttpClient.Create;
begin
  inherited;
	FHeaders := TStringList.Create;
  FFormFields := TStringList.Create;
  FFormFields.NameValueSeparator := '=';
  FFormFields.CaseSensitive := false;
end;

destructor TOAuth2HttpClient.Destroy;
begin
	FHeaders.Free;
	FFormFields.Free;
  inherited;
end;

procedure TOAuth2HttpClient.ClearFormFields;
begin
  FFormFields.Clear;
end;

procedure TOAuth2HttpClient.AddFormField(const AKey, AValue: string);
var
	i: integer;
begin
  i := FFormFields.IndexOfName(AKey);
  if i = -1 then begin
    FFormFields.Add(Format('%s=%s', [AKey, AValue]));
  end else begin
    FFormFields[i] := Format('%s=%s', [AKey, AValue]);
  end;
end;

procedure TOAuth2HttpClient.ClearHeader;
begin

end;

procedure TOAuth2HttpClient.AddHeader(const AKey, AValue: string);
begin

end;

end.
