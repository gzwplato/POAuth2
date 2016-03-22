{
  Simple OAuth2 client

  (C) 2016, Stefan Ascher
}

unit uOAuth2HttpClient;

{$IFDEF FPC}
  {$mode objfpc}
  {$H+}
{$ENDIF}

{
  Abstract HTTP client class.
}

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
  public
    constructor Create;
    destructor Destroy; override;
    function Get(const AUrl: string): TOAuth2Response; virtual; abstract;
    function Post(const AUrl: string): TOAuth2Response; virtual; abstract;

    function GetQuery: string;
    procedure AddFormField(const AKey, AValue: string); dynamic;
    procedure ClearFormFields; dynamic;
    procedure AddHeader(const AKey, AValue: string); dynamic;
    procedure RemoveHeader(const AKey: string); dynamic;
    procedure ClearHeader; dynamic;
  end;

implementation

constructor TOAuth2HttpClient.Create;
begin
  inherited;
  FHeaders := TStringList.Create;
  FHeaders.NameValueSeparator := ':';
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

function TOAuth2HttpClient.GetQuery: string;
var
  i: integer;
  key, value: string;
begin
  Result := '';
  for i := 0 to FFormFields.Count - 1 do begin
    key := FFormFields.Names[i];
    value := FFormFields.Values[key];
    Result := Format('%s=%s&', [key, value]);
  end;
  if Result <> '' then begin
    if Result[Length(Result)] = '&' then
      Delete(Result, Length(Result), 1);
  end;
end;

procedure TOAuth2HttpClient.ClearFormFields;
begin
  FFormFields.Clear;
end;

procedure TOAuth2HttpClient.AddFormField(const AKey, AValue: string);
begin
  FFormFields.Add(Format('%s=%s', [AKey, AValue]));
end;

procedure TOAuth2HttpClient.ClearHeader;
begin
  FHeaders.Clear;
end;

procedure TOAuth2HttpClient.AddHeader(const AKey, AValue: string);
var
  i: integer;
  key: string;
begin
  for i := 0 to FHeaders.Count - 1 do begin
    key := FHeaders.Names[i];
    if CompareText(AKey, key) = 0 then begin
      FHeaders[i] := Format('%s: %s', [AKey, AValue]);
      Exit;
    end;
  end;
  FHeaders.Add(Format('%s: %s', [AKey, AValue]));
end;

procedure TOAuth2HttpClient.RemoveHeader(const AKey: string);
var
  i: integer;
  key: string;
begin
  for i := 0 to FHeaders.Count - 1 do begin
    key := FHeaders.Names[i];
    if CompareText(AKey, key) = 0 then begin
      FHeaders.Delete(i);
      Break;
    end;
  end;
end;

end.
