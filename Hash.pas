{
  Hash functions
  Copyright (C) 2007, Ascher Stefan. All rights reserved.
}

unit Hash;

interface

uses
  SysUtils, Classes;

type
  THash = class;
  THashClass = class of THash;

  PHashType = ^THashType;
  THashType = record
    HashClass: THashClass;
    Name: string;
    Size: Integer;
  end;

  THash = class
  public
    procedure Init; virtual; abstract;
    procedure Update(const Buffer; Size: Cardinal); virtual; abstract;
    procedure Final(var Digest: array of Byte); virtual; abstract;
    procedure Burn; virtual; abstract;
    procedure UpdateStr(const AString: string); virtual;
    function SelfTest: boolean; virtual;
    class function GetSize: integer; virtual; abstract;

    // Helper methods
    procedure HashFile(const AFilename: string; var Digest: array of Byte);
    procedure HashString(const AString: string; var Digest: array of Byte);
    class procedure CalcFile(const AFilename: string; AHash: THashClass; var Digest: array of Byte);
    class procedure CalcString(const AString: string; AHash: THashClass; var Digest: array of Byte);
    class function DigestToString(const Digest: array of Byte): string;

    // Hash register methods
    class procedure RegisterHashType(const AName: string; AHashClass: THashClass);
    class procedure UnRegisterHashType(AHashClass: THashClass);
    class function GetHashTypesCount: integer;
    class function GetHashType(const Index: integer): PHashType; overload;
    class function GetHashType(AHash: THashClass): PHashType; overload;
    class function GetHashType(AName: string): PHashType; overload;
  end;

implementation
{$R-}{$Q-}

const
  BuffSize = 4096;

type
  THashTypesList = class(TList)
  public
    destructor Destroy; override;
    function FindClassName(const AName: string): THashClass;
    procedure Remove(AClass: THashClass);
  end;

{ THashTypesList }

destructor THashTypesList.Destroy;
var
  i: integer;
  h: PHashType;
begin
  for i := 0 to Count-1 do begin
    h := Items[i];
    Dispose(h);
  end;
  inherited;
end;

function THashTypesList.FindClassName(const AName: string): THashClass;
var
  I: Integer;
begin
  for I := Count-1 downto 0 do
  begin
    Result := PHashType(Items[I])^.HashClass;
    if Result.ClassName = AName then Exit;
  end;
  Result := nil;
end;

procedure THashTypesList.Remove(AClass: THashClass);
var
  I: Integer;
  P: PHashType;
begin
  for I := Count-1 downto 0 do
  begin
    P := PHashType(Items[I]);
    if P^.HashClass.InheritsFrom(AClass) then
    begin
      Dispose(P);
      Delete(I);
    end;
  end;
end;

{ THash }

class function THash.DigestToString(const Digest: array of Byte): string;
var
  i: integer;
begin
  Result := '';
  for i := Low(Digest) to High(Digest) do
    Result := Result + IntToHex(integer(Digest[i]), 2);
end;

procedure THash.HashFile(const AFilename: string; var Digest: array of Byte);
var
  fs: TFileStream;
  buff: array[0..BuffSize-1] of Byte;
  read: Longint;
begin
  fs := TFileStream.Create(AFilename, fmOpenRead);
  try
    Init;
    repeat
      read := fs.Read(buff, BuffSize);
      if read > 0 then
        Update(buff, read);
    until read = 0;
    Final(Digest);
  finally
    fs.Free;
  end;
end;

procedure THash.HashString(const AString: string; var Digest: array of Byte);
begin
  Init;
  Update(AString[1], Length(AString));
  Final(Digest);
end;

class procedure THash.CalcFile(const AFilename: string; AHash: THashClass; var Digest: array of Byte);
var
  h: THash;
begin
  h := AHash.Create;
  try
    h.HashFile(AFilename, Digest);
  finally
    h.Free;
  end;
end;

class procedure THash.CalcString(const AString: string; AHash: THashClass; var Digest: array of Byte);
var
  h: THash;
begin
  h := AHash.Create;
  try
    h.HashString(AString, Digest);
  finally
    h.Free;
  end;
end;

procedure THash.UpdateStr(const AString: string);
begin
  Update(AString[1], Length(AString));
end;

function THash.SelfTest: boolean;
begin
  Result := false;
end;

var
  HashTypes: THashTypesList = nil;

function GetHashTypes: THashTypesList;
begin
  if HashTypes = nil then HashTypes := THashTypesList.Create;
  Result := HashTypes;
end;

class procedure THash.RegisterHashType(const AName: string; AHashClass: THashClass);
var
  h: PHashType;
begin
  New(h);
  h^.Name := AName;
  h^.HashClass := AHashClass;
  h^.Size := AHashClass.GetSize;
  GetHashTypes.Add(h);
end;

class procedure THash.UnRegisterHashType(AHashClass: THashClass);
begin
  if HashTypes <> nil then
    HashTypes.Remove(AHashClass);
end;

class function THash.GetHashTypesCount: integer;
begin
  if HashTypes <> nil then
    Result := HashTypes.Count
  else
    Result := 0;
end;

class function THash.GetHashType(const Index: integer): PHashType;
begin
  if HashTypes <> nil then
    Result := PHashType(HashTypes[Index])
  else
    Result := nil;
end;

class function THash.GetHashType(AHash: THashClass): PHashType;
var
  i: integer;
begin
  if HashTypes <> nil then begin
    for i := 0 to HashTypes.Count - 1 do begin
      Result := PHashType(HashTypes[i]);
      if Result^.HashClass = AHash then
        Exit;
    end;
    Result := nil;
  end else
    Result := nil;
end;

class function THash.GetHashType(AName: string): PHashType;
var
  i: integer;
begin
  if HashTypes <> nil then begin 
    for i := 0 to HashTypes.Count - 1 do begin
      Result := PHashType(HashTypes[i]);
      if CompareText(Result^.Name, AName) = 0 then
        Exit;
    end;
    Result := nil;
  end else
    Result := nil;
end;

initialization

finalization
  if HashTypes <> nil then
    FreeAndNil(HashTypes);
end.
