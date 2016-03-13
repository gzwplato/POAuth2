{
	Simple OAuth2 client
}

unit uJson;

interface

{
	JSON parser
  https://github.com/koldev/JsonParser
}

uses
  SysUtils, Classes;

type
  TJsonNumber = Double;
  TJsonString = WideString;
  TJsonChar = WideChar;
  TJsonWord = (JWUnknown, JWTrue, JWFalse, JWNull);
  TJsonValueKind = (JVKUnknown, JVKNumber, JVKString, JVKWord, JVKArray, JVKObject);
  TJsonValue = record
    Kind: TJsonValueKind;
    Index: Integer;
  end;
  TJsonArray = array of TJsonValue;
  TJsonPair = record
    Key: TJsonString;
    Value: TJsonValue;
  end;
  TJsonObject = array of TJsonPair;
  TJsonParserOutput = record
    Numbers: array of TJsonNumber;
    Strings: array of TJsonString;
    Words: array of TJsonWord;
    Arrays: array of TJsonArray;
    Objects: array of TJsonObject; // The root object is the first one
    Errors: array of TJsonString;
  end;
  TJsonParser = record
    At: Integer; // The index of the current character
    Ch: TJsonChar; // The current character
    Text: TJsonString;
    Output: TJsonParserOutput;
  end;

  TJson = class;

  TJsonValueParser = function: TJsonValue of object;

  TJson = class
  private
    At: Integer; // The index of the current character
    Ch: TJsonChar; // The current character
    Text: TJsonString;
    FOutput: TJsonParserOutput;
    procedure Error(Msg: TJsonString);
    function Next(C: TJsonChar): TJsonChar;
    function Number: Double;
    function String_: TJsonString;
    procedure White;
    function Word_: TJsonWord;
    function Array_: TJsonArray;
    function Object_: TJsonObject;
    function Value: TJsonValue;
    procedure PrintObject(Index, Indent: Integer; Lines: TStrings; CommaAfter: TJsonString);
    procedure PrintArray(Index, Indent: Integer; Lines: TStrings; CommaAfter: TJsonString);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const AStr: string);
    procedure Clear;
    procedure Print(Lines: TStrings);

    function GetValue(const Key: string): TJsonValue;

    property Output: TJsonParserOutput read FOutput;
  end;

implementation

function IndentString(Indent: Integer): TJsonString;
var
  I: Integer;
begin
  for I := 1 to 4 * Indent do
    Result := Result + ' ';
end;

{ TJson }

constructor TJson.Create;
begin
  inherited;
end;

destructor TJson.Destroy;
begin
  inherited;
end;

procedure TJson.Error(Msg: TJsonString);
var
  ErrorMsg: TJsonString;
  N: Integer;
begin
  ErrorMsg := Format('Error: "%s". Position: %d. Text: "%s"', [Msg, At, Text]);
  N := Length(FOutput.Errors);
  SetLength(FOutput.Errors, N + 1);
  FOutput.Errors[N] := ErrorMsg;
end;

function TJson.Next(C: TJsonChar): TJsonChar;
begin
  Result := #0;
  // If a non-#0 C parameter is provided, verify that it matches the current character.
  if (C <> #0) and (C <> Ch) then
  begin
    Error('Expected "' + C + '" instead of "' + Ch + '"');
    Exit;
  end;
  // Get the next character. When there are no more characters, return #0.
  if At > Length(Text) then
  begin
    Ch := #0;
    Exit;
  end;
  Ch := Text[At];
  Inc(At);
  Result := Ch;
end;

// Parse a number value.
function TJson.Number: Double;
var
  S: WideString;
begin
  Result := 0;
  S := '';
  if Ch = '-' then
  begin
    S := '-';
    Next('-');
  end;
  while (Ch >= '0') and (Ch <= '9') do
  begin
    S := S + Ch;
    Next(#0);
  end;
  if Ch = '.' then
  begin
    S := S + '.';
    while (Next(#0) <> #0) and (Ch >= '0') and (Ch <= '9') do
      S := S + Ch;
  end;
  if (Ch = 'e') or (Ch = 'E') then
  begin
    S := S + Ch;
    Next(#0);
    if (Ch = '-') or (Ch = '+') then
    begin
      S := S + Ch;
      Next(#0);
    end;
    while (Ch >= '0') and (Ch <= '9') do
    begin
      S := S + Ch;
      Next(#0);
    end;
  end;
  if S = '' then
    Error('Bad number')
  else
    Result := StrToFloat(S);
end;

// Parse a string value.
function TJson.String_: TJsonString;
var
  HexDigit, HexValue: Integer;
  I: Integer;
  SpecChar: TJsonChar;
begin
  Result := '';
  // When parsing for string values, we must look for " and \ characters.
  if Ch = '"' then
  begin
    while Next(#0) <> #0 do
    begin
      if Ch = '"' then
      begin
        Next(#0);
        Exit;
      end;
      if Ch = '\' then
      begin
        Next(#0);
        if Ch = 'u' then
        begin
          HexValue := 0;
          for I := 1 to 4 do
          begin
            HexDigit := StrToInt('0x' + Next(#0));
            HexValue := HexValue * 16 + HexDigit;
          end;
          Result := Result + Chr(HexValue);
        end
        else
        begin
          case Ch of
            '"': SpecChar := '"';
            '\': SpecChar := '\';
            '/': SpecChar := '/';
            'b': SpecChar := #8;
            'f': SpecChar := #12;
            'n': SpecChar := #10;
            'r': SpecChar := #13;
            't': SpecChar := #9;
          else
            Break;
          end;
        end;
      end
      else
        Result := Result + Ch;
    end;
  end;
  Error('Bad string');
end;

// Skip whitespace.
procedure TJson.White;
begin
  while (Ch <> #0) and (Ch <= ' ') do
    Next(#0);
end;

// true, false, or null.
function TJson.Word_: TJsonWord;
begin
  Result := JWUnknown;
  case Ch of
    't':
    begin
      Next('t');
      Next('r');
      Next('u');
      Next('e');
      Result := JWTrue;
      Exit;
    end;
    'f':
    begin
      Next('f');
      Next('a');
      Next('l');
      Next('s');
      Next('e');
      Result := JWFalse;
      Exit;
    end;
    'n':
    begin
      Next('n');
      Next('u');
      Next('l');
      Next('l');
      Result := JWNull;
      Exit;
    end;
  end;
  Error('Unexpected "' + Ch + '"');
end;

// Parse an array value.
function TJson.Array_: TJsonArray;
var
  N: Integer;
begin
  SetLength(Result, 0); // Empty array
  N := 0;
  if Ch = '[' then
  begin
    Next('[');
    White;
    if Ch = ']' then
    begin
      Next(']');
      Exit; // Return empty array
    end;
    while Ch <> #0 do
    begin
      Inc(N);
      SetLength(Result, N);
      Result[N - 1] := Value;
      White;
      if Ch = ']' then
      begin
        Next(']');
        Exit;
      end;
      Next(',');
      White;
    end;
  end;
  Error('Bad array');
end;

// Parse an object value.
function TJson.Object_: TJsonObject;
var
  Key: TJsonString;
  I, N: Integer;
begin
  SetLength(Result, 0); // Empty object
  N := 0;
  if Ch = '{' then
  begin
    Next('{');
    White;
    if Ch = '}' then
    begin
      Next('}');
      Exit; // Return empty object
    end;
    while Ch <> #0 do
    begin
      Key := String_;
      White;
      Next(':');
      for I := 0 to N - 1 do
      begin
        if Key = Result[I].Key then
          Error('Duplicate key "' + Key + '"');
      end;
      Inc(N);
      SetLength(Result, N);
      Result[N - 1].Key := Key;
      Result[N - 1].Value := Value;
      White;
      if Ch = '}' then
      begin
        Next('}');
        Exit;
      end;
      Next(',');
      White;
    end;
  end;
  Error('Bad object');
end;

// Parse a JSON value. It could be a number, a string, a word, an array, or an object.
function TJson.Value: TJsonValue;
var
  N: Integer;
begin
  Result.Kind := JVKUnknown;
  Result.Index := -1;
  White;
  case Ch of
    '-', '0'..'9':
    begin
      N := Length(FOutput.Numbers);
      SetLength(FOutput.Numbers, N + 1);
      FOutput.Numbers[N] := Number;
      Result.Kind := JVKNumber;
      Result.Index := N;
    end;
    '"':
    begin
      N := Length(FOutput.Strings);
      SetLength(FOutput.Strings, N + 1);
      FOutput.Strings[N] := String_;
      Result.Kind := JVKString;
      Result.Index := N;
    end;
    't', 'f', 'n':
    begin
      N := Length(FOutput.Words);
      SetLength(FOutput.Words, N + 1);
      FOutput.Words[N] := Word_;
      Result.Kind := JVKWord;
      Result.Index := N;
    end;
    '[':
    begin
      N := Length(FOutput.Arrays);
      SetLength(FOutput.Arrays, N + 1);
      FOutput.Arrays[N] := Array_;
      Result.Kind := JVKArray;
      Result.Index := N;
    end;
    '{':
    begin
      N := Length(FOutput.Objects);
      SetLength(FOutput.Objects, N + 1);
      FOutput.Objects[N] := Object_;
      Result.Kind := JVKObject;
      Result.Index := N;
    end;
  else
    Error('Bad JSON value');
  end;
end;

procedure TJson.Parse(const AStr: string);
var 
  old_ds, old_ts: Char;
begin
  old_ds := FormatSettings.DecimalSeparator;
  old_ts := FormatSettings.ThousandSeparator;
	FormatSettings.DecimalSeparator := '.';
  FormatSettings.ThousandSeparator := ',';
  
  if AStr = '' then
    Exit;
  At := 1;
  Ch := ' ';
  Text := AStr;
  Value;
  White;
  if Ch <> #0 then
    Error('Syntax error');
    
	FormatSettings.DecimalSeparator := old_ds;
  FormatSettings.ThousandSeparator := old_ts;
end;

procedure TJson.Clear;
begin
  At := 0;
  Ch := #0;
  Text := '';
  SetLength(FOutput.Numbers, 0);
  SetLength(FOutput.Strings, 0);
  SetLength(FOutput.Words, 0);
  SetLength(FOutput.Arrays, 0);
  SetLength(FOutput.Objects, 0);
  SetLength(FOutput.Errors, 0);
end;

procedure TJson.PrintObject(Index, Indent: Integer; Lines: TStrings; CommaAfter: TJsonString);
var
  IS0, IS1: TJsonString;
  I: Integer;
  K: TJsonString;
  V: TJsonValue;
  S, Comma: TJsonString;
begin
  IS0 := IndentString(Indent);
  IS1 := IndentString(Indent + 1);
  Lines.Add(IS0 + '{');
  for I := 0 to Length(FOutput.Objects[Index]) - 1 do
  begin
    if I < Length(FOutput.Objects[Index]) - 1 then
      Comma := ','
    else
      Comma := '';
    K := '"' + FOutput.Objects[Index][I].Key + '"';
    V := FOutput.Objects[Index][I].Value;
    case V.Kind of
      JVKUnknown: Lines.Add(IS1 + K + ': ?kind?' + Comma);
      JVKNumber: Lines.Add(Format('%s: %g' + Comma, [IS1 + K, Output.Numbers[V.Index]]));
      JVKString: Lines.Add(IS1 + K + ': "' + FOutput.Strings[V.Index] + '"' + Comma);
      JVKWord:
      begin
        case FOutput.Words[V.Index] of
          JWUnknown: S := '?word?';
          JWTrue: S := 'true';
          JWFalse: S := 'false';
          JWNull: S := 'null';
        end;
        Lines.Add(IS1 + K + ': ' + S + Comma);
      end;
      JVKArray:
      begin
        Lines.Add(IS1 + K + ':');
        PrintArray(V.Index, Indent + 1, Lines, Comma);
      end;
      JVKObject:
      begin
        Lines.Add(IS1 + K + ':');
        PrintObject(V.Index, Indent + 1, Lines, Comma);
      end;
    end;
  end;
  Lines.Add(IS0 + '}' + CommaAfter);
end;

procedure TJson.PrintArray(Index, Indent: Integer; Lines: TStrings; CommaAfter: TJsonString);
var
  IS0, IS1: TJsonString;
  I: Integer;
  V: TJsonValue;
  S, Comma: TJsonString;
begin
  IS0 := IndentString(Indent);
  IS1 := IndentString(Indent + 1);
  Lines.Add(IS0 + '[');
  for I := 0 to Length(FOutput.Arrays[Index]) - 1 do
  begin
    if I < Length(FOutput.Arrays[Index]) - 1 then
      Comma := ','
    else
      Comma := '';
    V := FOutput.Arrays[Index][I];
    case V.Kind of
      JVKUnknown: Lines.Add(IS1 + '?kind?' + Comma);
      JVKNumber: Lines.Add(Format('%s%g' + Comma, [IS1, FOutput.Numbers[V.Index]]));
      JVKString: Lines.Add(IS1 + '"' + FOutput.Strings[V.Index] + '"' + Comma);
      JVKWord:
      begin
        case FOutput.Words[V.Index] of
          JWUnknown: S := '?word?';
          JWTrue: S := 'true';
          JWFalse: S := 'false';
          JWNull: S := 'null';
        end;
        Lines.Add(IS1 + S + Comma);
      end;
      JVKArray: PrintArray(V.Index, Indent + 1, Lines, Comma);
      JVKObject: PrintObject(V.Index, Indent + 1, Lines, Comma);
    end;
  end;
  Lines.Add(IS0 + ']' + CommaAfter);
end;

procedure TJson.Print(Lines: TStrings);
begin
  PrintObject(0, 0, Lines, '');
end;

function TJson.GetValue(const Key: string): TJsonValue;
var
  i: integer;
  O: TJsonPair;
begin
  for i := 0 to Length(FOutput.Objects[0]) do begin
    O := FOutput.Objects[0][i];
    if O.Key = Key then begin
      Result := O.Value;
      Exit;
    end;
  end;
  Result.Kind := JVKUnknown;
  Result.Index := -1;
end;

end.
