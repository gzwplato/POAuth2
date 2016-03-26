unit frmhistory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Types,
  LCLType;

type
  THistoryItem = class
  public
    Url: string;
    Fields: TStringList;
    constructor Create(const AUrl, AFields: string);
    destructor Destroy; override;
    function GetText: string;
  end;

  { THistoryForm }
  THistoryForm = class(TForm)
    lstHistory: TListBox;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure lstHistoryClick(Sender: TObject);
    procedure lstHistoryDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure lstHistoryMeasureItem(Control: TWinControl; Index: Integer;
      var AHeight: Integer);
  private
    { private declarations }
    FHistory: TList;
    FOnSelect: TNotifyEvent;
  public
    { public declarations }
    procedure Add(const AUrl, AFields: string);
    function Get(const Index: integer): THistoryItem;
    function GetSelected: THistoryItem;
  published
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  end;

var
  HistoryForm: THistoryForm;

implementation

uses
  frmmain;

{$R *.lfm}

const
  MAX_HISTORY = 30;

{ THistoryItem }

constructor THistoryItem.Create(const AUrl, AFields: string);
begin
  inherited Create;
  Url := AUrl;
  Fields := TStringList.Create;
  Fields.Delimiter := '&';
  Fields.DelimitedText := AFields;
end;

destructor THistoryItem.Destroy;
begin
  Fields.Free;
  inherited;
end;

function THistoryItem.GetText: string;
begin
  if Fields.Text <> '' then begin
    if Pos('?', Url) <> 0 then
      Result := Url + '&' + Fields.DelimitedText
    else
      Result := Url + '?' + Fields.DelimitedText;
  end else
    Result := Url;
end;

{ THistoryForm }

procedure THistoryForm.FormCreate(Sender: TObject);
var
  i, c, p: integer;
  url, fields, s: string;
begin
  FHistory := TList.Create;
  MainForm.IniPropStorage.IniSection := 'history';
  c := MainForm.IniPropStorage.ReadInteger('count', 0);
  for i := 0 to c - 1 do begin
    s := MainForm.IniPropStorage.ReadString(IntToStr(i), '');
    if s <> '' then begin
      p := Pos('|', s);
      if p <> 0 then begin
        url := Copy(s, 1, p - 1);
        fields := Copy(s, p + 1, MaxInt);
      end else begin
        url := s;
        fields := '';
      end;
      Add(url, fields);
    end;
  end;
  lstHistory.Canvas.Font.Assign(lstHistory.Font);
end;

procedure THistoryForm.lstHistoryClick(Sender: TObject);
begin
  if Assigned(FOnSelect) then
    FOnSelect(Self);
end;

procedure THistoryForm.lstHistoryDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  hi: THistoryItem;
  y: integer;
begin
  hi := THistoryItem(FHistory[Index]);
  if (odSelected in State) then begin
    lstHistory.Canvas.Brush.Color := clHighlight;
  end else begin
    lstHistory.Canvas.Brush.Color := lstHistory.Color;
  end;
  lstHistory.Canvas.FillRect(ARect);
  lstHistory.Canvas.Font.Color := clWindowText;
  lstHistory.Canvas.TextOut(ARect.Left + 3, ARect.Top, hi.Url);
  y := ARect.Top;
  Inc(y, lstHistory.Canvas.TextHeight('^_'));
  lstHistory.Canvas.Font.Color := clGrayText;
  lstHistory.Canvas.TextOut(ARect.Left + 20, y, hi.Fields.DelimitedText);
end;

procedure THistoryForm.lstHistoryMeasureItem(Control: TWinControl;
  Index: Integer; var AHeight: Integer);
begin
  AHeight := lstHistory.Canvas.TextHeight('^_') * 2;
end;

procedure THistoryForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i, c: integer;
  s: string;
  hi: THistoryItem;
begin
  MainForm.IniPropStorage.IniSection := 'history';
  c := 0;
  for i := 0 to FHistory.Count - 1 do begin
    hi := THistoryItem(FHistory[i]);
    s := hi.Url;
    if hi.Fields.DelimitedText <> '' then
      s := s + '|' + hi.Fields.DelimitedText;
    MainForm.IniPropStorage.WriteString(IntToStr(i), s);
    Inc(c);
    if c >= MAX_HISTORY then
      Break;
  end;
  MainForm.IniPropStorage.WriteString('count', IntToStr(c));

  for i := 0 to FHistory.Count - 1 do begin
    hi := THistoryItem(FHistory[i]);
    hi.Free;
  end;
  FHistory.Free;
end;

procedure THistoryForm.Add(const AUrl, AFields: string);
var
  i: integer;
  s: string;
  hi: THistoryItem;
begin
  if AFields <> '' then begin
    if Pos('?', AUrl) <> 0 then
      s := Format('%s&%s', [AUrl, AFields])
    else
      s := Format('%s?%s', [AUrl, AFields])
  end else
    s := AUrl;
  for i := 0 to FHistory.Count - 1 do begin
    hi := THistoryItem(FHistory[i]);
    if hi.GetText = s then begin
      FHistory.Move(i, 0);
      lstHistory.Items.Move(i, 0);
      Exit;
    end;
  end;
  hi := THistoryItem.Create(AUrl, AFields);
  FHistory.Insert(0, hi);
  lstHistory.Items.Insert(0, hi.Url);
  lstHistory.ItemIndex := 0;
end;

function THistoryForm.GetSelected: THistoryItem;
begin
  if lstHistory.ItemIndex <> -1 then
    Result := Get(lstHistory.ItemIndex)
  else
    Result := nil;
end;

function THistoryForm.Get(const Index: integer): THistoryItem;
begin
  if (Index > -1) and (Index < FHistory.Count) then
    Result := THistoryItem(FHistory[Index])
  else
    Result := nil;
end;

end.

