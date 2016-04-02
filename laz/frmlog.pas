unit frmlog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, ExtCtrls;

type
  { TLogForm }
  TLogForm = class(TForm)
    MenuItem1: TMenuItem;
    PopupMenu1: TPopupMenu;
    Splitter1: TSplitter;
    txtSent: TMemo;
    txtRecv: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure AddSent(const AString: string);
    procedure AddRecv(const AString: string);
  end;

var
  LogForm: TLogForm;

implementation

{$R *.lfm}

const
  MAX_LOGLINES = 300;

{ TLogForm }

procedure TLogForm.FormCreate(Sender: TObject);
begin
  {$IFDEF Linux}
    // Find a monospace font
    if Screen.Fonts.IndexOf('DejaVu Sans Mono') <> -1 then begin
      txtSent.Font.Name := 'DejaVu Sans Mono';
      txtRecv.Font.Name := 'DejaVu Sans Mono';
    end;
  {$ENDIF}
end;

procedure TLogForm.AddSent(const AString: string);
var
  sent, s, s2: string;
  sl: TStringList;
  i, p: integer;
begin
  sent := StringReplace(AString, '<EOL>', LineEnding, [rfReplaceAll]);
  sl := TStringList.Create;
  try
    sl.Text := sent;
    for i := sl.Count - 1 downto 1 do begin
      // Add new line between requests
      s := sl[i];
      p := Pos('POST /', s);
      if p = 0 then
        p := Pos('GET /', s);
      if p > 1 then begin
        s2 := Copy(s, p, MaxInt);
        Delete(s, p, MaxInt);
        sl[i] := s2;
        sl.Insert(i, s);
        sl.Insert(i+1, '');
      end;
    end;
    if sl.Count <> 0 then begin
      txtSent.Lines.AddStrings(sl);
      txtSent.Lines.Add('----------------------8<----------------------');
      while txtSent.Lines.Count > MAX_LOGLINES do
        txtSent.Lines.Delete(0);
    end;
  finally
    sl.Free;
  end;
end;

procedure TLogForm.AddRecv(const AString: string);
var
  recv, s, s2: string;
  sl: TStringList;
  i, p: integer;
begin
  recv := StringReplace(AString, '<EOL>', LineEnding, [rfReplaceAll]);
  sl := TStringList.Create;
  try
    sl.Text := recv;
    for i := sl.Count - 1 downto 1 do begin
      s := sl[i];
      p := Pos('HTTP/1.', s);
      if p > 1 then begin
        s2 := Copy(s, p, MaxInt);
        Delete(s, p, MaxInt);
        sl[i] := s2;
        sl.Insert(i, s);
        sl.Insert(i+1, '');
      end;
    end;
    if sl.Count <> 0 then begin
      txtRecv.Lines.AddStrings(sl);
      txtRecv.Lines.Add('----------------------8<----------------------');
      while txtRecv.Lines.Count > MAX_LOGLINES do
        txtRecv.Lines.Delete(0);
    end;
  finally
    sl.Free;
  end;
end;

procedure TLogForm.FormResize(Sender: TObject);
begin
  txtSent.Width := ClientWidth div 2;
end;

procedure TLogForm.MenuItem1Click(Sender: TObject);
begin
  txtSent.Lines.Clear;
  txtRecv.Lines.Clear;
end;

end.

