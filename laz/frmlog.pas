unit frmlog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus;

type

  { TLogForm }

  TLogForm = class(TForm)
    MenuItem1: TMenuItem;
    PopupMenu1: TPopupMenu;
    txtLog: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  LogForm: TLogForm;

implementation

{$R *.lfm}

{ TLogForm }

procedure TLogForm.FormCreate(Sender: TObject);
begin
  {$IFDEF Linux}
    // Find a monospace font
    if Screen.Fonts.IndexOf('DejaVu Sans Mono') <> -1 then begin
      txtLog.Font.Name := 'DejaVu Sans Mono'
    end;
  {$ENDIF}
end;

procedure TLogForm.MenuItem1Click(Sender: TObject);
begin
  txtLog.Lines.Clear;
end;

end.

