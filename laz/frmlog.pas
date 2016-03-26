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
      txtSent.Font.Name := 'DejaVu Sans Mono';
      txtRecv.Font.Name := 'DejaVu Sans Mono';
    end;
  {$ENDIF}
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

