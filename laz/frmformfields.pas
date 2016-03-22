unit frmformfields;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type
  { TFormFieldsDialog }
  TFormFieldsDialog = class(TForm)
    Button1: TButton;
    Button2: TButton;
    txtFormFields: TMemo;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormFieldsDialog: TFormFieldsDialog;

implementation

uses
  frmmain;

{$R *.lfm}

{ TFormFieldsDialog }

procedure TFormFieldsDialog.FormCreate(Sender: TObject);
var
  c, i: integer;
begin
  txtFormFields.Lines.Clear;
  MainForm.IniPropStorage.IniSection := 'postfields';
  c := MainForm.IniPropStorage.ReadInteger('count', 0);
  for i := 0 to c - 1 do begin
    txtFormFields.Lines.Add(MainForm.IniPropStorage.ReadString(IntToStr(i), ''));
  end;
end;

end.

