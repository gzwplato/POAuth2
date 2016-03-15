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
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormFieldsDialog: TFormFieldsDialog;

implementation

{$R *.lfm}

end.

