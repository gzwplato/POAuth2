{
  Simple OAuth2 client

  (C) 2016, Stefan Ascher
}

unit frmJson;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TJsonForm = class(TForm)
    txtJson: TMemo;
    txtResult: TMemo;
    Splitter1: TSplitter;
    Panel1: TPanel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  JsonForm: TJsonForm;

implementation

uses
  uJson;

{$R *.dfm}

procedure TJsonForm.Button1Click(Sender: TObject);
var
  j: integer;
begin
  with TJson.Create do try
    Parse(txtJson.Text);
    txtResult.Lines.Clear;
    Print(txtResult.Lines);
    if Length(Output.Errors) > 0 then begin
      txtResult.Lines.Add('Errors:');
      for J := 0 to Length(Output.Errors) - 1 do
        txtResult.Lines.Add(Output.Errors[J]);
    end;

  finally
    Free;
  end;
end;

end.
