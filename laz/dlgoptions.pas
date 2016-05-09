unit dlgoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type
  { TOptionsDialog }
  TOptionsDialog = class(TForm)
    Button1: TButton;
    Button2: TButton;
    txtUsername: TEdit;
    txtPassword: TEdit;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    txtAccessTokenEndpoint: TEdit;
    Label1: TLabel;
    Panel1: TPanel;
    procedure txtAccessTokenEndpointExit(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  OptionsDialog: TOptionsDialog;

implementation

uses
  uOAuth2Tools;

{$R *.lfm}

{ TOptionsDialog }

procedure TOptionsDialog.txtAccessTokenEndpointExit(Sender: TObject);
begin
  if Pos('://', txtAccessTokenEndpoint.Text) = 0 then
    txtAccessTokenEndpoint.Text := AddLeadingSlash(txtAccessTokenEndpoint.Text);
end;

end.

