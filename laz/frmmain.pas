unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IdBaseComponent, IdComponent, IdHTTP, uIndyClient,
  uOAuth2HttpClient, uOAuth2Client;

type
  { TMainForm }
  TMainForm = class(TForm)
    Button1: TButton;
    txtResponse: TMemo;
    txtResource: TEdit;
    Label9: TLabel;
    txtExpires: TEdit;
    Label7: TLabel;
    Label8: TLabel;
    txtAccessToken: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    txtRefreshToken: TEdit;
    txtClientSecret: TEdit;
    txtUser: TEdit;
    Label2: TLabel;
    txtSite: TEdit;
    Label1: TLabel;
    txtPass: TEdit;
    txtClientId: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure txtResourceExit(Sender: TObject);
    procedure txtSiteExit(Sender: TObject);
  private
    { private declarations }
    FClient: TIndyHttpClient;
    FOAuthClient: TOAuth2Client;
    FIdHttp: TIdHTTP;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  uOAuth2Tools, uJson;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FIdHttp := TIdHTTP.Create(Self);
  FClient := TIndyHttpClient.Create(FIdHttp);
  FOAuthClient := TOAuth2Client.Create(FClient);
end;

procedure TMainForm.txtResourceExit(Sender: TObject);
begin
  txtResource.Text := AddLeadingSlash(txtResource.Text);
end;

procedure TMainForm.txtSiteExit(Sender: TObject);
begin
  txtSite.Text := RemoveTrailingSlash(txtSite.Text);
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FOAuthClient.Free;
  FClient.Free;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  res: TOAuth2Response;
begin
  FOAuthClient.Site := txtSite.Text;
  FOAuthClient.GrantType := 'password';
  FOAuthClient.UserName := txtUser.Text;
  FOAuthClient.PassWord := txtPass.Text;
  FOAuthClient.ClientId := txtClientId.Text;
  FOAuthClient.ClientSecret := txtClientSecret.Text;
  res := FOAuthClient.GetResource(txtResource.Text);
  with TJson.Create do try
    Parse(res.Body);
    Print(txtResponse.Lines);
  finally
    Free;
  end;
  txtAccessToken.Text := FOAuthClient.AccessToken.AccessToken;
  txtRefreshToken.Text := FOAuthClient.AccessToken.RefreshToken;
  txtExpires.Text := IntToStr(FOAuthClient.AccessToken.ExpiresIn);
end;

end.

