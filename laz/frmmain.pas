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
    Label10: TLabel;
    txtTook: TEdit;
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
  uOAuth2Tools, uJson, uOAuth2Consts, LCLIntf;

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
  start, stop: DWord;
begin
  FOAuthClient.Site := txtSite.Text;
  FOAuthClient.GrantType := 'password';
  FOAuthClient.UserName := txtUser.Text;
  FOAuthClient.PassWord := txtPass.Text;
  FOAuthClient.ClientId := txtClientId.Text;
  FOAuthClient.ClientSecret := txtClientSecret.Text;
  try
    start := LCLIntf.GetTickCount;
    res := FOAuthClient.GetResource(txtResource.Text);
    stop := LCLIntf.GetTickCount;
    txtTook.Text := FloatToStr(stop - start);
    txtAccessToken.Text := FOAuthClient.AccessToken.AccessToken;
    txtRefreshToken.Text := FOAuthClient.AccessToken.RefreshToken;
    txtExpires.Text := IntToStr(FOAuthClient.AccessToken.ExpiresIn);
    txtResponse.Lines.Clear;
    if res.Code = HTTP_OK then begin
      with TJson.Create do try
        Parse(res.Body);
        Print(txtResponse.Lines);
      finally
        Free;
      end;
    end else begin
      txtResponse.Text := Format('Error (%d): %s', [res.Code, res.Body]);
    end;
  except
    on E: Exception do
      txtResponse.Text := Format('Error: %s', [E.Message]);
  end;
end;

end.

