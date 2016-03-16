{
  Simple OAuth2 client

  (C) 2016, Stefan Ascher
}

unit frmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, uIndyClient, uOAuth2HttpClient,
  uOAuth2Client, Vcl.Menus;

type
  TMainForm = class(TForm)
    txtUser: TEdit;
    txtPass: TEdit;
    txtClientId: TEdit;
    txtClientSecret: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    txtResource: TEdit;
    Label5: TLabel;
    Button2: TButton;
    txtResponse: TMemo;
    Label6: TLabel;
    txtSite: TEdit;
    Label7: TLabel;
    txtAccessToken: TEdit;
    Label8: TLabel;
    txtRefreshToken: TEdit;
    Label9: TLabel;
    txtExpires: TEdit;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    est1: TMenuItem;
    JSON1: TMenuItem;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure JSON1Click(Sender: TObject);
    procedure txtSiteExit(Sender: TObject);
    procedure txtResourceExit(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
    FClient: TIndyHttpClient;
    FOAuthClient: TOAuth2Client;
    FIdHttp: TIdHTTP;
  public
    { Public-Deklarationen }
  end;

var
  MainForm: TMainForm;

implementation

uses
  frmJson, uOAuth2Tools, uJson, uOAuth2Consts;

{$R *.dfm}

procedure TMainForm.Button1Click(Sender: TObject);
var
  res: TOAuth2Response;
  formfields: TStringList;
begin
  FOAuthClient.Site := txtSite.Text;
  FOAuthClient.GrantType := 'password';
  FOAuthClient.UserName := txtUser.Text;
  FOAuthClient.PassWord := txtPass.Text;
  FOAuthClient.ClientId := txtClientId.Text;
  FOAuthClient.ClientSecret := txtClientSecret.Text;
  formfields := TStringList.Create;
  try
    formfields.NameValueSeparator := '=';
    formfields.Add(Format('%s=%s', ['testkey', 'testvalue']));
    try
      res := FOAuthClient.Post(txtResource.Text, formfields);
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
  finally
    formfields.Free;
  end;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  res: TOAuth2Response;
begin
  FOAuthClient.Site := txtSite.Text;
  FOAuthClient.GrantType := 'password';
  FOAuthClient.UserName := txtUser.Text;
  FOAuthClient.PassWord := txtPass.Text;
  FOAuthClient.ClientId := txtClientId.Text;
  FOAuthClient.ClientSecret := txtClientSecret.Text;
  try
    res := FOAuthClient.Get(txtResource.Text);
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

procedure TMainForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FOAuthClient.Free;
  FClient.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FIdHttp := TIdHTTP.Create(Self);
  FIdHttp.Request.UserAgent := 'Mozilla/3.0 (compatible; POAuth2)';
  FClient := TIndyHttpClient.Create(FIdHttp);
  FOAuthClient := TOAuth2Client.Create(FClient);
end;

procedure TMainForm.JSON1Click(Sender: TObject);
begin
  JsonForm.Show;
end;

procedure TMainForm.txtResourceExit(Sender: TObject);
begin
  txtResource.Text := AddLeadingSlash(txtResource.Text);
end;

procedure TMainForm.txtSiteExit(Sender: TObject);
begin
  txtSite.Text := RemoveTrailingSlash(txtSite.Text);
end;

end.

