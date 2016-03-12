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
    Button1: TButton;
    txtResource: TEdit;
    Label5: TLabel;
    Button2: TButton;
    txtResponse: TMemo;
    Label6: TLabel;
    txtSite: TEdit;
    Label7: TLabel;
    Edit5: TEdit;
    Label8: TLabel;
    Edit6: TEdit;
    Label9: TLabel;
    Edit7: TEdit;
    IdHTTP1: TIdHTTP;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    est1: TMenuItem;
    JSON1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure JSON1Click(Sender: TObject);
  private
    { Private-Deklarationen }
    FClient: TIndyHttpClient;
  public
    { Public-Deklarationen }
  end;

var
  MainForm: TMainForm;

implementation

uses
	frmJson;

{$R *.dfm}

procedure TMainForm.Button2Click(Sender: TObject);
var
  client: TOAuth2Client;
  res: TOAuth2Response;
begin
  client := TOAuth2Client.Create(FClient);
  try
    client.Site := txtSite.Text;
    client.GrantType := 'password';
    client.UserName := txtUser.Text;
    client.PassWord := txtPass.Text;
    client.ClientId := txtClientId.Text;
    client.ClientSecret := txtClientSecret.Text;
    res := client.GetReosurce('/resource?name=value');
  finally
    client.Free;
  end;
end;

procedure TMainForm.Exit1Click(Sender: TObject);
begin
	Close;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FClient.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FClient := TIndyHttpClient.Create(IdHTTP1);
end;

procedure TMainForm.JSON1Click(Sender: TObject);
begin
  JsonForm.Show;
end;

end.

