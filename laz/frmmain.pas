unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IniPropStorage, IdBaseComponent, IdComponent, IdHTTP, uIndyClient,
  uOAuth2HttpClient, uOAuth2Client;

type
  { TMainForm }
  TMainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    IniPropStorage: TIniPropStorage;
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
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
  uOAuth2Tools, uJson, uOAuth2Consts, LCLIntf, frmformfields;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FIdHttp := TIdHTTP.Create(Self);
  FIdHttp.Request.UserAgent := 'Mozilla/3.0 (compatible; POAuth2)';
  FClient := TIndyHttpClient.Create(FIdHttp);
  FOAuthClient := TOAuth2Client.Create(FClient);
  IniPropStorage.IniFileName := GetAppConfigDir(false) + 'poat.ini';
  IniPropStorage.Restore;
  IniPropStorage.IniSection := 'general';
  txtSite.Text := IniPropStorage.ReadString('site', txtSite.Text);
  txtUser.Text := IniPropStorage.ReadString('user', txtUser.Text);
  txtPass.Text := IniPropStorage.ReadString('pass', txtPass.Text);
  txtClientId.Text := IniPropStorage.ReadString('client_id', txtClientId.Text);
  txtClientSecret.Text := IniPropStorage.ReadString('client_secret', txtClientSecret.Text);
  txtResource.Text := IniPropStorage.ReadString('resource', txtResource.Text);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
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
var
  i: integer;
begin
  IniPropStorage.EraseSections;
  IniPropStorage.IniSection := 'general';
  IniPropStorage.WriteString('site', txtSite.Text);
  IniPropStorage.WriteString('user', txtUser.Text);
  IniPropStorage.WriteString('pass', txtPass.Text);
  IniPropStorage.WriteString('client_id', txtClientId.Text);
  IniPropStorage.WriteString('client_secret', txtClientSecret.Text);
  IniPropStorage.WriteString('resource', txtResource.Text);
  IniPropStorage.IniSection := 'postfields';
  IniPropStorage.WriteString('count', IntToStr(FormFieldsDialog.txtFormFields.Lines.Count));
  for i := 0 to FormFieldsDialog.txtFormFields.Lines.Count - 1 do begin
    IniPropStorage.WriteString(IntToStr(i), FormFieldsDialog.txtFormFields.Lines[i]);
  end;
  IniPropStorage.Save;
  FOAuthClient.Free;
  FClient.Free;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  res: TOAuth2Response;
  start, stop: DWord;
begin
  FOAuthClient.Site := txtSite.Text;
  FOAuthClient.GrantType := gtPassword;
  FOAuthClient.UserName := txtUser.Text;
  FOAuthClient.PassWord := txtPass.Text;
  FOAuthClient.ClientId := txtClientId.Text;
  FOAuthClient.ClientSecret := txtClientSecret.Text;
  try
    start := LCLIntf.GetTickCount;
    res := FOAuthClient.Get(txtResource.Text);
    stop := LCLIntf.GetTickCount;
    txtTook.Text := IntToStr(stop - start);
    txtAccessToken.Text := FOAuthClient.AccessToken.AccessToken;
    txtRefreshToken.Text := FOAuthClient.AccessToken.RefreshToken;
    txtExpires.Text := IntToStr(FOAuthClient.AccessToken.ExpiresIn);
    txtResponse.Lines.Clear;
    if res.Code = HTTP_OK then begin
      if IsJson(res.ContentType) then begin
        with TJson.Create do try
          Parse(res.Body);
          Print(txtResponse.Lines);
        finally
          Free;
        end;
      end else begin
        txtResponse.Text := res.Body;
      end;
    end else begin
      txtResponse.Text := Format('Error (%d): %s', [res.Code, res.Body]);
    end;
  except
    on E: Exception do
      txtResponse.Text := Format('Error: %s', [E.Message]);
  end;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  res: TOAuth2Response;
  start, stop: DWord;
  ff: TStringList;
  i: integer;
  c: integer;
begin
  FOAuthClient.Site := txtSite.Text;
  FOAuthClient.GrantType := gtPassword;
  FOAuthClient.UserName := txtUser.Text;
  FOAuthClient.PassWord := txtPass.Text;
  FOAuthClient.ClientId := txtClientId.Text;
  FOAuthClient.ClientSecret := txtClientSecret.Text;
  if FormFieldsDialog.txtFormFields.Text = '' then begin
    IniPropStorage.IniSection := 'postfields';
    c := IniPropStorage.ReadInteger('count', 0);
    for i := 0 to c - 1 do begin
      FormFieldsDialog.txtFormFields.Lines.Add(IniPropStorage.ReadString(IntToStr(i), ''));
    end;
  end;
  if FormFieldsDialog.ShowModal = mrOK then begin
    ff := TStringList.Create;
    ff.AddStrings(FormFieldsDialog.txtFormFields.Lines);
    try
      try
        start := LCLIntf.GetTickCount;
        res := FOAuthClient.Post(txtResource.Text, ff);
        stop := LCLIntf.GetTickCount;
        txtTook.Text := IntToStr(stop - start);
        txtAccessToken.Text := FOAuthClient.AccessToken.AccessToken;
        txtRefreshToken.Text := FOAuthClient.AccessToken.RefreshToken;
        txtExpires.Text := IntToStr(FOAuthClient.AccessToken.ExpiresIn);
        txtResponse.Lines.Clear;
        if res.Code = HTTP_OK then begin
          if IsJson(res.ContentType) then begin
            with TJson.Create do try
              Parse(res.Body);
              Print(txtResponse.Lines);
            finally
              Free;
            end;
          end else begin
            txtResponse.Text := res.Body;
          end;
        end else begin
          txtResponse.Text := Format('Error (%d): %s', [res.Code, res.Body]);
        end;
      except
        on E: Exception do
          txtResponse.Text := Format('Error: %s', [E.Message]);
      end;
    finally
      ff.Free;
    end;
  end;
end;

end.

