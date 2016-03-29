unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IniPropStorage, ComCtrls, ExtCtrls, Menus, IdBaseComponent, IdComponent,
  IdHTTP, uIndyClient, uOAuth2HttpClient, uOAuth2Client, IdLogStream, frmhistory;

type
  { TMainForm }
  TMainForm = class(TForm)
    btnGet: TButton;
    btnPost: TButton;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    txtResource: TEdit;
    IniPropStorage: TIniPropStorage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    mnuMain: TMainMenu;
    pnlForm: TPanel;
    pnlClient: TPanel;
    pnlTop: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar: TStatusBar;
    txtAccessToken: TEdit;
    txtClientId: TEdit;
    txtClientSecret: TEdit;
    txtExpires: TEdit;
    txtFormFields: TMemo;
    txtPass: TEdit;
    txtRefreshToken: TEdit;
    txtResponse: TMemo;
    txtSite: TEdit;
    txtTook: TEdit;
    txtUser: TEdit;
    procedure btnGetClick(Sender: TObject);
    procedure btnPostClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure txtFormFieldsExit(Sender: TObject);
    procedure txtResourceExit(Sender: TObject);
    procedure txtSiteExit(Sender: TObject);
  private
    { private declarations }
    FClient: TIndyHttpClient;
    FOAuthClient: TOAuth2Client;
    FIdHttp: TIdHTTP;
    FSendStream: TMemoryStream;
    FReceiveStream: TMemoryStream;
    FIdlog: TIdLogStream;
    FHistoryForm: THistoryForm;
    procedure AddHistory;
    function GetFormFields: string;
    procedure ReadStreams;
    procedure History_Select(Sender: TObject);
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  uOAuth2Tools, uJson, uOAuth2Consts, LCLIntf, frmlog, dlgoptions, uOAuth2Config,
  frmhash;

{$R *.lfm}

const
  MAX_LOGLINES = 300;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  i, c: integer;
  s: string;
  cfg: TOAuth2Config;
begin
  Constraints.MinWidth := Width;
  pnlclient.Constraints.MinWidth := pnlClient.Width;
  pnlTop.Constraints.MinHeight := pnlTop.Height;
  FIdHttp := TIdHTTP.Create(Self);
  FIdHttp.Request.UserAgent := 'Mozilla/3.0 (compatible; POAuth2)';
  FClient := TIndyHttpClient.Create(FIdHttp);
  FSendStream := TMemoryStream.Create;
  FReceiveStream := TMemoryStream.Create;
  FIdLog := TIdLogStream.Create(nil);
  FIdLog.SendStream := FSendStream;
  FIdLog.ReceiveStream := FReceiveStream;
  FIdLog.FreeStreams := false;
  FIdHttp.Intercept := FIdLog;
  FIdLog.Active := true;

  IniPropStorage.IniFileName := GetAppConfigDir(false) + 'poat.ini';
  StatusBar.SimpleText := 'Settings stored in: ' + IniPropStorage.IniFileName;
  IniPropStorage.Restore;
  IniPropStorage.IniSection := 'general';
  txtSite.Text := IniPropStorage.ReadString('site', txtSite.Text);
  txtUser.Text := IniPropStorage.ReadString('user', txtUser.Text);
  txtPass.Text := IniPropStorage.ReadString('pass', txtPass.Text);
  txtClientId.Text := IniPropStorage.ReadString('client_id', txtClientId.Text);
  txtClientSecret.Text := IniPropStorage.ReadString('client_secret', txtClientSecret.Text);
  txtResource.Text := IniPropStorage.ReadString('resource', txtResource.Text);

  IniPropStorage.IniSection := 'options';
  cfg.TokenEndPoint := IniPropStorage.ReadString('at_endpoint', DEF_OATUH2_CONFIG.TokenEndPoint);
  FOAuthClient := TOAuth2Client.Create(cfg, FClient);
  FClient.Username := IniPropStorage.ReadString('ba_username', '');
  FClient.Password := IniPropStorage.ReadString('ba_password', '');

  txtFormFields.Lines.Clear;
  IniPropStorage.IniSection := 'postfields';
  c := IniPropStorage.ReadInteger('count', 0);
  for i := 0 to c - 1 do begin
    s := IniPropStorage.ReadString(IntToStr(i), '');
    if s <> '' then
      txtFormFields.Lines.Add(s);
  end;

  FHistoryForm := THistoryForm.Create(Self);
  FHistoryForm.OnSelect := @History_Select;

{$IFDEF Linux}
  // Find a monospace font
  if Screen.Fonts.IndexOf('DejaVu Sans Mono') <> -1 then begin
    txtResponse.Font.Name := 'DejaVu Sans Mono'
  end;
{$ENDIF}
  Application.Title := Caption;
end;

procedure TMainForm.History_Select(Sender: TObject);
var
  hi: THistoryItem;
begin
  hi := FHistoryForm.GetSelected;
  if hi <> nil then begin
    txtResource.Text := hi.Url;
    txtFormFields.Lines.Clear;
    txtFormFields.Lines.AddStrings(hi.Fields);
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  l: integer;
begin
  l := Left - FHistoryForm.Width - 20;
  if l < 0 then begin
    l := Left + Width + 20;
  end;
  FhistoryForm.Left := l;
  FHistoryForm.Top := Top;
  FHistoryForm.Height := Height;
  FHistoryForm.Show;
end;

procedure TMainForm.MenuItem2Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.MenuItem4Click(Sender: TObject);
begin
  LogForm.Show;
end;

procedure TMainForm.MenuItem5Click(Sender: TObject);
begin
  FHistoryForm.Show;
end;

procedure TMainForm.MenuItem7Click(Sender: TObject);
var
  cfg: TOAuth2Config;
begin
  with TOptionsDialog.Create(Self) do try
    cfg := FOAuthClient.Config;
    txtAccessTokenEndpoint.Text := cfg.TokenEndPoint;
    txtUsername.Text := FClient.Username;
    txtPassword.Text := FClient.Password;
    if ShowModal = mrOK then begin
      cfg.TokenEndPoint := txtAccessTokenEndpoint.Text;
      FOAuthClient.Config := cfg;
      FClient.Username := txtUsername.Text;
      FClient.Password := txtPassword.Text;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.MenuItem8Click(Sender: TObject);
begin
  HashForm.Show;
end;

procedure TMainForm.txtFormFieldsExit(Sender: TObject);
var
  i, c: integer;
begin
  c := txtFormFields.Lines.Count;
  for i := c - 1 downto 0 do begin
    if Trim(txtFormFields.Lines[i]) = '' then
      txtFormFields.Lines.Delete(i);
  end;
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
  i, c: integer;
  s: string;
begin
  IniPropStorage.EraseSections;
  IniPropStorage.IniSection := 'general';
  IniPropStorage.WriteString('site', txtSite.Text);
  IniPropStorage.WriteString('user', txtUser.Text);
  IniPropStorage.WriteString('pass', txtPass.Text);
  IniPropStorage.WriteString('client_id', txtClientId.Text);
  IniPropStorage.WriteString('client_secret', txtClientSecret.Text);
  IniPropStorage.WriteString('resource', txtResource.Text);

  IniPropStorage.IniSection := 'options';
  IniPropStorage.WriteString('at_endpoint', FOAuthClient.Config.TokenEndPoint);
  IniPropStorage.WriteString('ba_username', FClient.Username);
  IniPropStorage.WriteString('ba_password', FClient.Password);

  IniPropStorage.IniSection := 'postfields';
  c := 0;
  for i := 0 to txtFormFields.Lines.Count - 1 do begin
    s := txtFormFields.Lines[i];
    if s <> '' then begin
      IniPropStorage.WriteString(IntToStr(i), s);
      Inc(c);
    end;
  end;
  IniPropStorage.WriteString('count', IntToStr(c));

  FHistoryForm.Close;
  FHistoryForm.Free;
  IniPropStorage.Save;
  FOAuthClient.Free;
  FClient.Free;
  FSendStream.Free;
  FReceiveStream.Free;
  FIdlog.Free;
end;

procedure TMainForm.btnGetClick(Sender: TObject);
var
  res: TOAuth2Response;
  start, stop: DWord;
begin
  Screen.Cursor := crHourGlass;
  try
    AddHistory;
    FSendStream.Clear;
    FReceiveStream.Clear;
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
      txtExpires.Text := FormatDateTime('hh:nn:ss', FOAuthClient.AccessToken.ExpiresAt);
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
        txtResponse.Text := Format('%s: %s', [E.ClassName, E.Message]);
    end;
  finally
    ReadStreams;
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.btnPostClick(Sender: TObject);
var
  res: TOAuth2Response;
  start, stop: DWord;
  ff: TStringList;
begin
  Screen.Cursor := crHourGlass;
  try
    AddHistory;
    FSendStream.Clear;
    FReceiveStream.Clear;
    FOAuthClient.Site := txtSite.Text;
    FOAuthClient.GrantType := gtPassword;
    FOAuthClient.UserName := txtUser.Text;
    FOAuthClient.PassWord := txtPass.Text;
    FOAuthClient.ClientId := txtClientId.Text;
    FOAuthClient.ClientSecret := txtClientSecret.Text;
    ff := TStringList.Create;
    try
      ff.AddStrings(txtFormFields.Lines);
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
          txtResponse.Text := Format('%s: %s', [E.ClassName, E.Message]);
      end;
    finally
      ff.Free;
    end;
  finally
    ReadStreams;
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.AddHistory;
var
  r: string;
begin
  r := txtResource.Text;
  if r <> '' then begin
    FHistoryForm.Add(r, GetFormFields);
  end;
end;

function TMainForm.GetFormFields: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to txtFormFields.Lines.Count - 1 do begin
    Result := Result + txtFormFields.Lines[i] + '&';
  end;
  if Result <> '' then begin
    if Result[Length(Result)] = '&' then
      Delete(Result, Length(Result), 1);
  end;
end;

procedure TMainForm.ReadStreams;
var
  sent, recv: string;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    if FSendStream.Size > 0 then begin
      SetLength(sent, FSendStream.Size);
      FSendStream.Position := 0;
      FSendStream.Read(sent[1], FSendStream.Size);
      sent := StringReplace(sent, '<EOL>', LineEnding, [rfReplaceAll]);
      sl.Text := sent;
      LogForm.txtSent.Lines.AddStrings(sl);
    end;
    if FReceiveStream.Size > 0 then begin
      SetLength(recv, FReceiveStream.Size);
      FReceiveStream.Position := 0;
      FReceiveStream.Read(recv[1], FReceiveStream.Size);
      recv := StringReplace(recv, '<EOL>', LineEnding, [rfReplaceAll]);
      sl.Text := recv;
      LogForm.txtRecv.Lines.AddStrings(sl);
    end;
    LogForm.txtSent.Lines.Add('----------------------8<----------------------');
    LogForm.txtRecv.Lines.Add('----------------------8<----------------------');
  finally
    sl.Free;
  end;
  while LogForm.txtSent.Lines.Count > MAX_LOGLINES do
    LogForm.txtSent.Lines.Delete(0);
  while LogForm.txtRecv.Lines.Count > MAX_LOGLINES do
    LogForm.txtRecv.Lines.Delete(0);
end;

end.

