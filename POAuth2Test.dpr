program POAuth2Test;

uses
  Forms,
  frmMain in 'frmMain.pas' {MainForm},
  uOAuth2Client in 'uOAuth2Client.pas',
  uOAuth2HttpClient in 'uOAuth2HttpClient.pas',
  uOAuth2Token in 'uOAuth2Token.pas',
  uJson in 'uJson.pas',
  uOAuth2Consts in 'uOAuth2Consts.pas',
  uIndyClient in 'uIndyClient.pas',
  uOAuth2Tools in 'uOAuth2Tools.pas',
  frmJson in 'frmJson.pas' {JsonForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TJsonForm, JsonForm);
  Application.Run;
end.
