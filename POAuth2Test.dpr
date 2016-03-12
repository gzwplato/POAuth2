program POAuth2Test;

uses
  Vcl.Forms,
  frmMain in 'frmMain.pas' {Form1},
  uOAuth2Client in 'uOAuth2Client.pas',
  uOAuth2HttpClient in 'uOAuth2HttpClient.pas',
  uOAuth2Token in 'uOAuth2Token.pas',
  uJson in 'uJson.pas',
  uOAuth2Consts in 'uOAuth2Consts.pas',
  uIndyClient in 'uIndyClient.pas',
  uOAuth2Tools in 'uOAuth2Tools.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
