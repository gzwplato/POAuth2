program POAuth2Test;

uses
  Forms,
  frmMain in 'frmMain.pas' {MainForm},
  frmJson in 'frmJson.pas' {JsonForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TJsonForm, JsonForm);
  Application.Run;
end.
