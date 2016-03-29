program poat;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, frmmain, uOAuth2Token, uIndyClient, uOAuth2HttpClient, uOAuth2Config,
  uOAuth2Client, uJson, frmlog, frmhistory, dlgoptions, frmhash, dlgabout;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TLogForm, LogForm);
  Application.CreateForm(THashForm, HashForm);
  Application.Run;
end.

