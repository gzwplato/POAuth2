unit dlgabout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LCLIntf;

type
  { TAboutDialog }
  TAboutDialog = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    lblUrl: TLabel;
    lblGithub: TLabel;
    lblCompileDate: TLabel;
    lblCompiler: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure lblUrlClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  AboutDialog: TAboutDialog;

implementation

{$R *.lfm}

{ TAboutDialog }

procedure TAboutDialog.FormCreate(Sender: TObject);
begin
  lblCompiler.Caption := 'Compiled with FPC ' + {$i %FPCVERSION%} + ' for ' + {$I %FPCTARGETOS%} + ' on ' + {$I %FPCTARGETCPU%};
  lblCompileDate.Caption := {$i %DATE%} + ' ' + {$i %TIME%};
end;

procedure TAboutDialog.lblUrlClick(Sender: TObject);
begin
  OpenURL((Sender as TLabel).Caption);
end;

end.

