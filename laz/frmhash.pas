unit frmhash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LCLType;

type
  { THashForm }
  THashForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    cboHashes: TComboBox;
    txtHmacKey: TEdit;
    Panel2: TPanel;
    txtResult: TEdit;
    Panel1: TPanel;
    txtInput: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  HashForm: THashForm;

implementation

uses
  Hash, HashHaval, HashMD5, HashRipeMD128, HashRipeMD160, HashSHA1, HashSHA256,
  HashSHA384, HashSHA512, HashTiger, uOAuth2Tools, uOAuth2Hmac;

{$R *.lfm}

{ THashForm }

procedure THashForm.FormCreate(Sender: TObject);
var
  i: integer;
  ht: PHashType;
begin
  for i := 0 to THash.GetHashTypesCount - 1 do begin
    ht := THash.GetHashType(i);
    cboHashes.AddItem(ht^.Name, TObject(ht));
  end;
  cboHashes.ItemIndex := cboHashes.Items.IndexOf('SHA-256');
end;

procedure THashForm.Button1Click(Sender: TObject);
var
  ht: PHashType;
  i: integer;
begin
  i := cboHashes.ItemIndex;
  ht := PHashType(cboHashes.Items.Objects[i]);
  txtResult.Text := LowerCase(HashString(txtInput.Text, ht^.HashClass));
end;

procedure THashForm.Button2Click(Sender: TObject);
var
  ht: PHashType;
  i: integer;
begin
  i := cboHashes.ItemIndex;
  ht := PHashType(cboHashes.Items.Objects[i]);
  txtResult.Text := LowerCase(HmacCalc(txtInput.Text, txtHmacKey.Text, ht^.Name));
end;

end.

