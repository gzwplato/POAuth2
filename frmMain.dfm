object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'POAuth2 Test'
  ClientHeight = 624
  ClientWidth = 633
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    633
    624)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 56
    Width = 52
    Height = 13
    Caption = 'Username:'
  end
  object Label2: TLabel
    Left = 256
    Top = 56
    Width = 50
    Height = 13
    Caption = 'Password:'
  end
  object Label3: TLabel
    Left = 8
    Top = 112
    Width = 45
    Height = 13
    Caption = 'Client ID:'
  end
  object Label4: TLabel
    Left = 256
    Top = 112
    Width = 65
    Height = 13
    Caption = 'Client Secret:'
  end
  object Label5: TLabel
    Left = 8
    Top = 266
    Width = 49
    Height = 13
    Caption = 'Resource:'
  end
  object Label6: TLabel
    Left = 8
    Top = 8
    Width = 22
    Height = 13
    Caption = 'Site:'
  end
  object Label7: TLabel
    Left = 8
    Top = 160
    Width = 69
    Height = 13
    Caption = 'Access Token:'
  end
  object Label8: TLabel
    Left = 8
    Top = 206
    Width = 74
    Height = 13
    Caption = 'Refresh Token:'
  end
  object Label9: TLabel
    Left = 504
    Top = 160
    Width = 39
    Height = 13
    Caption = 'Expires:'
  end
  object txtUser: TEdit
    Left = 8
    Top = 75
    Width = 233
    Height = 21
    TabOrder = 0
    Text = 'demouser'
  end
  object txtPass: TEdit
    Left = 256
    Top = 75
    Width = 233
    Height = 21
    TabOrder = 1
    Text = 'testpass'
  end
  object txtClientId: TEdit
    Left = 8
    Top = 131
    Width = 233
    Height = 21
    TabOrder = 2
    Text = 'demoapp'
  end
  object txtClientSecret: TEdit
    Left = 256
    Top = 131
    Width = 233
    Height = 21
    TabOrder = 3
    Text = 'demopass'
  end
  object Button1: TButton
    Left = 498
    Top = 73
    Width = 127
    Height = 25
    Caption = 'Authorize'
    TabOrder = 4
  end
  object txtResource: TEdit
    Left = 8
    Top = 285
    Width = 482
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
    Text = '/resource?query=somevalue'
  end
  object Button2: TButton
    Left = 496
    Top = 283
    Width = 127
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Get Resource'
    TabOrder = 6
    OnClick = Button2Click
  end
  object txtResponse: TMemo
    Left = 8
    Top = 314
    Width = 615
    Height = 302
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
  end
  object txtSite: TEdit
    Left = 8
    Top = 27
    Width = 617
    Height = 21
    TabOrder = 8
    Text = 'http://localhost/lockdin'
  end
  object Edit5: TEdit
    Left = 8
    Top = 179
    Width = 481
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 9
    Text = 'Edit5'
  end
  object Edit6: TEdit
    Left = 8
    Top = 225
    Width = 481
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 10
    Text = 'Edit5'
  end
  object Edit7: TEdit
    Left = 504
    Top = 179
    Width = 121
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 11
    Text = 'Edit5'
  end
  object IdHTTP1: TIdHTTP
    AllowCookies = True
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Request.ContentLength = -1
    Request.ContentRangeEnd = -1
    Request.ContentRangeStart = -1
    Request.ContentRangeInstanceLength = -1
    Request.Accept = 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8'
    Request.BasicAuthentication = False
    Request.UserAgent = 'Mozilla/3.0 (compatible; Indy Library)'
    Request.Ranges.Units = 'bytes'
    Request.Ranges = <>
    HTTPOptions = [hoForceEncodeParams]
    Left = 528
    Top = 120
  end
end
