object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'POAuth2 Test'
  ClientHeight = 604
  ClientWidth = 633
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    633
    604)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 56
    Width = 52
    Height = 13
    Caption = 'Username:'
    Color = clBtnFace
    ParentColor = False
  end
  object Label2: TLabel
    Left = 256
    Top = 56
    Width = 50
    Height = 13
    Caption = 'Password:'
    Color = clBtnFace
    ParentColor = False
  end
  object Label3: TLabel
    Left = 8
    Top = 112
    Width = 45
    Height = 13
    Caption = 'Client ID:'
    Color = clBtnFace
    ParentColor = False
  end
  object Label4: TLabel
    Left = 256
    Top = 112
    Width = 65
    Height = 13
    Caption = 'Client Secret:'
    Color = clBtnFace
    ParentColor = False
  end
  object Label5: TLabel
    Left = 8
    Top = 266
    Width = 49
    Height = 13
    Caption = 'Resource:'
    Color = clBtnFace
    ParentColor = False
  end
  object Label6: TLabel
    Left = 8
    Top = 8
    Width = 22
    Height = 13
    Caption = 'Site:'
    Color = clBtnFace
    ParentColor = False
  end
  object Label7: TLabel
    Left = 8
    Top = 160
    Width = 69
    Height = 13
    Caption = 'Access Token:'
    Color = clBtnFace
    ParentColor = False
  end
  object Label8: TLabel
    Left = 8
    Top = 206
    Width = 74
    Height = 13
    Caption = 'Refresh Token:'
    Color = clBtnFace
    ParentColor = False
  end
  object Label9: TLabel
    Left = 504
    Top = 160
    Width = 39
    Height = 13
    Caption = 'Expires:'
    Color = clBtnFace
    ParentColor = False
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
  object txtResource: TEdit
    Left = 8
    Top = 285
    Width = 482
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
    Text = '/resource?query=somevalue'
    OnExit = txtResourceExit
  end
  object Button2: TButton
    Left = 496
    Top = 283
    Width = 127
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Get Resource'
    TabOrder = 5
    OnClick = Button2Click
  end
  object txtResponse: TMemo
    Left = 8
    Top = 314
    Width = 615
    Height = 282
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
  end
  object txtSite: TEdit
    Left = 8
    Top = 27
    Width = 617
    Height = 21
    TabOrder = 7
    Text = 'http://localhost/lockdin'
    OnExit = txtSiteExit
  end
  object txtAccessToken: TEdit
    Left = 8
    Top = 179
    Width = 481
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 8
  end
  object txtRefreshToken: TEdit
    Left = 8
    Top = 225
    Width = 481
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 9
  end
  object txtExpires: TEdit
    Left = 504
    Top = 179
    Width = 121
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 10
  end
  object MainMenu1: TMainMenu
    Left = 544
    Top = 216
    object File1: TMenuItem
      Caption = '&File'
      object Exit1: TMenuItem
        Caption = '&Exit'
        OnClick = Exit1Click
      end
    end
    object est1: TMenuItem
      Caption = '&Test'
      object JSON1: TMenuItem
        Caption = '&JSON...'
        OnClick = JSON1Click
      end
    end
  end
end
