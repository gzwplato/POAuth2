object JsonForm: TJsonForm
  Left = 0
  Top = 0
  Caption = 'JSON'
  ClientHeight = 384
  ClientWidth = 519
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 193
    Width = 519
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitLeft = 8
    ExplicitTop = 202
  end
  object txtJson: TMemo
    Left = 0
    Top = 0
    Width = 519
    Height = 193
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    Lines.Strings = (
      '{'
      '  "access_token":"03ef77aafaebf61cdbe9798df78947e9f2363d44",'
      '  "expires_in":3600,'
      '  "token_type":"Bearer",'
      '  "scope":null,'
      '  "refresh_token":"7d8809067491b520cb3a8ceab6fbcb1d71e78b10"'
      '}')
    ParentFont = False
    TabOrder = 0
  end
  object txtResult: TMemo
    Left = 0
    Top = 196
    Width = 519
    Height = 147
    Align = alBottom
    Color = clBtnFace
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 343
    Width = 519
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Button1: TButton
      Left = 432
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Parse'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
end
