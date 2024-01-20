object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Windows Path Settings'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  TextHeight = 15
  object StatusBar1: TStatusBar
    Left = 0
    Top = 422
    Width = 624
    Height = 19
    Panels = <>
    ExplicitTop = 414
    ExplicitWidth = 622
  end
  object PanelMain: TPanel
    Left = 8
    Top = 19
    Width = 217
    Height = 270
    Padding.Left = 10
    Padding.Top = 10
    Padding.Right = 10
    Padding.Bottom = 10
    TabOrder = 1
    object SystemGroupBox: TGroupBox
      Left = 8
      Top = 144
      Width = 185
      Height = 105
      Caption = 'System Path'
      Padding.Left = 10
      Padding.Top = 10
      Padding.Right = 10
      Padding.Bottom = 10
      TabOrder = 0
      object SystemPathList: TListBox
        Left = 12
        Top = 27
        Width = 161
        Height = 66
        Align = alClient
        ItemHeight = 15
        TabOrder = 0
      end
    end
    object UserGroupBox: TGroupBox
      Left = 8
      Top = 16
      Width = 185
      Height = 105
      Caption = 'User Path'
      Padding.Left = 10
      Padding.Top = 10
      Padding.Right = 10
      Padding.Bottom = 10
      TabOrder = 1
      object UserPathList: TListBox
        Left = 12
        Top = 27
        Width = 161
        Height = 66
        Align = alClient
        ItemHeight = 15
        TabOrder = 0
      end
    end
  end
end
