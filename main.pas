unit main;

interface

uses
  Registry,

  System.SysUtils, System.Variants, System.Classes,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.ComCtrls,

  Winapi.Windows, Winapi.Messages;

type
  TfrmMain = class(TForm)
    StatusBar1: TStatusBar;
    PanelMain: TPanel;
    SystemGroupBox: TGroupBox;
    SystemPathList: TListBox;
    UserGroupBox: TGroupBox;
    UserPathList: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
  pathProperty = 'Path';

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure PopulateList(pathList: TListBox; theHKey: HKEY;
  regKey, regProperty: String);
var
  Reg: TRegistry;
  pathArray: Tarray<String>;
  pathStr: String;

begin
  Reg := TRegistry.Create(KEY_READ);

  Reg.rootKey := theHKey;
  if Reg.KeyExists(regKey) then begin
    if Reg.OpenKey(regKey, false) then begin
      if Reg.ValueExists(regProperty) then begin
        pathStr := Reg.Readstring(regProperty);
        if Length(pathStr) > 0 then begin
          pathArray := pathStr.Split([';'], TStringSplitOptions.ExcludeEmpty);
          pathList.Items.AddStrings(pathArray);
        end;
      end else begin
        ShowMessage(Format('Property "%s" does not exist.', [regProperty]));
      end;
      Reg.CloseKey;
    end else begin
      ShowMessage(Format('Path and cannot open open: %s', [regKey]));
    end;
  end;

end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  PopulateList(UserPathList, HKEY_CURRENT_USER, 'Environment', pathProperty);
  PopulateList(SystemPathList, HKEY_LOCAL_MACHINE,
    '\SYSTEM\CurrentControlSet\Control\Session Manager\Environment',
    pathProperty);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  PanelMain.Align := alClient;
  UserGroupBox.Height :=
    (PanelMain.Height - (UserGroupBox.Padding.Top +
    UserGroupBox.Padding.Bottom)) div 2;
  UserGroupBox.Align := alTop;
  SystemGroupBox.Align := alClient;
end;

end.
