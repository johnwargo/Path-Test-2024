unit main;

interface

uses
  Log, Registry,

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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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

procedure PopulateList(pathList: TListBox; source: String; rootKey: HKEY;
  regKey, regProperty: String);
var
  Reg: TRegistry;
  pathArray: Tarray<String>;
  msg, pathStr: String;

begin
  LogMessage(Format('Reading "%s" Path from "%s"', [source, regKey]));
  // Reg := TRegistry.Create(KEY_ALL_ACCESS);
  // Reg := TRegistry.Create(KEY_ALL_ACCESS OR KEY_WOW64_64KEY);
  // Reg := TRegistry.Create(KEY_EXECUTE);
  // Reg := TRegistry.Create(KEY_EXECUTE OR KEY_WOW64_64KEY);
  // Reg := TRegistry.Create(KEY_READ OR KEY_WOW64_32KEY);
  Reg := TRegistry.Create(KEY_READ OR KEY_WOW64_64KEY);
  // Reg := TRegistry.Create(KEY_WRITE OR KEY_WOW64_32KEY);
  // https://stackoverflow.com/questions/2666807/registry-readstring-method-is-not-working-in-windows-7-in-delphi-7
  // Reg := TRegistry.Create(KEY_ENUMERATE_SUB_KEYS);

  with Reg do begin
    rootKey := rootKey;
    Access := KEY_READ;
    LogMessage(Format('Opening key: %s', [regKey]));
    if Reg.KeyExists(regKey) then begin
      if OpenKeyReadOnly(regKey) then begin
        LogMessage('Key opened');
        if ValueExists(regProperty) then begin
          LogMessage(Format('Reading property %s', [regProperty]));
          pathStr := Readstring(regProperty);
          if Length(pathStr) > 0 then begin
            LogMessage('Path: ' + pathStr);
            pathArray := pathStr.Split([';'], TStringSplitOptions.ExcludeEmpty);
            pathList.Items.AddStrings(pathArray);
          end;
        end else begin
          msg := Format('%s Path property "%s" does not exist.',
            [source, regProperty]);
          LogMessage(msg);
          ShowMessage(msg);
        end;
        CloseKey;
      end else begin
        msg := Format('Getting %s Path and cannot open open: %s',
          [source, regKey]);
        LogMessage(msg);
        ShowMessage(msg);
      end;
    end;
  end;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  PopulateList(UserPathList, 'User', HKEY_CURRENT_USER, 'Environment',
    pathProperty);
  PopulateList(SystemPathList, 'System', HKEY_LOCAL_MACHINE,
    'SYSTEM\CurrentControlSet\Control\Session Manager\Environment',
    pathProperty);
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  LogMessage('Closing application');
  CloseLog;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  OpenLog;
  LogMessage('Form created');

  PanelMain.Align := alClient;
  UserGroupBox.Align := alTop;
  UserGroupBox.Height := PanelMain.Height div 2;
  SystemGroupBox.Align := alClient;
end;

end.
