# Delphi Read Windows 11 System Path

I'm working on a Delphi 12 application running on 64 bit Windows 11. My application needs to read the Path from the registry (both the user and System path) and my application can read the User path, but not the System path. 

I used Tools -> Options -> Manifest to require admin privileges to execute the program and when I run the app it prompts me to authorize Admin mode, so I know that part of it is working. 

I've scoured the Internet for days looking for a solution for this and I've tried every option I can find (Embarcadero's web site being down this week made all of this more difficult as many articles I find reference docs there) and every time, the code successfully opens the specified key (`SYSTEM\CurrentControlSet\Control\Session Manager\Environment`) but can't read any properties there (like the `path` property).

I even tried populating a `TStringList` with all of the properties under the key using 

I created a sample application that demonstrates this, you can find it at https://github.com/johnwargo/Path-Test-2024. 

```pascal
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
```

Playing around, I added the following code:

`KeyInfo: TRegKeyInfo;`

```pascal
Reg.GetKeyInfo(KeyInfo);
LogMessage('Number of Keys: ' + IntToStr(KeyInfo.NumSubKeys));
LogMessage('Number of Subkeys: ' + IntToStr(KeyInfo.NumSubKeys));
LogMessage('Number of Values: ' + IntToStr(KeyInfo.NumValues));
```

It shows no keys, subkeys, or  values:

1/19/2024 5:25:52 PM Number of Keys: 0
1/19/2024 5:25:52 PM Number of Subkeys: 0
1/19/2024 5:25:52 PM Number of Values: 0

Ii I go a layer higher (`SYSTEM\CurrentControlSet\Control\Session Manager\`), I get the following results:

1/19/2024 5:25:52 PM Number of Keys: 1
1/19/2024 5:25:52 PM Number of Subkeys: 1
1/19/2024 5:25:52 PM Number of Values: 0

even though the Registry at that point has 10 or more keys and values. 

Can someone please help me understand what I'm doing wrong here?  I've been a TurboPascal developer since the mid 80's and a Delphi developer since version 1.0. I can normally find my way out of these types of situations, but at this point I'm stymied. There's something in Windows blocking access to those values.