unit Config;

{$mode ObjFPC}{$H+}

interface

type
  //Описание одного клиентского мода
  TConfigClientMod = record
    SteamID: Int64;
    Name: String;
  end;


  //Тип элемента бэкапа
  TConfigBackupItemType = (
    bitUnknown,
    bitFile,
    bitFileInFolder,
    bitFolder
  );

  //Элемент бэкапа
  TConfigBackupItem = record
    ItemType: TConfigBackupItemType;
    Value: String;
  end;


  //Общий конфиг
  TConfig = class
  public
    //Настройки профиля
    SteamLogin: String;
    SteamPassword: String;

    //Настройки игры
    SteamDayZID: Int64;
    SteamDayZModsID: Int64;

    //Клиентские модификации
    ClientMods: array of TConfigClientMod;

    //Элементы бэкапа
    BackupItems: array of TConfigBackupItem;

    //Элементы очистки
    ClearItems: array of TConfigBackupItem;

    //Настройки бэкапа
    BackupKeepDays: Integer;

    //Настройки сервера
    ServerConfig: String;
    ServerPort: Integer;
    ServerCPU: Integer;
    ServerFPSLimit: Integer;
    ServerEXE: String;

  public
    destructor Destroy; override;

    procedure LoadFromFile(FileName: String);
    procedure SaveToFile(FileName: String);
  end;


implementation

uses
  IniFiles, Classes, SysUtils;

const
  SECTION_AUTH = 'Auth';
  KEY_AUTH_LOGIN = 'Login';
  KEY_AUTH_PASSWORD = 'Password';

  SECTION_GAME = 'Game';
  KEY_GAME_STEAMID = 'SteamID';
  KEY_GAME_MODS_STEAMID = 'ModsSteamID';

  SECTION_CLIENT_MODS = 'ClientMods';

  SECTION_BACKUP_ITEMS = 'BackupItems';

  SECTION_BACKUP_CONFIG = 'Backup';
  KEY_BACKUP_CONFIG_KEEP_DAYS = 'KeepDays';

  SECTION_CLEAR_ITEMS = 'ClearItems';

  SECTION_SERVER = 'Server';
  KEY_SERVER_CONFIG = 'Config';
  KEY_SERVER_PORT = 'Port';
  KEY_SERVER_CPU = 'CPU';
  KEY_SERVER_FPSLimit = 'FPSLimit';
  KEY_SERVER_EXE = 'EXE';




function StringToConfigBackupItem(str: String): TConfigBackupItem;
var
  List: TStringList;
begin
  Result.ItemType := bitUnknown;
  Result.Value := '';

  List := TStringList.Create;
  try
    List.LineBreak := ';';
    List.Text := str;

    if List.Count <> 2 then
      Exit;

    case LowerCase(Trim(List.Strings[0])) of
      'file':        Result.ItemType := bitFile;
      'folder':      Result.ItemType := bitFolder;
      'folderfiles': Result.ItemType := bitFileInFolder;
    end;

    Result.Value := Trim(List.Strings[1]);

  finally
    List.Free;
  end;
end;


function ConfigBackupItemToString(BackupType: TConfigBackupItemType): String;
begin
  case BackupType of
    bitFile:         Result := 'File';
    bitFolder:       Result := 'Folder';
    bitFileInFolder: Result := 'FolderFiles';
    else             Result := '';
  end;
end;


destructor TConfig.Destroy;
begin
  SetLength(ClientMods, 0);
  SetLength(BackupItems, 0);
  SetLength(ClearItems, 0);
end;


procedure TConfig.LoadFromFile(FileName: String);
var
  F: TIniFile;
  List: TStringList;
  i, c: Integer;
  s: String;
  ModInfo: TConfigClientMod;
  BackupItem: TConfigBackupItem;
begin
  F := TIniFile.Create(FileName);
  List := TStringList.Create;
  try
    //Настройки профиля
    SteamLogin := f.ReadString(SECTION_AUTH, KEY_AUTH_LOGIN, '');
    SteamPassword := f.ReadString(SECTION_AUTH, KEY_AUTH_PASSWORD, '');

    //Настройки игры
    SteamDayZID := f.ReadInteger(SECTION_GAME, KEY_GAME_STEAMID, 223350);
    SteamDayZModsID := f.ReadInteger(SECTION_GAME, KEY_GAME_MODS_STEAMID, 221100);

    //Клиентские моды
    SetLength(ClientMods, 0);
    F.ReadSection(SECTION_CLIENT_MODS, List);
    for i := 0 to List.Count - 1 do
    begin
      //Заполним поля
      if not TryStrToInt64(List.Strings[i], ModInfo.SteamID) then
        Continue;
      ModInfo.Name := F.ReadString(SECTION_CLIENT_MODS, IntToStr(ModInfo.SteamID), '');

      //Добавим в список
      c := Length(ClientMods);
      SetLength(ClientMods, c + 1);
      ClientMods[c] := ModInfo;
    end;

    //Элементы бэкапа
    SetLength(BackupItems, 0);
    F.ReadSection(SECTION_BACKUP_ITEMS, List);
    for i := 0 to List.Count - 1 do
    begin
      //Строка с параметром
      s := F.ReadString(SECTION_BACKUP_ITEMS, List.Strings[i], '');

      BackupItem := StringToConfigBackupItem(s);
      if BackupItem.ItemType = bitUnknown then
        Continue;

      //Добавим в список
      c := Length(BackupItems);
      SetLength(BackupItems, c + 1);
      BackupItems[c] := BackupItem;
    end;

    //Настройки бэкапа
    BackupKeepDays := F.ReadInteger(SECTION_BACKUP_CONFIG, KEY_BACKUP_CONFIG_KEEP_DAYS, 14);


    //Элементы очистки
    SetLength(ClearItems, 0);
    F.ReadSection(SECTION_CLEAR_ITEMS, List);
    for i := 0 to List.Count - 1 do
    begin
      //Строка с параметром
      s := F.ReadString(SECTION_CLEAR_ITEMS, List.Strings[i], '');

      BackupItem := StringToConfigBackupItem(s);
      if BackupItem.ItemType = bitUnknown then
        Continue;

      //Добавим в список
      c := Length(ClearItems);
      SetLength(ClearItems, c + 1);
      ClearItems[c] := BackupItem;
    end;


    //Настройки сервера
    ServerConfig := F.ReadString(SECTION_SERVER, KEY_SERVER_CONFIG, 'serverDZ.cfg');
    ServerPort := F.ReadInteger(SECTION_SERVER, KEY_SERVER_PORT, 2302);
    ServerCPU := F.ReadInteger(SECTION_SERVER, KEY_SERVER_CPU, 6);
    ServerFPSLimit := F.ReadInteger(SECTION_SERVER, KEY_SERVER_FPSLimit, 100);
    ServerEXE := F.ReadString(SECTION_SERVER, KEY_SERVER_EXE, 'DayZServer_x64.exe');


  finally
    List.Free;
    F.Free;
  end;
end;


procedure TConfig.SaveToFile(FileName: String);
var
  F: TIniFile;
  i: Integer;
begin
  F := TIniFile.Create(FileName);
  try
    //Настройки профиля
    f.WriteString(SECTION_AUTH, KEY_AUTH_LOGIN, SteamLogin);
    f.WriteString(SECTION_AUTH, KEY_AUTH_PASSWORD, SteamPassword);

    //Настройки игры
    f.WriteInteger(SECTION_GAME, KEY_GAME_STEAMID, SteamDayZID);
    f.WriteInteger(SECTION_GAME, KEY_GAME_MODS_STEAMID, SteamDayZModsID);

    //Клиентские моды
    for i := 0 to Length(ClientMods) - 1 do
      f.WriteString(SECTION_CLIENT_MODS, IntToStr(ClientMods[i].SteamID), ClientMods[i].Name);

    //Элементы бэкапа
    for i := 0 to Length(BackupItems) - 1 do
      f.WriteString(SECTION_BACKUP_ITEMS, IntToStr(i), ConfigBackupItemToString(BackupItems[i].ItemType) + ';' + BackupItems[i].Value);

    //Настройки бэкапа
    F.WriteInteger(SECTION_BACKUP_CONFIG, KEY_BACKUP_CONFIG_KEEP_DAYS, BackupKeepDays);

    //Элементы очистки
    for i := 0 to Length(BackupItems) - 1 do
      f.WriteString(SECTION_CLEAR_ITEMS, IntToStr(i), ConfigBackupItemToString(ClearItems[i].ItemType) + ';' + ClearItems[i].Value);

    //Настройки сервера
    F.WriteString(SECTION_SERVER, KEY_SERVER_CONFIG, ServerConfig);
    F.WriteInteger(SECTION_SERVER, KEY_SERVER_PORT, ServerPort);
    F.WriteInteger(SECTION_SERVER, KEY_SERVER_CPU, ServerCPU);
    F.WriteInteger(SECTION_SERVER, KEY_SERVER_FPSLimit, ServerFPSLimit);
    F.WriteString(SECTION_SERVER, KEY_SERVER_EXE, ServerEXE);

  finally
    F.Free;
  end;
end;



end.

