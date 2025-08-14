unit Directory;

{$mode ObjFPC}{$H+}

interface

uses
  Classes;

type
  TDirectoryType = (
    dtTools,          //Инструменты
    dtToolsSteamCmd,  //SteamCmd
    dtServer,         //Сервер
    dtData,           //Данные
    dtServerMods,     //Серверные моды
    dtProfile,        //Профиль игрока
    dtStorage,        //Мир
    dtBackup,         //Каталог бэкапов

    dtUpdateServerMods  //Папка обновления серверных модов
  );


  TDirectory = class
  private
    FMain: String;

    procedure CreateDirectories;

    function GetDirectory(Index: TDirectoryType): String;
  public
    constructor Create(MainDir: String);

    class procedure GetDirectoryList(BaseDir: String; List: TStringList);
    class procedure GetFileList(BaseDir: String; List: TStringList);

    property Main: string read FMain;
    property Directory[Index: TDirectoryType]: String read GetDirectory;
  end;


implementation

uses
  SysUtils;

const
  TDirectoryPath: array[TDirectoryType] of string = (
    'Tools\',
    'Tools\SteamCmd\',
    'Server\',
    'Data\',
    'Data\ServerMods\',
    'Data\Profile\',
    'Data\Storage\',
    'Backup\',

    'Update\ServerMods\'
  );


procedure TDirectory.CreateDirectories;
var
  i: TDirectoryType;
  s: String;
begin
  for i := Low(TDirectoryType) to High(TDirectoryType) do
  begin
    s := FMain + TDirectoryPath[i];
    ForceDirectories(s);
  end;
end;


function TDirectory.GetDirectory(Index: TDirectoryType): String;
begin
  Result := FMain + TDirectoryPath[Index];
end;


constructor TDirectory.Create(MainDir: String);
begin
  FMain := IncludeTrailingBackslash(MainDir);

  CreateDirectories;
end;


class procedure TDirectory.GetDirectoryList(BaseDir: String; List: TStringList);
var
  o: TSearchRec;
  Idx: Integer;
begin
  BaseDir := IncludeTrailingBackslash(BaseDir);

  Idx := FindFirst(BaseDir+ '*', faAnyFile, o);
  while Idx = 0 do
  begin
    if (o.Name <> '.') and (o.Name <> '..') then
      if (o.Attr and faDirectory) = faDirectory then
        List.Add(o.Name);

    Idx := FindNext(o);
  end;

  FindClose(o);
end;


class procedure TDirectory.GetFileList(BaseDir: String; List: TStringList);
var
  o: TSearchRec;
  Idx: Integer;
begin
  BaseDir := IncludeTrailingBackslash(BaseDir);

  Idx := FindFirst(BaseDir + '*', faAnyFile, o);
  while Idx = 0 do
  begin
    if (o.Name <> '.') and (o.Name <> '..') then
      if (o.Attr and faDirectory) <> faDirectory then
        List.Add(o.Name);

    Idx := FindNext(o);
  end;

  FindClose(o);
end;


end.

