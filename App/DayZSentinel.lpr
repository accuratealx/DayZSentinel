program DayZSentinel;

{$mode objfpc}{$H+}
{$codepage UTF-8}

uses
  SysUtils, process, Classes, FileUtil, LazFileUtils, Zipper, ZStream, DateUtils,
  Config, Directory;

var
  Cfg: TConfig;
  Dir: TDirectory;


function GetSteamCmdFile: String; forward;
procedure UpdateServerMod(ModName: String); forward;
function UpdateClientModFromSteam(ModID: Int64): Boolean; forward;
function GetClientModDirectory: String; forward;
function GetModList(Directory: String): String; forward;


procedure Step_Wellcome;
begin
  WriteLn('DayZ Sentinel v1.0  [accuratealx@gmail.com]');
end;


procedure Step_ReadConfig;
var
  fn: String;
begin
  fn := Dir.Main + 'Sentinel.ini';

  WriteLn('');
  WriteLn('ЧТЕНИЕ ФАЙЛА НАСТРОЕК');

  if FileExists(fn) then
    Cfg.LoadFromFile(fn)
  else
  begin
    WriteLn('Нет файла настроек, инициализация', ' ', fn);

    Cfg.LoadFromFile(fn);
    Cfg.SaveToFile(fn);
  end;
end;


function Step_CheckSteamCmd: Boolean;
var
  fn: String;
begin
  fn := GetSteamCmdFile;

  WriteLn('');
  WriteLn('ПРОВЕРКА SteamCmd');

  Result := True;
  if not FileExists(fn) then
  begin
    Result := False;
    WriteLn(Format('В каталоге %s отсутствует SteamCmd, продолжение невозможно', [Dir.Directory[dtToolsSteamCmd]]));
  end;
end;


function Step_UpdateSever: Boolean;
var
  P: TProcess;
begin
  Result := False;

  WriteLn('');
  WriteLn('ОБНОВЛЕНИЕ СЕРВЕРА');

  P := TProcess.Create(nil);
  try
    P.Executable := GetSteamCmdFile;
    p.Options := [poWaitOnExit];

    P.Parameters.Add('+force_install_dir');
    P.Parameters.Add(Dir.Directory[dtServer]);
    P.Parameters.Add('+login');
    P.Parameters.Add(Cfg.SteamLogin);
    P.Parameters.Add(Cfg.SteamPassword);
    P.Parameters.Add('+app_update');
    P.Parameters.Add(IntToStr(Cfg.SteamDayZID));
    P.Parameters.Add('+quit');

    P.Execute;

    Result := True;

  finally
    P.Free;
    if not Result then
      WriteLn('Не удалось обновить сервер, пропуск');
  end;
end;


procedure Step_UpdateSeverMods;
var
  List: TStringList;
  i: Integer;
  sMod: String;
begin
  List := TStringList.Create;
  try
    Dir.GetDirectoryList(Dir.Directory[dtUpdateServerMods], List);

    for i := 0 to List.Count - 1 do
    begin
      sMod := List.Strings[i];
      WriteLn('');
      WriteLn('ОБНОВЛЕНИЕ СЕРВЕРНОГО МОДА: ', sMod);
      UpdateServerMod(sMod);
    end;

  finally
    List.Free;
  end;
end;


procedure Step_UpdateClientMods;
var
  i: Integer;
  ModInfo: TConfigClientMod;
  ModSrcDir, ModDstDir: String;
begin
  for i := 0 to Length(Cfg.ClientMods) - 1 do
  begin
    ModInfo := Cfg.ClientMods[i];

    WriteLn('');
    WriteLn('ОБНОВЛЕНИЕ КЛИЕНТСКОГО МОДА: ', ModInfo.Name);

    //Обновим мод через стим
    UpdateClientModFromSteam(ModInfo.SteamID);

    //Скопируем файлы ключей в сервер
    ModSrcDir := GetClientModDirectory + IntToStr(ModInfo.SteamID) + '\keys';
    ModDstDir := Dir.Directory[dtServer] + 'keys\';
    CopyDirTree(ModSrcDir, ModDstDir, [cffCreateDestDirectory]);
  end;
end;


procedure Step_Backup;
var
  TmpDir, ArcName: String;
  i, j: Integer;
  Item: TConfigBackupItem;
  pathSrc, pathDst, fn: String;
  List: TStringList;
  Zip: TZipper;
  ZipEntry: TZipFileEntry;
begin
  WriteLn('');
  WriteLn('АРХИВИРОВАНИЕ');

  //Имя архива
  ArcName := FormatDateTime('yyyy.mm.dd-hh.nn.ss', Now);

  //Подготовить временную папку
  TmpDir := Dir.Directory[dtBackup] + ArcName + '\';
  ForceDirectories(TmpDir);

  //Скопируем файлы
  for i := 0 to Length(Cfg.BackupItems) - 1 do
  begin
    Item := Cfg.BackupItems[i];

    case Item.ItemType of
      bitFile:
      begin
        fn := ExtractFileName(Item.Value);
        pathSrc := ExtractFilePath(Item.Value);
        pathDst := IncludeTrailingBackslash(TmpDir + pathSrc);
        ForceDirectories(pathDst);
        CopyFile(Dir.Main + Item.Value, pathDst + fn);
      end;


      bitFolder:
      begin
        pathDst := TmpDir + Item.Value;
        CopyDirTree(Dir.Main + Item.Value, pathDst, [cffCreateDestDirectory, cffOverwriteFile]);
      end;


      bitFileInFolder:
      begin
        pathSrc := IncludeTrailingBackslash(Dir.Main + Item.Value);

        //Подготовить каталог назначения
        pathDst := IncludeTrailingBackslash(TmpDir + Item.Value);
        ForceDirectories(pathDst);

        List := TStringList.Create;
        Dir.GetFileList(pathSrc, List);

        for j := 0 to List.Count - 1 do
        begin
          fn := List.Strings[j];
          CopyFile(pathSrc + fn, pathDst + fn);
        end;

        List.Free;
      end;
    end;
  end;

  //Запакуем архив
  fn := Dir.Directory[dtBackup] + ArcName + '.zip';
  Zip := TZipper.Create;
  Zip.FileName := Dir.Directory[dtBackup] + ArcName + '.zip';

  List := FindAllFiles(TmpDir, '*.*');
  for i := 0 to List.Count - 1 do
  begin
    ZipEntry := Zip.Entries.AddFileEntry(List.Strings[i]);
    ZipEntry.CompressionLevel := clmax;
    ZipEntry.ArchiveFileName := CreateRelativePath(List.Strings[i], TmpDir);
  end;
  List.Free;

  Zip.ZipAllFiles;
  Zip.Free;
  WriteLn('Создан архив: ', fn);

  //Удалим временный каталог
  DeleteDirectory(TmpDir, False);
end;


procedure Step_DeleteOldBackup;
var
  List: TStringList;
  Time: TDateTime;
  i: Integer;
begin
  WriteLn('');
  WriteLn('УДАЛЕНИЕ СТАРЫХ АРХИВОВ');

  List := FindAllFiles(Dir.Directory[dtBackup], '*.zip');
  for i := 0 to List.Count - 1 do
  begin
    FileAge(List.Strings[i], Time);
    if IncDay(Time, Cfg.BackupKeepDays) < Now then
      DeleteFile(List.Strings[i]);
  end;
  List.Free;
end;


procedure Step_ClearTrash;
var
  i, j: Integer;
  Item: TConfigBackupItem;
  List: TStringList;
begin
  WriteLn('');
  WriteLn('ОЧИСТКА МУСОРА');

  for i := 0 to Length(Cfg.ClearItems) - 1 do
  begin
    Item := Cfg.ClearItems[i];

    case Item.ItemType of
      bitFile:
        DeleteFile(Dir.Main + Item.Value);

      bitFolder:
        DeleteDirectory(Dir.Main + Item.Value, False);

      bitFileInFolder:
      begin
        List := TStringList.Create;
        Dir.GetFileList(Item.Value, List);

        for j := 0 to List.Count - 1 do
          DeleteFile(IncludeTrailingBackslash(Dir.Main + Item.Value) + List.Strings[j]);

        List.Free;
      end;
    end;
  end;
end;


procedure Step_StartServer;
var
  P: TProcess;
  s: String;
begin
  WriteLn('');
  WriteLn('ЗАПУСК СЕРВЕРА: ', DateTimeToStr(Now));

  P := TProcess.Create(nil);
  try
    P.Executable := Dir.Directory[dtServer] + Cfg.ServerEXE;
    p.Options := [poWaitOnExit];

    P.Parameters.Add('-doLogs');
    P.Parameters.Add('-adminLog');
    P.Parameters.Add('-freezeCheck');
    P.Parameters.Add(Format('-config=%s', [IncludeTrailingBackslash(Dir.Main) + Cfg.ServerConfig]));
    P.Parameters.Add(Format('-port=%d', [Cfg.ServerPort]));
    P.Parameters.Add(Format('-cpuCount=%d', [Cfg.ServerCPU]));
    P.Parameters.Add(Format('-limitFPS=%d', [Cfg.ServerFPSLimit]));
    P.Parameters.Add(Format('-profiles=%s', [Dir.Directory[dtProfile]]));
    P.Parameters.Add(Format('-storage=%s', [Dir.Directory[dtStorage]]));

    s := GetModList(Dir.Directory[dtServerMods]);
    P.Parameters.Add(Format('-serverMod=%s', [s]));

    s := Format('%ssteamapps\workshop\content\%d', [Dir.Directory[dtToolsSteamCmd], Cfg.SteamDayZModsID]);
    s := GetModList(s);
    P.Parameters.Add(Format('-mod=%s', [s]));

    WriteLn(P.Parameters.Text);

    P.Execute;

  finally
    P.Free;
  end;
end;


function GetModList(Directory: String): String;
var
  List: TStringList;
  i: Integer;
begin
  List := TStringList.Create;
  List.LineBreak := ';';
  try
    Dir.GetDirectoryList(Directory, List);

    for i := 0 to List.Count - 1 do
      List.Strings[i] := IncludeTrailingBackslash(Directory) + List.Strings[i];

    Result := List.Text;

  finally
    List.Free;
  end;
end;

//Обновление клиентского мода через стим
function UpdateClientModFromSteam(ModID: Int64): Boolean;
var
  P: TProcess;
begin
  Result := False;

  P := TProcess.Create(nil);
  try
    P.Executable := GetSteamCmdFile;
    p.Options := [poWaitOnExit];

    P.Parameters.Add('+login');
    P.Parameters.Add(Cfg.SteamLogin);
    P.Parameters.Add(Cfg.SteamPassword);
    P.Parameters.Add('+workshop_download_item');
    P.Parameters.Add(IntToStr(Cfg.SteamDayZModsID));
    P.Parameters.Add(IntToStr(ModID));
    P.Parameters.Add('validate');
    P.Parameters.Add('+quit');

    P.Execute;

    Result := True;

  finally
    P.Free;
    if not Result then
      WriteLn('Не удалось обновить клиентский мод ', ModID, ' пропуск');
  end;
end;


//Обновить серверный мод
procedure UpdateServerMod(ModName: String);
var
  DstDir, UpdDir, KeySrcDir, KeyDstDir: String;
begin
  DstDir := Dir.Directory[dtServerMods] + ModName;
  UpdDir := Dir.Directory[dtUpdateServerMods] + ModName;

  //Удалить старый каталог со всем содержимым
  if DirectoryExists(DstDir) then
    DeleteDirectory(DstDir, False);

  //Скопировать новый каталог в старое место
  if not CopyDirTree(UpdDir, DstDir, [cffCreateDestDirectory, cffOverwriteFile]) then
  begin
    WriteLn('НЕ УДАЛОСЬ ОБНОВИТЬ СЕРВЕРНЫЙ МОД: ', ModName);
    Exit;
  end;

  //Скопировать ключи в каталог сервера
  KeySrcDir := DstDir + '\keys\';
  KeyDstDir := Dir.Directory[dtServer] + 'keys\';
  CopyDirTree(KeySrcDir, KeyDstDir, [cffCreateDestDirectory, cffOverwriteFile]);

  //Удалить из папки обновлений
  DeleteDirectory(UpdDir, False);
  RemoveDir(UpdDir);
end;


//Основной метод
procedure Work;
label
  Start;
begin
  //Приветствие
  Step_Wellcome;

Start:

  //Чтение конфига
  Step_ReadConfig;

  //Проверка SteamCmd
  if not Step_CheckSteamCmd then
    Exit;

  //Обновление сервера, если не удалось пропустим шаг
  Step_UpdateSever;

  //Обновление серверных модов
  Step_UpdateSeverMods;

  //Обновление клиентских модов
  Step_UpdateClientMods;

  //Почистим мусор
  Step_ClearTrash;

  //Запуск сервера
  Step_StartServer;

  //Бэкап
  Step_Backup;

  //Удалим старые архивы
  Step_DeleteOldBackup;


  goto Start;
end;





//Исполняемый файл SteamCmd
function GetSteamCmdFile: String;
begin
  Result := Dir.Directory[dtToolsSteamCmd] + 'SteamCmd.exe';
end;

//Каталог со скаченными модами
function GetClientModDirectory: String;
begin
  Result := Format('%ssteamapps\workshop\content\%d\', [Dir.Directory[dtToolsSteamCmd], Cfg.SteamDayZModsID]);
end;

//Создание объектов
procedure CreateObjects;
begin
  Cfg := TConfig.Create;
  Dir := TDirectory.Create(ExtractFilePath(ParamStr(0)));
end;


//Удаление объектов
procedure FreeObjects;
begin
  Dir.Free;
  Cfg.Free;
end;


{$R *.res}

begin
  CreateObjects;
  Work;
  FreeObjects;

  ReadLn;
end.

