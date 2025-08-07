program DayZSentinel;

{$mode objfpc}{$H+}
{$codepage UTF-8}

uses
  SysUtils, process, Classes,
  Config, Directory;

var
  Cfg: TConfig;
  Dir: TDirectory;


function GetSteamCmdFile: String; forward;



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
  sMod, sDir, uDir: String;
begin
  List := TStringList.Create;
  try
    Dir.GetDirectoryList(Dir.Directory[dtUpdateServerMods], List);

    for i := 0 to List.Count - 1 do
    begin
      sMod := List.Strings[i];
      sDir := Dir.Directory[dtServerMods] + sMod;
      uDir := Dir.Directory[dtUpdateServerMods] + sMod;;

      WriteLn('');
      WriteLn('ОБНОВЛЕНИЕ СЕРВЕРНОГО МОДА: ' + sMod);

      //Удалить мод в каталоге



    end;

  finally
    List.Free;
  end;
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
  //Step_UpdateSever;

  //Обновление серверных модов
  Step_UpdateSeverMods;

  //Обновление клиентских модов

  //Бэкап

  //Запуск сервера

  //goto Start;
end;





//Исполняемый файл SteamCmd
function GetSteamCmdFile: String;
begin
  Result := Dir.Directory[dtToolsSteamCmd] + 'SteamCmd.exe';
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

