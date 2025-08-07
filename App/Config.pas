unit Config;

{$mode ObjFPC}{$H+}

interface

type
  TConfig = class
  public
    //Настройки профиля
    SteamLogin: String;
    SteamPassword: String;

    //Настройки игры
    SteamDayZID: Integer;

  public
    procedure LoadFromFile(FileName: String);
    procedure SaveToFile(FileName: String);
  end;


implementation

uses
  IniFiles;

const
  SECTION_AUTH = 'Auth';
  KEY_AUTH_LOGIN = 'Login';
  KEY_AUTH_PASSWORD = 'Password';

  SECTION_GAME = 'Game';
  KEY_Game_SteamID = 'SteamID';


procedure TConfig.LoadFromFile(FileName: String);
var
  F: tinifile;
begin
  F := TIniFile.Create(FileName);
  try
    //Настройки профиля
    SteamLogin := f.ReadString(SECTION_AUTH, KEY_AUTH_LOGIN, 'theserverofdream');
    SteamPassword := f.ReadString(SECTION_AUTH, KEY_AUTH_PASSWORD, 'rTKp8NmSjC7YDF6');

    //Настройки игры
    SteamDayZID := f.ReadInteger(SECTION_GAME, KEY_Game_SteamID, 223350);

  finally
    F.Free;
  end;
end;


procedure TConfig.SaveToFile(FileName: String);
var
  F: tinifile;
begin
  F := TIniFile.Create(FileName);
  try
    //Настройки профиля
    f.WriteString(SECTION_AUTH, KEY_AUTH_LOGIN, SteamLogin);
    f.WriteString(SECTION_AUTH, KEY_AUTH_PASSWORD, SteamPassword);

    //Настройки игры
    f.WriteInteger(SECTION_GAME, KEY_Game_SteamID, SteamDayZID);

  finally
    F.Free;
  end;
end;



end.

