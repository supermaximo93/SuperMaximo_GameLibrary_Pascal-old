{
SuperMaximo GameLibrary : Music class unit
by Max Foster

License : http://creativecommons.org/licenses/by/3.0/
}
unit MusicClass;
{$mode objfpc}{$H+}

interface

uses SDL_mixer;

type
  PMusic = ^TMusic;
  TMusic = object
  strict private
    mixMusic : PMix_Music;
    name_ : string;
  public
    //Load music with the specified name from the file provided
    constructor create(newName, fileName : string);
    destructor destroy;
    function name : string;
    procedure play;
  end;

function music(searchName : string) : PMusic;
function addMusic(newName, fileName : string) : PMusic;
procedure destroyMusic(searchName : string);
procedure destroyAllMusic;

implementation

uses SysUtils, Classes;

var
  allMusic : array['a'..'z'] of TList;

constructor TMusic.create(newName, fileName : string);
begin
  name_ := newName;
  mixMusic := Mix_LoadMUS(pchar(setDirSeparators(fileName)));
end;

destructor TMusic.destroy;
begin
  Mix_FreeMusic(mixMusic);
end;

function TMusic.name : string;
begin
  result := name_;
end;

procedure TMusic.play;
begin
  Mix_PlayMusic(mixMusic, -1);
end;

function music(searchName : string) : PMusic;
var
  letter : char;
  i : word;
  tempMusic : PMusic;
begin
  letter := searchName[1];
  result := nil;
  if (allMusic[letter].count > 0) then
  begin
    for i := 0 to allMusic[letter].count-1 do
    begin
      tempMusic := PMusic(allMusic[letter][i]);
      if (tempMusic^.name = searchName) then result := tempMusic;
    end;
  end;
end;

function addMusic(newName, fileName : string) : PMusic;
var
  letter : char;
begin
  letter := newName[1];
  allMusic[letter].add(new(PMusic, create(newName, fileName)));
  result := allMusic[letter].last;
end;

procedure destroyMusic(searchName : string);
var
  letter : char;
  i : word;
  tempMusic : PMusic;
begin
  letter := searchName[1];
  if (allMusic[letter].count > 0) then
  begin
    for i := 0 to allMusic[letter].count-1 do
    begin
      tempMusic := PMusic(allMusic[letter][i]);
      if (tempMusic^.name = searchName) then
      begin
        dispose(tempMusic, destroy);
        allMusic[letter].delete(i);
        break;
      end;
    end;
  end;
end;

procedure destroyAllMusic;
var
  i : char;
  j : integer;
  tempMusic : PMusic;
begin
  for i := 'a' to 'z' do
  begin
    if (allMusic[i].count > 0) then
    begin
      for j := 0 to allMusic[i].count-1 do
      begin
        tempMusic := PMusic(allMusic[i][j]);
        dispose(tempMusic, destroy);
      end;
      allMusic[i].clear;
    end;
  end;
end;

procedure initializeAllMusic;
var
  i : char;
begin
  for i := 'a' to 'z' do
  begin
    allMusic[i] := TList.create;
  end;
end;

procedure finalizeAllMusic;
var
  i : char;
begin
  for i := 'a' to 'z' do
  begin
    allMusic[i].destroy;
  end;
end;

initialization

initializeAllMusic;

finalization

finalizeAllMusic;

end.
