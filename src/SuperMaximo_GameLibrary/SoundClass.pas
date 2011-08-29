{
SuperMaximo GameLibrary : Sound class unit
by Max Foster

License : http://creativecommons.org/licenses/by/3.0/
}
unit SoundClass;
{$mode objfpc}{$H+}

interface

uses SDL_mixer;

type
  PSound = ^TSound;
  TSound = object
  strict private
    chunk : PMix_Chunk;
    volume_, currentChannel : integer;
    name_ : string;
  public
    //Create a sound with the specified name, loaded up from a sound file
    constructor create(newName, fileName : string);
    destructor destroy;
    function name : string;
    procedure setVolume(percentage : integer; relative : boolean = false); //Set the volume of the sound (0-100)
    function volume : integer;

    //Play the sound with the specified volume (0-100), channel and number of times to loop. Use the default
    //parameters to play the sound at a previously set volume (may be 100), in any free channel and with 0 loops
    function play(newVolume : integer = -1; channel : integer = -1; loops : integer = 0) : integer;
    function playing : boolean;
    procedure stop;
    procedure fade(time : longint); //Fade out the sound over a certain time period
    procedure setSoundPosition(angle : integer = 90; distance : integer = 0); //Set the virtual position of the sound in 3D space
  end;

//Allocate a number of channels to be used in sound playback (a good number is 16)
procedure allocateSoundChannels(channels : word);

function sound(searchName : string) : PSound;
function sound(channel : integer) : PSound;
function addSound(newName, fileName : string) : PSound;
procedure destroySound(searchName : string);
procedure destroySound(channel : integer);
procedure destroyAllSounds;

implementation

uses SysUtils, Classes;

var
  allSounds : array['a'..'z'] of TList;
  allChannels : array of PSound;

procedure allocateSoundChannels(channels : word);
var
  i : word;
begin
  Mix_AllocateChannels(channels);
  i := length(allChannels);
  if (i > channels) then setLength(allChannels, channels)
else
  if (i < channels) then
  begin
    setLength(allChannels, channels);
    for i := i to channels-1 do allChannels[i] := nil;
  end;
end;

constructor TSound.create(newName, fileName : string);
begin
  name_ := newName;
  chunk := Mix_LoadWAV(pchar(setDirSeparators(fileName)));
  volume_ := 100;
  currentChannel := -1;
end;

destructor TSound.destroy;
begin
  Mix_FreeChunk(chunk);
end;

function TSound.name : string;
begin
  result := name_;
end;

procedure TSound.setVolume(percentage : integer; relative : boolean);
begin
  if (relative) then volume_ += percentage else volume_ := percentage;
  if (volume_ > 100) then volume_ := 100 else if (volume_ < 0) then volume_ := 0;
  if (currentChannel > -1) then Mix_Volume(currentChannel, round((MIX_MAX_VOLUME/100)*volume_));
end;

function TSound.volume : integer;
begin
  result := volume_;
end;

function TSound.play(newVolume : integer = -1; channel : integer = -1; loops : integer = 0) : integer;
begin
  currentChannel := Mix_PlayChannel(channel, chunk, loops);
  if (currentChannel > -1) then
  begin
    allChannels[currentChannel] := @self;
    if (newVolume > -1) then volume_ := newVolume;
    Mix_Volume(currentChannel, round((MIX_MAX_VOLUME/100)*volume_));
  end;
  result := currentChannel;
end;

function TSound.playing : boolean;
begin
  if (Mix_GetChunk(currentChannel) = chunk) then result := (Mix_Playing(currentChannel) = 1) else result := false;
end;

procedure TSound.stop;
begin
  if (Mix_GetChunk(currentChannel) = chunk) then Mix_HaltChannel(currentChannel);
end;

procedure TSound.fade(time : longint);
begin
  if (Mix_GetChunk(currentChannel) = chunk) then Mix_FadeOutChannel(currentChannel, time);
end;

procedure TSound.setSoundPosition(angle : integer = 90; distance : integer = 0);
begin
  if (Mix_GetChunk(currentChannel) = chunk) then Mix_SetPosition(currentChannel, angle, distance);
end;

function sound(searchName : string) : PSound;
var
  letter : char;
  i : word;
  tempSound : PSound;
begin
  letter := searchName[1];
  result := nil;
  if (allSounds[letter].count > 0) then
  begin
    for i := 0 to allSounds[letter].count-1 do
    begin
      tempSound := PSound(allSounds[letter][i]);
      if (tempSound^.name = searchName) then result := tempSound;
    end;
  end;
end;

function sound(channel : integer) : PSound;
begin
  result := allChannels[channel];
end;

function addSound(newName, fileName : string) : PSound;
var
  letter : char;
begin
  letter := newName[1];
  allSounds[letter].add(new(PSound, create(newName, fileName)));
  result := allSounds[letter].last;
end;

procedure destroySound(searchName : string);
var
  letter : char;
  i : word;
  tempSound : PSound;
begin
  letter := searchName[1];
  if (allSounds[letter].count > 0) then
  begin
    for i := 0 to allSounds[letter].count-1 do
    begin
      tempSound := PSound(allSounds[letter][i]);
      if (tempSound^.name = searchName) then
      begin
        dispose(tempSound, destroy);
        allSounds[letter].delete(i);
        break;
      end;
    end;
  end;
end;

procedure destroySound(channel : integer);
var
  letter : char;
  i : word;
  tempSound : PSound;
begin
  if (allChannels[channel] <> nil) then
  begin
    letter := allChannels[channel]^.name[1];
    if (allSounds[letter].count > 0) then
    begin
      for i := 0 to allSounds[letter].count-1 do
      begin
        tempSound := PSound(allSounds[letter][i]);
        if (tempSound^.name = allChannels[channel]^.name) then
        begin
          dispose(tempSound, destroy);
          allSounds[letter].delete(i);
          allChannels[channel] := nil;
          break;
        end;
      end;
    end;
  end;
end;

procedure destroyAllSounds;
var
  i : char;
  j : integer;
  tempSound : PSound;
begin
  for i := 'a' to 'z' do
  begin
    if (allSounds[i].count > 0) then
    begin
      for j := 0 to allSounds[i].count-1 do
      begin
        tempSound := PSound(allSounds[i][j]);
        dispose(tempSound, destroy);
      end;
      allSounds[i].clear;
    end;
  end;
  for j := 0 to length(allChannels)-1 do allChannels[j] := nil
end;

procedure initializeAllSounds;
var
  i : char;
begin
  for i := 'a' to 'z' do
  begin
    allSounds[i] := TList.create;
  end;
end;

procedure finalizeAllSounds;
var
  i : char;
begin
  for i := 'a' to 'z' do
  begin
    allSounds[i].destroy;
  end;
end;

initialization

initializeAllSounds;

finalization

finalizeAllSounds;

end.
