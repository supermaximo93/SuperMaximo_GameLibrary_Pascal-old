{
SuperMaximo GameLibrary : Audio unit
by Max Foster

License : http://creativecommons.org/licenses/by/3.0/
}
unit Audio;
{$mode objfpc}{$H+}

interface

//Initialise audio, with a specific number of audio channels
procedure initAudio(channels : word = 16);
procedure quitAudio;

//Set a sound channel to simulate coming from a certain position in 3D space
procedure soundPosition(channel : integer; angle : integer = 90; distance : integer = 0);

//Set the music volume (0-100)
procedure musicVolume(percentage : integer);

procedure pauseMusic;
procedure resumeMusic;
procedure restartMusic;
procedure stopMusic;
procedure fadeMusic(time : longint);//Fade out the music within a time period

implementation

uses SDL_mixer, SoundClass;

procedure initAudio(channels : word = 16);
begin
   Mix_OpenAudio(44100, MIX_DEFAULT_FORMAT, 2, 4096);
   allocateSoundChannels(channels);
end;

procedure quitAudio;
begin
  Mix_CloseAudio;
end;

procedure soundPosition(channel : integer; angle : integer = 90; distance : integer = 0);
begin
  Mix_SetPosition(channel, angle, distance);
end;

procedure musicVolume(percentage : integer);
begin
  Mix_VolumeMusic(round((MIX_MAX_VOLUME/100)*percentage));
end;

procedure pauseMusic;
begin
  Mix_PauseMusic();
end;

procedure resumeMusic;
begin
  Mix_ResumeMusic();
end;

procedure restartMusic;
begin
  Mix_RewindMusic();
end;

procedure stopMusic;
begin
  Mix_HaltMusic();
end;

procedure fadeMusic(time : longint);
begin
  Mix_FadeOutMusic(time);
end;

end.
