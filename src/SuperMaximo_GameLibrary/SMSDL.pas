{
SuperMaximo GameLibrary : SuperMaximo SDL unit
by Max Foster

License : http://creativecommons.org/licenses/by/3.0/
}
unit SMSDL;

interface

uses SDL;

const
  SDL_INIT_TIMER = $00000001;
  SDL_INIT_AUDIO = $00000010;
  SDL_INIT_VIDEO = $00000020;
  SDL_INIT_CDROM = $00000100;
  SDL_INIT_JOYSTICK = $00000200;
  SDL_INIT_NOPARACHUTE = $00100000;
  SDL_INIT_EVENTTHREAD = $01000000;
  SDL_INIT_EVERYTHING = $0000FFFF;

procedure initSDL(flags : Uint32);

procedure quitSDL;

procedure wait(time : longint);

implementation

procedure initSDL(flags : Uint32);
begin
  if (flags <> SDL_INIT_NOPARACHUTE) then SDL_Init(flags or SDL_INIT_NOPARACHUTE) else SDL_Init(SDL_INIT_NOPARACHUTE);
end;

procedure quitSDL;
begin
  SDL_Quit;
end;

procedure wait(time : longint);
begin
  SDL_Delay(time);
end;

end.
