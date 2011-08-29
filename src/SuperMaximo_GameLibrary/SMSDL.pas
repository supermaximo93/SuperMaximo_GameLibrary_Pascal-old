{
SuperMaximo GameLibrary : SuperMaximo SDL unit
by Max Foster

License : http://creativecommons.org/licenses/by/3.0/
}

{

Thanks for your interest in the SuperMaximo GameLibrary!
I just want to discuss two things that are used across many of the units in the library.

BANKS
=====
For each class, there is a 'bank' associated with it. This allows for quick and easy prototyping
as it means you don't need to manage instances of objects yourself. Taking TTexture as an example,
to create and display an instance's using the banks, you can do:

addTexture("myTexture", <other args>);

writeln(texture("myTexture")^.name);

destroyTexture("myTexture");
//OR
destroyAllTextures;

The bank is indexed which helps speed up the bank search, but I still do not recommend using it
in a CPU intensive game!

RELATIVE
========
Some methods in some classes (especially in GameObject) have a 'relative' parameter, which defaults to
FALSE. If this value is set to TRUE then the amount being passed will be multiplied by a compensation value
and then added to the value that you are modifying. For example, if you wanted an instance of GameObject, with
X position 0, to get to position 60 in exactly one second, with an 'idealFramerate' of 60 (see the Display unit),
regardless of a computers processing speed you can do:

myGameObject.setX(0); //Set the position absolutely

myGameObject.setX(1, true); //Add 1*<frame compensation> to the X value every frame

This is crucial to maintaining a good gameplay experience, because it means no matter what computer you play
the game on, the gameplay will remain the same speed, no matter what the video framerate is!

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
