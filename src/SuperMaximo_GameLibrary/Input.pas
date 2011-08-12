{
SuperMaximo GameLibrary : Input unit
by Max Foster

License : http://creativecommons.org/licenses/by/3.0/
}
unit Input;
{$mode objfpc}{$H+}

interface

const
  DPAD_UP = 0;
  DPAD_DOWN = 1;
  DPAD_LEFT = 2;
  DPAD_RIGHT = 3;

procedure initInput;
procedure quitInput;

procedure refreshEvents;

function keyPressed(code : integer) : boolean;

procedure setMousePosition(x, y : integer);
procedure hideCursor;
procedure showCursor;

function mouseX : integer;
function mouseY : integer;

function mouseLeft : boolean;
function mouseRight : boolean;
function mouseMiddle : boolean;
function mouseOther : boolean;

function mouseWheelUp : boolean;
function mouseWheelDown : boolean;

function closeClicked : boolean;

function joystickCount : integer;
function joystickButtonPressed(code : integer; controllerId : integer = 0) : boolean;
function joystickDpadPressed(code : integer; controllerId : integer = 0) : boolean;
function joystickAxisValue(axis : integer; controllerId : integer = 0) : integer;

procedure resetEvents;

implementation

uses SDL;

type
  joystick = record
    joystickHandle : PSDL_Joystick;
    id : integer;
    buttons : array[0..15] of boolean;
    dpad : array[0..3] of boolean;
    axis : array[0..15] of Sint16;
  end;

var
  event : TSDL_Event;
  keys : array[0..320] of boolean;
  mouseLeft_, mouseRight_, mouseMiddle_, mouseOther_, mouseWheelUp_, mouseWheelDown_, closeClicked_, eventsRefreshed : boolean;
  mouseX_, mouseY_, joystickCount_ : integer;
  joysticks : array of joystick;

procedure initJoystick(var newJoystick : joystick; num : integer);
var
  i : integer;
begin
  newJoystick.id := num;
  newJoystick.joystickHandle := SDL_JoystickOpen(num);
  for i := 0 to 15 do
  begin
    newJoystick.buttons[i] := false;
    newJoystick.axis[i] := 0;
  end;
  for i := 0 to 3 do newJoystick.dpad[i] := false;
end;

procedure initInput;
var
  i : word;
  newJoystick : joystick;
begin
  for i := 0 to length(keys)-1 do keys[i] := false;
  mouseLeft_ := false;
  mouseRight_ := false;
  mouseMiddle_ := false;
  mouseOther_ := false;
  mouseWheelUp_ := false;
  mouseWheelDown_ := false;
  closeClicked_ := false;
  eventsRefreshed := false;
  mouseX_ := 0;
  mouseY_ := 0;
  SDL_JoystickEventState(SDL_ENABLE);
  joystickCount_ := SDL_NumJoysticks;
  if (joystickCount_ > 0) then
  begin
    for i := 0 to SDL_NumJoysticks-1 do
    begin
      initJoystick(newJoystick, i);
      setLength(joysticks, length(joysticks)+1);
      joysticks[length(joysticks)-1] := newJoystick;
    end;
  end;
end;

procedure quitInput;
var
  i : word;
begin
  if (length(joysticks) > 0) then for i := 0 to length(joysticks)-1 do SDL_JoystickClose(joysticks[i].joystickHandle);
  setLength(joysticks, 0);
  SDL_JoystickEventState(SDL_DISABLE);
end;

procedure refreshEvents;
begin
  while (SDL_PollEvent(@event) = 1) do
  begin
    case event.type_ of
    SDL_KEYDOWN: keys[event.key.keysym.sym] := true;
    SDL_KEYUP: keys[event.key.keysym.sym] := false;
    SDL_MOUSEMOTION:
      begin
        mouseX_ := event.motion.x;
        mouseY_ := event.motion.y;
      end;
    SDL_MOUSEBUTTONDOWN:
      begin
        case event.button.button of
        SDL_BUTTON_LEFT: mouseLeft_ := true;
        SDL_BUTTON_RIGHT: mouseRight_ := true;
        SDL_BUTTON_MIDDLE: mouseMiddle_ := true;
        SDL_BUTTON_WHEELUP: mouseWheelUp_ := true;
        SDL_BUTTON_WHEELDOWN: mouseWheelDown_ := true;
        else mouseOther_ := true;
        end;
      end;
    SDL_MOUSEBUTTONUP:
      begin
        case event.button.button of
        SDL_BUTTON_LEFT: mouseLeft_ := false;
        SDL_BUTTON_RIGHT: mouseRight_ := false;
        SDL_BUTTON_MIDDLE: mouseMiddle_ := false;
        SDL_BUTTON_WHEELUP: mouseWheelUp_ := false;
        SDL_BUTTON_WHEELDOWN: mouseWheelDown_ := false;
        else mouseOther_ := false;
        end;
      end;
    SDL_QUITEV: closeClicked_ := true;
    SDL_JOYBUTTONDOWN: joysticks[event.jbutton.which].buttons[event.jbutton.button] := true;
    SDL_JOYBUTTONUP: joysticks[event.jbutton.which].buttons[event.jbutton.button] := false;
    SDL_JOYHATMOTION:
      begin
        case event.jhat.value of
        SDL_HAT_UP:
          begin
            joysticks[event.jhat.which].dpad[0] := true;
            joysticks[event.jhat.which].dpad[1] := false;
          end;
        SDL_HAT_DOWN:
          begin
            joysticks[event.jhat.which].dpad[1] := true;
            joysticks[event.jhat.which].dpad[0] := false;
          end;
        SDL_HAT_LEFT:
          begin
            joysticks[event.jhat.which].dpad[2] := true;
            joysticks[event.jhat.which].dpad[3] := false;
          end;
        SDL_HAT_RIGHT:
          begin
            joysticks[event.jhat.which].dpad[3] := true;
            joysticks[event.jhat.which].dpad[2] := false;
          end;
        SDL_HAT_CENTERED:
          begin
            joysticks[event.jhat.which].dpad[0] := false;
            joysticks[event.jhat.which].dpad[1] := false;
            joysticks[event.jhat.which].dpad[2] := false;
            joysticks[event.jhat.which].dpad[3] := false;
          end;
        end;
      end;
    SDL_JOYAXISMOTION: joysticks[event.jaxis.which].axis[event.jaxis.axis] := event.jaxis.value;
    end;
  end;
  eventsRefreshed := true;
end;

function keyPressed(code : integer) : boolean;
begin
  if (not eventsRefreshed) then refreshEvents;
  result := keys[code];
end;

procedure setMousePosition(x, y : integer);
begin
  SDL_WarpMouse(x, y);
end;

procedure hideCursor;
begin
  SDL_ShowCursor(0);
end;

procedure showCursor;
begin
  SDL_ShowCursor(1);
end;

function mouseX : integer;
begin
  if (not eventsRefreshed) then refreshEvents;
  result := mouseX_;
end;

function mouseY : integer;
begin
  if (not eventsRefreshed) then refreshEvents;
  result := mouseY_;
end;

function mouseLeft : boolean;
begin
  if (not eventsRefreshed) then refreshEvents;
  result := mouseLeft_;
end;

function mouseRight : boolean;
begin
  if (not eventsRefreshed) then refreshEvents;
  result := mouseRight_;
end;

function mouseMiddle : boolean;
begin
  if (not eventsRefreshed) then refreshEvents;
  result := mouseMiddle_;
end;

function mouseOther : boolean;
begin
  if (not eventsRefreshed) then refreshEvents;
  result := mouseOther_;
end;

function mouseWheelUp : boolean;
begin
  if (not eventsRefreshed) then refreshEvents;
  result := mouseWheelUp_;
end;

function mouseWheelDown : boolean;
begin
  if (not eventsRefreshed) then refreshEvents;
  result := mouseWheelDown_;
end;

function closeClicked : boolean;
begin
  if (not eventsRefreshed) then refreshEvents;
  result := closeClicked_;
end;

function joystickCount : integer;
begin
  result := joystickCount_;
end;

function joystickButtonPressed(code : integer; controllerId : integer = 0) : boolean;
begin
  if (controllerId < joystickCount_) then
  begin
    if (not eventsRefreshed) then refreshEvents;
    result := joysticks[controllerId].buttons[code];
  end else result := false;
end;

function joystickDpadPressed(code : integer; controllerId : integer = 0) : boolean;
begin
  if (controllerId < joystickCount_) then
  begin
    if (not eventsRefreshed) then refreshEvents;
    result := joysticks[controllerId].dpad[code];
  end else result := false;
end;

function joystickAxisValue(axis : integer; controllerId : integer = 0) : integer;
begin
  if (controllerId < joystickCount_) then
  begin
    if (not eventsRefreshed) then refreshEvents;
    result := joysticks[controllerId].axis[axis];
  end else result := 0;
end;

procedure resetEvents;
begin
  eventsRefreshed := false;
end;

end.
