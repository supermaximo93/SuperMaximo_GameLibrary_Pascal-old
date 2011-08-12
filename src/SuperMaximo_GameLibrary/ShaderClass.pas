{
SuperMaximo GameLibrary : Shader class unit
by Max Foster

License : http://creativecommons.org/licenses/by/3.0/
}
unit ShaderClass;
{$mode objfpc}{$H+}

interface

uses dglOpenGL;

const
  //Shader attribute constants
  VERTEX_ATTRIBUTE = 0;
  NORMAL_ATTRIBUTE = 1;
  COLOR0_ATTRIBUTE = 2;
  COLOR1_ATTRIBUTE = 3;
  COLOR2_ATTRIBUTE = 4;
  TEXTURE0_ATTRIBUTE = 5;
  EXTRA0_ATTRIBUTE = 6;
  EXTRA1_ATTRIBUTE = 7;
  EXTRA2_ATTRIBUTE = 8;
  EXTRA3_ATTRIBUTE = 9;
  EXTRA4_ATTRIBUTE = 10;

  //Shader uniform location constants
  MODELVIEW_LOCATION = 0;
  PROJECTION_LOCATION = 1;
  TEXSAMPLER_LOCATION = 2;
  EXTRA0_LOCATION = 3;
  EXTRA1_LOCATION = 4;
  EXTRA2_LOCATION = 5;
  EXTRA3_LOCATION = 6;
  EXTRA4_LOCATION = 7;
  EXTRA5_LOCATION = 8;
  EXTRA6_LOCATION = 9;
  EXTRA7_LOCATION = 10;
  EXTRA8_LOCATION = 11;
  EXTRA9_LOCATION = 12;
  TEXCOMPAT_LOCATION = 13;

type
  matrix4d = array[0..15] of GLfloat;
  matrix3d = array[0..8] of GLfloat;

  PShader = ^TShader;
  TShader = object
  strict private
    program_ : GLhandle;
    uniformLocation_ : array[0..TEXCOMPAT_LOCATION] of GLint;
    name_ : string;
  public
    constructor create(newName, vertexShaderFile, fragmentShaderFile : string; enums : array of integer; attributeNames : array of string);
    destructor destroy;

    function name : string;
    procedure bind;
    procedure use;
    function getProgram : GLhandle;
    function setUniformLocation(dstLocation : integer; locationName : string) : GLint;
    function uniformLocation(location : integer) : GLint;

    procedure setUniform16(location : integer; data : matrix4d);
    procedure setUniform9(location : integer; data : matrix3d);
    procedure setUniform1(location, data : integer);
    procedure setUniform1(location : integer; data : GLfloat);
    procedure setUniform4(location : integer; data1, data2, data3, data4 : GLfloat);
  end;

function shader(searchName : string) : PShader;
function addShader(newName, vertexShaderFile, fragmentShaderFile : string; enums : array of integer; attributeNames : array of string) : PShader;
procedure destroyShader(searchName : string);
procedure destroyAllShaders;

implementation

uses SysUtils, Classes, Display;

var
  allShaders : array['a'..'z'] of TList;

constructor TShader.create(newName, vertexShaderFile, fragmentShaderFile : string; enums : array of integer; attributeNames : array of string);
var
  i, success, null : integer;
  shaderText, tempStr : string;
  shaderFile : text;
  vertexShader, fragmentShader : GLhandle;
  log : array[0..1023] of char;
  arr : array[0..0] of PGLChar;
begin
  name_ := newName;
  program_ := GLuint(nil);
  for i := 0 to EXTRA9_LOCATION do uniformLocation_[i] := -1;
  shaderText := '';
  vertexShaderFile := setDirSeparators(vertexShaderFile);
  fragmentShaderFile := setDirSeparators(fragmentShaderFile);

  assign(shaderFile, vertexShaderFile);
  reset(shaderFile);
  repeat
    readln(shaderFile, tempStr);
    shaderText += tempStr+#13#10;
  until Eof(shaderFile);
  shaderText += #0;
  close(shaderFile);

  vertexShader := glCreateShader(GL_VERTEX_SHADER);
  arr[0] := PGLChar(pchar(shaderText));
  glShaderSource(vertexShader, 1, PPGLchar(arr), nil);
  glCompileShader(vertexShader);

  glGetShaderiv(vertexShader, GL_COMPILE_STATUS, @success);
  if (success = GL_FALSE) then
  begin
    writeln('Error with compiling vertex shader ('+vertexShaderFile+')');
    glGetShaderInfoLog(vertexShader, 1024, null, @log);
    writeln(log);
    glDeleteShader(vertexShader);
    exit;
  end;

  shaderText := '';
  assign(shaderFile, fragmentShaderFile);
  reset(shaderFile);
  repeat
    readln(shaderFile, tempStr);
    shaderText += tempStr+#13#10;
  until Eof(shaderFile);
  shaderText += #0;
  close(shaderFile);

  fragmentShader := glCreateShader(GL_FRAGMENT_SHADER);
  arr[0] := PGLChar(pchar(shaderText));
  glShaderSource(fragmentShader, 1, PPGLchar(arr), nil);
  glCompileShader(fragmentShader);

  glGetShaderiv(fragmentShader, GL_COMPILE_STATUS, @success);
  if (success = GL_FALSE) then
  begin
    writeln('Error with compiling fragment shader ('+fragmentShaderFile+')');
    glGetShaderInfoLog(fragmentShader, 1024, null, @log);
    writeln(log);
    glDeleteShader(fragmentShader);
    glDeleteShader(vertexShader);
    exit;
  end;

  program_ := glCreateProgram();
  glAttachShader(program_, vertexShader);
  glAttachShader(program_, fragmentShader);

  for i := 0 to length(enums)-1 do glBindAttribLocation(program_, enums[i], pchar(attributeNames[i]));

  glLinkProgram(program_);
  glDeleteShader(vertexShader);
  glDeleteShader(fragmentShader);

  glGetProgramiv(program_, GL_LINK_STATUS, @success);
  if (success = GL_FALSE) then
  begin
    writeln('Error with linking shader program ('+vertexShaderFile+', '+fragmentShaderFile+')');
    glGetProgramInfoLog(program_, 1024, null, @log);
    writeln(log);
    glDeleteProgram(program_);
    exit;
  end;
end;

destructor TShader.destroy;
begin
  glDeleteProgram(program_);
end;

function TShader.name : string;
begin
  result := name_;
end;

procedure TShader.bind;
begin
  globalBindShader(@self);
end;

procedure TShader.use;
begin
  glUseProgram(program_);
end;

function TShader.getProgram : GLhandle;
begin
  result := program_;
end;

function TShader.setUniformLocation(dstLocation : integer; locationName : string) : GLint;
begin
  uniformLocation_[dstLocation] := glGetUniformLocation(program_, pchar(locationName));
  result := uniformLocation_[dstLocation];
end;

function TShader.uniformLocation(location : integer) : GLint;
begin
  result := uniformLocation_[location];
end;

procedure TShader.setUniform16(location : integer; data : matrix4d);
begin
  glUniformMatrix4fv(uniformLocation_[location], 1, false, @data);
end;

procedure TShader.setUniform9(location : integer; data : matrix3d);
begin
  glUniformMatrix3fv(uniformLocation_[location], 1, false, @data);
end;

procedure TShader.setUniform1(location, data : integer);
begin
  glUniform1i(uniformLocation_[location], data);
end;

procedure TShader.setUniform1(location : integer; data : GLFloat);
begin
  glUniform1f(uniformLocation_[location], data);
end;

procedure TShader.setUniform4(location : integer; data1, data2, data3, data4 : GLfloat);
begin
  glUniform4f(uniformLocation_[location], data1, data2, data3, data4);
end;

function shader(searchName : string) : PShader;
var
  letter : char;
  i : word;
  tempShader : PShader;
begin
  letter := searchName[1];
  result := nil;
  if (allShaders[letter].count > 0) then
  begin
    for i := 0 to allShaders[letter].count-1 do
    begin
      tempShader := PShader(allShaders[letter][i]);
      if (tempShader^.name = searchName) then result := tempShader;
    end;
  end;
end;

function addShader(newName, vertexShaderFile, fragmentShaderFile : string; enums : array of integer; attributeNames : array of string) : PShader;
var
  letter : char;
begin
  letter := newName[1];
  allShaders[letter].add(new(PShader, create(newName, vertexShaderFile, fragmentShaderFile, enums, attributeNames)));
  result := allShaders[letter].last;
end;

procedure destroyShader(searchName : string);
var
  letter : char;
  i : word;
  tempShader : PShader;
begin
  letter := searchName[1];
  if (allShaders[letter].count > 0) then
  begin
    for i := 0 to allShaders[letter].count-1 do
    begin
      tempShader := PShader(allShaders[letter][i]);
      if (tempShader^.name = searchName) then
      begin
        dispose(tempShader, destroy);
        allShaders[letter].delete(i);
        break;
      end;
    end;
  end;
end;

procedure destroyAllShaders;
var
  i : char;
  j : integer;
  tempShader : PShader;
begin
  for i := 'a' to 'z' do
  begin
    if (allShaders[i].count > 0) then
    begin
      for j := 0 to allShaders[i].count-1 do
      begin
        tempShader := PShader(allShaders[i][j]);
        dispose(tempShader, destroy);
      end;
      allShaders[i].clear;
    end;
  end;
end;

procedure initializeAllShaders;
var
  i : char;
begin
  for i := 'a' to 'z' do
  begin
    allShaders[i] := TList.create;
  end;
end;

procedure finalizeAllShaders;
var
  i : char;
begin
  for i := 'a' to 'z' do
  begin
    allShaders[i].destroy;
  end;
end;

initialization

initializeAllShaders;

finalization

finalizeAllShaders;

end.
