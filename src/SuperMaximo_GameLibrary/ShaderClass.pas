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
    //Load a shader from the files provided, with arrays of attributes used in the shader
    constructor create(newName, vertexShaderFile, fragmentShaderFile : string; enums : array of integer; attributeNames : array of string);
    destructor destroy;

    function name : string;
    procedure bind; //Bind the shader to be used when an object is drawn
    procedure use;  //Binds the shader right now so that we can send down uniform data to it
    function getProgram : GLhandle;  //Get the OpenGL handle for the program to be used in OpenGL functions
    function setUniformLocation(dstLocation : integer; locationName : string) : GLint; //Tells the shader about uniform variables in the shader
    function uniformLocation(location : integer) : GLint; //Returns the OpenGL location of the uniform variable in the shader

    //Use these procedures to pass data to the shaders. Use the shader uniform location constants for the 'location' parameter
    procedure setUniform16(location : integer; data : matrix4d); //Sends down a 4x4 matrix uniform to the shader
    procedure setUniform9(location : integer; data : matrix3d); //Sends down a 3x3 matrix uniform to the shader
    procedure setUniform1(location, data : integer);  //Sends down a single integer to the shader
    procedure setUniform1(location : integer; data : GLfloat);  //Sends down a single GLfloat (which is equivalent to a Single) to the shader
    procedure setUniform4(location : integer; data1, data2, data3, data4 : GLfloat); //Sends down a GLSL vec4 to the shader with four GLfloats
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

  //Load up the vertex shader file
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

  //Create the vertex shader
  vertexShader := glCreateShader(GL_VERTEX_SHADER);
  arr[0] := PGLChar(pchar(shaderText)); //Very awkward but its the only way I got it to work
  glShaderSource(vertexShader, 1, PPGLchar(arr), nil);
  glCompileShader(vertexShader);

  //Check if our vertex shader compiled, otherwise throw an error and exit
  glGetShaderiv(vertexShader, GL_COMPILE_STATUS, @success);
  if (success = GL_FALSE) then
  begin
    writeln('Error with compiling vertex shader ('+vertexShaderFile+')');
    glGetShaderInfoLog(vertexShader, 1024, null, @log);
    writeln(log);
    glDeleteShader(vertexShader);
    exit;
  end;

  //Load the fragment shader file, tell OpenGL about it and error check it, as before
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

  //Create a GLSL program and attach the shaders to be processed
  program_ := glCreateProgram();
  glAttachShader(program_, vertexShader);
  glAttachShader(program_, fragmentShader);

  //Tell the program about the attributes that have been passed in
  for i := 0 to length(enums)-1 do glBindAttribLocation(program_, enums[i], pchar(attributeNames[i]));

  //Link the program and delete the shaders that are no longer needed
  glLinkProgram(program_);
  glDeleteShader(vertexShader);
  glDeleteShader(fragmentShader);

  //Error check
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
