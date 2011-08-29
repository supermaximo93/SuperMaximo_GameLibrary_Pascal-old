{
SuperMaximo GameLibrary : Display unit
by Max Foster

License : http://creativecommons.org/licenses/by/3.0/
}
unit Display;
{$mode objfpc}{$H+}

interface

uses dglOpenGL, ShaderClass;

const
  //Texture unit constants
  TEXTURE0 = GL_TEXTURE0;
  TEXTURE1 = GL_TEXTURE1;
  TEXTURE2 = GL_TEXTURE2;
  TEXTURE3 = GL_TEXTURE3;
  TEXTURE4 = GL_TEXTURE4;
  TEXTURE5 = GL_TEXTURE5;
  TEXTURE6 = GL_TEXTURE6;
  TEXTURE7 = GL_TEXTURE7;
  TEXTURE8 = GL_TEXTURE8;
  TEXTURE9 = GL_TEXTURE9;
  TEXTURE10 = GL_TEXTURE10;
  TEXTURE11 = GL_TEXTURE11;
  TEXTURE12 = GL_TEXTURE12;
  TEXTURE13 = GL_TEXTURE13;
  TEXTURE14 = GL_TEXTURE14;
  TEXTURE15 = GL_TEXTURE15;

  //Matrix constants
  MODELVIEW_MATRIX = 0;
  PERSPECTIVE_MATRIX = 1;
  ORTHOGRAPHIC_MATRIX = 2;
  PROJECTION_MATRIX = 3;
  IDENTITY_MATRIX = 4; //Make sure IDENTITY_MATRIX is last

  //Blend function constants
  ZERO = GL_ZERO;
  ONE = GL_ONE;
  SRC_COLOR = GL_SRC_COLOR;
  ONE_MINUS_SRC_COLOR = GL_ONE_MINUS_SRC_COLOR;
  DST_COLOR = GL_DST_COLOR;
  ONE_MINUS_DST_COLOR = GL_ONE_MINUS_DST_COLOR;
  SRC_ALPHA = GL_SRC_ALPHA;
  ONE_MINUS_SRC_ALPHA = GL_ONE_MINUS_SRC_ALPHA;
  DST_ALPHA = GL_DST_ALPHA;
  ONE_MINUS_DST_ALPHA = GL_ONE_MINUS_DST_ALPHA;
  CONSTANT_COLOR = GL_CONSTANT_COLOR_EXT;
  ONE_MINUS_CONSTANT_COLOR = GL_ONE_MINUS_CONSTANT_COLOR;
  CONSTANT_ALPHA = GL_CONSTANT_ALPHA;
  ONE_MINUS_CONSTANT_ALPHA = GL_ONE_MINUS_CONSTANT_ALPHA;
  SRC_ALPHA_SATURATE = GL_SRC_ALPHA_SATURATE;

  //Blend function equation constants
  FUNC_ADD = GL_FUNC_ADD;
  FUNC_SUBTRACT = GL_FUNC_SUBTRACT;
  FUNC_REVERSE_SUBTRACT = GL_FUNC_REVERSE_SUBTRACT;
  MIN = GL_MIN;
  MAX = GL_MAX;

type
  customDrawFunctionType = procedure(pClass : Pointer; shader : PShader; data : Pointer);

//Create a window to draw on with the specified dimentions. ('depth' is the depth in 3D space). 'maxFramerate' is the maximum framerate the game
//is allowed to run at. The library automatically compensates for framerate issues. Set maxFramrate to 0 to go at full pelt!
function initDisplay(width, height, depth : word; maxFramerate : word = 0; fullScreen : boolean = false; windowTitle : string = 'My Game') : boolean;
procedure quitDisplay;

//Get the window dimentions and resize it
function screenWidth : integer;
function screenHeight : integer;
function resizeScreen(width, height : word; fullScreen : boolean = false) : boolean;

//Get projection matrices for both perspective and orthographic projection
function getPerspectiveMatrix(left, right, bottom, top, front, back : real) : matrix4d;
function getPerspectiveMatrix(angle, aspectRatio, front, back : real) : matrix4d;
function getOrthographicMatrix(left, right, bottom, top, front, back : real) : matrix4d;

//Bind a shader to be used when an object is drawn (has the same effect as TShader.bind)
procedure globalBindShader(shader : PShader);
function globalBoundShader : PShader;

//Overwrite a sprite the default sprite drawing procedure with your own
procedure globalBindCustomDrawFunction(newCustomDrawFunction : customDrawFunctionType);
function globalBoundCustomDrawFunction : customDrawFunctionType;

//Binds the OpenGL texture unit specified so that you can bind multiple textures at once
//Is equivalent to calling glActiveTexture
procedure bindTextureUnit(textureUnit : GLenum);
function boundTextureUnit : GLenum;

//Bind the matrix stack that you want to manipulate
procedure setMatrix(matrixId : integer);
function currentMatrix : integer;
//Copy the data from the top of one matrix stack to another
procedure copyMatrix(srcMatrixId, dstMatrixId : integer);
//Copy your own matrix data to the top of a matrix stack
procedure copyMatrix(srcMatrix : matrix4d; dstMatrixId : integer);
//Return the matrix data on the top of the matrix stack
function getMatrix(matrixId : integer) : matrix4d;
//Push the current matrix data onto the stack, which effectively 'saves' the matrix data
//and gives you another copy of the matrix to use
procedure pushMatrix;
//Pop the current matrix of the stack and go back to the matrix state that you were using before
procedure popMatrix;

//Perform a translation with the specified amounts in each axis
procedure translateMatrix(x, y, z : real);
//Perform a rotation with the specified angle, and ratios of that angle in each axis
procedure rotateMatrix(angle, x, y, z : real);
//Scale the matrix up or down in each axis
procedure scaleMatrix(xScale, yScale, zScale : real);

//Multiply two 4x4 matrices together
function multiplyMatrix(operandMatrix, multiplierMatrix : matrix4d) : matrix4d;
//Set a 4x4 matrix to the identity matrix
procedure initIdentityMatrix(var dstMatrix : matrix4d);

//Swaps the framebuffers and tells the Input unit that it can gather input data again. This procedure MUST be called at least once
//in a game loop!
procedure refreshScreen;
function getFramerate : word;
function getTickDifference : word; //Get the time difference between frames

//Set the 'ideal' framerate. This is the speed that the game will simulate, independently of what framerate the game is actually at
//This means that people with slower computers will not have the gameplay slowed down, and people with faster computers
//will not have the gameplay sped up. This can also be useful for slow motion effects!
procedure setIdealFramerate(newIdealFramerate : word);
function getIdealFramerate : word;

//Multiply values that you want to increment by independently of the framerate to the return value of this function.
//I.e. if you want A to equal 60 (starting from 0) in exactly one second when the idealFramerate is set to 60, just do:
//A += compensation;
//And A will equal 60 in one second (with an idealFramerate of 60) on any computer! For 120 in one second just use 2.0*compensation, etc.
function compensation : real;

//Enable/disable colour blending (have a look at some OpenGL documentation on glBlendFunc and glBlendEquation for a detailed explanation)
procedure enableBlending(srcBlendFunc : GLenum = ONE; dstBlendFunc : GLenum = ZERO; blendFuncEquation : GLenum = FUNC_ADD);
procedure disableBlending;
function blendingEnabled : boolean;

//Enable/disable depth testing in 3D space
procedure enableDepthTesting;
procedure disableDepthTesting;
function depthTestingEnabled : boolean;

//Return the OpenGL and GLSL version
function openGlVersion : real;
function glSlVersion : real;

//Return whether Vertex Array Objects are supported
function vertexArrayObjectSupported : boolean;

implementation

uses SDL, math, SysUtils, Input, GraphicalAssetClasses;

const
  STACK_SIZE = 64;

var
  screen : PSDL_Surface;
  screenW, screenH, screenD, currentMatrixId, maximumFramerate, tickDifference, idealFramerate, framerate : word;
  matrix : array[0..IDENTITY_MATRIX] of matrix4d;
  matrixStack : array[0..IDENTITY_MATRIX-1] of array[0..STACK_SIZE-1] of matrix4d;
  matrixLevel : array[0..IDENTITY_MATRIX-1] of integer;
  blendingEnabled_, depthTestingEnabled_, vertexArrayObjectSupported_ : boolean;
  boundShader_ : PShader = nil;
  customDrawFunction : customDrawFunctionType = nil;
  boundTextureUnit_ : GLenum = TEXTURE0;
  ticks, lastTicks : Uint32;
  openGlVersion_, glSlVersion_, compensation_ : real;

function initDisplay(width, height, depth : word; maxFramerate : word = 0; fullScreen : boolean = false; windowTitle : string = 'My Game') : boolean;
var
  i : integer;
begin
  if ((width > 0) and (height > 0)) then
  begin
    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
    if (fullScreen) then screen := SDL_SetVideoMode(width, height, 32, SDL_OPENGL or SDL_FULLSCREEN) else screen := SDL_SetVideoMode(width, height, 32, SDL_OPENGL);
    if (screen = nil) then
    begin
      result := false;
      exit;
    end;
    initOpenGL;
    readExtensions;

    vertexArrayObjectSupported_ := (dglCheckExtension('GL_ARB_vertex_array_object') or dglCheckExtension('GL_ATI_vertex_array_object') or dglCheckExtension('GL_APPLE_vertex_array_object')
      or (openGlVersion >= 3.0));

    SDL_WM_SetCaption(pchar(windowTitle), pchar(windowTitle));
    screenW := width;
    screenH := height;
    screenD := depth;
    framerate := 0;
    maximumFramerate := maxFramerate;
    if (maxFramerate > 0) then idealFramerate := maxFramerate;
    ticks := SDL_GetTicks;
    lastTicks := 0;
    tickDifference := 1;
    idealFramerate := 60;

    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LEQUAL);
    depthTestingEnabled_ := true;
    glDisable(GL_BLEND);
    blendingEnabled_ := false;

    initIdentityMatrix(matrix[IDENTITY_MATRIX]);
    initIdentityMatrix(matrix[MODELVIEW_MATRIX]);
    initIdentityMatrix(matrix[PROJECTION_MATRIX]);
    matrix[PERSPECTIVE_MATRIX] := getPerspectiveMatrix(45.0, width/height, 1.0, depth);
    matrix[ORTHOGRAPHIC_MATRIX] := getOrthographicMatrix(0.0, screenW, screenH, 0.0, 1.0, depth);

    matrixStack[MODELVIEW_MATRIX][0] := matrix[MODELVIEW_MATRIX];
    matrixStack[PERSPECTIVE_MATRIX][0] := matrix[PERSPECTIVE_MATRIX];
    matrixStack[ORTHOGRAPHIC_MATRIX][0] := matrix[ORTHOGRAPHIC_MATRIX];
    matrixStack[PROJECTION_MATRIX][0] := matrix[PROJECTION_MATRIX];

    for i := 0 to IDENTITY_MATRIX-1 do matrixLevel[i] := 0;

    currentMatrixId := MODELVIEW_MATRIX;

    glViewport(0, 0, width, height);

    glClearColor(0.0, 0.0, 0.0, 0.0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    result := true;
  end else result := false;
end;

procedure quitDisplay;
begin

end;

function screenWidth : integer;
begin
  result := screenW;
end;

function screenHeight : integer;
begin
  result := screenH;
end;

function screenDepth : integer;
begin
  result := screenD;
end;

function resizeScreen(width, height : word; fullScreen : boolean = false) : boolean;
begin
  if ((width > 0) and (height > 0) and (length(matrixStack[ORTHOGRAPHIC_MATRIX]) = 1)) then
  begin
    SDL_FreeSurface(screen);
    if (fullScreen) then screen := SDL_SetVideoMode(width, height, 32, SDL_OPENGL or SDL_FULLSCREEN) else screen := SDL_SetVideoMode(width, height, 32, SDL_OPENGL);
    if (screen = nil) then
    begin
      result := false;
    end;
    screenW := width;
    screenH := height;
    matrix[ORTHOGRAPHIC_MATRIX] := getOrthographicMatrix(0.0, screenW, screenH, 0.0, 1.0, screenD);
    matrix[PERSPECTIVE_MATRIX] := getPerspectiveMatrix(45.0, screenW/screenH, 1.0, screenD);
    glViewport(0, 0, width, height);
    result := true;
  end else result := false;
end;

function getPerspectiveMatrix(left, right, bottom, top, front, back : real) : matrix4d;
var
  returnMatrix : matrix4d;
begin
  returnMatrix[0] := (2.0*front)/(right-left);
  returnMatrix[1] := 0.0;
  returnMatrix[2] := 0.0;
  returnMatrix[3] := 0.0;

  returnMatrix[4] := 0.0;
  returnMatrix[5] := (2.0*front)/(top-bottom);
  returnMatrix[6] := 0.0;
  returnMatrix[7] := 0.0;

  returnMatrix[8] := (right+left)/(right-left);
  returnMatrix[9] := (top+bottom)/(top-bottom);
  returnMatrix[10] := (-(back+front))/(back-front);
  returnMatrix[11] := -1.0;

  returnMatrix[12] := 0.0;
  returnMatrix[13] := 0.0;
  returnMatrix[14] := (-2.0*back*front)/(back-front);
  returnMatrix[15] := 0.0;
  result := returnMatrix;
end;

function getPerspectiveMatrix(angle, aspectRatio, front, back : real) : matrix4d;
var
  returnMatrix : matrix4d;
  tangent, height, width : real;
begin
  tangent := tan(degToRad(angle/2));
  height := front*tangent;
  width := height*aspectRatio;

  returnMatrix := getPerspectiveMatrix(-width, width, -height, height, front, back);
  result := returnMatrix;
end;

function getOrthographicMatrix(left, right, bottom, top, front, back : real) : matrix4d;
var
  returnMatrix : matrix4d;
begin
  returnMatrix[0] := 2.0/(right-left);
  returnMatrix[1] := 0.0;
  returnMatrix[2] := 0.0;
  returnMatrix[3] := 0.0;

  returnMatrix[4] := 0.0;
  returnMatrix[5] := 2.0/(top-bottom);
  returnMatrix[6] := 0.0;
  returnMatrix[7] := 0.0;

  returnMatrix[8] := 0.0;
  returnMatrix[9] := 0.0;
  returnMatrix[10] := -2.0/(back-front);
  returnMatrix[11] := 0.0;

  returnMatrix[12] := -((right+left)/(right-left));
  returnMatrix[13] := -((top+bottom)/(top-bottom));
  returnMatrix[14] := -((back+front)/(back-front));
  returnMatrix[15] := 1.0;
  result := returnMatrix;
end;

procedure globalBindShader(shader : PShader);
begin
  glUseProgram(shader^.getProgram);
  boundShader_ := shader;
end;

function globalBoundShader : PShader;
begin
  result := boundShader_;
end;

procedure globalBindCustomDrawFunction(newCustomDrawFunction : customDrawFunctionType);
begin
  customDrawFunction := newCustomDrawFunction;
end;

function globalBoundCustomDrawFunction : customDrawFunctionType;
begin
  result := customDrawFunction;
end;

procedure bindTextureUnit(textureUnit : GLenum);
begin
  glActiveTexture(textureUnit);
  boundTextureUnit_ := textureUnit;
end;

function boundTextureUnit : GLenum;
begin
  result := boundTextureUnit_;
end;

procedure setMatrix(matrixId : integer);
begin
  if (currentMatrixId <> IDENTITY_MATRIX) then currentMatrixId := matrixId;
end;

function currentMatrix : integer;
begin
  result := currentMatrixId;
end;

procedure copyMatrix(srcMatrixId, dstMatrixId : integer);
begin
  if (dstMatrixId <> IDENTITY_MATRIX) then matrix[dstMatrixId] := matrix[srcMatrixId];
end;

procedure copyMatrix(srcMatrix : matrix4d; dstMatrixId : integer);
begin
  if (dstMatrixId <> IDENTITY_MATRIX) then matrix[dstMatrixId] := srcMatrix;
end;

function getMatrix(matrixId : integer) : matrix4d;
begin
  result := matrix[matrixId];
end;

procedure pushMatrix;
begin
  if (matrixLevel[currentMatrixId] < STACK_SIZE) then
  begin
    matrixLevel[currentMatrixId] += 1;
    matrixStack[currentMatrixId][matrixLevel[currentMatrixId]] := matrix[currentMatrixId];
  end else writeln('Matrix stack full!');
end;

procedure popMatrix;
begin
  if (matrixLevel[currentMatrixId] > 0) then
  begin
    matrix[currentMatrixId] := matrixStack[currentMatrixId][matrixLevel[currentMatrixId]];
    matrixLevel[currentMatrixId] -= 1;
  end else writeln('Matrix stack empty!');
end;

procedure translateMatrix(x, y, z : real);
var
  transformationMatrix : matrix4d;
begin
  initIdentityMatrix(transformationMatrix);
  transformationMatrix[12] := x;
  transformationMatrix[13] := y;
  transformationMatrix[14] := z;
  matrix[currentMatrixId] := multiplyMatrix(matrix[currentMatrixId], transformationMatrix);
end;

procedure rotateMatrix(angle, x, y, z : real);
var
  len, c, s, x2, y2, z2, t : real;
  transformationMatrix : matrix4d;
begin
  angle := degToRad(angle);
  len := sqrt((x*x)+(y*y)+(z*z));
  x /= len;
  y /= len;
  z /= len;

  c := cos(angle);
  s := sin(angle);
  x2 := x*x;
  y2 := y*y;
  z2 := z*z;
  t := 1.0-c;

  transformationMatrix[0] := (x2*t)+c;
  transformationMatrix[1] := (y*x*t)+z*s;
  transformationMatrix[2] := (x*z*t)-(y*s);
  transformationMatrix[3] := 0.0;

  transformationMatrix[4] := (x*y*t)-(z*s);
  transformationMatrix[5] := (y2*t)+c;
  transformationMatrix[6] := (y*z*t)+(x*s);
  transformationMatrix[7] := 0.0;

  transformationMatrix[8] := (x*z*t)+(y*s);
  transformationMatrix[9] := (y*z*t)-(x*s);
  transformationMatrix[10] := (z2*t)+c;
  transformationMatrix[11] := 0.0;

  transformationMatrix[12] := 0.0;
  transformationMatrix[13] := 0.0;
  transformationMatrix[14] := 0.0;
  transformationMatrix[15] := 1.0;

  matrix[currentMatrixId] := multiplyMatrix(matrix[currentMatrixId], transformationMatrix);
end;

procedure scaleMatrix(xScale, yScale, zScale : real);
var
  transformationMatrix : matrix4d;
begin
  initIdentityMatrix(transformationMatrix);
  transformationMatrix[0] := xScale;
  transformationMatrix[5] := yScale;
  transformationMatrix[10] := zScale;
  matrix[currentMatrixId] := multiplyMatrix(matrix[currentMatrixId], transformationMatrix);
end;

function multiplyMatrix(operandMatrix, multiplierMatrix : matrix4d) : matrix4d;
var
  returnMatrix: matrix4d;
begin
  returnMatrix[0] := (operandMatrix[0]*multiplierMatrix[0])+(operandMatrix[4]*multiplierMatrix[1])+(operandMatrix[8]*multiplierMatrix[2])+(operandMatrix[12]*multiplierMatrix[3]);
  returnMatrix[1] := (operandMatrix[1]*multiplierMatrix[0])+(operandMatrix[5]*multiplierMatrix[1])+(operandMatrix[9]*multiplierMatrix[2])+(operandMatrix[13]*multiplierMatrix[3]);
  returnMatrix[2] := (operandMatrix[2]*multiplierMatrix[0])+(operandMatrix[6]*multiplierMatrix[1])+(operandMatrix[10]*multiplierMatrix[2])+(operandMatrix[14]*multiplierMatrix[3]);
  returnMatrix[3] := (operandMatrix[3]*multiplierMatrix[0])+(operandMatrix[7]*multiplierMatrix[1])+(operandMatrix[11]*multiplierMatrix[2])+(operandMatrix[15]*multiplierMatrix[3]);

  returnMatrix[4] := (operandMatrix[0]*multiplierMatrix[4])+(operandMatrix[4]*multiplierMatrix[5])+(operandMatrix[8]*multiplierMatrix[6])+(operandMatrix[12]*multiplierMatrix[7]);
  returnMatrix[5] := (operandMatrix[1]*multiplierMatrix[4])+(operandMatrix[5]*multiplierMatrix[5])+(operandMatrix[9]*multiplierMatrix[6])+(operandMatrix[13]*multiplierMatrix[7]);
  returnMatrix[6] := (operandMatrix[2]*multiplierMatrix[4])+(operandMatrix[6]*multiplierMatrix[5])+(operandMatrix[10]*multiplierMatrix[6])+(operandMatrix[14]*multiplierMatrix[7]);
  returnMatrix[7] := (operandMatrix[3]*multiplierMatrix[4])+(operandMatrix[7]*multiplierMatrix[5])+(operandMatrix[11]*multiplierMatrix[6])+(operandMatrix[15]*multiplierMatrix[7]);

  returnMatrix[8] := (operandMatrix[0]*multiplierMatrix[8])+(operandMatrix[4]*multiplierMatrix[9])+(operandMatrix[8]*multiplierMatrix[10])+(operandMatrix[12]*multiplierMatrix[11]);
  returnMatrix[9] := (operandMatrix[1]*multiplierMatrix[8])+(operandMatrix[5]*multiplierMatrix[9])+(operandMatrix[9]*multiplierMatrix[10])+(operandMatrix[13]*multiplierMatrix[11]);
  returnMatrix[10] := (operandMatrix[2]*multiplierMatrix[8])+(operandMatrix[6]*multiplierMatrix[9])+(operandMatrix[10]*multiplierMatrix[10])+(operandMatrix[14]*multiplierMatrix[11]);
  returnMatrix[11] := (operandMatrix[3]*multiplierMatrix[8])+(operandMatrix[7]*multiplierMatrix[9])+(operandMatrix[11]*multiplierMatrix[10])+(operandMatrix[15]*multiplierMatrix[11]);

  returnMatrix[12] := (operandMatrix[0]*multiplierMatrix[12])+(operandMatrix[4]*multiplierMatrix[13])+(operandMatrix[8]*multiplierMatrix[14])+(operandMatrix[12]*multiplierMatrix[15]);
  returnMatrix[13] := (operandMatrix[1]*multiplierMatrix[12])+(operandMatrix[5]*multiplierMatrix[13])+(operandMatrix[9]*multiplierMatrix[14])+(operandMatrix[13]*multiplierMatrix[15]);
  returnMatrix[14] := (operandMatrix[2]*multiplierMatrix[12])+(operandMatrix[6]*multiplierMatrix[13])+(operandMatrix[10]*multiplierMatrix[14])+(operandMatrix[14]*multiplierMatrix[15]);
  returnMatrix[15] := (operandMatrix[3]*multiplierMatrix[12])+(operandMatrix[7]*multiplierMatrix[13])+(operandMatrix[11]*multiplierMatrix[14])+(operandMatrix[15]*multiplierMatrix[15]);

  result := returnMatrix;
end;

procedure initIdentityMatrix(var dstMatrix : matrix4d);
var
  i : byte;
begin
  for i := 0 to 15 do dstMatrix[i] := 0.0;
  dstMatrix[0] := 1.0;
  dstMatrix[5] := 1.0;
  dstMatrix[10] := 1.0;
  dstMatrix[15] := 1.0;
end;

procedure refreshScreen;
var
  delay : integer;
begin
  SDL_GL_SwapBuffers;
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  resetEvents;

  lastTicks := ticks;
  ticks := SDL_GetTicks;
  tickDifference := ticks-lastTicks;
  if (tickDifference = 0) then tickDifference := 1;
  framerate := round(1000/tickDifference);
  delay := 0;
  if (maximumFramerate > 0) then delay := round(1000/maximumFramerate)-tickDifference;
  if (delay >= 0) then SDL_Delay(delay);
  compensation_ := tickDifference/(1000.0/idealFramerate);
end;

function getFramerate : word;
begin
  result := framerate;
end;

function getTickDifference : word;
begin
  result := tickDifference;
end;

procedure setIdealFramerate(newIdealFramerate : word);
begin
  idealFramerate := newIdealFramerate;
  if (idealFramerate = 0) then idealFramerate := 60;
end;

function getIdealFramerate : word;
begin
  result := idealFramerate;
end;

function compensation : real;
begin
  result := compensation_;
end;

procedure enableBlending(srcBlendFunc : GLenum = ONE; dstBlendFunc : GLenum = ZERO; blendFuncEquation : GLenum = FUNC_ADD);
begin
  if (not blendingEnabled_) then
  begin
    glEnable(GL_BLEND);
    blendingEnabled_ := true;
  end;
  glBlendEquation(blendFuncEquation);
  glBlendFunc(srcBlendFunc, dstBlendFunc);
end;

procedure disableBlending;
begin
  if (blendingEnabled_) then
  begin
    glDisable(GL_BLEND);
    blendingEnabled_ := false;
  end;
end;

function blendingEnabled : boolean;
begin
  result := blendingEnabled_;
end;

procedure enableDepthTesting;
begin
  if (not depthTestingEnabled_) then
  begin
    glEnable(GL_DEPTH_TEST);
    depthTestingEnabled_ := true;
  end;
end;

procedure disableDepthTesting;
begin
  if (depthTestingEnabled_) then
  begin
    glDisable(GL_DEPTH_TEST);
    depthTestingEnabled_ := false;
  end;
end;

function depthTestingEnabled : boolean;
begin
  result := depthTestingEnabled_;
end;

function openGlVersion : real;
var
  str : string;
  null : integer;
begin
  if (openGlVersion_ = 0.0) then
  begin
    str := leftStr(glGetString(GL_VERSION), 3);
    val(str, openGlVersion_, null);
  end;
  result := openGlVersion_;
end;

function glSlVersion : real;
var
  str : string;
  null : integer;
begin
  if (glSlVersion_ = 0.0) then
  begin
    str := leftStr(glGetString(GL_SHADING_LANGUAGE_VERSION), 3);
    val(str, glSlVersion_, null);
  end;
  result := glSlVersion_;
end;

function vertexArrayObjectSupported : boolean;
begin
  result := vertexArrayObjectSupported_;
end;

initialization

openGlVersion_ := 0.0;
glSlVersion_ := 0.0;
compensation_ := 1.0;

end.
