{
SuperMaximo GameLibrary : Graphical Asset class (sprites, models and gameObjects) unit
by Max Foster

License : http://creativecommons.org/licenses/by/3.0/
}
unit GraphicalAssetClasses;
{$mode objfpc}{$H+}

interface

uses SDL, dglOpenGL, Display, ShaderClass;

type
  pSpriteDrawParams = ^tSpriteDrawParams;
  PSprite = ^TSprite;
  pkeyFrame = ^keyFrame;
  pbone = ^bone;
  ptriangle = ^triangle;
  pbox = ^box;
  PModel = ^TModel;
  PGameObject = ^TGameObject;

  customSpriteBufferFunctionType = procedure(buffer : PGLuint; sprite : PSprite; data : Pointer);
  customModelBufferFunctionType = procedure(buffer : PGLuint; sprite : PModel; data : Pointer);

  tSpriteDrawParams = record
    x, y, depth, rotation, xScale, yScale, alpha, frame : real;
  end;

  TSprite = object
  private
    image : PSDL_Surface;
    texture_ : array of GLuint;
    frames, vertices_, framerate_ : word;
    originX_, originY_ : integer;
    name_ : string;
    rect_ : SDL_Rect;
    vao_, vbo_ : GLuint;
    boundShader_ : PShader;
    customDrawFunction : customDrawFunctionType;
    procedure initBuffer;
  public
    //Create a sprite with the specified name, loaded from a file. 'imageX/Y' are the coordinates of where to start 'cutting out' the sprite
    //from the image provided, and 'imageWidth/Height' are the width and height of the new sprite. 'aniframes' is the number of frames that
    //the sprite has, if provided on a spritesheet, and 'framerate' lets you set the animation speed in frames per second. 'newOrignX/Y' is the
    //point on the sprite where it will pivot for rotation. 'customBufferFunction' allows you to pass a procedure pointer to your own sprite
    //buffering procedure for advanced usage and 'customData' allows you to pass a pointer to the data you want to access in your custom buffer
    //procedure
    constructor create(newName, fileName : string; imageX, imageY, imageWidth, imageHeight : integer; aniframes : integer = 1; framerate : word = 1;
      newOriginX : integer = 0; newOriginY : integer = 0; customBufferFunction : customSpriteBufferFunctionType = nil; customData : Pointer = nil);
    destructor destroy;

    function name : string;
    function frameCount : word;
    procedure setFramerate(newFramerate : word);
    function framerate : word;
    function rect : SDL_Rect;

    function originX : integer;
    function originY : integer;

    //Draws the sprite with the specified coordinates and transformations. 'alpha' is only for a convienience if you want to use it in a
    //custom draw procedure where you send the alpha value as a shader uniform. The frame is used when using spritesheets. You can override
    //the shader bound to the sprite or globally with 'shaderOverride' and override the drawing procedure with 'customDrawFunctionOverride'
    procedure draw(x, y, depth : real; rotation : real = 0.0; xScale : real = 1.0; yScale : real = 1.0; alpha : real = 1.0; frame : real = 1.0;
      shaderOverride : PShader = nil; customDrawFunctionOverride : customDrawFunctionType = nil);
    procedure draw(objectParam : PGameObject); //Draws the sprite from the details of a GameObject
    procedure defaultDraw(shaderToUse : PShader; params : pSpriteDrawParams); //The default drawing procedure for the sprite

    function width : integer;
    function height : integer;
    function surface : PSDL_Surface;
    function texture(frame : word) : GLuint; //Return the OpenGL texture ID for passing to OpenGL functions
    function vertices : word;

    //Bind a shader to be used with the sprite instead of using one that is globally bound with globalBindShader or 'Shader.bind'
    procedure bindShader(newShader : PShader);
    function boundShader : PShader;

    //Bind a custom draw procedure to be used when the sprite is drawn
    procedure bindCustomDrawFunction(newCustomDrawFunction : customDrawFunctionType);
    function boundCustomDrawFunction : customDrawFunctionType;

    //Returns the OpenGL Vertex Array Object contained in the Sprite
    function vao : GLuint;
    //Returns the OpenGL Vertex Buffer Object contained in the Sprite
    function vbo : GLuint;
  end;


  keyFrameData = record
    boneId : integer;
    xRot, yRot, zRot : real;
  end;

  keyFrame = record
    boneData : array of keyFrameData;
    step : word;
  end;

  normal = record
    x, y, z : real;
  end;

  vertex = record
    x, y, z : real;
    normal_ : normal;
  end;

  color = record
    r, g, b : real;
  end;

  material = record
    name, fileName : string;
    textureId : integer;
    hasTexture : boolean;
    ambientColor, diffuseColor, specularColor : color;
    shininess, alpha : real;
  end;

  triangle = record
    coords, texCoords : array[0..2] of vertex;
    mtlNum, boneId : integer;
    pBone_ : pbone;
    sharedCoord : array[0..2] of boolean;
  end;

  box = record
    x, y, z, l, w, h, xRot, yRot, zRot, rl, rw, rh : real;
  end;

  bone = record
    id, offset : integer;
    x, y, z, endX, endY, endZ, xRot, yRot, zRot : real;
    parent : pBone;
    child : array of pbone;
    triangles : array of ptriangle;
    hitbox : box;
  end;

  animation = record
    name : string;
    frames : array of keyFrame;
  end;

  TModel = object
  private
    name_ : string;
    triangles : array of triangle;
    materials : array of material;
    bones : array of pbone;
    animations : array of animation;
    vao, vbo, texture : GLuint;
    boundShader_ : PShader;
    loadedFromObj : boolean;
    framerate_ : word;

    //Has not been implemented
    procedure loadObj(path, fileName : string; customBufferFunction : customModelBufferFunctionType = nil; customData : Pointer = nil);

    //Load SuperMaximo* files
    procedure loadSmm(fileName : string);  //SuperMaximo Model
    procedure loadSms(fileName : string);  //SuperMaximo Skeleton
    procedure loadSma(fileName : string);  //SuperMaximo Animation
    procedure loadSmo(path, fileName : string; customBufferFunction : customModelBufferFunctionType = nil;
              customData : Pointer = nil);  //SuperMaximo Object

    //Buffer the vertex data for each file type
    procedure initBufferObj;
    procedure initBufferSmo;

    procedure drawObj(shaderToUse : PShader);

    function calculatePoints(nx, ny, nz : real; matrix : matrix4d) : normal;
    procedure calculateHitbox(pBone_ : pbone; matrix : matrix4d);
    procedure drawBone(pBone_ : pbone; shaderToUse : PShader; skipHitboxes : boolean = false);
  public
    //Loads a model, from the path and file name
    constructor create(newName, path, fileName : string; framerate : word = 60; customBufferFunction : customModelBufferFunctionType = nil;
                customData : Pointer = nil);
    destructor destroy;

    function name : string;

    //Draw the model using details from a GameObject. You can skip animation and hitbox orientation to save processing time
    //NOTE: The hitboxes probably don't work, and have actually been removed from the latest version of the library!
    procedure draw(objectParam : PGameObject; skipAnimation : boolean = false; skipHitboxes : boolean = false);

    //Bind a shader to be used when drawing the model instead of a globally bound one
    procedure bindShader(newShader : PShader);
    function boundShader : PShader;

    function animationId(searchName : string) : integer; //Return the ID of an animation with the specified name (-1 is returned on failure)

    procedure setFramerate(newFramerate : word);
    function framerate : word;
  end;


  TGameObject = object
  protected
    sprite_ : PSprite;
    model_ : PModel;
    hasModel_, interpolating_ : boolean;
    currentAnimationId, nextAnimationId : word;
    x_, y_, z_, xRotation_, yRotation_, zRotation_, xScale_, yScale_, zScale_, width_, height_, alpha_, xRotatedWidth_, yRotatedWidth_, zRotatedWidth_, xRotatedHeight_,
      yRotatedHeight_, zRotatedHeight_, originX, originY, frame_ : real;
    name_ : string;
    boundShader_ : PShader;
    customDrawFunction : customDrawFunctionType;
    fakeKeyFrame1, fakeKeyFrame2 : pkeyFrame;
  public
    //Create a GameObject at the specified coordinates with either a sprite or model bound to it
    constructor create(newName : string; destX, destY, destZ : real; newSprite : PSprite = nil; startFrame : word = 0);
    constructor create(newName : string; destX, destY, destZ : real; newModel : PModel = nil);
    destructor destroy;

    function name : string;

    procedure setSprite(newSprite : PSprite);
    function sprite : PSprite;
    procedure setModel(newModel : PModel);
    function model : PModel;
    function hasModel : boolean;

    //Bind a shader to be used when drawing instead of the globally bound one
    procedure bindShader(shader : PShader);
    function boundShader : PShader;

    //Bind a custom draw procedure to be used when drawing the object
    procedure bindCustomDrawFunction(newCustomDrawFunction : customDrawFunctionType);
    function boundCustomDrawFunction : customDrawFunctionType;

    procedure setPosition(xAmount, yAmount, zAmount : real; relative : boolean = false);
    function setX(amount : real; relative : boolean = false) : real;
    function setY(amount : real; relative : boolean = false) : real;
    function setZ(amount : real; relative : boolean = false) : real;
    function x : real;
    function y : real;
    function z : real;

    function width : real;
    function height : real;
    procedure calcZRotatedDimensions;

    procedure scale(xAmount, yAmount, zAmount : real; relative : boolean = false; recalculateDimensions : boolean = true);
    function xScale : real;
    function yScale : real;
    function zScale : real;

    //Rotate by an amount around each axis. Set 'recalculateDimensions' to recalculate sprite bounds for collisions
    procedure rotate(xAmount, yAmount, zAmount : real; relative : boolean = false; recalculateDimensions : boolean = true);
    function rotate(amount : real; relative : boolean = false; recalculateDimensions : boolean = true) : real; //2D rotation (i.e. around Z axis)
    function xRotation : real;
    function yRotation : real;
    function zRotation : real;

    function setAlpha(amount : real; relative : boolean = false) : real;
    function alpha : real;

    //Set and get the animation that the object is playing
    procedure setCurrentAnimation(animationId : word);
    function currentAnimation : word;

    procedure setFrame(newFrame : real; relative : boolean = false);
    function frame : real;

    procedure animate(start, finish : word; animationId : word = 0); //Animate between two particular frames

    //Interpolate between the frame of one animation and the frame of another animation, over a set time period in frames
    procedure setInterpolation(startFrame, animation1Id, endFrame, animation2Id, numFramesToTake : integer);
    procedure setInterpolation; //Stop interpolation
    function interpolating : boolean;

    procedure draw(skipAnimation : boolean = false; skipHitboxes : boolean = false);

    //2D collision detection for sprites
    function mouseOverBox : boolean;
    function roughMouseOverBox : boolean;
    function mouseOverCircle(extraX : real = 0; extraY : real = 0; extraZ : real = 0) : boolean;
    function boxCollision(other : PGameObject; allStages : boolean = true) : boolean;
    function roughBoxCollision(other : PGameObject) : boolean;
    function circleCollision(other : PGameObject; extraX1 : real = 0; extraY1 : real = 0; extraZ1 : real = 0; extraX2 : real = 0; extraY2 : real = 0;
      extraZ2 : real = 0) : boolean;

    //3D hitbox collision detection
    //NOTE: Does not work, and has been removed from newer versions of the library!
    function roughHitboxCollision(bone1, bone2 : pbone) : boolean;
    function hitBoxCollision(bone1, bone2 : pbone) : boolean;
    function roughModelCollision(other : PGameObject; hitboxId : integer = -1; hitboxOtherId : integer = -1) : boolean;
    function modelCollision(other : PGameObject; hitboxId : integer = -1; hitboxOtherId : integer = -1) : boolean;
  end;

//Set a model to be in a particular pose from data in a keyframe
procedure setKeyFrame(frame : pkeyFrame; model : PModel);

function subtractNormal(operandNormal, subtractingNormal : normal) : normal;
function subtractVertex(operandVertex, subtractingVertex : vertex) : vertex;

//Get the surface normal of a triangle in 3D space
function getSurfaceNormal(srcTriangle : ptriangle) : normal;

//Initialise the variables in a box
procedure initBox(dstBox : pbox);


function sprite(searchName : string) : PSprite;
function addSprite(newName, fileName : string; imageX, imageY, imageWidth, imageHeight : integer; aniframes : integer = 1; frameChangePerSecond : integer = 1;
  newOriginX : integer = 0; newOriginY : integer = 0; customBufferFunction : customSpriteBufferFunctionType = nil; customData : Pointer = nil) : PSprite;
procedure destroySprite(searchName : string);
procedure destroyAllSprites;

function model(searchName : string) : PModel;
function addModel(newName, path, fileName : string; framerate : word = 60; customBufferFunction : customModelBufferFunctionType = nil; customData : Pointer = nil) : PModel;
procedure destroyModel(searchName : string);
procedure destroyAllModels;

function gameObject(searchName : string) : PGameObject;
function addGameObject(newName : string; destX, destY, destZ : real; newSprite : PSprite = nil; startFrame : word = 0) : PGameObject;
function addGameObject(newName : string; destX, destY, destZ : real; newModel : PModel = nil) : PGameObject;
procedure destroyGameObject(searchName : string);
procedure destroyAllGameObjects;

implementation

uses SysUtils, Classes, Math, SDL_image, Input;

var
  allSprites : array['a'..'z'] of TList;
  allModels : array['a'..'z'] of TList;
  allGameObjects : array['a'..'z'] of TList;

constructor TSprite.create(newName, fileName : string; imageX, imageY, imageWidth, imageHeight : integer; aniframes : integer = 1; framerate : word = 1;
  newOriginX : integer = 0; newOriginY : integer = 0; customBufferFunction : customSpriteBufferFunctionType = nil; customData : Pointer = nil);
var
  textureFormat : GLenum;
  tempSurface : PSDL_Surface;
  tempRect : SDL_Rect;
  i, frame, row, numFrames : integer;
begin
  name_ := newName;
  fileName := setDirSeparators(fileName);
  frames := aniFrames;
  framerate_ := framerate;
  rect_.x := imageX;
  rect_.y := imageY;
  rect_.w := imageWidth;
  rect_.h := imageHeight;
  originX_ := newOriginX;
  originY_ := newOriginY;
  customDrawFunction := nil;
  image := IMG_Load(pchar(fileName));
  if (image = nil) then writeln('Could not load image ', fileName) else
  begin
    SDL_SetAlpha(image, 0, 0);
    if (image^.format^.BytesPerPixel = 4) then
    begin
      if (image^.format^.Rmask = $000000ff) then textureFormat := GL_RGBA else textureFormat := GL_BGRA;
    end
  else
    begin
      if (image^.format^.Rmask = $000000ff) then textureFormat := GL_RGB else textureFormat := GL_BGR;
    end;
    tempSurface := SDL_CreateRGBSurface(SDL_HWSURFACE, rect_.w, rect_.h, image^.format^.BitsPerPixel, image^.format^.Rmask, image^.format^.Gmask,
        image^.format^.Bmask, image^.format^.Amask);
    tempRect := rect_;
    for i := 0 to frames-1 do
    begin
      setLength(texture_, length(texture_)+1);
      glGenTextures(1, @texture_[i]);
      frame := i;
      row := 0;
      numFrames := image^.w div rect_.w;
      if (numFrames > 0) then
      begin
        while (frame-(row*numFrames) >= numFrames) do
        begin
          row += 1;
          frame -= numFrames;
        end
      end;
      tempRect.x := frame*rect_.w;
      tempRect.y := row*rect_.h;
      SDL_BlitSurface(image, @tempRect, tempSurface, nil);
      glBindTexture(GL_TEXTURE_RECTANGLE, texture_[i]);
      glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexImage2D(GL_TEXTURE_RECTANGLE, 0, tempSurface^.format^.BytesPerPixel, rect_.w, rect_.h, 0, textureFormat, GL_UNSIGNED_BYTE, tempSurface^.pixels);
    end;
    SDL_FreeSurface(tempSurface);
  end;
  boundShader_ := nil;
  vertices_ := 6;

  if (vertexArrayObjectSupported) then
  begin
    glGenVertexArrays(1, @vao_);
    glBindVertexArray(vao_);
  end;

  if (customBufferFunction = nil) then initBuffer else customBufferFunction(@vbo_, @self, customData);
  if (vertexArrayObjectSupported) then glBindVertexArray(0);

  for i := 0 to 15 do glDisableVertexAttribArray(i);

  glBindTexture(GL_TEXTURE_RECTANGLE, 0);
end;

destructor TSprite.destroy;
var
  i : integer;
begin
  if (image <> nil) then
  begin
    SDL_FreeSurface(image);
    for i := 0 to frames-1 do glDeleteTextures(1, @texture_[i]);
    if (vertexArrayObjectSupported) then glDeleteVertexArrays(1, @vao_);
    glDeleteBuffers(1, @vbo_);
  end;
end;

procedure TSprite.initBuffer;
var
  vertexArray : array[0..23] of GLfloat = (0.0, 1, 0.0, 1.0,  0.0, 0.0, 0.0, 1.0,  1, 0.0, 0.0, 1.0,
    0.0, 1, 0.0, 1.0,  1, 1, 0.0, 1.0,  1, 0.0, 0.0, 1.0);
begin
  vertexArray[1] := rect_.h;
  vertexArray[8] := rect_.w;
  vertexArray[13] := rect_.h;
  vertexArray[16] := rect_.w;
  vertexArray[17] := rect_.h;
  vertexArray[20] := rect_.w;
  glGenBuffers(1, @vbo_);
  glBindBuffer(GL_ARRAY_BUFFER, vbo_);
  glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat)*length(vertexArray), @vertexArray, GL_STATIC_DRAW);
  if (vertexArrayObjectSupported) then
  begin
    glVertexAttribPointer(VERTEX_ATTRIBUTE, 4, GL_FLOAT, false, 0, nil);
    glEnableVertexAttribArray(VERTEX_ATTRIBUTE);
  end;
  glBindBuffer(GL_ARRAY_BUFFER, 0);
end;

function TSprite.name : string;
begin
  result := name_;
end;

function TSprite.frameCount : word;
begin
  result := frames;
end;

procedure TSprite.setFramerate(newFramerate : word);
begin
  framerate_ := newFramerate;
  if (framerate_ = 0) then framerate_ := 1;
end;

function TSprite.framerate : word;
begin
  result := framerate_;
end;

function TSprite.rect : SDL_Rect;
begin
  result := rect_;
end;

function TSprite.originX : integer;
begin
  result := originX_;
end;

function TSprite.originY : integer;
begin
  result := originY_;
end;

procedure TSprite.draw(x, y, depth : real; rotation : real = 0.0; xScale : real = 1.0; yScale : real = 1.0; alpha : real = 1.0; frame : real = 1.0;
  shaderOverride : PShader = nil; customDrawFunctionOverride : customDrawFunctionType = nil);
var
  shaderToUse : PShader;
  drawFunctionToUse : customDrawFunctionType;
  params : tSpriteDrawParams;
begin
  if (shaderOverride <> nil) then shaderToUse := shaderOverride else if (boundShader_ <> nil) then shaderToUse := boundShader_ else shaderToUse := globalBoundShader;

  if (customDrawFunctionOverride <> nil) then drawFunctionToUse := customDrawFunctionOverride else if (customDrawFunction <> nil) then
    drawFunctionToUse := customDrawFunction else drawFunctionToUse := globalBoundCustomDrawFunction;

  params.x := x;
  params.y := y;
  params.depth := depth;
  params.rotation := rotation;
  params.xScale := xScale;
  params.yScale := yScale;
  params.alpha := alpha;
  params.frame := frame;

  if (drawFunctionToUse <> nil) then drawFunctionToUse(@self, shaderToUse, @params) else defaultDraw(shaderToUse, @params);

  if (globalBoundShader <> nil) then glUseProgram(globalBoundShader^.getProgram) else glUseProgram(0);
end;

procedure TSprite.draw(objectParam : PGameObject);
begin
  draw(objectParam^.x_, objectParam^.y_, objectParam^.z_, objectParam^.zRotation_, objectParam^.xScale_, objectParam^.yScale_, objectParam^.alpha_, objectParam^.frame_,
    objectParam^.boundShader_, objectParam^.customDrawFunction);
end;

procedure TSprite.defaultDraw(shaderToUse : PShader; params : pSpriteDrawParams);
var
  frameToUse : integer;
begin
  if (shaderToUse <> nil) then
  begin
    while (params^.frame >= frames) do params^.frame -= 1;
    glActiveTexture(GL_TEXTURE0);
    frameToUse := round(params^.frame);
    while (frameToUse >= length(texture_)) do frameToUse -= 1;
    if (frameToUse < 0) then frameToUse := 0;
    glBindTexture(GL_TEXTURE_RECTANGLE, texture_[frameToUse]);

    pushMatrix;
      translateMatrix(params^.x-originX_, params^.y-originY_, params^.depth);
      translateMatrix(originX_, originY_, 0.0);
      rotateMatrix(params^.rotation, 0.0, 0.0, 1.0);
      scaleMatrix(params^.xScale, params^.yScale, 0.0);
      translateMatrix(-originX_, -originY_, 0.0);

      glUseProgram(shaderToUse^.getProgram);
      shaderToUse^.setUniform16(MODELVIEW_LOCATION, getMatrix(MODELVIEW_MATRIX));
      shaderToUse^.setUniform16(PROJECTION_LOCATION, getMatrix(PROJECTION_MATRIX));
      shaderToUse^.setUniform1(TEXSAMPLER_LOCATION, 0);

      if (vertexArrayObjectSupported) then
      begin
        glBindVertexArray(vao_);
        glBindBuffer(GL_ARRAY_BUFFER, vbo_);
      end
    else
      begin
        glBindBuffer(GL_ARRAY_BUFFER, vbo_);
        glVertexAttribPointer(VERTEX_ATTRIBUTE, 4, GL_FLOAT, false, 0, nil);
        glEnableVertexAttribArray(VERTEX_ATTRIBUTE);
      end;

      glDrawArrays(GL_TRIANGLES, 0, vertices_);
      glBindBuffer(GL_ARRAY_BUFFER, 0);
      if (vertexArrayObjectSupported) then glBindVertexArray(0) else glDisableVertexAttribArray(VERTEX_ATTRIBUTE);
    popMatrix;
  end;
end;

function TSprite.width : integer;
begin
  result := rect_.w;
end;

function TSprite.height : integer;
begin
  result := rect_.h
end;

function TSprite.surface : PSDL_Surface;
begin
  result := image;
end;

function TSprite.texture(frame : word) : GLuint;
begin
  if (frame >= frames) then frame := frames-1;
  result := texture_[frame];
end;

function TSprite.vertices : word;
begin
  result := vertices_;
end;

procedure TSprite.bindShader(newShader : PShader);
begin
  boundShader_ := newShader;
end;

function TSprite.boundShader : PShader;
begin
  result := boundShader_;
end;

procedure TSprite.bindCustomDrawFunction(newCustomDrawFunction : customDrawFunctionType);
begin
  customDrawFunction := newCustomDrawFunction;
end;

function TSprite.boundCustomDrawFunction : customDrawFunctionType;
begin
  result := customDrawFunction;
end;

function TSprite.vao : GLuint;
begin
  result := vao_;
end;

function TSprite.vbo : GLuint;
begin
  result := vbo_;
end;


constructor TModel.create(newName, path, fileName : string; framerate : word = 60; customBufferFunction : customModelBufferFunctionType = nil; customData : Pointer = nil);
begin
  name_ := newName;
  path := setDirSeparators(path);
  boundShader_ := nil;
  framerate_ := framerate;
  if (lowerCase(rightStr(fileName, 3)) = 'smo') then loadSmo(path, fileName, customBufferFunction, customData) else
    if (lowerCase(rightStr(fileName, 3)) = 'obj') then loadObj(path, fileName, customBufferFunction, customData);
end;

destructor TModel.destroy;
var
  i : integer;
begin
  glDeleteTextures(1, @texture);

  if (length(bones) > 0) then
  begin
    for i := 0 to length(bones)-1 do dispose(bones[i]);
  end;

  glDeleteBuffers(1, @vbo);
  if (vertexArrayObjectSupported) then glDeleteVertexArrays(1, @vao);
end;

procedure TModel.loadObj(path, fileName : string; customBufferFunction : customModelBufferFunctionType = nil; customData : Pointer = nil);
begin

end;

procedure TModel.loadSmm(fileName : string);
var
  fileText : array of string;
  modelFile : text;
  tempStr : string;
  totalTriangles, totalMaterials, size : longword;
  newTriangle : triangle;
  i, j, null : integer;
  newMaterial : material;
  image : PSDL_Surface;
  textureFormat : GLenum;
  initialised : boolean;
begin
  setLength(fileText, 0);
  assign(modelFile, fileName);
  reset(modelFile);
  repeat
    readln(modelFile, tempStr);
    while ((rightStr(tempStr, 1) = #13) or (rightStr(tempStr, 1) = #10)) do tempStr := leftStr(tempStr, length(tempStr)-1);
    setLength(fileText, length(fileText)+1);
    fileText[length(fileText)-1] := tempStr;
  until Eof(modelFile);
  close(modelFile);

  while (fileText[length(fileText)-1] = '') do setLength(fileText, length(fileText)-1);
  setLength(triangles, 0);

  totalTriangles := strToInt(fileText[0]);
  for i := 0 to totalTriangles-1 do
  begin
    for j := 0 to 2 do
    begin
      val(fileText[(i*32)+(j*6)+1], newTriangle.coords[j].x, null);
      val(fileText[(i*32)+(j*6)+2], newTriangle.coords[j].y, null);
      val(fileText[(i*32)+(j*6)+3], newTriangle.coords[j].z, null);
      val(fileText[(i*32)+(j*6)+4], newTriangle.coords[j].normal_.x, null);
      val(fileText[(i*32)+(j*6)+5], newTriangle.coords[j].normal_.y, null);
      val(fileText[(i*32)+(j*6)+6], newTriangle.coords[j].normal_.z, null);
    end;
    for j := 0 to 2 do
    begin
      val(fileText[(i*32)+(j*3)+19], newTriangle.texCoords[j].x, null);
      val(fileText[(i*32)+(j*3)+20], newTriangle.texCoords[j].y, null);
      val(fileText[(i*32)+(j*3)+21], newTriangle.texCoords[j].z, null);
    end;
    newTriangle.mtlNum := strToInt(fileText[(i*32)+28]);
    for j := 0 to 2 do
    begin
      if (strToInt(fileText[(i*32)+j+29]) = 1) then newTriangle.sharedCoord[j] := true else newTriangle.sharedCoord[j] := false;
    end;
    if (strToInt(fileText[(i*32)+32]) < 0) then newTriangle.pBone_ := nil else newTriangle.pBone_ := bones[strToInt(fileText[(i*32)+32])];
    setLength(triangles, length(triangles)+1);
    triangles[length(triangles)-1] := newTriangle;
  end;

  setLength(materials, 0);
  totalTriangles *= 32;
  totalMaterials := strToInt(fileText[totalTriangles+1]);
  initialised := false;
  for i := 0 to totalMaterials-1 do
  begin
    newMaterial.name := fileText[totalTriangles+(i*14)+2];
    newMaterial.fileName := setDirSeparators(fileText[totalTriangles+(i*14)+3]);
    val(fileText[totalTriangles+(i*14)+4], newMaterial.ambientColor.r, null);
    val(fileText[totalTriangles+(i*14)+5], newMaterial.ambientColor.g, null);
    val(fileText[totalTriangles+(i*14)+6], newMaterial.ambientColor.b, null);
    val(fileText[totalTriangles+(i*14)+7], newMaterial.diffuseColor.r, null);
    val(fileText[totalTriangles+(i*14)+8], newMaterial.diffuseColor.g, null);
    val(fileText[totalTriangles+(i*14)+9], newMaterial.diffuseColor.b, null);
    val(fileText[totalTriangles+(i*14)+10], newMaterial.specularColor.r, null);
    val(fileText[totalTriangles+(i*14)+11], newMaterial.specularColor.g, null);
    val(fileText[totalTriangles+(i*14)+12], newMaterial.specularColor.b, null);
    val(fileText[totalTriangles+(i*14)+13], newMaterial.shininess, null);
    val(fileText[totalTriangles+(i*14)+14], newMaterial.alpha);
    if (strToInt(fileText[totalTriangles+(i*14)+15]) = 1) then newMaterial.hasTexture := true else newMaterial.hasTexture := false;
    newMaterial.textureId := -1;

    if (newMaterial.hasTexture) then
    begin
      image := IMG_Load(pchar(newMaterial.fileName));
      if (image = nil) then writeln('Could not load texture ', newMaterial.fileName) else
      begin
        if (image^.format^.BytesPerPixel = 4) then
        begin
          if (image^.format^.Rmask = $000000ff) then textureFormat := GL_RGBA else textureFormat := GL_BGRA;
        end
      else
        begin
          if (image^.format^.Rmask = $000000ff) then textureFormat := GL_RGB else textureFormat := GL_BGR;
        end;
        if (not initialised) then
        begin
          initialised := true;
          if (glSlVersion < 1.5) then
          begin
            glGenTextures(1, @texture);
            glBindTexture(GL_TEXTURE_2D, texture);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
            glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE);
            glTexImage2D(GL_TEXTURE_2D, 0, image^.format^.BytesPerPixel, image^.w*totalMaterials, image^.h, 0, textureFormat, GL_UNSIGNED_BYTE, nil);
          end
        else
          begin
            glGenTextures(1, @texture);
            glBindTexture(GL_TEXTURE_2D_ARRAY, texture);
            glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
            glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
            glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_GENERATE_MIPMAP, GL_TRUE);
            glTexImage3D(GL_TEXTURE_2D_ARRAY, 0, image^.format^.BytesPerPixel, image^.w, image^.h, totalMaterials, 0, textureFormat, GL_UNSIGNED_BYTE, nil);
          end;
        end;
        if (glSlVersion < 1.5) then glTexSubImage2D(GL_TEXTURE_2D, 0, image^.w*i, 0, image^.w, image^.h, textureFormat, GL_UNSIGNED_BYTE, image^.pixels)
          else glTexSubImage3D(GL_TEXTURE_2D_ARRAY, 0, 0, 0, i, image^.w, image^.h, 1, textureFormat, GL_UNSIGNED_BYTE, image^.pixels);

        SDL_FreeSurface(image);
        newMaterial.textureId := i;
      end;
    end;
    size := length(materials);
    setLength(materials, size+1);
    materials[size] := newMaterial;
  end;
  if (glSlVersion < 1.5) then glBindTexture(GL_TEXTURE_2D, 0) else glBindTexture(GL_TEXTURE_2D_ARRAY, 0);
  for i := 0 to length(triangles)-1 do
  begin
    if (triangles[i].pBone_ <> nil) then
    begin
      size := length(triangles[i].pBone_^.triangles);
      setLength(triangles[i].pBone_^.triangles, size+1);
      triangles[i].pBone_^.triangles[size] := @triangles[i];
    end;
  end;
end;

procedure TModel.loadSms(fileName : string);
var
  fileText : array of string;
  skeletonFile : text;
  tempStr : string;
  i, j, totalBones, null, size : integer;
  b : pbone;
begin
  setLength(fileText, 0);
  assign(skeletonFile, fileName);
  reset(skeletonFile);
  repeat
    readln(skeletonFile, tempStr);
    while ((rightStr(tempStr, 1) = #13) or (rightStr(tempStr, 1) = #10)) do tempStr := leftStr(tempStr, length(tempStr)-1);
    setLength(fileText, length(fileText)+1);
    fileText[length(fileText)-1] := tempStr;
  until Eof(skeletonFile);
  close(skeletonFile);

  while (fileText[length(fileText)-1] = '') do setLength(fileText, length(fileText)-1);

  if (length(triangles) > 0) then for i := 0 to length(triangles)-1 do triangles[i].pBone_ := nil;
  if (length(bones) > 0) then for i := 0 to length(bones)-1 do dispose(bones[i]);
  setLength(bones, 0);
  totalBones := strToInt(fileText[0]);
  for i := 0 to totalBones-1 do
  begin
    setLength(bones, length(bones)+1);
    bones[i] := new(pbone);
    b := bones[i];
    setLength(b^.triangles, 0);
    b^.id := strToInt(fileText[(i*17)+1]);
    val(fileText[(i*17)+2], b^.x, null);
    val(fileText[(i*17)+3], b^.y, null);
    val(fileText[(i*17)+4], b^.z, null);
    val(fileText[(i*17)+5], b^.endX, null);
    val(fileText[(i*17)+6], b^.endY, null);
    val(fileText[(i*17)+7], b^.endZ, null);
    if (strToInt(fileText[(i*17)+8]) > -1) then b^.parent := bones[strToInt(fileText[(i*17)+8])] else b^.parent := nil;

    initBox(@b^.hitbox);
    val(fileText[(i*17)+9], b^.hitbox.x, null);
    val(fileText[(i*17)+10], b^.hitbox.y, null);
    val(fileText[(i*17)+11], b^.hitbox.z, null);
    val(fileText[(i*17)+12], b^.hitbox.l, null);
    val(fileText[(i*17)+13], b^.hitbox.w, null);
    val(fileText[(i*17)+14], b^.hitbox.h, null);
    val(fileText[(i*17)+15], b^.hitbox.xRot, null);
    val(fileText[(i*17)+16], b^.hitbox.yRot, null);
    val(fileText[(i*17)+17], b^.hitbox.zRot, null);
    b^.hitBox.rl := b^.hitBox.l;
    b^.hitBox.rw := b^.hitBox.w;
    b^.hitBox.rh := b^.hitBox.h;
  end;

  for i := 0 to length(bones)-1 do
  begin
    for j := 0 to length(bones)-1 do if (bones[i] = bones[j]^.parent) then
    begin
      size := length(bones[i]^.child);
      setLength(bones[i]^.child, size+1);
      bones[i]^.child[size] := bones[j];
    end;
  end;

  for i := 0 to length(triangles)-1 do
  begin
    if (triangles[i].boneId > -1) then triangles[i].pBone_ := bones[triangles[i].boneId] else triangles[i].pBone_ := nil;
  end;
end;

procedure TModel.loadSma(fileName : string);
var
  fileText : array of string;
  animationFile : text;
  tempStr : string;
  i, j, totalFrames, line, totalBoneData, null, size : integer;
  newAnimation : animation;
  newFrame : keyFrame;
  newBoneData : keyFrameData;
begin
  setLength(fileText, 0);
  assign(animationFile, fileName);
  reset(animationFile);
  repeat
    readln(animationFile, tempStr);
    while ((rightStr(tempStr, 1) = #13) or (rightStr(tempStr, 1) = #10)) do tempStr := leftStr(tempStr, length(tempStr)-1);
    setLength(fileText, length(fileText)+1);
    fileText[length(fileText)-1] := tempStr;
  until Eof(animationFile);
  close(animationFile);

  while (fileText[length(fileText)-1] = '') do setLength(fileText, length(fileText)-1);

  newAnimation.name := fileText[0];
  totalFrames := strToInt(fileText[1]);
  line := 2;
  for i := 0 to totalFrames-1 do
  begin
    setLength(newFrame.boneData, 0);
    newFrame.step := strToInt(fileText[line]);
    line += 1;
    totalBoneData := strToInt(fileText[line]);
    line += 1;
    for j := 0 to totalBoneData-1 do
    begin
      newBoneData.boneId := strToInt(fileText[line]);
      line += 1;
      val(fileText[line], newBoneData.xRot, null);
      line += 1;
      val(fileText[line], newBoneData.yRot, null);
      line += 1;
      val(fileText[line], newBoneData.zRot, null);
      line += 1;
      size := length(newFrame.boneData);
      setLength(newFrame.boneData, size+1);
      newFrame.boneData[size] := newBoneData;
    end;
    size := length(newAnimation.frames);
    setLength(newAnimation.frames, size+1);
    newAnimation.frames[size] := newFrame;
  end;
  size := length(animations);
  setLength(animations, size+1);
  animations[size] := newAnimation;
end;

procedure TModel.loadSmo(path, fileName : string; customBufferFunction : customModelBufferFunctionType = nil; customData : Pointer = nil);
var
  fileText : array of string;
  objectFile : text;
  tempStr : string;
  i : integer;
begin
  setLength(fileText, 0);
  assign(objectFile, path+fileName);
  reset(objectFile);
  repeat
    readln(objectFile, tempStr);
    while ((rightStr(tempStr, 1) = #13) or (rightStr(tempStr, 1) = #10)) do tempStr := leftStr(tempStr, length(tempStr)-1);
    setLength(fileText, length(fileText)+1);
    fileText[length(fileText)-1] := tempStr;
  until Eof(objectFile);
  close(objectFile);

  while (fileText[length(fileText)-1] = '') do setLength(fileText, length(fileText)-1);

  loadSms(setDirSeparators(path+fileText[1]));
  loadSmm(setDirSeparators(path+fileText[0]));
  for i := 2 to length(fileText)-1 do loadSma(setDirSeparators(path+fileText[i]));

  if (customBufferFunction = nil) then initBufferSmo else customBufferFunction(@vbo, @self, customData);
  if (vertexArrayObjectSupported) then glBindVertexArray(0);
  for i := 0 to 15 do glDisableVertexAttribArray(i);
  loadedFromObj := false;
end;

procedure TModel.initBufferObj;
var
  vertexArray : array of GLfloat;
  count, i : word;
  j : integer;
begin
  setLength(vertexArray, length(triangles)*3*23);
  count := 0;
  for i := 0 to length(triangles)-1 do
  begin
    for j := 0 to 2 do
    begin
      vertexArray[count] := triangles[i].coords[j].x;
      count += 1;
      vertexArray[count] := triangles[i].coords[j].y;
      count += 1;
      vertexArray[count] := triangles[i].coords[j].z;
      count += 1;
      vertexArray[count] := 1.0;
      count += 1;
      vertexArray[count] := triangles[i].coords[j].normal_.x;
      count += 1;
      vertexArray[count] := triangles[i].coords[j].normal_.y;
      count += 1;
      vertexArray[count] := triangles[i].coords[j].normal_.z;
      count += 1;
      vertexArray[count] := materials[triangles[i].mtlNum].ambientColor.r;
      count += 1;
      vertexArray[count] := materials[triangles[i].mtlNum].ambientColor.g;
      count += 1;
      vertexArray[count] := materials[triangles[i].mtlNum].ambientColor.b;
      count += 1;
      vertexArray[count] := materials[triangles[i].mtlNum].diffuseColor.r;
      count += 1;
      vertexArray[count] := materials[triangles[i].mtlNum].diffuseColor.g;
      count += 1;
      vertexArray[count] := materials[triangles[i].mtlNum].diffuseColor.b;
      count += 1;
      vertexArray[count] := materials[triangles[i].mtlNum].specularColor.r;
      count += 1;
      vertexArray[count] := materials[triangles[i].mtlNum].specularColor.g;
      count += 1;
      vertexArray[count] := materials[triangles[i].mtlNum].specularColor.b;
      count += 1;
      vertexArray[count] := triangles[i].texCoords[j].x;
      count += 1;
      vertexArray[count] := triangles[i].texCoords[j].y;
      count += 1;
      vertexArray[count] := triangles[i].texCoords[j].z;
      count += 1;
      vertexArray[count] := triangles[i].mtlNum;
      count += 1;
      if (materials[triangles[i].mtlNum].hasTexture) then vertexArray[count] := 1.0 else vertexArray[count] := 0.0;
      count += 1;
      vertexArray[count] := materials[triangles[i].mtlNum].shininess;
      count += 1;
      vertexArray[count] := materials[triangles[i].mtlNum].alpha;
      count += 1;
    end;
  end;

  glGenBuffers(1, @vbo);
  glBindBuffer(GL_ARRAY_BUFFER, vbo);
  glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat)*length(vertexArray), @vertexArray, GL_STATIC_DRAW);
  glVertexAttribPointer(VERTEX_ATTRIBUTE, 4, GL_FLOAT, false, sizeof(GLfloat)*23, nil);
  glVertexAttribPointer(NORMAL_ATTRIBUTE, 3, GL_FLOAT, false, sizeof(GLfloat)*23, PGLvoid(sizeof(GLfloat)*4));
  glVertexAttribPointer(COLOR0_ATTRIBUTE, 3, GL_FLOAT, false, sizeof(GLfloat)*23, PGLvoid(sizeof(GLfloat)*7));
  glVertexAttribPointer(COLOR1_ATTRIBUTE, 3, GL_FLOAT, false, sizeof(GLfloat)*23, PGLvoid(sizeof(GLfloat)*10));
  glVertexAttribPointer(COLOR2_ATTRIBUTE, 3, GL_FLOAT, false, sizeof(GLfloat)*23, PGLvoid(sizeof(GLfloat)*13));
  glVertexAttribPointer(TEXTURE0_ATTRIBUTE, 3, GL_FLOAT, false, sizeof(GLfloat)*23, PGLvoid(sizeof(GLfloat)*16));
  glVertexAttribPointer(EXTRA0_ATTRIBUTE, 1, GL_FLOAT, false, sizeof(GLfloat)*23, PGLvoid(sizeof(GLfloat)*19));
  glVertexAttribPointer(EXTRA1_ATTRIBUTE, 1, GL_FLOAT, false, sizeof(GLfloat)*23, PGLvoid(sizeof(GLfloat)*20));
  glVertexAttribPointer(EXTRA2_ATTRIBUTE, 1, GL_FLOAT, false, sizeof(GLfloat)*23, PGLvoid(sizeof(GLfloat)*21));
  glVertexAttribPointer(EXTRA3_ATTRIBUTE, 1, GL_FLOAT, false, sizeof(GLfloat)*23, PGLvoid(sizeof(GLfloat)*22));

  glEnableVertexAttribArray(VERTEX_ATTRIBUTE);
  glEnableVertexAttribArray(NORMAL_ATTRIBUTE);
  glEnableVertexAttribArray(COLOR0_ATTRIBUTE);
  glEnableVertexAttribArray(COLOR1_ATTRIBUTE);
  glEnableVertexAttribArray(COLOR2_ATTRIBUTE);
  glEnableVertexAttribArray(TEXTURE0_ATTRIBUTE);
  glEnableVertexAttribArray(EXTRA0_ATTRIBUTE);
  glEnableVertexAttribArray(EXTRA1_ATTRIBUTE);
  glEnableVertexAttribArray(EXTRA2_ATTRIBUTE);
  glEnableVertexAttribArray(EXTRA3_ATTRIBUTE);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
end;

procedure TModel.initBufferSmo;
var
  vertexArray : array[0..50000] of GLfloat;//2069999
  count, offset, i, j, k : integer;
begin
  count := 0;
  for i := 0 to length(bones)-1 do
  begin
    count += length(bones[i]^.triangles)*3;
  end;

  if (vertexArrayObjectSupported) then
  begin
    glGenVertexArrays(1, @vao);
    glBindVertexArray(vao);
  end;

  glGenBuffers(1, @vbo);
  glBindBuffer(GL_ARRAY_BUFFER, vbo);
  glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat)*count*23, nil, GL_STATIC_DRAW);

  offset := 0;

  for i := 0 to length(bones)-1 do
  begin
    bones[i]^.offset := offset;
    count := 0;
    if (length(bones[i]^.triangles) > 0) then
    begin
      for j := 0 to length(bones[i]^.triangles)-1 do
      begin
        for k := 0 to 2 do
        begin
          vertexArray[count] := bones[i]^.triangles[j]^.coords[k].x;
          count += 1;
          vertexArray[count] := bones[i]^.triangles[j]^.coords[k].y;
          count += 1;
          vertexArray[count] := bones[i]^.triangles[j]^.coords[k].z;
          count += 1;
          if (bones[i]^.triangles[j]^.sharedCoord[k]) then vertexArray[count] := 1.0 else vertexArray[count] := 0.0;
          count += 1;
          vertexArray[count] := bones[i]^.triangles[j]^.coords[k].normal_.x;
          count += 1;
          vertexArray[count] := bones[i]^.triangles[j]^.coords[k].normal_.y;
          count += 1;
          vertexArray[count] := bones[i]^.triangles[j]^.coords[k].normal_.z;
          count += 1;
          vertexArray[count] := materials[bones[i]^.triangles[j]^.mtlNum].ambientColor.r;
          count += 1;
          vertexArray[count] := materials[bones[i]^.triangles[j]^.mtlNum].ambientColor.g;
          count += 1;
          vertexArray[count] := materials[bones[i]^.triangles[j]^.mtlNum].ambientColor.b;
          count += 1;
          vertexArray[count] := materials[bones[i]^.triangles[j]^.mtlNum].diffuseColor.r;
          count += 1;
          vertexArray[count] := materials[bones[i]^.triangles[j]^.mtlNum].diffuseColor.g;
          count += 1;
          vertexArray[count] := materials[bones[i]^.triangles[j]^.mtlNum].diffuseColor.b;
          count += 1;
          vertexArray[count] := materials[bones[i]^.triangles[j]^.mtlNum].specularColor.r;
          count += 1;
          vertexArray[count] := materials[bones[i]^.triangles[j]^.mtlNum].specularColor.g;
          count += 1;
          vertexArray[count] := materials[bones[i]^.triangles[j]^.mtlNum].specularColor.b;
          count += 1;
          vertexArray[count] := bones[i]^.triangles[j]^.texCoords[k].x;
          count += 1;
          vertexArray[count] := bones[i]^.triangles[j]^.texCoords[k].y;
          count += 1;
          vertexArray[count] := bones[i]^.triangles[j]^.texCoords[k].z;
          count += 1;
          vertexArray[count] := bones[i]^.triangles[j]^.mtlNum;
          count += 1;
          if (materials[bones[i]^.triangles[j]^.mtlNum].hasTexture) then vertexArray[count] := 1.0 else vertexArray[count] := 0.0;
          count += 1;
          vertexArray[count] := materials[bones[i]^.triangles[j]^.mtlNum].shininess;
          count += 1;
          vertexArray[count] := materials[bones[i]^.triangles[j]^.mtlNum].alpha;
          count += 1;
        end;
      end;
      glBufferSubData(GL_ARRAY_BUFFER, sizeof(GLfloat)*offset*23, sizeof(GLfloat)*count, @vertexArray);
      offset += length(bones[i]^.triangles)*3;
    end;
  end;

  if (vertexArrayObjectSupported) then
  begin
    glVertexAttribPointer(VERTEX_ATTRIBUTE, 4, GL_FLOAT, false, sizeof(GLfloat)*23, nil);
    glVertexAttribPointer(NORMAL_ATTRIBUTE, 3, GL_FLOAT, false, sizeof(GLfloat)*23, PGLvoid(sizeof(GLfloat)*4));
    glVertexAttribPointer(COLOR0_ATTRIBUTE, 3, GL_FLOAT, false, sizeof(GLfloat)*23, PGLvoid(sizeof(GLfloat)*7));
    glVertexAttribPointer(COLOR1_ATTRIBUTE, 3, GL_FLOAT, false, sizeof(GLfloat)*23, PGLvoid(sizeof(GLfloat)*10));
    glVertexAttribPointer(COLOR2_ATTRIBUTE, 3, GL_FLOAT, false, sizeof(GLfloat)*23, PGLvoid(sizeof(GLfloat)*13));
    glVertexAttribPointer(TEXTURE0_ATTRIBUTE, 3, GL_FLOAT, false, sizeof(GLfloat)*23, PGLvoid(sizeof(GLfloat)*16));
    glVertexAttribPointer(EXTRA0_ATTRIBUTE, 1, GL_FLOAT, false, sizeof(GLfloat)*23, PGLvoid(sizeof(GLfloat)*19));
    glVertexAttribPointer(EXTRA1_ATTRIBUTE, 1, GL_FLOAT, false, sizeof(GLfloat)*23, PGLvoid(sizeof(GLfloat)*20));
    glVertexAttribPointer(EXTRA2_ATTRIBUTE, 1, GL_FLOAT, false, sizeof(GLfloat)*23, PGLvoid(sizeof(GLfloat)*21));
    glVertexAttribPointer(EXTRA3_ATTRIBUTE, 1, GL_FLOAT, false, sizeof(GLfloat)*23, PGLvoid(sizeof(GLfloat)*22));

    glEnableVertexAttribArray(VERTEX_ATTRIBUTE);
    glEnableVertexAttribArray(NORMAL_ATTRIBUTE);
    glEnableVertexAttribArray(COLOR0_ATTRIBUTE);
    glEnableVertexAttribArray(COLOR1_ATTRIBUTE);
    glEnableVertexAttribArray(COLOR2_ATTRIBUTE);
    glEnableVertexAttribArray(TEXTURE0_ATTRIBUTE);
    glEnableVertexAttribArray(EXTRA0_ATTRIBUTE);
    glEnableVertexAttribArray(EXTRA1_ATTRIBUTE);
    glEnableVertexAttribArray(EXTRA2_ATTRIBUTE);
    glEnableVertexAttribArray(EXTRA3_ATTRIBUTE);
  end;

  glBindBuffer(GL_ARRAY_BUFFER, 0);
  if (vertexArrayObjectSupported) then glBindVertexArray(0);
  for i := 0 to 15 do glDisableVertexAttribArray(i);
end;

procedure TModel.drawObj(shaderToUse : PShader);
begin
  shaderToUse^.setUniform16(MODELVIEW_LOCATION, getMatrix(MODELVIEW_MATRIX));
  shaderToUse^.setUniform16(PROJECTION_LOCATION, getMatrix(PROJECTION_MATRIX));

  glBindVertexArray(vao);
  glBindBuffer(GL_ARRAY_BUFFER, vbo);
  glDrawArrays(GL_TRIANGLES, 0, length(triangles)*3);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
end;

function TModel.calculatePoints(nx, ny, nz : real; matrix : matrix4d) : normal;
begin
  result.x := (nx*matrix[0])+(ny*matrix[4])+(nz*matrix[8])+matrix[12];
  result.y := (nx*matrix[1])+(ny*matrix[5])+(nz*matrix[9])+matrix[13];
  result.z := (nx*matrix[2])+(ny*matrix[6])+(nz*matrix[10])+matrix[14];
end;

procedure TModel.calculateHitbox(pBone_ : pbone; matrix : matrix4d);
var
  nx, ny, nz, lLowerBound, lUpperBound, wLowerBound, wUpperBound, hLowerBound, hUpperBound : real;
  point : array[0..7] of normal;
  i : integer;
begin
  nx := 0.0;
  ny := -pBone_^.hitbox.h/2.0;
  nz := pBone_^.hitbox.w/2.0;
  point[0] := calculatePoints(nx, ny, nz, matrix);

  nx := 0.0;
  ny := pBone_^.hitbox.h/2.0;
  nz := pBone_^.hitbox.w/2.0;
  point[1] := calculatePoints(nx, ny, nz, matrix);

  nx := 0.0;
  ny := pBone_^.hitbox.h/2.0;
  nz := -pBone_^.hitbox.w/2.0;
  point[2] := calculatePoints(nx, ny, nz, matrix);

  nx := 0.0;
  ny := -pBone_^.hitbox.h/2.0;
  nz := -pBone_^.hitbox.w/2.0;
  point[3] := calculatePoints(nx, ny, nz, matrix);

  nx := pBone_^.hitbox.l;
  ny := -pBone_^.hitbox.h/2.0;
  nz := pBone_^.hitbox.w/2.0;
  point[4] := calculatePoints(nx, ny, nz, matrix);

  nx := pBone_^.hitbox.l;
  ny := pBone_^.hitbox.h/2.0;
  nz := pBone_^.hitbox.w/2.0;
  point[5] := calculatePoints(nx, ny, nz, matrix);

  nx := pBone_^.hitbox.l;
  ny := pBone_^.hitbox.h/2.0;
  nz := -pBone_^.hitbox.w/2.0;
  point[6] := calculatePoints(nx, ny, nz, matrix);

  nx := pBone_^.hitbox.l;
  ny := -pBone_^.hitbox.h/2.0;
  nz := -pBone_^.hitbox.w/2.0;
  point[7] := calculatePoints(nx, ny, nz, matrix);

  lLowerBound := point[0].x;
  lUpperBound := point[0].x;
  wLowerBound := point[0].z;
  wUpperBound := point[0].z;
  hLowerBound := point[0].y;
  hUpperBound := point[0].y;
  for i := 0 to 7 do
  begin
    if (point[i].x < lLowerBound) then lLowerBound := point[i].x else if (point[i].x > lUpperBound) then lUpperBound := point[i].x;
    if (point[i].z < wLowerBound) then wLowerBound := point[i].z else if (point[i].z > wUpperBound) then wUpperBound := point[i].z;
    if (point[i].y < hLowerBound) then hLowerBound := point[i].y else if (point[i].y > hUpperBound) then hUpperBound := point[i].y;
  end;
  pBone_^.hitbox.rl := lUpperBound-lLowerBound;
  pBone_^.hitbox.rw := wUpperBound-wLowerBound;
  pBone_^.hitbox.rh := hUpperBound-hLowerBound;
end;

procedure TModel.drawBone(pBone_ : pbone; shaderToUse : PShader; skipHitboxes : boolean = false);
var
  modelMatrix, modelNormMatrix : matrix4d;
  i : integer;
  nx, ny, nz : real;
begin
  pushMatrix;
    pushMatrix;
      copyMatrix(IDENTITY_MATRIX, MODELVIEW_MATRIX);
      translateMatrix(pBone_^.x, pBone_^.y, pBone_^.z);
      rotateMatrix(pBone_^.xRot, 1.0, 0.0, 0.0);
      rotateMatrix(pBone_^.yRot, 0.0, 1.0, 0.0);
      rotateMatrix(pBone_^.zRot, 0.0, 0.0, 1.0);
      translateMatrix(-pBone_^.x, -pBone_^.y, -pBone_^.z);
      modelMatrix := getMatrix(MODELVIEW_MATRIX);
    popMatrix;

    shaderToUse^.setUniform16(EXTRA0_LOCATION, getMatrix(MODELVIEW_MATRIX));

    copyMatrix(multiplyMatrix(getMatrix(MODELVIEW_MATRIX), modelMatrix), MODELVIEW_MATRIX);

    shaderToUse^.setUniform16(MODELVIEW_LOCATION, getMatrix(MODELVIEW_MATRIX));
    shaderToUse^.setUniform16(PROJECTION_LOCATION, getMatrix(PROJECTION_MATRIX));

    glDrawArrays(GL_TRIANGLES, pBone_^.offset, length(pBone_^.triangles)*3);

    if (not skipHitboxes) then
    begin
      if (pBone_^.parent = nil) then
      begin
        pBone_^.hitbox.x := 0.0;
        pBone_^.hitbox.y := 0.0;
        pBone_^.hitbox.z := 0.0;
      end
    else
      begin
        pBone_^.hitbox.x := pBone_^.parent^.hitbox.x+pBone_^.parent^.endX;
        pBone_^.hitbox.y := pBone_^.parent^.hitbox.y+pBone_^.parent^.endY;
        pBone_^.hitbox.z := pBone_^.parent^.hitbox.z+pBone_^.parent^.endZ;
      end;
      nx := pBone_^.endX;
      ny := pBone_^.endY;
      nz := pBone_^.endZ;
      pBone_^.endX := (nx*modelNormMatrix[0])+(ny*modelNormMatrix[4])+(nz*modelNormMatrix[8])+modelNormMatrix[12];
      pBone_^.endY := (nx*modelNormMatrix[1])+(ny*modelNormMatrix[5])+(nz*modelNormMatrix[9])+modelNormMatrix[13];
      pBone_^.endZ := (nx*modelNormMatrix[2])+(ny*modelNormMatrix[6])+(nz*modelNormMatrix[10])+modelNormMatrix[14];
      calculateHitbox(pBone_, modelNormMatrix);
    end;

    if (length(pBone_^.child) > 0) then for i := 0 to length(pBone_^.child)-1 do drawBone(pBone_^.child[i], shaderToUse, skipHitboxes);
  popMatrix;
end;

function TModel.name : string;
begin
  result := name_;
end;

procedure TModel.draw(objectParam : PGameObject; skipAnimation : boolean = false; skipHitboxes : boolean = false);
var
  shaderToUse : PShader;
  i, currentKeyFrame, stepDiff, size : integer;
  thisKeyFrame, nextKeyFrame : pkeyFrame;
  tempFrame, diff, diff1, diff2 : real;
begin
  if (objectParam^.model_ = nil) then exit;
  if (objectParam^.boundShader <> nil) then shaderToUse := objectParam^.boundShader else if (boundShader_ <> nil) then shaderToUse := boundShader_ else
    shaderToUse := globalBoundShader;

  if (shaderToUse <> nil) then
  begin
    shaderToUse^.use;
    glActiveTexture(GL_TEXTURE0);
    if (glSlVersion < 1.5) then
    begin
      glBindTexture(GL_TEXTURE_2D, texture);
      shaderToUse^.setUniform1(TEXCOMPAT_LOCATION, length(materials));
    end else glBindTexture(GL_TEXTURE_2D_ARRAY, texture);
    shaderToUse^.setUniform1(TEXSAMPLER_LOCATION, 0);

    setMatrix(MODELVIEW_MATRIX);
    pushMatrix;
      translateMatrix(objectParam^.x_, objectParam^.y_, objectParam^.z_);
      rotateMatrix(objectParam^.xRotation_, 1.0, 0.0, 0.0);
      rotateMatrix(objectParam^.yRotation_, 0.0, 1.0, 0.0);
      rotateMatrix(objectParam^.zRotation_, 0.0, 0.0, 1.0);
      scaleMatrix(objectParam^.xScale_, objectParam^.yScale_, objectParam^.zScale_);

      if (vertexArrayObjectSupported) then glBindVertexArray(vao);
      if (loadedFromObj) then drawObj(shaderToUse) else
      begin
        if (not skipAnimation) then
        begin
          if (length(animations) > 0) then
          begin
            if (length(animations[objectParam^.currentAnimationId].frames) > 1) then
            begin
              if (objectParam^.interpolating) then
              begin
                thisKeyFrame := objectParam^.fakeKeyFrame1;
                nextKeyFrame := objectParam^.fakeKeyFrame2;
                if (objectParam^.frame_ > nextKeyFrame^.step) then objectParam^.frame_ -= nextKeyFrame^.step else
                  if (objectParam^.frame_ < 0) then objectParam^.frame_ := nextKeyFrame^.step+(objectParam^.frame_+1);
              end
            else
              begin
                currentKeyFrame := 0;
                size := length(animations[objectParam^.currentAnimationId].frames);
                while (objectParam^.frame_ > animations[objectParam^.currentAnimationId].frames[size-1].step) do objectParam^.frame_ -= animations[objectParam^.currentAnimationId].frames[size-1].step;
                while (objectParam^.frame_ > animations[objectParam^.currentAnimationId].frames[currentKeyFrame+1].step) do
                begin
                  currentKeyFrame += 1;
                  if (currentKeyFrame >= length(animations[objectParam^.currentAnimationId].frames)) then
                  begin
                    currentKeyFrame := 0;
                    size := length(animations[objectParam^.currentAnimationId].frames);
                    objectParam^.frame_ -= animations[objectParam^.currentAnimationId].frames[size-1].step;
                    break;
                  end;
                end;

                thisKeyFrame := @animations[objectParam^.currentAnimationId].frames[currentKeyFrame];
                if (currentKeyFrame+1 < length(animations[objectParam^.currentAnimationId].frames)) then
                  nextKeyFrame := @animations[objectParam^.currentAnimationId].frames[currentKeyFrame+1] else
                    nextKeyFrame := @animations[objectParam^.currentAnimationId].frames[0];
              end;

              tempFrame := objectParam^.frame_-thisKeyFrame^.step;
              stepDiff := nextKeyFrame^.step-thisKeyFrame^.step;

              for i := 0 to length(bones)-1 do
              begin
                diff1 := nextKeyFrame^.boneData[i].xRot-thisKeyFrame^.boneData[i].xRot;
                diff2 := 360-nextKeyFrame^.boneData[i].xRot;
                if (abs(diff1) < abs(diff2)) then diff := diff1 else diff := diff2;
                bones[thisKeyFrame^.boneData[i].boneId]^.xRot := thisKeyFrame^.boneData[i].xRot+((diff/stepDiff)*tempFrame);

                diff1 := nextKeyFrame^.boneData[i].yRot-thisKeyFrame^.boneData[i].yRot;
                diff2 := 360-nextKeyFrame^.boneData[i].yRot;
                if (abs(diff1) < abs(diff2)) then diff := diff1 else diff := diff2;
                bones[thisKeyFrame^.boneData[i].boneId]^.yRot := thisKeyFrame^.boneData[i].yRot+((diff/stepDiff)*tempFrame);

                diff1 := nextKeyFrame^.boneData[i].zRot-thisKeyFrame^.boneData[i].zRot;
                diff2 := 360-nextKeyFrame^.boneData[i].zRot;
                if (abs(diff1) < abs(diff2)) then diff := diff1 else diff := diff2;
                bones[thisKeyFrame^.boneData[i].boneId]^.zRot := thisKeyFrame^.boneData[i].zRot+((diff/stepDiff)*tempFrame);
              end;
            end;
          end;
        end;

        if (not vertexArrayObjectSupported) then
        begin
          glBindBuffer(GL_ARRAY_BUFFER, vbo);
          glVertexAttribPointer(VERTEX_ATTRIBUTE, 4, GL_FLOAT, false, sizeof(GLfloat)*23, nil);
          glVertexAttribPointer(NORMAL_ATTRIBUTE, 3, GL_FLOAT, false, sizeof(GLfloat)*23, PGLvoid(sizeof(GLfloat)*4));
          glVertexAttribPointer(COLOR0_ATTRIBUTE, 3, GL_FLOAT, false, sizeof(GLfloat)*23, PGLvoid(sizeof(GLfloat)*7));
          glVertexAttribPointer(COLOR1_ATTRIBUTE, 3, GL_FLOAT, false, sizeof(GLfloat)*23, PGLvoid(sizeof(GLfloat)*10));
          glVertexAttribPointer(COLOR2_ATTRIBUTE, 3, GL_FLOAT, false, sizeof(GLfloat)*23, PGLvoid(sizeof(GLfloat)*13));
          glVertexAttribPointer(TEXTURE0_ATTRIBUTE, 3, GL_FLOAT, false, sizeof(GLfloat)*23, PGLvoid(sizeof(GLfloat)*16));
          glVertexAttribPointer(EXTRA0_ATTRIBUTE, 1, GL_FLOAT, false, sizeof(GLfloat)*23, PGLvoid(sizeof(GLfloat)*19));
          glVertexAttribPointer(EXTRA1_ATTRIBUTE, 1, GL_FLOAT, false, sizeof(GLfloat)*23, PGLvoid(sizeof(GLfloat)*20));
          glVertexAttribPointer(EXTRA2_ATTRIBUTE, 1, GL_FLOAT, false, sizeof(GLfloat)*23, PGLvoid(sizeof(GLfloat)*21));
          glVertexAttribPointer(EXTRA3_ATTRIBUTE, 1, GL_FLOAT, false, sizeof(GLfloat)*23, PGLvoid(sizeof(GLfloat)*22));

          glEnableVertexAttribArray(VERTEX_ATTRIBUTE);
          glEnableVertexAttribArray(NORMAL_ATTRIBUTE);
          glEnableVertexAttribArray(COLOR0_ATTRIBUTE);
          glEnableVertexAttribArray(COLOR1_ATTRIBUTE);
          glEnableVertexAttribArray(COLOR2_ATTRIBUTE);
          glEnableVertexAttribArray(TEXTURE0_ATTRIBUTE);
          glEnableVertexAttribArray(EXTRA0_ATTRIBUTE);
          glEnableVertexAttribArray(EXTRA1_ATTRIBUTE);
          glEnableVertexAttribArray(EXTRA2_ATTRIBUTE);
          glEnableVertexAttribArray(EXTRA3_ATTRIBUTE);
        end;

        drawBone(bones[0], shaderToUse, skipHitboxes);
      end;
      if (vertexArrayObjectSupported) then glBindVertexArray(0) else
      begin
        glDisableVertexAttribArray(VERTEX_ATTRIBUTE);
        glDisableVertexAttribArray(NORMAL_ATTRIBUTE);
        glDisableVertexAttribArray(COLOR0_ATTRIBUTE);
        glDisableVertexAttribArray(COLOR1_ATTRIBUTE);
        glDisableVertexAttribArray(COLOR2_ATTRIBUTE);
        glDisableVertexAttribArray(TEXTURE0_ATTRIBUTE);
        glDisableVertexAttribArray(EXTRA0_ATTRIBUTE);
        glDisableVertexAttribArray(EXTRA1_ATTRIBUTE);
        glDisableVertexAttribArray(EXTRA2_ATTRIBUTE);
        glDisableVertexAttribArray(EXTRA3_ATTRIBUTE);
      end;
    popMatrix;
  end;
  if (globalBoundShader <> nil) then glUseProgram(globalBoundShader()^.getProgram) else glUseProgram(0);
end;

procedure TModel.bindShader(newShader : PShader);
begin
  boundShader_ := newShader;
end;

function TModel.boundShader : PShader;
begin
  result := boundShader_;
end;

function TModel.animationId(searchName : string) : integer;
var
  i : integer;
begin
  result := -1;
  for i := 0 to length(animations)-1 do if (animations[i].name = searchName) then
  begin
    result := i;
    break;
  end;
end;

procedure TModel.setFramerate(newFramerate : word);
begin
  framerate_ := newFramerate;
end;

function TModel.framerate : word;
begin
  result := framerate_;
end;


constructor TGameObject.create(newName : string; destX, destY, destZ : real; newSprite : PSprite = nil; startFrame : word = 0);
begin
  name_ := newName;
  x_ := destX;
  y_ := destY;
  z_ := destZ;
  sprite_ := newSprite;
  frame_ := startFrame;
  currentAnimationId := 0;
  nextAnimationId := 0;
  xRotation_ := 0;
  yRotation_ := 0;
  zRotation_ := 0;
  xScale_ := 1;
  yScale_ := 1;
  zScale_ := 1;
  alpha_ := 1;
  hasModel_ := false;
  if (sprite_ <> nil) then
  begin
    width_ := sprite_^.rect.w;
    height_ := sprite_^.rect.h;
    originX := sprite_^.originX;
    originY := sprite_^.originY;
  end;
  zRotatedWidth_ := width;
  zRotatedHeight_ := height;
  boundShader_ := nil;
  customDrawFunction := nil;
  fakeKeyFrame1 := nil;
  fakeKeyFrame2 := nil;
  interpolating_ := false;
end;

constructor TGameObject.create(newName : string; destX, destY, destZ : real; newModel : PModel = nil);
begin
  name_ := newName;
  x_ := destX;
  y_ := destY;
  z_ := destZ;
  model_ := newModel;
  frame_ := 0;
  currentAnimationId := 0;
  nextAnimationId := 0;
  xRotation_ := 0;
  yRotation_ := 0;
  zRotation_ := 0;
  xScale_ := 1;
  yScale_ := 1;
  zScale_ := 1;
  alpha_ := 1;
  hasModel_ := true;
  boundShader_ := nil;
  customDrawFunction := nil;
  fakeKeyFrame1 := new(pkeyFrame);
  fakeKeyFrame2 := new(pkeyFrame);
  interpolating_ := false;
end;

destructor TGameObject.destroy;
begin
  if (fakeKeyFrame1 <> nil) then dispose(fakeKeyFrame1);
  if (fakeKeyFrame2 <> nil) then dispose(fakeKeyFrame2);
end;

function TGameObject.name : string;
begin
  result := name_;
end;

procedure TGameObject.setSprite(newSprite : PSprite);
begin
  sprite_ := newSprite;
  if (sprite_ <> nil) then
  begin
    width_ := sprite_^.rect.w;
    height_ := sprite_^.rect.h;
    originX := sprite_^.originX;
    originY := sprite_^.originY;
  end;
  hasModel_ := false;
  if (fakeKeyFrame1 <> nil) then dispose(fakeKeyFrame1);
  if (fakeKeyFrame2 <> nil) then dispose(fakeKeyFrame2);
end;

function TGameObject.sprite : PSprite;
begin
  result := sprite_;
end;

procedure TGameObject.setModel(newModel : PModel);
begin
  model_ := newModel;
  hasModel_ := true;
  if (fakeKeyFrame1 <> nil) then fakeKeyFrame1 := new(pkeyFrame);
  if (fakeKeyFrame2 <> nil) then fakeKeyFrame2 := new(pkeyFrame);
end;

function TGameObject.model : PModel;
begin
  result := model_;
end;

function TGameObject.hasModel : boolean;
begin
  result := hasModel_;
end;

procedure TGameObject.bindShader(shader : PShader);
begin
  boundShader_ := shader;
end;

function TGameObject.boundShader : PShader;
begin
  result := boundShader_;
end;

procedure TGameObject.bindCustomDrawFunction(newCustomDrawFunction : customDrawFunctionType);
begin
  customDrawFunction := newCustomDrawFunction;
end;

function TGameObject.boundCustomDrawFunction : customDrawFunctionType;
begin
  result := customDrawFunction;
end;

procedure TGameObject.setPosition(xAmount, yAmount, zAmount : real; relative : boolean = false);
begin
  if (relative) then
  begin
    x_ += xAmount*compensation;
    y_ += yAmount*compensation;
    z_ += zAmount*compensation;
  end
else
  begin
    x_ := xAmount;
    y_ := yAmount;
    z_ := zAmount;
  end;
end;

function TGameObject.setX(amount : real; relative : boolean = false) : real;
begin
  if (relative) then x_ += amount*compensation else x_ := amount;
  result := x_;
end;

function TGameObject.setY(amount : real; relative : boolean = false) : real;
begin
  if (relative) then y_ += amount*compensation else y_ := amount;
  result := y_;
end;

function TGameObject.setZ(amount : real; relative : boolean = false) : real;
begin
  if (relative) then z_ += amount*compensation else z_ := amount;
  result := z_;
end;

function TGameObject.x : real;
begin
  result := x_;
end;

function TGameObject.y : real;
begin
  result := y_;
end;

function TGameObject.z : real;
begin
  result := z_;
end;

function TGameObject.width : real;
begin
  result := width_;
end;

function TGameObject.height : real;
begin
  result := height_;
end;

procedure TGameObject.calcZRotatedDimensions;
var
  pointX, pointY : array[0..3] of real;
  i : integer;
  pointDist, angle, calc, wLowerBound, wUpperBound, hLowerBound, hUpperBound : real;
begin
  if ((zRotation_ = 0.0) or (zRotation = 180.0)) then
  begin
    zRotatedWidth_ := width_;
    zRotatedHeight_ := height_;
  end else if ((zRotation_ = 90.0) or (zRotation_ = 270.0)) then
  begin
    zRotatedWidth_ := height_;
    zRotatedHeight_ := width_;
  end
else
  begin
    pointX[0] := -originX;
    pointY[0] := -originY;

    pointX[1] := -originX;
    pointY[1] := -originY+height_;

    pointX[2] := -originX+width_;
    pointY[2] := -originY+height_;

    pointX[3] := -originX+width_;
    pointY[3] := -originY;

    for i := 0 to 3 do
    begin
      pointDist := sqrt((pointX[i]*pointX[i])+(pointY[i]*pointY[i]));
      angle := arctan2(pointY[i], pointX[i]);
      calc := angle-degToRad(zRotation_);
      pointX[i] := pointDist*cos(calc);
      pointY[i] := pointDist*sin(calc);
    end;
    wLowerBound := pointX[0];
    wUpperBound := pointX[0];
    hLowerBound := pointY[0];
    hUpperBound := pointY[0];
    for i := 0 to 3 do
    begin
      if (pointX[i] < wLowerBound) then wLowerBound := pointX[i];
      if (pointX[i] > wUpperBound) then wUpperBound := pointX[i];
      if (pointY[i] < hLowerBound) then hLowerBound := pointY[i];
      if (pointY[i] > hUpperBound) then hUpperBound := pointY[i];
    end;
    zRotatedWidth_ := wUpperBound-wLowerBound;
    zRotatedHeight_ := hUpperBound-hLowerBound;
  end;
end;

procedure TGameObject.scale(xAmount, yAmount, zAmount : real; relative : boolean = false; recalculateDimensions : boolean = true);
begin
  if (relative) then
  begin
    xScale_ += xAmount*compensation;
    yScale_ += yAmount*compensation;
    zScale_ += zAmount*compensation;
  end
else
  begin
    xScale_ := xAmount;
    yScale_ := yAmount;
    zScale_ := zAmount;
  end;
  width_ *= xScale_;
  height_ *= yScale_;
  originX *= xScale_;
  originY *= yScale_;
  if (recalculateDimensions) then calcZRotatedDimensions;
end;

function TGameObject.xScale : real;
begin
  result := xScale_;
end;

function TGameObject.yScale : real;
begin
  result := yScale_;
end;

function TGameObject.zScale : real;
begin
  result := zScale_;
end;

procedure TGameObject.rotate(xAmount, yAmount, zAmount : real; relative : boolean = false; recalculateDimensions : boolean = true);
begin
  if (relative) then
  begin
    xRotation_ += xAmount*compensation;
    yRotation_ += yAmount*compensation;
    zRotation_ += zAmount*compensation;
  end
else
  begin
    xRotation_ := xAmount;
    yRotation_ := yAmount;
    zRotation_ := zAmount;
  end;

  if (xRotation_ >= 360.0) then xRotation_ -= 360.0 else if (xRotation_ < 0.0) then xRotation_ += 360.0;
  if (yRotation_ >= 360.0) then yRotation_ -= 360.0 else if (yRotation_ < 0.0) then yRotation_ += 360.0;
  if (zRotation_ >= 360.0) then zRotation_ -= 360.0 else if (zRotation_ < 0.0) then zRotation_ += 360.0;
  if (recalculateDimensions) then calcZRotatedDimensions;
end;

function TGameObject.rotate(amount : real; relative : boolean = false; recalculateDimensions : boolean = true) : real;
begin
  if (relative) then zRotation_ += amount*compensation else zRotation_ := amount;
  if (zRotation_ >= 360.0) then zRotation_ -= 360.0 else if (zRotation_ < 0.0) then zRotation_ += 360.0;
  if (recalculateDimensions) then calcZRotatedDimensions;
  result := zRotation_;
end;

function TGameObject.xRotation : real;
begin
  result := xRotation_;
end;

function TGameObject.yRotation : real;
begin
  result := yRotation_;
end;

function TGameObject.zRotation : real;
begin
  result := zRotation_;
end;

function TGameObject.setAlpha(amount : real; relative : boolean = false) : real;
begin
  if (relative) then alpha_ += amount*compensation else alpha_ := amount;
  result := alpha_;
end;

function TGameObject.alpha : real;
begin
  result := alpha_;
end;

procedure TGameObject.setCurrentAnimation(animationId : word);
begin
  currentAnimationId := animationId;
end;

function TGameObject.currentAnimation : word;
begin
  result := currentAnimationId;
end;

procedure TGameObject.setFrame(newFrame : real; relative : boolean = false);
begin
  if (relative) then frame_ += newFrame*compensation else frame_ := newFrame;
  if (not hasModel_) then
  begin
    if (frame_ > sprite_^.frames) then frame_ := (frame_-1.0)-sprite_^.frames else if (frame_ < 0.0) then frame_ := sprite_^.frames+(frame_+1.0);
  end;
end;

function TGameObject.frame : real;
begin
  result := frame_;
end;

procedure TGameObject.animate(start, finish : word; animationId : word = 0);
begin
  if (hasModel_) then
  begin
    currentAnimationId := animationId;
    while (currentAnimationId >= length(model_^.animations)) do currentAnimationId -= length(model_^.animations);
    if (start < finish) then
    begin
      frame_ += compensation;
      if (frame_ > finish) then frame_ := start+((frame_-1.0)-finish);
    end else if (start > finish) then
    begin
      frame_ -= compensation;
      if (frame_ < 0.0) then frame_ := finish+(frame_+1.0) else if (frame_ < start) then frame_ := finish+((frame_-start)+1.0);
    end
  end
else
  begin
    if (start < finish) then
    begin
      frame_ += compensation;
      if (frame_ > finish) then frame_ := start+((frame_-1.0)-finish) else if (frame_ > sprite_^.frames) then frame_ := start+((frame_-1.0)-sprite_^.frames);
    end else if (start > finish) then
    begin
      frame_ -= compensation;
      if (frame_ < 0.0) then frame_ := finish+(frame_+1.0) else if (frame_ < start) then frame_ := finish+((frame_-start)+1.0);
    end;
  end;
end;

procedure TGameObject.setInterpolation(startFrame, animation1Id, endFrame, animation2Id, numFramesToTake : integer);
var
  i, animationId, currentKeyFrame, size, tempFrame, stepDiff, frameCount : integer;
  frameToUse : ^integer;
  fakeKeyFrame, thisKeyFrame, nextKeyFrame : pkeyFrame;
  diff, diff1, diff2 : real;
begin
  if (hasModel_) then
  begin
    interpolating_ := true;
    fakeKeyFrame1^.step := 0;
    fakeKeyFrame1^.boneData := model_^.animations[animation1Id].frames[0].boneData;
    fakeKeyFrame2^.step := numFramesToTake-1;
    fakeKeyFrame2^.boneData := model_^.animations[animation2Id].frames[0].boneData;

    for frameCount := 0 to 1 do
    begin
      if (frameCount = 1) then
      begin
        frameToUse := @endFrame;
        fakeKeyFrame := fakeKeyFrame2;
        animationId := animation2Id;
      end
    else
      begin
        frameToUse := @startFrame;
        fakeKeyFrame := fakeKeyFrame1;
        animationId := animation1Id;
      end;

      currentKeyFrame := 0;
      while (frameToUse^ > model_^.animations[animationId].frames[currentKeyFrame+1].step) do
      begin
        currentKeyFrame += 1;
        if (currentKeyFrame >= length(model_^.animations[animationId].frames)) then
        begin
          currentKeyFrame := 0;
          size := length(model_^.animations[animationId].frames);
          frameToUse^ -= model_^.animations[animationId].frames[size-1].step;
          break;
        end;
      end;

      thisKeyFrame := @model_^.animations[animationId].frames[currentKeyFrame];
      if (currentKeyFrame+1 < length(model_^.animations[animationId].frames)) then nextKeyFrame := @model_^.animations[animationId].frames[currentKeyFrame+1]
        else nextKeyFrame := @model_^.animations[animationId].frames[0];

      tempFrame := frameToUse^-thisKeyFrame^.step;
      stepDiff := nextKeyFrame^.step-thisKeyFrame^.step;

      for i := 0 to length(model_^.bones)-1 do
      begin
        diff1 := nextKeyFrame^.boneData[i].xRot-thisKeyFrame^.boneData[i].xRot;
        diff2 := 360.0-nextKeyFrame^.boneData[i].xRot;
        if (abs(diff1) < abs(diff2)) then diff := diff1 else diff := diff2;
        fakeKeyFrame^.boneData[i].xRot := thisKeyFrame^.boneData[i].xRot+((diff/stepDiff)*tempFrame);

        diff1 := nextKeyFrame^.boneData[i].yRot-thisKeyFrame^.boneData[i].yRot;
        diff2 := 360-nextKeyFrame^.boneData[i].yRot;
        if (abs(diff1) < abs(diff2)) then diff := diff1 else diff := diff2;
        fakeKeyFrame^.boneData[i].yRot := thisKeyFrame^.boneData[i].yRot+((diff/stepDiff)*tempFrame);

        diff1 := nextKeyFrame^.boneData[i].zRot-thisKeyFrame^.boneData[i].zRot;
        diff2 := 360-nextKeyFrame^.boneData[i].zRot;
        if (abs(diff1) < abs(diff2)) then diff := diff1 else diff := diff2;
        fakeKeyFrame^.boneData[i].zRot := thisKeyFrame^.boneData[i].zRot+((diff/stepDiff)*tempFrame);
      end;
    end;
  end;
end;

procedure TGameObject.setInterpolation;
begin
  interpolating_ := false;
end;

function TGameObject.interpolating : boolean;
begin
  result := interpolating_;
end;

procedure TGameObject.draw(skipAnimation : boolean = false; skipHitboxes : boolean = false);
begin
  if (hasModel_) then model_^.draw(@self, skipAnimation, skipHitboxes) else sprite_^.draw(@self);
end;

function TGameObject.mouseOverBox : boolean;
var
  mouseX_, mouseY_, mouseXDist, mouseYDist, mouseDist, angle : real;
begin
  result := false;
  if (not hasModel_) then
  begin
    if (zRotation_ <> 0.0) then
    begin
      mouseXDist := mouseX-x_;
      mouseYDist := mouseY-y_;
      mouseDist := sqrt((mouseXDist*mouseXDist)+(mouseYDist*mouseYDist));
      angle := arctan2(mouseYDist, mouseXDist);
      mouseX_ := (mouseDist*cos(angle-degToRad(zRotation_)))+x_;
      mouseY_ := (mouseDist*sin(angle-degToRad(zRotation_)))+y_;
    end
  else
    begin
      mouseX_ := mouseX;
      mouseY_ := mouseY;
    end;

    if ((mouseX_ >= x_-originX) and (mouseX_ <= x_-originX+width_)) then
    begin
      if ((mouseY_ >= y_-originY) and (mouseY_ <= y_-originY+height_)) then result := true;
    end;
  end;
end;

function TGameObject.roughMouseOverBox : boolean;
begin
  result := true;
  if (not hasModel_) then
  begin
    if (mouseX < x_-originX) then
    begin
      result := false;
      exit;
    end;
    if (mouseX > x_-originX+zRotatedWidth_) then
    begin
      result := false;
      exit;
    end;
    if (mouseY < y_-originY) then
    begin
      result := false;
      exit;
    end;
    if (mouseY > y_-originY+zRotatedHeight_) then
    begin
      result := false;
      exit;
    end;
  end else result := false;
end;

function TGameObject.mouseOverCircle(extraX : real = 0; extraY : real = 0; extraZ : real = 0) : boolean;
var
  mouseXDist, mouseYDist, mouseDist : real;
begin
  result := false;
  if (not hasModel_) then
  begin
    mouseXDist := mouseX-(x_+extraX);
    mouseYDist := mouseY-(y_+extraY);
    mouseDist := (mouseXDist*mouseXDist)+(mouseYDist*mouseYDist);
    if (mouseDist <= (width_/2)*(width_/2)) then result := true;
  end;
end;

function TGameObject.boxCollision(other : PGameObject; allStages : boolean = true) : boolean;
var
  i : integer;
  pointX, pointY, tempPointX, tempPointY, tempPointDist, tempAngle, angle, calc, pointXDist, pointYDist, pointDist : real;
begin
  result := false;
  if ((not hasModel_) and (not other^.hasModel_)) then
  begin
    for i := 0 to 3 do
    begin
      case i of
      0:  begin
        tempPointX := -other^.originX;
        tempPointY := -other^.originY;
        end;
      1:  begin
        tempPointX := -other^.originX;
        tempPointY := -other^.originY+other^.height_;
        end;
      2:  begin
        tempPointX := -other^.originX+other^.width_;
        tempPointY := -other^.originY+other^.height_;
        end;
      3:  begin
        tempPointX := -other^.originX+other^.width_;
        tempPointY := -other^.originY;
        end;
      end;
      if (other^.zRotation_ = 0.0) then
      begin
        tempPointX += other^.x_;
        tempPointY += other^.y_;
      end
    else
      begin
        tempPointDist := sqrt((tempPointX*tempPointX)+(tempPointY*tempPointY));
        tempAngle := arctan2(tempPointY, tempPointX);
        calc := tempAngle-degToRad(other^.zRotation_);
        tempPointX := (tempPointDist*cos(calc))+other^.x_;
        tempPointY := (tempPointDist*sin(calc))+other^.y_;
      end;

      if (zRotation_ = 0.0) then
      begin
        pointX := tempPointX;
        pointY := tempPointY;
      end
    else
      begin
        pointXDist := tempPointX-x_;
        pointYDist := tempPointY-y_;
        pointDist := sqrt((pointXDist*pointXDist)+(pointYDist*pointYDist));
        angle := arctan2(pointYDist, pointXDist);
        calc := angle-degToRad(zRotation_);
        pointX := (pointDist*cos(calc))+x_;
        pointY := (pointDist*sin(calc))+y_;
      end;
      if ((pointX >= x_-originX) and (pointX <= (x_-originX)+width_)) then
      begin
        if ((pointY >= y_-originY) and (pointY <= (y_-originY)+height_)) then
        begin
          result := true;
          exit;
        end;
      end;
    end;
    if (not allStages) then
    begin
      if ((other^.x_ >= x_-originX) and (other^.x_ <= (x_-originX)+width_)) then
      begin
        if ((other^.y_ >= y_-originY) and (other^.y_ <= (y_-originY)+height_)) then
        begin
          result := true;
          exit;
        end;
      end;
      result := false;
      exit;
    end;
    for i := 0 to 4 do
    begin
      if (i < 4) then
      begin
        case i of
        0:  begin
          tempPointX := -originX;
          tempPointY := -originY;
          end;
        1:  begin
          tempPointX := -originX;
          tempPointY := -originY+height_;
          end;
        2:  begin
          tempPointX := -originX+width_;
          tempPointY := -originY+height_;
          end;
        3:  begin
          tempPointX := -originX+width_;
          tempPointY := -originY;
          end;
        end;
        if (zRotation_ = 0.0) then
        begin
          tempPointX += x_;
          tempPointY += y_;
        end
      else
        begin
          tempPointDist := sqrt((tempPointX*tempPointX)+(tempPointY*tempPointY));
          tempAngle := arctan2(tempPointY, tempPointX);
          calc := tempAngle-degToRad(zRotation_);
          tempPointX := (tempPointDist*cos(calc))+x_;
          tempPointY := (tempPointDist*sin(calc))+y_;
        end;
        if (other^.zRotation_ = 0.0) then
        begin
          pointX := tempPointX;
          pointY := tempPointY;
        end
      else
        begin
          pointXDist := tempPointX-other^.x_;
          pointYDist := tempPointY-other^.y_;
          pointDist := sqrt((pointXDist*pointXDist)+(pointYDist*pointYDist));
          angle := arctan2(pointYDist, pointXDist);
          calc := angle-degToRad(other^.zRotation_);
          pointX := (pointDist*cos(calc))+other^.x_;
          pointY := (pointDist*sin(calc))+other^.y_;
        end;
      end
    else
      begin
        pointX := x_;
        pointY := y_;
      end;
      if ((pointX >= other^.x_-other^.originX) and (pointX <= (other^.x_-other^.originX)+other^.width_)) then
      begin
        if ((pointY >= other^.y_-other^.originY) and (pointY <= (other^.y_-other^.originY)+other^.height_)) then
        begin
          result := true;
          exit;
        end;
      end;
    end;
  end;
end;

function TGameObject.roughBoxCollision(other : PGameObject) : boolean;
begin
  result := true;
  if ((not hasModel_) and (not other^.hasModel_)) then
  begin
    if (x_+zRotatedWidth_-originX < other^.x_-other^.originX) then
    begin
      result := false;
      exit;
    end;
    if (other^.x_+other^.zRotatedWidth_-other^.originX < x_-originX) then
    begin
      result := false;
      exit;
    end;
    if (y_+zRotatedHeight_-originY < other^.y_-other^.originY) then
    begin
      result := false;
      exit;
    end;
    if (other^.y_+other^.zRotatedHeight_-other^.originY < y_-originY) then
    begin
      result := false;
      exit;
    end;
  end else result := false;
end;

function TGameObject.circleCollision(other : PGameObject; extraX1 : real = 0; extraY1 : real = 0; extraZ1 : real = 0; extraX2 : real = 0; extraY2 : real = 0;
  extraZ2 : real = 0) : boolean;
begin
  result := false;
end;

function TGameObject.roughHitboxCollision(bone1, bone2 : pbone) : boolean;
begin
  if ((bone1 <> nil) and (bone2 <> nil)) then
  begin
    if (bone1^.hitbox.x+bone1^.hitbox.rl < bone2^.hitbox.x) then
    begin
      result := false;
      exit;
    end;
    if (bone2^.hitbox.x+bone2^.hitbox.rl < bone1^.hitbox.x) then
    begin
      result := false;
      exit;
    end;

    if (bone1^.hitbox.y+bone1^.hitbox.rh < bone2^.hitbox.y) then
    begin
      result := false;
      exit;
    end;
    if (bone2^.hitbox.y+bone2^.hitbox.rh < bone1^.hitbox.y) then
    begin
      result := false;
      exit;
    end;

    if (bone1^.hitbox.z+bone1^.hitbox.rw < bone2^.hitbox.z) then
    begin
      result := false;
      exit;
    end;
    if (bone2^.hitbox.z+bone2^.hitbox.rw < bone1^.hitbox.z) then
    begin
      result := false;
      exit;
    end;
  end;
  result := true;
end;

function TGameObject.hitBoxCollision(bone1, bone2 : pbone) : boolean;
begin
  result := false;
end;

function TGameObject.roughModelCollision(other : PGameObject; hitboxId : integer = -1; hitboxOtherId : integer = -1) : boolean;
var
  i, j : integer;
begin
  if (hasModel_ and other^.hasModel_) then
  begin
    if (hitboxId < 0) then
    begin
      if (hitboxOtherId < 0) then
      begin
        for i := 0 to length(model_^.bones)-1 do
        begin
          for j := 0 to length(other^.model_^.bones)-1 do
          begin
            if (roughHitboxCollision(model_^.bones[i], other^.model_^.bones[j])) then
            begin
              result := true;
              exit;
            end;
          end;
        end;
      end
    else
      begin
        for i := 0 to length(model_^.bones)-1 do
        begin
          if (roughHitboxCollision(model_^.bones[i], other^.model_^.bones[hitboxOtherId])) then
          begin
            result := true;
            exit;
          end;
        end;
      end;
    end
  else
    begin
      if (hitboxOtherId < 0) then
      begin
        for i := 0 to length(other^.model_^.bones)-1 do
        begin
          if (roughHitboxCollision(model_^.bones[hitboxId], other^.model_^.bones[i])) then
          begin
            result := true;
            exit;
          end;
        end;
      end
    else
      begin
        if (roughHitboxCollision(model_^.bones[hitboxId], other^.model_^.bones[hitboxOtherId])) then
        begin
          result := true;
          exit;
        end;
      end;
    end;
  end;
  result := false;
end;

function TGameObject.modelCollision(other : PGameObject; hitboxId : integer = -1; hitboxOtherId : integer = -1) : boolean;
begin
  result := false;
end;


procedure setKeyFrame(frame : pkeyFrame; model : PModel);
var
  i : integer;
begin
  for i := 0 to length(frame^.boneData)-1 do
  begin
    model^.bones[frame^.boneData[i].boneId]^.xRot := frame^.boneData[i].xRot;
    model^.bones[frame^.boneData[i].boneId]^.yRot := frame^.boneData[i].yRot;
    model^.bones[frame^.boneData[i].boneId]^.zRot := frame^.boneData[i].zRot;
  end;
end;

function subtractNormal(operandNormal, subtractingNormal : normal) : normal;
begin
  result.x := operandNormal.x-subtractingNormal.x;
  result.y := operandNormal.y-subtractingNormal.y;
  result.z := operandNormal.z-subtractingNormal.z;
end;

function subtractVertex(operandVertex, subtractingVertex : vertex) : vertex;
begin
  result.x := operandVertex.x-subtractingVertex.x;
  result.y := operandVertex.y-subtractingVertex.y;
  result.z := operandVertex.z-subtractingVertex.z;
end;

function getSurfaceNormal(srcTriangle : ptriangle) : normal;
var
  u, v : vertex;
  normal_ : normal;
  len : real;
begin
  u := subtractVertex(srcTriangle^.coords[1], srcTriangle^.coords[0]);
  v := subtractVertex(srcTriangle^.coords[2], srcTriangle^.coords[0]);
  normal_.x := (u.y*v.z)-(u.z*v.y);
  normal_.y := (u.z*v.x)-(u.x*v.z);
  normal_.z := (u.x*v.y)-(u.y*v.x);

  len := sqrt((normal_.x*normal_.x)+(normal_.y*normal_.y)+(normal_.z*normal_.z));
  if (len = 0) then len := 1;
  normal_.x /= len;
  normal_.y /= len;
  normal_.z /= len;

  result := normal_;
end;

procedure initBox(dstBox : pbox);
begin
  dstBox^.x := 0.0;
  dstBox^.y := 0.0;
  dstBox^.z := 0.0;
  dstBox^.l := 0.0;
  dstBox^.w := 0.0;
  dstBox^.h := 0.0;
  dstBox^.xRot := 0.0;
  dstBox^.yRot := 0.0;
  dstBox^.zRot := 0.0;
end;


function sprite(searchName : string) : PSprite;
var
  letter : char;
  i : word;
  tempSprite : PSprite;
begin
  letter := searchName[1];
  result := nil;
  if (allSprites[letter].count > 0) then
  begin
    for i := 0 to allSprites[letter].count-1 do
    begin
      tempSprite := PSprite(allSprites[letter][i]);
      if (tempSprite^.name = searchName) then result := tempSprite;
    end;
  end;
end;

function addSprite(newName, fileName : string; imageX, imageY, imageWidth, imageHeight : integer; aniframes : integer = 1; frameChangePerSecond : integer = 1;
  newOriginX : integer = 0; newOriginY : integer = 0; customBufferFunction : customSpriteBufferFunctionType = nil; customData : Pointer = nil) : PSprite;
var
  letter : char;
begin
  letter := newName[1];
  allSprites[letter].add(new(PSprite, create(newName, fileName, imageX, imageY, imageWidth, imageHeight, aniframes, frameChangePerSecond, newOriginX, newOriginY, customBufferFunction, customData)));
  result := allSprites[letter].last;
end;

procedure destroySprite(searchName : string);
var
  letter : char;
  i : word;
  tempSprite : PSprite;
begin
  letter := searchName[1];
  if (allSprites[letter].count > 0) then
  begin
    for i := 0 to allSprites[letter].count-1 do
    begin
      tempSprite := PSprite(allSprites[letter][i]);
      if (tempSprite^.name = searchName) then
      begin
        dispose(tempSprite, destroy);
        allSprites[letter].delete(i);
        break;
      end;
    end;
  end;
end;

procedure destroyAllSprites;
var
  i : char;
  j : integer;
  tempSprite : PSprite;
begin
  for i := 'a' to 'z' do
  begin
    if (allSprites[i].count > 0) then
    begin
      for j := 0 to allSprites[i].count-1 do
      begin
        tempSprite := PSprite(allSprites[i][j]);
        dispose(tempSprite, destroy);
      end;
      allSprites[i].clear;
    end;
  end;
end;

function model(searchName : string) : PModel;
var
  letter : char;
  i : word;
  tempModel : PModel;
begin
  letter := searchName[1];
  result := nil;
  if (allModels[letter].count > 0) then
  begin
    for i := 0 to allModels[letter].count-1 do
    begin
      tempModel := PModel(allModels[letter][i]);
      if (tempModel^.name = searchName) then result := tempModel;
    end;
  end;
end;

function addModel(newName, path, fileName : string; framerate : word = 60; customBufferFunction : customModelBufferFunctionType = nil; customData : Pointer = nil) : PModel;
var
  letter : char;
begin
  letter := newName[1];
  allModels[letter].add(new(PModel, create(newName, path, fileName, framerate, customBufferFunction, customData)));
  result := allModels[letter].last;
end;

procedure destroyModel(searchName : string);
var
  letter : char;
  i : word;
  tempModel : PModel;
begin
  letter := searchName[1];
  if (allModels[letter].count > 0) then
  begin
    for i := 0 to allModels[letter].count-1 do
    begin
      tempModel := PModel(allModels[letter][i]);
      if (tempModel^.name = searchName) then
      begin
        dispose(tempModel, destroy);
        allModels[letter].delete(i);
        break;
      end;
    end;
  end;
end;

procedure destroyAllModels;
var
  i : char;
  j : integer;
  tempModel : PModel;
begin
  for i := 'a' to 'z' do
  begin
    if (allModels[i].count > 0) then
    begin
      for j := 0 to allModels[i].count-1 do
      begin
        tempModel := PModel(allModels[i][j]);
        dispose(tempModel, destroy);
      end;
      allModels[i].clear;
    end;
  end;
end;

function gameObject(searchName : string) : PGameObject;
var
  letter : char;
  i : word;
  tempGameObject : PGameObject;
begin
  letter := searchName[1];
  result := nil;
  if (allGameObjects[letter].count > 0) then
  begin
    for i := 0 to allGameObjects[letter].count-1 do
    begin
      tempGameObject := PGameObject(allGameObjects[letter][i]);
      if (tempGameObject^.name = searchName) then result := tempGameObject;
    end;
  end;
end;

function addGameObject(newName : string; destX, destY, destZ : real; newSprite : PSprite = nil; startFrame : word = 0) : PGameObject;
var
  letter : char;
begin
  letter := newName[1];
  allGameObjects[letter].add(new(PGameObject, create(newName, destX, destY, destZ, newSprite, startFrame)));
  result := allGameObjects[letter].last;
end;

function addGameObject(newName : string; destX, destY, destZ : real; newModel : PModel = nil) : PGameObject;
var
  letter : char;
begin
  letter := newName[1];
  allGameObjects[letter].add(new(PGameObject, create(newName, destX, destY, destZ, newModel)));
  result := allGameObjects[letter].last;
end;

procedure destroyGameObject(searchName : string);
var
  letter : char;
  i : word;
  tempGameObject : PGameObject;
begin
  letter := searchName[1];
  if (allGameObjects[letter].count > 0) then
  begin
    for i := 0 to allGameObjects[letter].count-1 do
    begin
      tempGameObject := PGameObject(allGameObjects[letter][i]);
      if (tempGameObject^.name = searchName) then
      begin
        dispose(tempGameObject, destroy);
        allGameObjects[letter].delete(i);
        break;
      end;
    end;
  end;
end;

procedure destroyAllGameObjects;
var
  i : char;
  j : integer;
  tempGameObject : PGameObject;
begin
  for i := 'a' to 'z' do
  begin
    if (allGameObjects[i].count > 0) then
    begin
      for j := 0 to allGameObjects[i].count-1 do
      begin
        tempGameObject := PGameObject(allGameObjects[i][j]);
        dispose(tempGameObject, destroy);
      end;
      allGameObjects[i].clear;
    end;
  end;
end;

procedure initializeAllGraphicalAssets;
var
  i : char;
begin
  for i := 'a' to 'z' do
  begin
    allSprites[i] := TList.create;
    allModels[i] := TList.create;
    allGameObjects[i] := TList.create;
  end;
end;

procedure finalizeAllGraphicalAssets;
var
  i : char;
begin
  for i := 'a' to 'z' do
  begin
    allSprites[i].destroy;
    allModels[i].destroy;
    allGameObjects[i].destroy;
  end;
end;

initialization

initializeAllGraphicalAssets;

finalization

finalizeAllGraphicalAssets;

end.
