{
SuperMaximo GameLibrary : Texture class unit
by Max Foster

License : http://creativecommons.org/licenses/by/3.0/
}
unit TextureClass;
{$mode objfpc}{$H+}

interface

uses dglOpenGL;

const
  TEXTURE_1D = GL_TEXTURE_1D; //A single line of colours
  TEXTURE_2D = GL_TEXTURE_2D; //A regular two dimentional image
  TEXTURE_3D = GL_TEXTURE_3D; //A stack of multiple TEXTURE_2Ds in one package. Essentially an array of TEXTURE_2D
  TEXTURE_RECTANGLE = GL_TEXTURE_RECTANGLE; //A two dimentional image where texture coordinates do not need to be normalised
  TEXTURE_CUBE = GL_TEXTURE_CUBE_MAP; //Six textures that form a net of a cube. Useful for 'skyboxes'; a box with a background
                                      //scene that the 3D world is contained inside
	
type
  PTexture = ^TTexture;
  TTexture = object
  private
    name_ : string;
    texture_ : GLuint;
    type__ : GLenum;
  public
    //Creates an OpenGL texture object with the specified name and texture type. The texture files to load
    //are passed in an array of strings
    constructor create(newName : string; textureType : GLenum; fileNames : array of string);
    destructor destroy;

    //Erase the old texture data (if any) and load a new texture
    procedure reload(textureType : GLenum; fileNames : array of string);
    
    function name : string;
    function texture : GLuint; //Return the OpenGL texture value to pass to OpenGL functions
    function type_ : GLenum; //Return the OpenGL texture type
  end;

function texture(searchName : string) : PTexture;
function addTexture(newName : string; textureType : GLenum; fileNames : array of string) : PTexture;
procedure destroyTexture(searchName : string);
procedure destroyAllTextures;

implementation

uses SysUtils, Classes, SDL, SDL_image, Display;

var
  allTextures : array['a'..'z'] of TList;

constructor TTexture.create(newName : string; textureType : GLenum; fileNames : array of string);
var
  i : integer;
begin
  name_ := newName;
  for i := 0 to length(fileNames)-1 do
  begin
    fileNames[i] := setDirSeparators(fileNames[i]);
  end;
  reload(textureType, fileNames); //Let's keep it DRY (Don't Repeat Yourself)
end;

destructor TTexture.destroy;
begin
  glDeleteTextures(1, @texture_);
end;

procedure TTexture.reload(textureType : GLenum; fileNames : array of string);
var
  initialised : boolean = false;
  i : word;
  image : PSDL_Surface;
  textureFormat : GLenum;
  //Sides of the cube map
  sides : array[0..4] of GLenum = (GL_TEXTURE_CUBE_MAP_NEGATIVE_X, GL_TEXTURE_CUBE_MAP_POSITIVE_Y, GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
        GL_TEXTURE_CUBE_MAP_POSITIVE_Z, GL_TEXTURE_CUBE_MAP_NEGATIVE_Z);
begin
  type__ := textureType;
  if (textureType = TEXTURE_3D) then
  begin
    if (glSlVersion < 1.5) then
    begin  //If the GPU doesn't support TEXTURE_2D_ARRAYS, create a texture atlas
      textureType := GL_TEXTURE_2D;
      glGenTextures(1, @texture_);
      glBindTexture(GL_TEXTURE_2D, texture_);
      //Tell OpenGL to mipmap nicely
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE);
    end
  else
    begin
      textureType := GL_TEXTURE_2D_ARRAY;
      glGenTextures(1, @texture_);
      glBindTexture(GL_TEXTURE_2D_ARRAY, texture_);
      glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
      glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_GENERATE_MIPMAP, GL_TRUE);
    end;

    for i := 0 to length(fileNames)-1 do
    begin
      image := IMG_Load(pchar(fileNames[i]));
      if (image = nil) then writeln('Could not load image ', fileNames[i]) else
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
        begin   //Set up the texture buffer
          if (glSlVersion < 1.5) then glTexImage2D(GL_TEXTURE_2D, 0, image^.format^.BytesPerPixel, image^.w*length(fileNames), image^.h, 0, textureFormat, GL_UNSIGNED_BYTE, nil)
            else glTexImage3D(GL_TEXTURE_2D_ARRAY, 0, image^.format^.BytesPerPixel, image^.w, image^.h, length(fileNames), 0, textureFormat, GL_UNSIGNED_BYTE, nil);
          initialised := true;
        end;
        //Slot the textures into the relevant places in the buffer
        if (glSlVersion < 1.5) then glTexSubImage2D(GL_TEXTURE_2D, 0, image^.w*i, 0, image^.w, image^.h, textureFormat, GL_UNSIGNED_BYTE, image^.pixels)
          else glTexSubImage3D(GL_TEXTURE_2D_ARRAY, 0, 0, 0, i, image^.w, image^.h, 1, textureFormat, GL_UNSIGNED_BYTE, image^.pixels);
        SDL_FreeSurface(image);
      end;
    end;
    glBindTexture(textureType, 0);  //Unbind the texture
  end
else
  begin
    image := IMG_Load(pchar(fileNames[0]));
    if (image = nil) then writeln('Could not load image ', fileNames[0]) else
    begin
      if (image^.format^.BytesPerPixel = 4) then
      begin
        if (image^.format^.Rmask = $000000ff) then textureFormat := GL_RGBA else textureFormat := GL_BGRA;
      end
    else
      begin
        if (image^.format^.Rmask = $000000ff) then textureFormat := GL_RGB else textureFormat := GL_BGR;
      end;
      glGenTextures(1, @texture_);
      glBindTexture(textureType, texture_);
      glTexParameteri(textureType, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
      glTexParameteri(textureType, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE);

      if (textureType <> TEXTURE_CUBE) then
      begin  //If this is a TEXTURE_2D, don't do anything fancy, just put the pixel data straight into graphics memory
        glTexImage2D(textureType, 0, image^.format^.BytesPerPixel, image^.w, image^.h, 0, textureFormat, GL_UNSIGNED_BYTE, image^.pixels);
        SDL_FreeSurface(image);
      end
    else
      begin
        //For a cube map we need to loop through each face of the cube
        glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X, 0, image^.format^.BytesPerPixel, image^.w, image^.h, 0, textureFormat, GL_UNSIGNED_BYTE, image^.pixels);
        SDL_FreeSurface(image);
        for i := 0 to 3 do
        begin
          image := IMG_Load(pchar(fileNames[i+1]));
          if (image = nil) then writeln('Could not load image ', fileNames[i+1]) else
          begin
            glTexImage2D(sides[i], 0, image^.format^.BytesPerPixel, image^.w, image^.h, 0, textureFormat, GL_UNSIGNED_BYTE, image^.pixels);
            SDL_FreeSurface(image);
          end;
        end;
      end;
      glBindTexture(textureType, 0);
    end;
  end;
end;

function TTexture.name : string;
begin
  result := name_;
end;

function TTexture.texture : GLuint;
begin
  result := texture_;
end;

function TTexture.type_ : GLenum;
begin
  result := type__;
end;

function texture(searchName : string) : PTexture;
var
  letter : char;
  i : word;
  tempTexture : PTexture;
begin
  letter := searchName[1];
  result := nil;
  if (allTextures[letter].count > 0) then
  begin
    for i := 0 to allTextures[letter].count-1 do
    begin
      tempTexture := PTexture(allTextures[letter][i]);
      if (tempTexture^.name = searchName) then result := tempTexture;
    end;
  end;
end;

function addTexture(newName : string; textureType : GLenum; fileNames : array of string) : PTexture;
var
  letter : char;
begin
  letter := newName[1];
  allTextures[letter].add(new(PTexture, create(newName, textureType, fileNames)));
  result := allTextures[letter].last;
end;

procedure destroyTexture(searchName : string);
var
  letter : char;
  i : word;
  tempTexture : PTexture;
begin
  letter := searchName[1];
  if (allTextures[letter].count > 0) then
  begin
    for i := 0 to allTextures[letter].count-1 do
    begin
      tempTexture := PTexture(allTextures[letter][i]);
      if (tempTexture^.name = searchName) then
      begin
        dispose(tempTexture, destroy);
        allTextures[letter].delete(i);
        break;
      end;
    end;
  end;
end;

procedure destroyAllTextures;
var
  i : char;
  j : integer;
  tempTexture : PTexture;
begin
  for i := 'a' to 'z' do
  begin
    if (allTextures[i].count > 0) then
    begin
      for j := 0 to allTextures[i].count-1 do
      begin
        tempTexture := PTexture(allTextures[i][j]);
        dispose(tempTexture, destroy);
      end;
      allTextures[i].clear;
    end;
  end;
end;

procedure initializeAllTextures;
var
  i : char;
begin
  for i := 'a' to 'z' do
  begin
    allTextures[i] := TList.create;
  end;
end;

procedure finalizeAllTextures;
var
  i : char;
begin
  for i := 'a' to 'z' do
  begin
    allTextures[i].destroy;
  end;
end;

initialization

initializeAllTextures;

finalization

finalizeAllTextures;

end.
