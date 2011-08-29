{
SuperMaximo GameLibrary : Font class unit
by Max Foster

License : http://creativecommons.org/licenses/by/3.0/
}
unit FontClass;
{$mode objfpc}{$H+}

interface

uses SDL_ttf, ShaderClass;

type
  PFont = ^TFont;
  TFont = object
  strict private
    font : PTTF_Font;
    size : word;
    name_ : string;
  public
    //Create a new font with the specified name, loaded from the file specified and with the specified size
    constructor create(newName, fileName : string; newSize : word);
    destructor destroy;

    function name : string;
    //Draw some text onto the screen, at the specified coordinates. Set useCache to true to take advantage of the caching
    //system that gives considerable speed boosts. This is highly recommended if you are displaying static text
    procedure write(text : string; x, y : integer; depth : real; useCache : boolean = true; rotation : real = 0.0;
              xScale : real = 1.0; yScale : real = 1.0);
    //Get the dimentions of a string typed out using this particular font
    function width(text : string) : integer;
    function height(text : string) : integer;

    //Cache text to take advantage of the caching system, and remove it once you are finished with it
    procedure cache(text : string);
    procedure removeFromCache(text : string);
  end;

procedure initFont(newFontShader : PShader);
procedure quitFont;
procedure bindFontShader(newFontShader : PShader);

function font(searchName : string) : PFont;
function addFont(newName, fileName : string; newSize : word) : PFont;
procedure destroyFont(searchName : string);
procedure destroyAllFonts;

//Clears the entire font cache. A good idea after transistioning from one level to the next, for example, so that the cache
//does not get bloated with unused text over time
procedure clearFontCache;

implementation

uses SysUtils, Classes, SDL, dglOpenGL, Display;

type
  fontCacheRecord = record
    text, fontName : string;
    texture : GLuint;
    w, h : integer;
    vbo : GLuint;
  end;

var
  allFonts : array['a'..'z'] of TList;
  fontCache : array[ord('a')..ord('z')+1] of array of fontCacheRecord;
  fontShader : PShader;
  vbo : GLuint;

constructor TFont.create(newName, fileName : string; newSize : word);
begin
  name_ := newName;
  size := newSize;
  font := TTF_OpenFont(pchar(setDirSeparators(fileName)), size);
end;

destructor TFont.destroy;
begin
  if (font <> nil) then TTF_CloseFont(font);
end;

function TFont.name : string;
begin
  result := name_;
end;

procedure TFont.write(text : string; x, y : integer; depth : real; useCache : boolean = true; rotation : real = 0.0;
          xScale : real = 1.0; yScale : real = 1.0);
var
  cacheSuccess : boolean;
  cacheIndex, i, w, h, letter : integer;
  textSurface : PSDL_Surface;
  color : TSDL_Color;
  textureFormat : GLenum;
  tempTexture : GLuint;
  vertexArray : array[0..23] of GLfloat;
begin
  cacheSuccess := false;
  cacheIndex := 1;
  letter := ord(text[1]);
  if ((letter < ord('a')) or (letter > ord('z'))) then letter := ord('z')+1;

  if (useCache) then
  begin
    //Search the cache to see if the text to draw has already been cached (this is quicker than
    //actually rendering the text from scratch)
    if (length(fontCache[letter]) > 0) then
    begin
      for i := 0 to length(fontCache[letter])-1 do
      begin
        if ((fontCache[letter][i].text = text) and (fontCache[letter][i].fontName = name_)) then
        begin
          cacheIndex := i;
          cacheSuccess := true;
          break;
        end;
      end;
    end;
    //If not, cache it and research (cacheSuccess will be TRUE this time)
    if (not cacheSuccess) then
    begin
      cache(text);
      for i := 0 to length(fontCache[letter])-1 do
      begin
        if ((fontCache[letter][i].text = text) and (fontCache[letter][i].fontName = name_)) then
        begin
          cacheIndex := i;
          cacheSuccess := true;
          break;
        end;
      end;
    end;
  end;

  //If we don't want caching prepare the text surface
  if (not cacheSuccess) then
  begin
    color.r := 255;
    color.g := 255;
    color.b := 255;
    textSurface := TTF_RenderText_Blended(font, pchar(text), color);

    w := textSurface^.w;
    h := textSurface^.h;
  end;

  glActiveTexture(GL_TEXTURE0);

  if (cacheSuccess) then
  begin
    //If we got the texture from the cache then bind it
    glBindTexture(GL_TEXTURE_RECTANGLE, fontCache[letter][cacheIndex].texture);
    w := fontCache[letter][cacheIndex].w;
    h := fontCache[letter][cacheIndex].h;
    glBindBuffer(GL_ARRAY_BUFFER, fontCache[letter][cacheIndex].vbo);
    glVertexAttribPointer(VERTEX_ATTRIBUTE, 4, GL_FLOAT, false, 0, nil);
  end
else
  begin
    //Otherwise put the text surface into graphics memory
    if (textSurface^.format^.BytesPerPixel = 4) then
    begin
      if (textSurface^.format^.Rmask = $000000ff) then textureFormat := GL_RGBA else textureFormat := GL_BGRA;
    end
  else
    begin
      if (textSurface^.format^.Rmask = $000000ff) then textureFormat := GL_RGB else textureFormat := GL_BGR;
    end;
    glGenTextures(1, @tempTexture);
    glBindTexture(GL_TEXTURE_RECTANGLE, tempTexture);
    glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexImage2D(GL_TEXTURE_RECTANGLE, 0, textSurface^.format^.BytesPerPixel, w, h, 0, textureFormat, GL_UNSIGNED_BYTE, textSurface^.pixels);

    for i := 0 to 22 do vertexArray[i] := 0.0;
    vertexArray[1] := h;
    vertexArray[3] := 1.0;
    vertexArray[7] := 1.0;
    vertexArray[8] := w;
    vertexArray[11] := 1.0;
    vertexArray[13] := h;
    vertexArray[15] := 1.0;
    vertexArray[16] := w;
    vertexArray[17] := h;
    vertexArray[19] := 1.0;
    vertexArray[20] := w;
    vertexArray[23] := 1.0;

    glGenBuffers(1, @vbo);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);
    glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat)*length(vertexArray), @vertexArray, GL_STATIC_DRAW);
    glVertexAttribPointer(VERTEX_ATTRIBUTE, 4, GL_FLOAT, false, 0, nil);
  end;
  glEnableVertexAttribArray(VERTEX_ATTRIBUTE);

  glUseProgram(PShader(fontShader)^.getProgram);
  pushMatrix;
    //Do the specified transforms
    translateMatrix(x, y, depth);
    rotateMatrix(rotation, 0.0, 0.0, 1.0);
    scaleMatrix(xScale, yScale, 0.0);

    //Send down matrix and texture details to the shader
    PShader(fontShader)^.setUniform16(MODELVIEW_LOCATION, getMatrix(MODELVIEW_MATRIX));
    PShader(fontShader)^.setUniform16(PROJECTION_LOCATION, getMatrix(PROJECTION_MATRIX));
    PShader(fontShader)^.setUniform1(TEXSAMPLER_LOCATION, 0);

    //Draw the text
    glDrawArrays(GL_TRIANGLES, 0, 6);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    glDisableVertexAttribArray(VERTEX_ATTRIBUTE);
  popMatrix;

  if (not cacheSuccess) then
  begin
    //If we didn't want caching, remove the text from memory
    SDL_FreeSurface(textSurface);
    glDeleteTextures(1, @tempTexture);
    glDeleteBuffers(1, @vbo);
  end;
end;

function TFont.width(text : string) : integer;
var
  newWidth, newHeight : integer;
begin
  TTF_SizeText(font, pchar(text), newWidth, newHeight);
  result := newWidth;
end;

function TFont.height(text : string) : integer;
var
  newWidth, newHeight : integer;
begin
  TTF_SizeText(font, pchar(text), newWidth, newHeight);
  result := newHeight;
end;

procedure TFont.cache(text : string);
var
  letter : integer;
  newRecord : fontCacheRecord;
  color : TSDL_color;
  textSurface: PSDL_Surface;
  textureFormat : GLenum;
  vertexArray : array[0..23] of GLfloat;
  i : integer;
begin
  letter := ord(text[1]);
  if ((letter < ord('a')) or (letter > ord('z'))) then letter := ord('z')+1;

  //Prepare the text surface to be put into graphics memory
  newRecord.text := text;
  newRecord.fontName := name_;
  TTF_SizeText(font, pchar(text), newRecord.w, newRecord.h);

  color.r := 255;
  color.g := 255;
  color.b := 255;
  textSurface := TTF_RenderText_Blended(font, pchar(text), color);

  if (textSurface^.format^.BytesPerPixel = 4) then
  begin
    if (textSurface^.format^.Rmask = $000000ff) then textureFormat := GL_RGBA else textureFormat := GL_BGRA;
  end
else
  begin
    if (textSurface^.format^.Rmask = $000000ff) then textureFormat := GL_RGB else textureFormat := GL_BGR;
  end;
  //Create the texture and put the surface into graphics memory
  glGenTextures(1, @newRecord.texture);
  glBindTexture(GL_TEXTURE_RECTANGLE, newRecord.texture);
  glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexImage2D(GL_TEXTURE_RECTANGLE, 0, textSurface^.format^.BytesPerPixel, textSurface^.w, textSurface^.h, 0, textureFormat, GL_UNSIGNED_BYTE, textSurface^.pixels);

  for i := 0 to 22 do vertexArray[i] := 0.0;
  vertexArray[1] := textSurface^.h;
  vertexArray[3] := 1.0;
  vertexArray[7] := 1.0;
  vertexArray[8] := textSurface^.w;
  vertexArray[11] := 1.0;
  vertexArray[13] := textSurface^.h;
  vertexArray[15] := 1.0;
  vertexArray[16] := textSurface^.w;
  vertexArray[17] := textSurface^.h;
  vertexArray[19] := 1.0;
  vertexArray[20] := textSurface^.w;
  vertexArray[23] := 1.0;

  SDL_FreeSurface(textSurface);

  //Generate a buffer to store the text rectangle vertex data
  glGenBuffers(1, @newRecord.vbo);
  glBindBuffer(GL_ARRAY_BUFFER, newRecord.vbo);
  glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat)*length(vertexArray), @vertexArray, GL_STATIC_DRAW);
  glVertexAttribPointer(VERTEX_ATTRIBUTE, 4, GL_FLOAT, false, 0, nil);

  i := length(fontCache[letter]);
  setLength(fontCache[letter], i+1);
  fontCache[letter][i] := newRecord;
end;

procedure TFont.removeFromCache(text : string);
var
  i, j, letter : integer;
  tempArray : array of fontCacheRecord;
  match : boolean = false;
begin
  letter := ord(text[1]);
  if ((letter < ord('a')) or (letter > ord('z'))) then letter := ord('z')+1;

  //Linearly search through the cache, and if there is a match, remove the cached data
  if (length(fontCache[letter]) > 0) then
  begin
    for i := 0 to length(fontCache[letter])-1 do
    begin
      if ((fontCache[letter][i].text = text) and (fontCache[letter][i].fontName = name_)) then
      begin
        glDeleteTextures(1, @fontCache[letter][i].texture);
        glDeleteBuffers(1, @fontCache[letter][i].vbo);
        match := true;
        break;
      end;
    end;
    if (match) then
    begin
      if (length(fontCache[letter])-1 > 0) then
      begin
        setLength(tempArray, length(fontCache[letter])-1);
        if (i > 0) then for j := 0 to i-1 do tempArray[j] := fontCache[letter][j];
        for j := i to length(tempArray)-1 do tempArray[j] := fontCache[letter][j+1];
        setLength(fontCache[letter], length(tempArray));
        fontCache[letter] := tempArray;
      end else setLength(fontCache[letter], 0);
    end;
  end;
end;

procedure initFont(newFontShader : PShader);
begin
  fontShader := newFontShader;
  TTF_Init;
end;

procedure quitFont;
begin
  TTF_Quit;
  fontShader := nil;
  clearFontCache;
end;

procedure bindFontShader(newFontShader : PShader);
begin
  fontShader := newFontShader;
end;

function font(searchName : string) : PFont;
var
  letter : char;
  i : word;
  tempFont : PFont;
begin
  letter := searchName[1];
  result := nil;
  if (allFonts[letter].count > 0) then
  begin
    for i := 0 to allFonts[letter].count-1 do
    begin
      tempFont := PFont(allFonts[letter][i]);
      if (tempFont^.name = searchName) then result := tempFont;
    end;
  end;
end;

function addFont(newName, fileName : string; newSize : word) : PFont;
var
  letter : char;
begin
  letter := newName[1];
  allFonts[letter].add(new(PFont, create(newName, fileName, newSize)));
  result := allFonts[letter].last;
end;

procedure destroyFont(searchName : string);
var
  letter : char;
  i : word;
  tempFont : PFont;
begin
  letter := searchName[1];
  if (allFonts[letter].count > 0) then
  begin
    for i := 0 to allFonts[letter].count-1 do
    begin
      tempFont := PFont(allFonts[letter][i]);
      if (tempFont^.name = searchName) then
      begin
        dispose(tempFont, destroy);
        allFonts[letter].delete(i);
        break;
      end;
    end;
  end;
end;

procedure destroyAllFonts;
var
  i : char;
  j : integer;
  tempFont : PFont;
begin
  for i := 'a' to 'z' do
  begin
    if (allFonts[i].count > 0) then
    begin
      for j := 0 to allFonts[i].count-1 do
      begin
        tempFont := PFont(allFonts[i][j]);
        dispose(tempFont, destroy);
      end;
      allFonts[i].clear;
    end;
  end;
end;

procedure clearFontCache;
var
  i, j : integer;
begin
  for i := ord('a') to ord('z')+1 do
  begin
    if (length(fontCache[i]) > 0) then
    begin
      for j := 0 to length(fontCache[i])-1 do
      begin
        glDeleteTextures(1, @fontCache[i][j].texture);
        glDeleteBuffers(1, @fontCache[i][j].vbo);
      end;
      setLength(fontCache[i], 0);
    end;
  end;
end;

procedure initializeAllFonts;
var
  i : char;
begin
  for i := 'a' to 'z' do
  begin
    allFonts[i] := TList.create;
  end;
end;

procedure finalizeAllFonts;
var
  i : char;
begin
  for i := 'a' to 'z' do
  begin
    allFonts[i].destroy;
  end;
end;

initialization

initializeAllFonts;

finalization

finalizeAllFonts;

end.
