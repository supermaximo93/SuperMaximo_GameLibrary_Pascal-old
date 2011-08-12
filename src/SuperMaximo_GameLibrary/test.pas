program test;
{$mode objfpc}{$H+}

uses SMSDL, Audio, SoundClass, MusicClass, Input, Display, ShaderClass, GraphicalAssetClasses, FontClass;

var
	spriteShader, modelShader, skeletonShader : PShader;
	spriteEnums : array[0..0] of integer = (VERTEX_ATTRIBUTE);
	spriteAttrs : array[0..0] of string = ('vertex');
	modelEnums : array[0..9] of integer = (VERTEX_ATTRIBUTE, NORMAL_ATTRIBUTE, COLOR0_ATTRIBUTE, COLOR1_ATTRIBUTE, COLOR2_ATTRIBUTE, TEXTURE0_ATTRIBUTE, EXTRA0_ATTRIBUTE,
		EXTRA1_ATTRIBUTE, EXTRA2_ATTRIBUTE, EXTRA3_ATTRIBUTE);
	modelAttrs : array[0..9] of string = ('vertex', 'normal', 'ambientColor', 'diffuseColor', 'specularColor', 'texCoords', 'mtlNum', 'hasTexture', 'shininess', 'alpha');

begin
	initSDL(SDL_INIT_VIDEO);
	initDisplay(600, 480, 1000);

	addSprite('background', 'images/background_space.bmp', 0, 0, 600, 480);
	addSprite('laz', 'images/laz.png', 0, 0, 48, 48);
	addSprite('block', 'images/animtest.bmp', 0, 0, 32, 32, true, 10, 1, 16, 16);
	addModel('test2', 'smo/cat/', 'cat.smo');
	addGameObject('test', 0, -10, -60, model('test2'));
	addGameObject('block', 200, 200, -5, sprite('block'));
	addGameObject('block2', 0, 0, -5, sprite('block'));
	gameObject('block')^.scale(3, 3, 1, false);

	spriteShader := addShader('spriteShader', 'shaders/sprite_vertex_shader.vs', 'shaders/sprite_fragment_shader.fs', spriteEnums, spriteAttrs);
	spriteShader^.setUniformLocation(MODELVIEW_LOCATION, 'modelviewMatrix');
	spriteShader^.setUniformLocation(PROJECTION_LOCATION, 'projectionMatrix');
	spriteShader^.setUniformLocation(TEXSAMPLER_LOCATION, 'colorMap');
	spriteShader^.bind;

	modelShader := addShader('modelShader', 'shaders/model_vertex_shader.vs', 'shaders/model_fragment_shader.fs', modelEnums, modelAttrs);
	modelShader^.setUniformLocation(MODELVIEW_LOCATION, 'modelviewMatrix');
	modelShader^.setUniformLocation(PROJECTION_LOCATION, 'projectionMatrix');
	modelShader^.setUniformLocation(TEXSAMPLER_LOCATION, 'colorMap');

	skeletonShader := addShader('skeletonShader', 'shaders/skeleton_vertex_shader.vs', 'shaders/model_fragment_shader.fs', modelEnums, modelAttrs);
	skeletonShader^.setUniformLocation(MODELVIEW_LOCATION, 'modelviewMatrix');
	skeletonShader^.setUniformLocation(PROJECTION_LOCATION, 'projectionMatrix');
	skeletonShader^.setUniformLocation(TEXSAMPLER_LOCATION, 'colorMap');
	skeletonShader^.setUniformLocation(EXTRA0_LOCATION, 'jointModelviewMatrix');
	skeletonShader^.setUniformLocation(EXTRA1_LOCATION, 'jointNormalRotationMatrix');

	initFont(spriteShader);
	addFont('test', 'fonts/test.ttf', 64);

	gameObject('test')^.bindShader(skeletonShader);
	gameObject('test')^.setCurrentAnimation(1);

	repeat
		enableBlending();
		sprite('background')^.draw(1, 1, -1000);

		gameObject('block2')^.rotate(-1, true);

		gameObject('block2')^.setX(mouseX);
		gameObject('block2')^.setY(mouseY);
		gameObject('block')^.rotate(5, true);
		//gameObject('block2')^.draw();

		//sprite('laz')^.draw(300, 300, -10);

		gameObject('block')^.setFrame(1, true);
		//gameObject('block')^.draw();

		copyMatrix(PERSPECTIVE_MATRIX, PROJECTION_MATRIX);
		gameObject('test')^.rotate(0.0, 1.0, 0.0, true);
		gameObject('test')^.setFrame(1, true);
		gameObject('test')^.draw();

		copyMatrix(ORTHOGRAPHIC_MATRIX, PROJECTION_MATRIX);

		enableBlending(SRC_COLOR, ONE_MINUS_SRC_COLOR);
		//font('test')^.write('Yay, it works!', 10, 100, -10);
		//font('test')^.write('Hmm...', 10, 10, -100);

		refreshScreen;
	until keyPressed(27);

	destroyShader('spriteShader');
	destroyShader('modelShader');
	destroyShader('skeletonShader');
	destroyShader('fontShader');
	destroyModel('test');
	destroyModel('test2');
	destroySprite('laz');
	destroySprite('block');
	destroySprite('background');
	destroyFont('test');

	quitFont;
	quitDisplay;
	quitSDL;
	writeln('Quit successfully');
end.
