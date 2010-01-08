%%%-------------------------------------------------------------------
%%% File    : sprite.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 20 Nov 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(sprite).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").
-include_lib("wx/include/glu.hrl").

-export([create_gl_RGBA_texture/1, create_gl_RGB_texture/1, delete_gl_texture/1,
	 blitTexture/3, blitTexture/5,
	 blitSquare/3, blitSquare/5, blitSquare/8]).


%% Load a texture RGBA in OpenGL from a wxImage
%% with red, gree, blue and alpha channel
create_gl_RGBA_texture(Img) ->
    RGBA = image:getRGBA(Img),
    W = wxImage:getWidth(Img),
    H = wxImage:getHeight(Img),
    [TId] = gl:genBuffers(1),
    gl:bindTexture(?GL_TEXTURE_2D, TId),
    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGBA, W, H, 0, ?GL_RGBA, ?GL_UNSIGNED_BYTE, RGBA),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, 16#2802, 16#812F),
    gl:texParameteri(?GL_TEXTURE_2D, 16#2803, 16#812F),
    TId.

%% Load a texture RGB in OpenGL from a wxImage
%% with red, gree and blue channel
create_gl_RGB_texture(Img) ->
    RGB = image:getRGB(Img),
    W = wxImage:getWidth(Img),
    H = wxImage:getHeight(Img),
    [TId] = gl:genBuffers(1),
    gl:bindTexture(?GL_TEXTURE_2D, TId),
    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGB, W, H, 0, ?GL_RGB, ?GL_UNSIGNED_BYTE, RGB),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, 16#2802, 16#812F),
    gl:texParameteri(?GL_TEXTURE_2D, 16#2803, 16#812F),
    TId.

%% Delete a texture in OpenGL
delete_gl_texture(TId) ->
    gl:deleteTextures(TId).

%% Blit a texture with the given size to the given coordinates
%% onto the GL canvas
blitTexture({X,Y}, {W,H}, TId) ->
    blitTexture(X, Y, W,H, TId).

blitTexture(X,Y, W,H, TId) ->
    gl:bindTexture(?GL_TEXTURE_2D, TId),
    gl:'begin'(?GL_QUADS),
    gl:texCoord2f(0, 0), gl:vertex2f(X,   Y),
    gl:texCoord2f(1, 0), gl:vertex2f(X+W, Y),
    gl:texCoord2f(1, 1), gl:vertex2f(X+W, Y+H),
    gl:texCoord2f(0, 1), gl:vertex2f(X,   Y+H),
    gl:'end'(),
    true.



%% Blit a square with the given size at the given coordinate
%% with the given color onto the GL canvas
blitSquare({X,Y}, {W,H}, {Red, Green, Blue}) ->
    blitSquare(X,Y, W,H, Red, Green, Blue, 255);
blitSquare({X,Y}, {W,H}, {Red, Green, Blue, Alpha}) ->
    blitSquare(X,Y, W,H, Red, Green, Blue, Alpha).

blitSquare({X,Y}, {W,H}, Red, Green, Blue) ->
    blitSquare(X,Y, W,H, Red, Green, Blue, 255);
blitSquare(X,Y, W,H, {Red, Green, Blue}) ->
    blitSquare(X,Y, W,H, Red, Green, Blue, 255);
blitSquare(X,Y, W,H, {Red, Green, Blue, Alpha}) ->
    blitSquare(X,Y, W,H, Red, Green, Blue, Alpha).

blitSquare(X,Y, W,H, Red, Green, Blue, Alpha) ->
    gl:color4ub(Red, Green, Blue, Alpha),
    gl:'begin'(?GL_QUADS),
    gl:vertex2f(X,   Y),
    gl:vertex2f(X+W ,Y),
    gl:vertex2f(X+W ,Y+H),
    gl:vertex2f(X,   Y+H),
    gl:'end'(),
    true.
