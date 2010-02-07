%%%-------------------------------------------------------------------
%%% File    : window.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 20 Nov 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(window).


-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").
-include_lib("wx/include/glu.hrl").

-include("engine.hrl").

-export([gl_frame/1, gl_frame/2, gl_resize/2]).

%% This function creates a frame with some menus and a OpenGL canvas
gl_frame(Title) ->
    gl_frame(Title, []).

gl_frame(Title, Options) ->
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, Title, Options),
    
    %% Menu bar
    MenuBar = wxMenuBar:new(),
    File    = wxMenu:new([]),
    Help    = wxMenu:new([]),

    wxMenu:append(File, ?wxID_NEW,  "New Game"),
    wxMenu:appendSeparator(File),    
    wxMenu:append(File, ?wxID_EXIT, "Exit"),

    wxMenu:append(Help, ?wxID_ABOUT, "About"), 

    wxMenuBar:append(MenuBar, File, "File"),
    wxMenuBar:append(MenuBar, Help, "Help"),

    wxFrame:setMenuBar(Frame, MenuBar),
 

    Canvas = gl_canvas(Frame),

    wxFrame:show(Frame),
    wxGLCanvas:setCurrent(Canvas),

    init_gl(Canvas),

    #gui{frame    = Frame,
	 canvas   = Canvas,
	 menu_bar = MenuBar}.



%% The frame has to be shown by calling wxFrame:show(Frame)
gl_canvas(Parent) ->
    GLAttrib = [{attribList, [?WX_GL_RGBA,?WX_GL_DOUBLEBUFFER,0]}],
    Canvas = wxGLCanvas:new(Parent, GLAttrib),
    Canvas.



%% This initializes the canvas making the coordinate system
%% to have its {0,0} coord in the upper left corner and
%% enables 2D texture and some other stuff
init_gl(Canvas) ->
    {W,H} = wxWindow:getClientSize(Canvas),
    io:format("~p\n", [{W,H}]),
    gl:clearColor(1,1,1,1),
    gl:enable(?GL_TEXTURE_2D),
    gl:enable(?GL_COLOR_MATERIAL),
    gl:enable(?GL_BLEND),
    gl:disable(?GL_DEPTH_TEST),
    gl:blendFunc(?GL_SRC_ALPHA,?GL_ONE_MINUS_SRC_ALPHA),
 
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
 
    glu:ortho2D(0, W,H, 0),
 
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    
    gl:clear(?GL_COLOR_BUFFER_BIT).


%% Resets the ortho view to the given size
gl_resize(W,H) ->
    gl:viewport(0, 0, W, H),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
 
    glu:ortho2D(0, W,H, 0),

    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    ok.




