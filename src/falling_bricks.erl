%%%-------------------------------------------------------------------
%%% File    : test.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 21 Nov 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(falling_bricks).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").
-include("misc.hrl").


-behaviour(wx_object).

-export([start/0, start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 handle_event/2, terminate/2, code_change/3]).

-record(state, {gui,bars, player}).

-record(player, {x,y,tid}).
-record(bar, {x,y,angle,shape}).

-define(SHAPE1, fun() ->
			if Angle == 0 ->    gl:translatef(X,Y,0);
			   Angle == 90 ->   gl:translatef(X+20,Y,0);
			   Angle == 180 ->  gl:translatef(X+30,Y+20,0);
			   Angle == 270 ->  gl:translatef(X,Y+30,0)
			end,
			gl:color4ubv(color:yellow4()),
			gl:rotatef(Angle,0,0,1),
			gl:'begin'(?GL_QUADS),
			gl:vertex2f(0,  0),
			gl:vertex2f(20, 0),
			gl:vertex2f(20, 10),
			gl:vertex2f(0,  10),
			
			gl:vertex2f(30, 10),
			gl:vertex2f(30, 20),
			gl:vertex2f(10, 20),
			gl:vertex2f(10, 10),
			gl:'end'()
		end).
	
-define(SHAPE2, fun() ->
			if Angle == 0 ->    gl:translatef(X,Y,0);
			   Angle == 90 ->   gl:translatef(X+20,Y,0);
			   Angle == 180 ->  gl:translatef(X+30,Y+20,0);
			   Angle == 270 ->  gl:translatef(X,Y+30,0)
			end,
			gl:color4ubv(color:red4()),
			gl:rotatef(Angle,0,0,1),
			gl:'begin'(?GL_QUADS),
			gl:vertex2f(10,  0),
			gl:vertex2f(30, 0),
			gl:vertex2f(30, 10),
			gl:vertex2f(10,  10),
			
			gl:vertex2f(0, 10),
			gl:vertex2f(20, 10),
			gl:vertex2f(20, 20),
			gl:vertex2f(0, 20),
			gl:'end'()
		end).

-define(SHAPE3, fun() ->
			if Angle == 0 ->    gl:translatef(X,Y,0);
			   Angle == 90 ->   gl:translatef(X+20,Y,0);
			   Angle == 180 ->  gl:translatef(X+20,Y+20,0);
			   Angle == 270 ->  gl:translatef(X,Y+20,0)
			end,
			gl:color4ubv(color:blue4()),
			gl:rotatef(Angle,0,0,1),
			gl:'begin'(?GL_QUADS),
			gl:vertex2f(0,  0),
			gl:vertex2f(20, 0),
			gl:vertex2f(20, 20),
			gl:vertex2f(0,  20),
			gl:'end'()
		end).

-define(SHAPE4, fun() ->
			if Angle == 0 ->    gl:translatef(X,Y,0);
			   Angle == 90 ->   gl:translatef(X+20,Y,0);
			   Angle == 180 ->  gl:translatef(X+20,Y+20,0);
			   Angle == 270 ->  gl:translatef(X,Y+30,0)
			end,
			gl:color4ubv(color:orange4()),
			gl:rotatef(Angle,0,0,1),
			gl:'begin'(?GL_QUADS),
			gl:vertex2f(0,  0),
			gl:vertex2f(10, 0),
			gl:vertex2f(10, 20),
			gl:vertex2f(0,  20),
			
			gl:vertex2f(0, 20),
			gl:vertex2f(20, 20),
			gl:vertex2f(20, 30),
			gl:vertex2f(0, 30),
			gl:'end'()
		end).

-define(SHAPE5, fun() ->
			if Angle == 0 ->    gl:translatef(X,Y,0);
			   Angle == 90 ->   gl:translatef(X+20,Y,0);
			   Angle == 180 ->  gl:translatef(X+20,Y+20,0);
			   Angle == 270 ->  gl:translatef(X,Y+30,0)
			end,
			gl:color4ubv(color:green4()),
			gl:rotatef(Angle,0,0,1),
			gl:'begin'(?GL_QUADS),
			gl:vertex2f(10,  0),
			gl:vertex2f(20, 0),
			gl:vertex2f(20, 20),
			gl:vertex2f(10, 20),
			
			gl:vertex2f(0, 20),
			gl:vertex2f(20, 20),
			gl:vertex2f(20, 30),
			gl:vertex2f(0, 30),
			gl:'end'()
		end).

-define(SHAPE6, fun() ->
			if Angle == 0 ->    gl:translatef(X,Y,0);
			   Angle == 90 ->   gl:translatef(X+10,Y,0);
			   Angle == 180 ->  gl:translatef(X+40,Y+10,0);
			   Angle == 270 ->  gl:translatef(X,Y+40,0)
			end,
			gl:color4ubv(color:black4()),
			gl:rotatef(Angle,0,0,1),
			gl:'begin'(?GL_QUADS),
			gl:vertex2f(0,  0),
			gl:vertex2f(40, 0),
			gl:vertex2f(40, 10),
			gl:vertex2f(0,  10),
			gl:'end'()
		end).


start() ->
    wx_object:start(?MODULE, [], []).

start_link() ->
    wx_object:start_link(?MODULE, [], []).

init([]) ->
    wx:new(),
    Gui = window:gl_frame("Falling Brickz", [{size, {400,300}}]),
    
    wxGLCanvas:connect(Gui#gui.canvas, key_down),
    wxGLCanvas:connect(Gui#gui.canvas, size),

    {Gui#gui.frame, #state{gui = Gui,
			  bars = [#bar{x=50,y=100,angle=0, shape=shape1},
				  #bar{x=90,y=100,angle=0, shape=shape2},
				  #bar{x=130,y=100,angle=0, shape=shape3},
				  #bar{x=170,y=100,angle=0, shape=shape4},
				  #bar{x=210,y=100,angle=0, shape=shape5},
				  #bar{x=250,y=100,angle=0, shape=shape6}]}}.

handle_event(#wx{event = #wxKey{keyCode = ?WXK_UP}}, State) ->
    gl:clear(?GL_COLOR_BUFFER_BIT),
    Bars = lists:map(fun(B) ->
			     if B#bar.angle == 270 -> B#bar{angle = 0};
				true -> B#bar{angle = B#bar.angle +90}
			     end
		     end, State#state.bars),
    wx:foreach(fun draw_bar/1, Bars),
    wxGLCanvas:swapBuffers((State#state.gui)#gui.canvas),
    {noreply, State#state{bars = Bars}};
handle_event(#wx{event = #wxKey{}}, State) ->
    {noreply, State};
handle_event(#wx{event = #wxSize{size = {W,H}}}, State) ->
    window:gl_resize(W,H),
    {noreply, State};
handle_event(#wx{}, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------



redraw(Canvas, X,Y,TId) ->
    gl:clear(?GL_COLOR_BUFFER_BIT),
    sprite:blitTexture({X,Y}, {32,32}, TId),
    wxGLCanvas:swapBuffers(Canvas).


draw_bar(#bar{x=X,y=Y, angle = Angle, shape = Shape}) ->
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),

    case Shape of
	shape1 ->  ?SHAPE1();
	shape2 ->  ?SHAPE2();
	shape3 ->  ?SHAPE3();
	shape4 ->  ?SHAPE4();
	shape5 ->  ?SHAPE5();
	shape6 ->  ?SHAPE6()
    end,

    ok.
