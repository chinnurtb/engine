%%%-------------------------------------------------------------------
%%% File    : image.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 20 Nov 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(image).

-include_lib("wx/include/wx.hrl").
-include("image.hrl").

-export([loadPNG/1, getRGBA/1, getRGB/1]).


loadPNG(File) ->
    Img = wxImage:new(File),
    case wxImage:hasAlpha(Img) of
	false ->
	    MaskColor = {wxImage:getMaskRed(Img),wxImage:getMaskGreen(Img),wxImage:getMaskBlue(Img)},
	    Alpha = gen_alpha(wxImage:getData(Img), MaskColor),
	    wxImage:setAlpha(Img, Alpha),
	    false;
	true ->
	    true
    end,
    Img.

getRGBA(Img) ->
    RGB = wxImage:getData(Img),
    Alpha = wxImage:getAlpha(Img),
    list_to_binary(
      lists:zipwith(fun({R, G, B}, A) ->
			    Color = <<R, G, B, A>>,
			    Color
		    end,
		    [{R,G,B} || <<R, G, B>> <= RGB],
		    [A || <<A>> <= Alpha])).

getRGB(Img) ->
    wxImage:getData(Img).


gen_alpha(Data, RGB) ->
    gen_alpha(Data, RGB, <<>>).

gen_alpha(<<RGB:3/binary,Rest/binary>>, Mask={R,G,B}, Acc) ->
    case RGB of
	<<R,G,B>> ->
	    gen_alpha(Rest, Mask, <<Acc/binary,0>>);
	_ ->
	    gen_alpha(Rest, Mask, <<Acc/binary,255>>)
    end;
gen_alpha(<<>>, _, Acc) ->
    Acc.
