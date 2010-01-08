%%%-------------------------------------------------------------------
%%% File    : color.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 20 Nov 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(color).

-export([red/0,      white/0, 
	 red3/0,     white3/0,
	 red4/0,     white4/0,

	 green/0,    black/0,
	 green3/0,   black3/0,
	 green4/0,   black4/0,

	 blue/0,     yellow/0, 
	 blue3/0,    yellow3/0,
	 blue4/0,    yellow4/0,

	 orange/0,
	 orange3/0,
	 orange4/0]).

white()    ->  white3().
white3()   ->  {255,255,255}.
white4()   ->  {255,255,255,255}.

black()    ->  black3().
black3()   ->  {0,0,0}.
black4()   ->  {0,0,0,255}.

red()    ->  red3().
red3()   ->  {255,0,0}.
red4()   ->  {255,0,0,255}.

green()  ->  green3().
green3() ->  {0,255,0}.
green4() ->  {0,255,0,255}.

blue()   ->  blue3().
blue3()  ->  {0,0,255}.
blue4()  ->  {0,0,255,255}.

yellow()   ->  yellow3().
yellow3()  ->  {255,255,0}.
yellow4()  ->  {255,255,0,255}.

orange()  ->  orange3().
orange3() ->  {255,165,0}.
orange4() ->  {255,165,0,255}.




