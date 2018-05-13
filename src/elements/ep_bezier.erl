%%% ==========================================================================
%%% ep_bezier.erl
%%%
%%% @copyright   2018 Lloyd R. Prentice
%%% @author      Lloyd R. Prentice
%%% @doc
%%% License: 
%%% Permission is hereby granted, free of charge, to any person obtaining a
%%% copy of this software and associated documentation files (the
%%% "Software"), to deal in the Software without restriction, including
%%% without limitation the rights to use, copy, modify, merge, publish,
%%% distribute, sublicense, and/or sell copies of the Software, and to permit
%%% persons to whom the Software is furnished to do so, subject to the
%%% following conditions:
%%% 
%%% The above copyright notice and this permission notice shall be included
%%% in all copies or substantial portions of the Software.
%%% 
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
%%% USE OR OTHER DEALINGS IN THE SOFTWARE.
%%% File:       ep_grid.erl
%%%
%%% File:       ep_bezier.erl
%%% Description: 
%%%    Layout primitives 
%%% @end
%%==========================================================================

-module (ep_bezier).

-export ([create/4]). 
-export([from/1, control1/1, control2/1, to/1]).
-export([width/1, color/1, format/1]).
-export ([update_from/2, update_control1/2, update_control2/2, update_to/2]).
-export ([update_width/2, update_color/2, update_format/2]).
-export([features/1]).
-export([bezier_to_pdf/2]).

-include("../../include/ep.hrl").

-define(DEFAULT_WIDTH, 1).
-define(DEFAULT_DASH, solid).
-define(DEFAULT_COLOR, black).
-define(DEFAULT_FORMAT, letter).


%% ***********************************************************
%% Create bezier map 
%% ***********************************************************

%% @doc Create bezier map

-spec create(Pt1 :: tuple(),
             Pt2 :: tuple(),
             Pt3 :: tuple(),
             Pt4 :: tuple()) -> map().

create(Pt1, Pt2, Pt3, Pt4) ->
   #{ from         => Pt1 
    , control1     => Pt2 
    , control2     => Pt3 
    , to           => Pt4   
    , width        => ?DEFAULT_WIDTH
    , color        => ?DEFAULT_COLOR
    , format       => ?DEFAULT_FORMAT
    }.

%% ***********************************************************
%% Get bezier attributes 
%% ***********************************************************


%% @doc Return start-of-bezier coordinates

-spec from(BezierMap :: map()) -> tuple().

from(BezierMap) ->
   maps:get(from, BezierMap).


%% @doc Return return control point 1 

-spec control1(BezierMap :: map()) -> tuple().

control1(BezierMap) ->
   maps:get(control1, BezierMap).


%% @doc Return return control point 2 

-spec control2(BezierMap :: map()) -> tuple().

control2(BezierMap) ->
   maps:get(control2, BezierMap).


%% @doc Return end-of-bezier coordinates

-spec to(BezierMap :: map()) -> tuple().

to(BezierMap) ->
   maps:get(to, BezierMap).


%% @doc Return width of bezier 

-spec width(BezierMap :: map()) -> integer().

width(BezierMap) ->
   maps:get(width, BezierMap).


%% @doc Return color of bezier 
%%      Colors: white, silver, gray, black, maroon, red, fuschia,
%%      purple, lime, green, olive, yellow, navy, blue, teal, aqua 

-spec color(BezierMap :: map()) -> integer().

color(BezierMap) ->
   maps:get(color, BezierMap).


%% @doc Return page format 

-spec format(BezierMap :: map()) -> integer().

format(BezierMap) ->
   maps:get(format, BezierMap).


%% @doc Return style of bezier; e.g. width, dash, color 

-spec features(BezierMap :: map()) -> integer().

features(BezierMap) ->
    Width  = width(BezierMap),
    Color  = color(BezierMap),
    {Width, Color}.


%% ***********************************************************
%% Update bezier attributes 
%% ***********************************************************

%% doc Update beinning-of-line coordinates

-spec update_from(From   :: tuple(),
                  BezierMap :: map()) -> map().

update_from(From, BezierMap) ->
    maps:put(from, From, BezierMap).


%% doc Update control point 1 

-spec update_control1(Control1   :: tuple(),
                      BezierMap :: map()) -> map().

update_control1(Control1, BezierMap) ->
    maps:put(control1, Control1, BezierMap).


%% doc Update control point d 

-spec update_control2(Control2   :: tuple(),
                      BezierMap :: map()) -> map().

update_control2(Control2, BezierMap) ->
    maps:put(control1, Control2, BezierMap).


%% doc Update end-of-bezier coordinates

-spec update_to(To   :: tuple(),
                BezierMap :: map()) -> map().

update_to(To, BezierMap) ->
    maps:put(to, To, BezierMap).


%% @doc Update width of bezier 

-spec update_width(Width   :: integer(),
                   BezierMap :: map()) -> map().

update_width(Width, BezierMap) ->
    maps:put(width, Width, BezierMap).


%% @doc Update color of line: e.g. white, silver, gray, 
%%      black, maroon, red, fuschia, purple, lime, green, 
%%      olive, yellow, navy, blue, teal, aqua 

-spec update_color(Color   :: atom(),
                   BezierMap :: map()) -> map().

update_color(Color, BezierMap) ->
    maps:put(color, Color, BezierMap).


%% @doc Update page format 
%%      SEE:  rp(ep_format:formats(). 

-spec update_format(Format  :: atom(),
                    BezierMap :: map()) -> map().

update_format(Format, BezierMap) ->
    maps:put(format, Format, BezierMap).


%% ***********************************************************
%% Bezier to pdf  
%% ***********************************************************

bezier_to_pdf(PDF, BezierMap) ->
    From            = maps:get(from, BezierMap),
    ControlA        = maps:get(control1, BezierMap),
    ControlB        = maps:get(control2, BezierMap),
    To              = maps:get(to,   BezierMap),
    Width           = maps:get(width, BezierMap),
    Color           = maps:get(color, BezierMap),
    Format          = maps:get(format, BezierMap),
    {FromX, FromY}  = From,
    FromY1          = ep_lib:v_flip(FromY, Format),
    FromA           = {FromX, FromY1},
    {CXA, CYA}      = ControlA,
    CYA1            = ep_lib:v_flip(CYA, Format),
    ControlA1       = {CXA, CYA1},
    {CXB, CYB}      = ControlB,
    CYB1            = ep_lib:v_flip(CYB, Format),
    ControlB1       = {CXB, CYB1}, 
    {ToX, ToY}      = To,
    ToY1            = ep_lib:v_flip(ToY, Format),
    ToA            = {ToX, ToY1},
    eg_pdf:save_state(PDF),
    eg_pdf:move_to(PDF, From),
    eg_pdf:set_line_width(PDF, Width),
    eg_pdf:bezier(PDF, FromA, ControlA1, ControlB1, ToA),
    eg_pdf:set_stroke_color(PDF, Color),
    eg_pdf:path(PDF, stroke),
    eg_pdf:restore_state(PDF),
    PDF.

    

