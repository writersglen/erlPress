%%==========================================================================
%% ep_circle.erl

%% @copyright  2018 Lloyd R. Prentice
%% @author     Lloyd R. Prentice
%% @doc
%% License: 
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to permit
%% persons to whom the Software is furnished to do so, subject to the
%% following conditions:
%% 
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
%% USE OR OTHER DEALINGS IN THE SOFTWARE.
%% File:       ep_grid.erl
%%
%% File:       ep_circle.erl
%% Description: 
%%    Layout primitives 
%% @end
%%==========================================================================

-module (ep_circle).

-export ([create/3]). 
-export ([center/1, radius/1, border_style/1, border_color/1]).
-export ([fill_color/1, format/1]). 
-export ([dimensions/1, border_specs/1, colors/1]). 
-export([update_center/3]).
-export ([update_radius/2, update_border/2, update_border_style/2]).
-export ([update_border_color/2, update_fill_color/2, update_format/2]).
-export([circle/2]).

% -compile(export_all).

-include("../../include/ep.hrl").

-define(DEFAULT_BORDER, 1).
-define(DEFAULT_BORDER_COLOR, black).
-define(DEFAULT_BORDER_STYLE, solid).
-define(DEFAULT_FILL_COLOR, white).
-define(DEFAULT_FORMAT, letter).

%% ***********************************************************
%% Create circle map 
%% ***********************************************************

%% @doc Create circle map

-spec create(CenterX :: integer(),
             CenterY :: integer(),
             Radius  :: integer()) -> map().

create(CenterX, CenterY, Radius) ->
   #{ center        => {CenterX, CenterY}
    , radius        => Radius 
    , border        => ?DEFAULT_BORDER
    , border_style  => ?DEFAULT_BORDER_STYLE
    , border_color  => ?DEFAULT_BORDER_COLOR
    , fill_color    => ?DEFAULT_FILL_COLOR
    , format        => ?DEFAULT_FORMAT
    }.


%% ***********************************************************
%% Get circle attributes 
%% ***********************************************************


%% @doc Return center coordinates 

-spec center(CircleMap :: map()) -> tuple().

center(CircleMap) ->
   maps:get(center, CircleMap).


%% @doc Return radius 

-spec radius(CircleMap :: map()) -> tuple().

radius(CircleMap) ->
   maps:get(radius, CircleMap).


%% @doc Return border  

-spec border(CircleMap :: map()) -> tuple().

border(CircleMap) ->
   maps:get(border, CircleMap).


%% @doc Return border style 

-spec border_style(CircleMap :: map()) -> tuple().

border_style(CircleMap) ->
   maps:get(border_style, CircleMap).


%% @doc Return border color 

-spec border_color(CircleMap :: map()) -> tuple().

border_color(CircleMap) ->
   maps:get(border_color, CircleMap).


%% @doc Return fill color 

-spec fill_color(CircleMap :: map()) -> tuple().

fill_color(CircleMap) ->
   maps:get(fill_color, CircleMap).


%% @doc Return page format 

-spec format(CircleMap :: map()) -> tuple().

format(CircleMap) ->
   maps:get(format, CircleMap).


%% @doc Return circle dimensions 

-spec dimensions(CircleMap :: map()) -> tuple().

dimensions(CircleMap) ->
    {X, Y} = center(CircleMap),
    Radius = radius(CircleMap),
    {X, Y, Radius}.


%% @doc Return border specifications 

-spec border_specs(CircleMap :: map()) -> tuple().

border_specs(CircleMap) ->
    Border = border(CircleMap),
    Style  = border_style(CircleMap),
    Color  = border_color(CircleMap),
    {Border, Style, Color}.


%% @doc Return border and fill colors 

-spec colors(CircleMap :: map()) -> tuple().

colors(CircleMap) ->
    BorderColor  = border_color(CircleMap),
    FillColor    = fill_color(CircleMap),
    {BorderColor, FillColor}.



%% ***********************************************************
%% Update circle parameters 
%% ***********************************************************

%% Outline: solid, dash, dot, dashdot
%% Colors: white, silver, gray, black, maroon, red, fuschia,
%%         purple, lime, green, olive, yellow, navy, blue, teal, aqua 


%% @doc Update center coordinates 

-spec update_center(X :: integer(),
                    Y :: integer(),
                    CircleMap :: map()) -> tuple().

update_center(X, Y, CircleMap) ->
    maps:put(center, {X, Y}, CircleMap).


%% @doc Update radius 

-spec update_radius(Radius :: integer(),
                    CircleMap :: map()) -> tuple().

update_radius(Radius, CircleMap) ->
    maps:put(y, Radius, CircleMap).


%% @doc Update border 

-spec update_border(Border :: integer(),
                    CircleMap :: map()) -> tuple().

update_border(Border, CircleMap) ->
    maps:put(radius, Border, CircleMap).


%% @doc Update border style 

-spec update_border_style(BorderStyle :: integer(),
                    CircleMap :: map()) -> tuple().

update_border_style(BorderStyle, CircleMap) ->
    maps:put(border_style, BorderStyle, CircleMap).


%% @doc Update border color 

-spec update_border_color(BorderColor :: integer(),
                    CircleMap :: map()) -> tuple().

update_border_color(BorderColor, CircleMap) ->
    maps:put(border_color, BorderColor, CircleMap).

%% @doc Update fill color 

-spec update_fill_color(FillColor :: integer(),
                    CircleMap :: map()) -> tuple().

update_fill_color(FillColor, CircleMap) ->
    maps:put(fill_color, FillColor, CircleMap).


%% @doc Update format 

-spec update_format(Format :: integer(),
                    CircleMap :: map()) -> tuple().

update_format(Format, CircleMap) ->
    maps:put(format, Format, CircleMap).


%% ***********************************************************
%% circle/2, solid_circle/2  
%% ***********************************************************

circle(PDF, CircleMap) ->
    {CenterX, CenterY, Radius} = dimensions(CircleMap),
    Format = format(CircleMap),
    CenterY1 = ep_lib:v_flip(CenterY, Format), 
    {Border, BorderStyle, BorderColor} = border_specs(CircleMap),
    FillColor = fill_color(CircleMap),
    eg_pdf:save_state(PDF),
    eg_pdf:set_line_width(PDF, Border),
    eg_pdf:set_dash(PDF, BorderStyle),
    eg_pdf:set_stroke_color(PDF, BorderColor),
    eg_pdf:set_fill_color(PDF, FillColor),
    eg_pdf:circle(PDF, {CenterX, CenterY1}, Radius),
    eg_pdf:path(PDF, fill_stroke),
    eg_pdf:restore_state(PDF).




