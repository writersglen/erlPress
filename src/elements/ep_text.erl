%%==========================================================================
%% ep_text.erl
%% {c) 2017 Lloyd R. Prentice
%% Author:     Lloyd R. Prentice
%%
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
%%==========================================================================

-module (ep_text).

-export ([new/3, circle/2]). 
-export ([circle_dimensions/1, circle_colors/1]). 
-export ([id/1, x/1, y/1, radius/1, outline/1, outline_type/1]).
-export ([outline_color/1, fill_color/1]).
-export ([update_id/2, update_x/2, update_y/2]).
-export ([update_radius/2, update_outline/2, update_outline_type/2]).
-export ([update_outline_color/2, update_fill_color/2]).
-export([test/0]).

% -compile(export_all).

-include("../../include/ep.hrl").

%% ***********************************************************
%% new/3 
%% ***********************************************************

%% Outline: solid, dash, dot, dashdot
%% Colors: white, silver, gray, black, maroon, red, fuschia,
%%         purple, lime, green, olive, yellow, navy, blue, teal, aqua 

new(X, Y, Radius) ->
   NewCircle = [{id,            {0, 0, undefined}},  % {Page, Panel, Alias}
                {x,             X},
                {y,             Y},
                {radius,        Radius},
                {outline,       2},
                {outline_type,  solid},
                {outline_color, black},
                {fill_color,    white}
               ],
    maps:from_list(NewCircle).

%% ***********************************************************
%% circle/2, solid_circle/2  
%% ***********************************************************

circle(PDF, Circle) ->
    {X, Y, Radius} = circle_dimensions(Circle),
    {Outline, OutlineType} = circle_outline(Circle),
    {Outline_Color, FillColor} = circle_colors(Circle),
    eg_pdf:set_line_width(PDF, Outline),
    eg_pdf:set_dash(PDF, OutlineType),
    eg_pdf:set_stroke_color(PDF, Outline_Color),
    eg_pdf:set_fill_color(PDF, FillColor),
    eg_pdf:circle(PDF, {X, Y}, Radius),
    eg_pdf:path(PDF, fill_stroke).


%% ***********************************************************
%% circle_dimensions/1, circle_colors/1  
%% ***********************************************************

circle_dimensions(Circle) ->
    X      = x(Circle),
    Y      = y(Circle),
    Radius = radius(Circle),
    {X, Y, Radius}.

circle_outline(Circle) ->
    Outline      = outline(Circle),
    OutlineType  = outline_type(Circle),
    {Outline, OutlineType}.

circle_colors(Circle) ->
    OutlineColor = outline_color(Circle),
    FillColor    = fill_color(Circle),
    {OutlineColor, FillColor}.


%% ***********************************************************
%% Get circle parameters 
%% ***********************************************************

id(Circle) ->
    maps:get(id, Circle).

x(Circle) ->
    maps:get(x, Circle).

y(Circle) ->
    maps:get(y, Circle).

radius(Circle) ->
    maps:get(radius, Circle).

outline(Circle) ->
    maps:get(outline, Circle).

outline_type(Circle) ->
    maps:get(outline_type, Circle).

outline_color(Circle) ->
    maps:get(outline_color, Circle).

fill_color(Circle) ->
    maps:get(fill_color, Circle).

%% ***********************************************************
%% Update circle parameters 
%% ***********************************************************

update_id(ID, Circle) ->
    maps:put(id, ID, Circle).

update_x(X, Circle) ->
    maps:put(x, X, Circle).

update_y(Y, Circle) ->
    maps:put(y, Y, Circle).

update_radius(Radius, Circle) ->
    maps:put(radius, Radius, Circle).

update_outline(Outline, Circle) ->
    maps:put(outline, Outline, Circle).

update_outline_type(OutlineType, Circle) ->
    maps:put(outline_type, OutlineType, Circle).

update_outline_color(Outline_Color, Circle) ->
    maps:put(outline_color, Outline_Color, Circle).

update_fill_color(FillColor, Circle) ->
    maps:put(fill_color, FillColor, Circle).


%% ***********************************************************
%% test_circle/0, test_solid_circle/0 
%% ***********************************************************

test() ->
    OFile = "test_circle.pdf", 
    PDF = eg_pdf:new(),

    Circle = test_circle(),
    circle(PDF, Circle),

   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?PDF_DIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).


%% ***********************************************************
%% Test parameters 
%% ***********************************************************


test_circle() ->
   Circle  = new(72, 720, 20),
   Circle1 = update_outline(1, Circle),
   Circle2 = update_outline_type(solid, Circle1),
   Circle3 = update_outline_color(red, Circle2),
   update_fill_color(yellow, Circle3).


