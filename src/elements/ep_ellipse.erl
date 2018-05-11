%%==========================================================================
%% ep_ellipse.erl
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
%% File:       ep_ellipse.erl
%% Description: 
%%    Layout primitives 
%%==========================================================================

-module (ep_ellipse).

-export ([new/4, ellipse/2]). 
-export ([dimensions/1, colors/1]). 
-export ([id/1, x/1, y/1, width/1, height/1]).
-export ([outline/1, outline_color/1, fill_color/1]).
-export ([path/1]).
-export ([update_id/2, update_x/2, update_y/2]).
-export ([update_width/2, update_height/2]).
-export ([update_outline/2, update_outline_color/2, update_fill_color/2]).
-export ([update_path/2]).

% -compile(export_all).

-include("../../include/ep.hrl").

%% ***********************************************************
%% new/3 
%% ***********************************************************

%% Outline: solid, dash, dot, dashdot
%% Colors: white, silver, gray, black, maroon, red, fuschia,
%%         purple, lime, green, olive, yellow, navy, blue, teal, aqua 
%% Path:   stroke, fill, fill_stroke

new(X, Y, Width, Height) ->
   NewCircle = [{id,            {0, 0, undefined}},  % {Page, Panel, Alias}
                {x,             X},
                {y,             Y},
                {width,         Width},
                {height,        Height},
                {outline,       solid},
                {outline_color, black},
                {fill_color,    white},
                {path,          fill_stroke}
               ],
    maps:from_list(NewCircle).

%% ***********************************************************
%% circle/2, solid_circle/2  
%% ***********************************************************

ellipse(PDF, Ellipse) ->
    {X, Y, Width, Height} = dimensions(Ellipse),
    {Outline, OutlineColor, FillColor} = colors(Ellipse),    
    Path = path(Ellipse),
    eg_pdf:set_dash(PDF, Outline),
    eg_pdf:set_stroke_color(PDF, OutlineColor),
    eg_pdf:set_fill_color(PDF, FillColor),
    eg_pdf:ellipse(PDF, {X, Y}, {Width, Height}),
    eg_pdf:path(PDF, Path).

    
%% ***********************************************************
%% ellipse_dimensions/1, circle_colors/1  
%% ***********************************************************

dimensions(Ellipse) ->
    X      = x(Ellipse),
    Y      = y(Ellipse),
    Width  = width(Ellipse),
    Height = height(Ellipse),
    {X, Y, Width, Height}.

colors(Ellipse) ->
    Outline      = outline(Ellipse),
    OutlineColor = outline_color(Ellipse),
    FillColor    = fill_color(Ellipse),
    {Outline, OutlineColor, FillColor}.


%% ***********************************************************
%% Get circle parameters 
%% ***********************************************************

id(Ellipse) ->
    maps:get(id, Ellipse).

x(Ellipse) ->
    maps:get(x, Ellipse).

y(Ellipse) ->
    maps:get(y, Ellipse).

width(Ellipse) ->
    maps:get(width, Ellipse).

height(Ellipse) ->
    maps:get(height, Ellipse).

outline(Ellipse) ->
    maps:get(outline, Ellipse).

outline_color(Ellipse) ->
    maps:get(outline_color, Ellipse).

fill_color(Ellipse) ->
    maps:get(fill_color, Ellipse).

path(Ellipse) ->
    maps:get(path, Ellipse).

%% ***********************************************************
%% Update ellipse parameters 
%% ***********************************************************

update_id(ID, Ellipse) ->
    maps:put(id, ID, Ellipse).

update_x(X, Ellipse) ->
    maps:put(x, X, Ellipse).

update_y(Y, Ellipse) ->
    maps:put(y, Y, Ellipse).

update_width(Width, Ellipse) ->
    maps:put(width, Width, Ellipse).

update_height(Height, Ellipse) ->
    maps:put(height, Height, Ellipse).

%% Outline: solid, dash, dot, dashdot

update_outline(Outline, Ellipse) ->
    maps:put(outline, Outline, Ellipse).

%% Colors: white, silver, gray, black, maroon, red, fuschia,
%%         purple, lime, green, olive, yellow, navy, blue, teal, aqua 

update_outline_color(Outline_Color, Ellipse) ->
    maps:put(outline_color, Outline_Color, Ellipse).

update_fill_color(FillColor, Ellipse) ->
    maps:put(fill_color, FillColor, Ellipse).

%% Path:   stroke, fill, fill_stroke

update_path(Path, Ellipse) ->
    maps:put(path, Path, Ellipse).




