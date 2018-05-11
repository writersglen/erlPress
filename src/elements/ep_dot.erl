%%==========================================================================
%% ep_dot.erl

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
%% File:       ep_dot.erl
%% Description: 
%%    Layout primitives 
%%==========================================================================

-module (ep_dot).

-export ([new/3, dot/2]). 
-export ([dot_dimensions/1, dot_color/1]). 
-export ([id/1, x/1, y/1, radius/1, fill_color/1]).
-export ([update_id/2, update_x/2, update_y/2]).
-export ([update_radius/2, update_fill_color/2]).

% -compile(export_all).

-include("../../include/ep.hrl").

%% ***********************************************************
%% new/3 
%% ***********************************************************

%% Colors: white, silver, gray, black, maroon, red, fuschia,
%%         purple, lime, green, olive, yellow, navy, blue, teal, aqua 

new(X, Y, Radius) ->
   New = [{id,            {0, 0, undefined}},  % {Page, Panel, Alias}
          {x,             X},
          {y,             Y},
          {radius,        Radius},
          {outline,       1},
          {outline_type,  solid},
          {outline_color, black},
          {fill_color,    black}
         ],
    maps:from_list(New).

%% ***********************************************************
%% dot/2  
%% ***********************************************************

dot(PDF, Dot) ->
    {X, Y, Radius} = dot_dimensions(Dot),
    DotColor = dot_color(Dot),
    eg_pdf:set_stroke_color(PDF, DotColor),
    eg_pdf:set_fill_color(PDF, DotColor),
    eg_pdf:circle(PDF, {X, Y}, Radius),
    eg_pdf:path(PDF, fill_stroke).


%% ***********************************************************
%% dot_dimensions/1, dot_color/1  
%% ***********************************************************

dot_dimensions(Dot) ->
    X      = x(Dot),
    Y      = y(Dot),
    Radius = radius(Dot),
    {X, Y, Radius}.

dot_color(Dot) ->
    fill_color(Dot).


%% ***********************************************************
%% Get dot parameters 
%% ***********************************************************

id(Dot) ->
    maps:get(id, Dot).

x(Dot) ->
    maps:get(x, Dot).

y(Dot) ->
    maps:get(y, Dot).

radius(Dot) ->
    maps:get(radius, Dot).

fill_color(Dot) ->
    maps:get(fill_color, Dot).

%% ***********************************************************
%% Update dot parameters 
%% ***********************************************************

update_id(ID, Dot) ->
    maps:put(id, ID, Dot).

update_x(X, Dot) ->
    maps:put(x, X, Dot).

update_y(Y, Dot) ->
    maps:put(y, Y, Dot).

update_radius(Radius, Dot) ->
    maps:put(radius, Radius, Dot).

update_fill_color(DotColor, Dot) ->
    maps:put(outline_color, DotColor, Dot),
    maps:put(fill_color, DotColor, Dot).



   


