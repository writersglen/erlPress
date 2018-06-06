%%==========================================================================
%% ep_dot.erl

%% @copyright  2018 Lloyd R. Prentice
%% @author     Lloyd R. Prentice
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
%% @end
%%==========================================================================


-module (ep_dot).

-export ([create/1, dot/3]).

% -compile(export_all).

-define(DEFAULT_RADIUS, 5).

-include("../../include/ep.hrl").

%% ***********************************************************
%% create 
%% ***********************************************************

%% Colors: white, silver, gray, black, maroon, red, fuschia,
%%         purple, lime, green, olive, yellow, navy, blue, teal, aqua 

%% @doc Create dot map

-spec create(Center :: tuple()) -> map().

create(Center) ->
   #{ center         => Center
    , radius         => ?DEFAULT_RADIUS
    , color          => black
    , border         => 1
    , border_type    => solid
    , border_color   =>  black
    }.


%% ***********************************************************
%% dot/3  
%% ***********************************************************

%% @doc Display dot

-spec dot(PDF    :: identifier(),
          Job    :: map(),
          DotMap :: map()) -> ok.


dot(PDF, Job, DotMap) ->
    {PaperStock, PagePosition} = ep_job:stock_position(Job),
    Center      = maps:get(center, DotMap),
    Center1     = ep_lib:impose_xy(Center, PagePosition, PaperStock), 
    Color       = maps:get(color, DotMap),
    Radius      = maps:get(radius, DotMap),
    Border      = maps:get(border, DotMap),
    BorderType  = maps:get(border_type, DotMap),
    BorderColor = maps:get(border_color, DotMap),
    eg_pdf:save_state(PDF),
    eg_pdf:set_line_width(PDF, Border),
    eg_pdf:set_dash(PDF, BorderType),
    eg_pdf:set_stroke_color(PDF, BorderColor),
    eg_pdf:set_fill_color(PDF, Color),
    eg_pdf:circle(PDF, Center1, Radius),
    eg_pdf:path(PDF, fill_stroke),
    eg_pdf:restore_state(PDF),
    ok.




