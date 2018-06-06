%%==========================================================================
%% ep_rectangle.erl
%%
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
%%
%% File:       ep_rectangle.erl
%% Description: 
%%    Layout primitives 
%% @end
%%==========================================================================

-module (ep_rectangle).

-export ([ create/2, rectangle/3 ]). 

-include("../../include/ep.hrl").

-define(BORDER, 1).
-define(BORDER_COLOR, black).
-define(BORDER_STYLE, solid).
-define(FILL_COLOR, white).

%% ***********************************************************
%% Create rectangle map 
%% ***********************************************************

%% @doc Create rectangle map

-spec create(Position :: tuple(),
             Size     :: tuple()) -> map().

create(Position, Size) ->
   #{ position       => Position 
    , size           => Size 
    , outline        => ?BORDER
    , outline_style  => ?BORDER_STYLE
    , outline_color  => ?BORDER_COLOR
    , fill_color     => ?FILL_COLOR
    }.


%% ***********************************************************
%% rectangle/2, solid_rectangle/2  
%% ***********************************************************

%% @doc Display rectangle

-spec rectangle(PDF          :: identifier(),
                Job          :: map(),
                RectangleMap :: map()) -> ok.

rectangle(PDF, Job, RectangleMap) ->
   {PaperStock, PagePosition} = ep_job:stock_position(Job),
   Position     = maps:get(position, RectangleMap),
   Position1    = ep_lib:impose_xy(Position, PagePosition, PaperStock),
   Size         = maps:get(size, RectangleMap),
   Outline      = maps:get(outline, RectangleMap),
   OutlineStyle = maps:get(outline_style, RectangleMap),
   OutlineColor = maps:get(outline_color, RectangleMap),
   FillColor    = maps:get(fill_color, RectangleMap),
   eg_pdf:set_line_width(PDF, Outline),
   eg_pdf:set_dash(PDF, OutlineStyle),
   eg_pdf:set_stroke_color(PDF, OutlineColor),
   eg_pdf:set_fill_color(PDF, FillColor), 
   eg_pdf:rectangle(PDF, Position1, Size),
   eg_pdf:path(PDF, fill_stroke),
   ok.

