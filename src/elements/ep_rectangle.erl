%%==========================================================================
%% ep_rectangle.erl
%%
%% {c) 2018 Lloyd R. Prentice
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
%% File:       ep_rectangle.erl
%% Description: 
%%    Layout primitives 
%%==========================================================================

-module (ep_rectangle).

-export ([ print_rectangle/3
         , test/0
        ]). 


-include("../../include/ep.hrl").

-define(ELEMENT_DIR, "./pdf/elements/").

test_rectangle() ->
   Box  = ep_box:create(72, 72, 100, 20),
   Box1 = ep_box:update_border(1, Box),
   Box2 = ep_box:update_border_type(dashdot, Box1),
   Box3 = ep_box:update_border_color(red, Box2),
   ep_box:update_fill_color(yellow, Box3).


test() ->
   PaperStock = letter,
   Rectangle = test_rectangle(),
   OFile      = "test_rectangle.pdf",
   print_rectangle(Rectangle, PaperStock, OFile).


print_rectangle(Rectangle, PaperStock, OFile) ->
    Rectangle  = test_rectangle(),
    Rectangle1 = ep_box:v_flip_box(Rectangle, PaperStock),
    PDF  = eg_pdf:new(),

    rectangle(PDF, Rectangle1),

   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?ELEMENT_DIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).



%% ***********************************************************
%% rectangle/2, solid_rectangle/2  
%% ***********************************************************

rectangle(PDF, Rectangle) ->
   X            = ep_box:x(Rectangle),
   Y            = ep_box:y(Rectangle),
   Width        = ep_box:width(Rectangle),
   Height       = ep_box:height(Rectangle),
   Outline      = ep_box:border(Rectangle),
   OutlineType  = ep_box:border_type(Rectangle),
   OutlineColor = ep_box:border_color(Rectangle),
   FillColor    = ep_box:fill_color(Rectangle),
   eg_pdf:set_line_width(PDF, Outline),
   eg_pdf:set_dash(PDF, OutlineType),
   eg_pdf:set_stroke_color(PDF, OutlineColor),
   eg_pdf:set_fill_color(PDF, FillColor), 
   eg_pdf:rectangle(PDF, {X, Y}, {Width, Height}),
   eg_pdf:path(PDF, fill_stroke).

