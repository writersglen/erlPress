%%==========================================================================
%% ep_check_box.erl
%%
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
%% File:       ep_check_box.erl
%% Description: 
%%    Layout primitives 
%%==========================================================================



-module (ep_check_box).

-export ([new/2, check_box/3, checked_box/3, check_mark/2]). 
-export ([check_box_dimensions/1, check_box_outline/1, check_box_colors/1]). 
-export ([id/1, x/1, y/1, width/1, height/1]).
-export ([outline/1, outline_color/1, fill_color/1]).
-export ([update_id/2, update_x/2, update_y/2, update_width/2, update_height/2]).
-export ([update_outline/2, update_outline_type/2, update_outline_color/2]).
-export ([update_fill_color/2]).

% -compile(export_all).

-include("../../include/ep.hrl").

-define(PDFDIR, "pdf/elements/").

%% ***********************************************************
%% new/3 
%% ***********************************************************

new(X, Y) ->
   New = [{id,            {0, 0, undefined}},  % {Page, Panel, Alias}
          {x,             X},
          {y,             Y},
          {width,         11},
          {height,        11},
          {outline,       1},
          {outline_type,  solid},
          {outline_color, black},
          {fill_color,    white}
         ],
    maps:from_list(New).

%% ***********************************************************
%% circle/2, solid_circle/2  
%% **********************************************************

 
check_box(PDF, X, Y) ->
   CheckBox = new(X, Y),
   CheckBox1 = update_fill_color(silver, CheckBox),
   check_box(PDF, CheckBox1). 

checked_box(PDF, X, Y) ->
   CheckBox = new(X, Y),
   CheckBox1 = update_fill_color(white, CheckBox),
   check_box(PDF, CheckBox1),
   check_mark(PDF, CheckBox1).
   

check_box(PDF, CheckBox) ->
   X            = x(CheckBox),
   Y            = y(CheckBox),
   Width        = width(CheckBox),
   Height       = height(CheckBox),
   Outline      = outline(CheckBox),
   OutlineType  = outline_type(CheckBox),
   OutlineColor = outline_color(CheckBox),
   FillColor    = fill_color(CheckBox),
   eg_pdf:set_line_width(PDF, Outline),
   eg_pdf:set_dash(PDF, OutlineType),
   eg_pdf:set_stroke_color(PDF, OutlineColor),
   eg_pdf:set_fill_color(PDF, FillColor), 
   eg_pdf:rectangle(PDF, {X, Y}, {Width, Height}),
   eg_pdf:path(PDF, fill_stroke).

check_mark(PDF, CheckBox) ->
    X = x(CheckBox),
    Y = y(CheckBox),
    check_mark(PDF, X + 2 , Y + 1).

check_mark(PDF, X, Y) ->
   eg_pdf:begin_text(PDF),
   eg_pdf:set_text_pos(PDF, X, Y),
   eg_pdf:set_font(PDF, "Helvetica-Bold", 12),
   eg_pdf:ensure_font_gets_loaded(PDF, "Helvetica-Bold"),
   eg_pdf:set_fill_color(PDF, black),
   eg_pdf:text(PDF, "X"),
   eg_pdf:end_text(PDF).


%% ***********************************************************
%% circle_dimensions/1, circle_colors/1  
%% ***********************************************************

check_box_dimensions(CheckBox) ->
    X      = x(CheckBox),
    Y      = y(CheckBox),
    W      = width(CheckBox),
    H      = height(CheckBox),
    {X, Y, W, H}.

check_box_outline(CheckBox) ->
    Outline      = outline(CheckBox),
    OutlineType  = outline_type(CheckBox),
    {Outline, OutlineType}.

check_box_colors(CheckBox) ->
    OutlineColor = outline_color(CheckBox),
    FillColor    = fill_color(CheckBox),
    {OutlineColor, FillColor}.


%% ***********************************************************
%% Get circle parameters 
%% ***********************************************************

id(CheckBox) ->
    maps:get(id, CheckBox).

x(CheckBox) ->
    maps:get(x, CheckBox).

y(CheckBox) ->
    maps:get(y, CheckBox).

width(CheckBox) ->
    maps:get(width, CheckBox).

height(CheckBox) ->
    maps:get(height, CheckBox).

outline(CheckBox) ->
    maps:get(outline, CheckBox).

outline_type(CheckBox) ->
    maps:get(outline_type, CheckBox).

outline_color(CheckBox) ->
    maps:get(outline_color, CheckBox).

fill_color(CheckBox) ->
    maps:get(fill_color, CheckBox).

%% ***********************************************************
%% Update circle parameters 
%% ***********************************************************

update_id(ID, CheckBox) ->
    maps:put(id, ID, CheckBox).

update_x(X, CheckBox) ->
    maps:put(x, X, CheckBox).

update_y(Y, CheckBox) ->
    maps:put(y, Y, CheckBox).

update_width(Width, CheckBox) ->
    maps:put(width, Width, CheckBox).

update_height(Height, CheckBox) ->
    maps:put(height, Height, CheckBox).

update_outline(Outline, CheckBox) ->
    maps:put(outline, Outline, CheckBox).

update_outline_type(OutlineType, CheckBox) ->
    maps:put(outline_type, OutlineType, CheckBox).

update_outline_color(Outline_Color, CheckBox) ->
    maps:put(outline_color, Outline_Color, CheckBox).

update_fill_color(FillColor, CheckBox) ->
    maps:put(fill_color, FillColor, CheckBox).

%% ***********************************************************
%% test/0 , test_checked/0
%% ***********************************************************

test() ->
    OFile = "test_check_box.pdf",
    PDF = eg_pdf:new(),

    eg_pdf_lib:showGrid(PDF, letter),
    test_check_box(PDF),

   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?PDFDIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).

test_checked() ->
    OFile = "test_checked.pdf",
    PDF = eg_pdf:new(),
    
    test_checked_box(PDF),

   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?PDFDIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).


%% ***********************************************************
%% Test parameters
%% ***********************************************************

test_check_box(PDF) ->
   check_box(PDF, 72, 720).

test_checked_box(PDF) ->
   checked_box(PDF, 72, 720).



