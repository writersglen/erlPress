%%==========================================================================
%% ep_test_elements.erl
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



-module (ep_test_elements).

-export([checkbox_test/0, checkbox_test_checked/0]).
-export([circle_test/0]).
-export([dot_test/0]).
-export([ellipse_test/0, ellipse_test_open/0, ellipse_test_solid/0]).
-export([line_test/0]).
-export([rectangle_test/0]).

% -compile(export_all).

-include("../../include/ep.hrl").

-define(PDFDIR, "pdf/elements/").


%% ***********************************************************
%% checkbox_test/0 , checkbox_test_checked/0
%% ***********************************************************

checkbox_test() ->
    OFile = "test_check_box.pdf",
    PDF = eg_pdf:new(),

    eg_pdf_lib:showGrid(PDF, letter),
    test_check_box(PDF),

   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?PDFDIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).

checkbox_test_checked() ->
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
   ep_check_box:check_box(PDF, 72, 720).

test_checked_box(PDF) ->
   ep_check_box:checked_box(PDF, 72, 720).


%% ***********************************************************
%% test_circle/0, test_solid_circle/0 
%% ***********************************************************

circle_test() ->
    OFile = "test_circle.pdf",
    PDF = eg_pdf:new(),

    Circle = test_circle(),
    ep_circle:circle(PDF, Circle),

   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?PDFDIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).


%% ***********************************************************
%% Test parameters 
%% ***********************************************************

test_circle() ->
   Circle  = ep_circle:new(72, 720, 20),
   Circle1 = ep_circle:update_outline(1, Circle),
   Circle2 = ep_circle:update_outline_type(solid, Circle1),
   Circle3 = ep_circle:update_outline_color(red, Circle2),
   ep_circle:update_fill_color(yellow, Circle3).

%% ***********************************************************
%% dot_test/0 
%% ***********************************************************

dot_test() ->
    OFile = "test_dot.pdf",
    PDF = eg_pdf:new(),

    test_dot(PDF),

   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?PDFDIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).


%% ***********************************************************
%% Test parameters 
%% ***********************************************************


test_dot(PDF) ->
   Dot  = ep_dot:new(72, 720, 1),
   Dot1 = ep_dot:update_fill_color(red, Dot),
   ep_dot:dot(PDF, Dot1).

%% ***********************************************************
%% test_ellipse/0, test_solid_circle/0 
%% ***********************************************************

ellipse_test() ->
    OFile = "test_ellipse.pdf",
    PDF = eg_pdf:new(),

    Ellipse = test(),
    Ellipse1 = ep_ellipse:update_fill_color(red, Ellipse),
    ep_ellipse:ellipse(PDF, Ellipse1),

   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?PDFDIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).

ellipse_test_open() ->
    OFile = "test_open_ellipse.pdf",
    PDF = eg_pdf:new(),

    Ellipse = test(),
    Ellipse1 = ep_ellipse:update_fill_color(white, Ellipse),
    ep_ellipse:ellipse(PDF, Ellipse1),

   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?PDFDIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).

ellipse_test_solid() ->
    OFile = "test_solid_ellipse.pdf",
    PDF = eg_pdf:new(),

    Ellipse = test_colors(),
    Ellipse1 = ep_ellipse:update_outline_color(red, Ellipse),
    ep_ellipse:ellipse(PDF, Ellipse1),

   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?PDFDIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).

%% ***********************************************************
%% Test parameters 
%% ***********************************************************

test() ->
   ep_ellipse:new(200, 720, 100, 50).
                                       

test_colors() ->
   Outline      = solid,
   OutlineColor = black,
   FillColor    = red,
   Ellipse      = test(),
   Ellipse1     = ep_ellipse:update_outline(Outline, Ellipse),
   Ellipse2      = ep_ellipse:update_outline_color(OutlineColor, Ellipse1),
   ep_ellipse:update_fill_color(FillColor, Ellipse2).

%% ***********************************************************
%% line_test/0 
%% ***********************************************************

line_test() ->
    OFile = "test_line.pdf",
    PDF = eg_pdf:new(),

    Line = line_parameters(),
    ep_line:line(PDF, Line),

   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?PDFDIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).


%% ***********************************************************
%% Test parameters 
%% ***********************************************************


line_parameters() ->
   Line  = ep_line:new(72, 720, 172, 720),
   Line1 = ep_line:update_width(2, Line),
   Line2 = ep_line:update_type(dashdot, Line1),
   ep_line:update_color(red, Line2).

                                            
                                          
%% ***********************************************************
%% rectangle_test/0 
%% ***********************************************************


rectangle_test() ->
    OFile = "test_rectangle.pdf",
    PDF = eg_pdf:new(),

    Rectangle = test_rectangle(),
    ep_rectangle:rectangle(PDF, Rectangle),

   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?PDFDIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).



%% ***********************************************************
%% Test parameters
%% ***********************************************************

test_rectangle() ->
   Rectangle  = ep_rectangle:new(72, 720, 100, 20),
   Rectangle1 = ep_rectangle:update_outline(1, Rectangle),
   Rectangle2 = ep_rectangle:update_outline_type(dashdot, Rectangle1),
   Rectangle3 = ep_rectangle:update_outline_color(red, Rectangle2),
   ep_rectangle:update_fill_color(yellow, Rectangle3).


  
