%%% *********************************************************
%%% ep_box.erl
%%% {c) 2018  Lloyd R. Prentice
%%% Author:   Lloyd R. Prentice
%%% License: 
%%% File:       ep_test_box.erl
%%
%%% Description: 
%%%   test ontent box functions 
%%% *********************************************************   

-module (ep_test_box).

-export([box_test1/0, box_test2/0, print_box/3]).

-define(PDFDIR, "./pdf/boxes/").


%% ***********************************************************
%% Test 1 
%% ***********************************************************

-spec box_test1() -> ok.

box_test1() ->
   PaperStock = letter,
   OFile = "box_test1.pdf",
   Box = ep_box:create(72, 0, 100, 100),
   print_box(Box, PaperStock, OFile).

%% ***********************************************************
%% Test 2 
%% ***********************************************************

%% @doc Print test box 2

-spec box_test2() -> ok.

box_test2() ->
   PaperStock = letter,
   OFile = "box_test2.pdf",
   Box  = ep_box:create(72, 72, 100, 100),
   Box1 = ep_box:update_border(1, Box),
   Box2 = ep_box:update_border_type(dashdot, Box1),
   Box3 = ep_box:update_border_color(red, Box2),
   Box4 = ep_box:update_fill_color(yellow, Box3),
   print_box(Box4, PaperStock, OFile).

-spec print_box(Box :: map(), PaperStock :: atom(), OFile :: string()) -> ok.

print_box(Box, PaperStock, OFile) ->
    Box1 = ep_box:v_flip_box(Box, PaperStock),
    PDF  = eg_pdf:new(),

    ep_show_grid:show_grid(PDF, PaperStock),
    box(PDF, Box1),

   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?PDFDIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).

%% ***********************************************************
%% Print box helper 
%% ***********************************************************

box(PDF, Box) ->
   X            = ep_box:x(Box),
   Y            = ep_box:y(Box),
   Width        = ep_box:width(Box),
   Height       = ep_box:height(Box),
   Border       = ep_box:border(Box),
   BorderType   = ep_box:border_type(Box),
   BorderColor  = ep_box:border_color(Box),
   FillColor    = ep_box:fill_color(Box),
   eg_pdf:set_line_width(PDF, Border),
   eg_pdf:set_dash(PDF, BorderType),
   eg_pdf:set_stroke_color(PDF, BorderColor),
   eg_pdf:set_fill_color(PDF, FillColor),
   eg_pdf:rectangle(PDF, {X, Y}, {Width, Height}),
   eg_pdf:path(PDF, fill_stroke).


