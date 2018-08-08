%%% *********************************************************
%%% ep_layout_test.erl
%%% Copyright:  {c) 2018  Lloyd R. Prentice
%%% Author:     Lloyd R. Prentice
%%% License: 
%%% File:       ep_layout_test.erl
%%% Description: 
%%%    Print layout 
%%% *********************************************************      


-module (ep_layout_test).

-export([layout/0, box5/0, invert_panel/2, get_panel/1 ]).


-define(PAPER_SIZE, letter).



layout() ->
   Boxes = [box4(), box2(), box3()],
   [invert_box(Box, ?PAPER_SIZE) || Box <- Boxes].

%% Need to derive boxes from page


box5() ->
   ep_box:create(72, 72, 100, 50).

box2() ->
   Box = ep_box:create(282, 72, 145, 48),
   ep_box:show_border(Box).


box3() ->
   Box = ep_box:create(72, 170, 355, 300),
   Box1 = ep_box:update_border(0, Box),
   Box2 = ep_box:update_border_color(white, Box1),
   ep_box:update_border_type(none, Box2).

box4() ->
   Box = ep_box:create(72, 72, 200, 100),
   Box1 = ep_box:show_border(Box),
   Box2 = ep_box:update_border(1, Box1),
   Box3 = ep_box:update_border_color(black, Box2),
   Box4 = ep_box:update_margin(10, Box3),
   Box5 = ep_box:update_background_color(yellow, Box4),
   Box6 = ep_box:update_text_color(black, Box5),
   Box6.


invert_box(Box, PaperStock) ->
   Y = ep_box:y(Box),
   Y1 = ep_lib:v_flip(Y, PaperStock),
   Box1 = ep_box:update_y(Y1, Box),
   YPtr = ep_box:y_ptr(Box1),
   io:format("YPtr: ~p~n", [YPtr]),
   YPtr1 = ep_lib:v_flip(YPtr, PaperStock),
   ep_box:update_y_ptr(YPtr1, Box1).


invert_panel(Panel, PaperStock) ->
   Box = ep_panel:get_box(Panel),
   Box1 = invert_box(Box, PaperStock),
   ep_panel:replace_box(Panel, Box1).


%% ********************************************************* 
%% Panel functions 
%%
%% NOTE: Y coordinates in grids run 0 to height of paper stock.
%%       They must be inverted, e.g. height of paper stock to 0
%%       for rendering in PDF.
%% ********************************************************


get_panel(Grid) ->
   PaperStock = ep_grid:paper_stock(Grid),
   Panel = ep_grid:get_panel(Grid, "body"),
   Panel1 = invert_panel(Panel, PaperStock),
   PanelMap = element(3, Panel1),
   PanelMap.




