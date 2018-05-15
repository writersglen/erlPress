
%%% ********************************************************* 
%%% ep_tests.erl
%%%
%%% @copyright   2018 Lloyd R. Prentice
%%% @author      Lloyd R. Prentice
%%% @doc         
%%%    License: MIT
%%%    File:    ep_tests.erl
%%%    Description: 
%%%       Tests 
%%% @end 
%%% ********************************************************* 

-module (ep_tests).

-export([galley1/1]).

-define(PAPERSTOCK, letter).
-define(FORMAT, letter).


%% **********************************************************
%% Test galley1
%% **********************************************************



galley1(PageNumber) -> 
   PaperStock = ?PAPERSTOCK,
   Format     = ?FORMAT,
   PageElements = page_elements(),
   ep_paste:paste_up(PaperStock, Format, PageNumber, PageElements).


page_elements() ->
   [{page_header,  page_header_map()},
    {page_number,  page_number_map()},
    {line,         line_map()},
    {line,         line_map1()},
    {line,         line_map2()},
    {line,         line_map3()},
    {circle,       circle_map()},
    {circle,       circle_map1()},
    {ellipse,      ellipse_map()},
    {ellipse,      ellipse_map1()},
    {bezier,       bezier_map()},
    {image,        image_map()},
    {image,        image_map1()}
   ].


page_header_map() ->
  ep_page_header:create({72, 72}, "Test galley 1").

page_number_map() ->
  ep_page_number:create({500, 72}, "page "). 

line_map() ->
  ep_line:create({100, 100}, {200, 200}).

line_map1() ->
  Map = ep_line:create({110, 100}, {210, 200}),
  ep_line:update_color(red, Map).

line_map2() ->
  Map = ep_line:create({120, 100}, {220, 200}),
  ep_line:update_color(green, Map).

line_map3() ->
  Map = ep_line:create({130, 100}, {230, 200}),
  ep_line:update_color(blue, Map).

circle_map() ->
  ep_circle:create({300, 150}, 50).
 
circle_map1() -> 
  Map = ep_circle:create({300, 250}, 50),
  ep_circle:update_fill_color(yellow, Map).
 
ellipse_map() -> 
  ep_ellipse:create({450, 250}, {100, 50}).
 
ellipse_map1() -> 
  Map = ep_ellipse:create({450, 150}, {100, 50}),
  ep_ellipse:update_fill_color(yellow, Map).
 
bezier_map() -> 
  Map = ep_bezier:create({110, 325}, {260, 300}, {410, 340}, {550, 325}),
  Map1 = ep_bezier:update_width(1, Map),
  ep_bezier:update_color(red, Map1).
 
image_map() ->
  ep_image:create("freein_pancho.jpg", {100, 650}, {width, 200}).
  
image_map1() ->
  ep_image:create("erlang.jpg", {350, 650}, {width, 200}).
  





