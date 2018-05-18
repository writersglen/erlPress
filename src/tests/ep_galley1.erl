
%%% ********************************************************* 
%%% ep_galley1.erl
%%%
%%% @copyright   2018 Lloyd R. Prentice
%%% @author      Lloyd R. Prentice
%%% @doc         
%%%    License: MIT
%%%    File:    ep_tests.erl
%%%    Description: 
%%%       Test galleys 
%%% @end 
%%% ********************************************************* 

-module (ep_galley1).

-export([page1/0]).


%% **********************************************************
%% Test galley1
%% **********************************************************

page1() -> 
   ProjectName = "Document Test1",
   Author      = "LRP",
   PageNumber  = 1,
   PageMakeup  = page_makeup(),
   PageMap     = ep_galley:create(ProjectName, Author, PageNumber), 
   {PageMap, PageMakeup}.

%   ep_galley:paste_galley(PageMap, PageMakeup).

page_makeup() ->
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
  From = {72, 72},
  Text = "Test galley 1",
  ep_page_header:create(From, Text).

page_number_map() ->
  From = {500, 72},
  Text = "page ",
  ep_page_number:create(From, Text). 

line_map() ->
  From = {100, 100},
  To   = {200, 200},
  ep_line:create(From, To).

line_map1() ->
  From = {110, 100},
  To   = {210, 200},
  Map  = ep_line:create(From, To),
  ep_line:update_color(red, Map).

line_map2() ->
  From = {120, 100},
  To   = {220, 200},
  Map  = ep_line:create(From, To),
  ep_line:update_color(green, Map).

line_map3() ->
  From = {130, 100},
  To   = {230, 200},
  Map  = ep_line:create(From, To),
  ep_line:update_color(blue, Map).

circle_map() ->
  Center = {300, 150},
  Radius = 50,
  ep_circle:create(Center, Radius).
 
circle_map1() -> 
  Center = {300, 250},
  Radius = 50,
  Map    = ep_circle:create(Center, Radius),
  ep_circle:update_fill_color(yellow, Map).
 
ellipse_map() -> 
  Center = {450, 250},
  Axes   = {100, 50},
  ep_ellipse:create(Center, Axes).
 
ellipse_map1() -> 
  Center = {450, 150},
  Axes   = {100, 50},
  Map = ep_ellipse:create(Center, Axes),
  ep_ellipse:update_fill_color(yellow, Map).
 
bezier_map() -> 
  Pt1 = {110, 325},
  Pt2 = {260, 300},
  Pt3 = {410, 340},
  Pt4 = {550, 325},
  Map = ep_bezier:create(Pt1, Pt2, Pt3, Pt4),
  Map1 = ep_bezier:update_width(1, Map),
  ep_bezier:update_color(red, Map1).
 
image_map() ->
  ImageFileName = "freein_pancho.jpg",
  Position      = {100, 660},
  Size          = {width, 200},
  ep_image:create(ImageFileName, Position, Size).
  
image_map1() ->
  ImageFileName = "erlang.jpg",
  Position      = {350, 660},
  Size          = {width, 200},
  ep_image:create(ImageFileName, Position, Size).
  





