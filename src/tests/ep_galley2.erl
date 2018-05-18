
%%% ********************************************************* 
%%% ep_galley2.erl
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

-module (ep_galley2).

-export([page2/0]).


%% **********************************************************
%% Test galley1
%% **********************************************************

page2() -> 
   ProjectName = "Document Test2",
   Author      = "LRP",
   PageNumber  = 2,
   PageMakeup  = page_makeup(),
   PageMap     = ep_galley:create(ProjectName, Author, PageNumber), 
   {PageMap, PageMakeup}.

%   ep_galley:paste_galley(PageMap, PageMakeup).

page_makeup() ->
   [ {page_header,  page_header_map()}
   , {page_number,  page_number_map()}
   , {dot,          dot_map()}
   , {dot,          dot_map1()}
   , {dot,          dot_map2()}
   , {dot,          dot_map3()}
   , {cropmark,     cropmark_map()}
   , {lines,        lines_map()}
   , {lines,        lines_map1()}
   ].


page_header_map() ->
  From = {72, 72},
  Text = "Test galley 1",
  ep_page_header:create(From, Text).

page_number_map() ->
  From = {500, 72},
  Text = "page ",
  ep_page_number:create(From, Text). 

dot_map() ->
  Center = {72, 200},
  ep_dot:create(Center).

dot_map1() ->
  Center  = {92, 200},
  DotMap  = ep_dot:create(Center),
  DotMap1 = maps:put(color, red, DotMap),
  maps:put(border_color, red, DotMap1).

dot_map2() ->
  Center = {112, 200},
  DotMap = ep_dot:create(Center),
  maps:put(color, green, DotMap).

dot_map3() ->
  Center = {132, 200},
  DotMap = ep_dot:create(Center),
  maps:put(color, blue, DotMap).

cropmark_map() ->
  Position = {72, 250},
  ep_cropmark:create(Position).

lines_map() ->
  LineList = [ {{200, 100}, {200, 200}}
             , {{220, 100}, {220, 200}}
             , {{240, 100}, {240, 200}}
             , {{260, 100}, {260, 200}}
             ],
  ep_lines:create(LineList). 


lines_map1() ->
  LineList = [ {{200, 100}, {260, 100}}
             , {{200, 120}, {260, 120}}
             , {{200, 140}, {260, 140}}
             , {{200, 160}, {260, 160}}
             , {{200, 180}, {260, 180}}
             , {{200, 200}, {260, 200}}
             ],
  ep_lines:create(LineList). 






