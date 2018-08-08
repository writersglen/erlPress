%%% *********************************************************
%%%  ep_page_grid.erl
%%%
%%% Copyright:  {c) 2018  Lloyd R. Prentice
%%% Author:     Lloyd R. Prentice
%%% License: 
%%% File:       ep_grid.erl
%%% Description: 
%%%   Page grids 
%%% *********************************************************  

-module (ep_page_grids).

-export([ print_test_grid/1
        , report_pg1/0
        , test_report_pg1/0
        , report_pgN/0
        , test_report_pgN/0
        , left_sidebar_report/0
        , test_left_sidebar_report/0
        , right_sidebar_report/0
        , test_right_sidebar_report/0
        , two_column_report/0
        , test_two_column_report/0
        , three_column_report/0
        , test_three_column_report/0
        , six_column_report/0
        , test_six_column_report/0
        , newsletter/0
        , test_newsletter/0 ]).

-include("../../include/ep.hrl").

-define(PDFDIR, "pdf/grids/").
-define(PAGE_GRID_DIR, filename:join(?PDF_DIR, "page_grids/")).


%%% *********************************************************      
%%% @doc Print test grid 
%%% *********************************************************      

-spec print_test_grid(Grid :: map()) -> ok.

print_test_grid(Grid) ->
    Name  = ep_grid:name(Grid),
    OFile = Name ++ ".pdf",
    PDF = eg_pdf:new(),
    ep_grid:page_grid_header(PDF, Grid),
    ep_grid:print_panels(PDF, Grid),
   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?PAGE_GRID_DIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF),
    ok.

%%% *********************************************************      
%%% @doc Predefined grid - report_pg1/0 
%%% *********************************************************      

-spec report_pg1() -> map().

report_pg1() ->
   Grid1 = ep_grid:create_grid("Report - Page 1", letter, letter),
   Grid2 = ep_grid:contain_content(Grid1, 72, 72, 72, 72),
   Grid3 = ep_grid:create_panel(Grid2, "header", left, top, full_width, 50),
   Grid4 = ep_grid:create_panel(Grid3, "date", under, "header", 200, 24),
   Grid5 = ep_grid:create_panel(Grid4, "blank", under, "date", full_width, 100),
   Grid6 = ep_grid:create_panel(Grid5, "footer", left, bottom, full_width, 36),
   Grid7 = ep_grid:create_panel(Grid6, "content", under, "blank", full_width, full_height),
   Grid7.

test_report_pg1() ->
   Grid = report_pg1(),
   print_test_grid(Grid).


%%% *********************************************************      
%%% @doc Predefined grid - report_pgN/0 
%%% *********************************************************      

-spec report_pgN() -> map().

report_pgN() ->
   Grid1 = ep_grid:create_grid("Report - Page N", letter, letter),
   Grid2 = ep_grid:contain_content(Grid1, 72, 72, 72, 72),
   Grid3 = ep_grid:create_panel(Grid2, "header", left, top, 300, 24),
   Grid4 = ep_grid:create_panel(Grid3, "date", right, top, 100, 24),
   Grid5 = ep_grid:create_panel(Grid4, "footer", left, bottom, full_width, 36),
   Grid6 = ep_grid:create_panel(Grid5, "content", under, "header", full_width, full_height),
   ep_grid:make(Grid6, "content", 10, shorter).

test_report_pgN() ->
   Grid = report_pgN(),
   print_test_grid(Grid).

%%% *********************************************************      
%%% @doc Predefined grid - left_sidebar_report/0 
%%% *********************************************************      

-spec left_sidebar_report() -> map().

left_sidebar_report() ->
   Grid1 = ep_grid:create_grid("Left Sidebar Report", letter, letter),
   Grid2 = ep_grid:contain_content(Grid1, 72, 72, 72, 72),
   Grid3 = ep_grid:create_panel(Grid2, "header", left, top, full_width, 36),
   Grid4 = ep_grid:create_panel(Grid3, "footer", left, bottom, full_width, 36),
   Grid5 = ep_grid:create_panel(Grid4, "left sidebar", under, "header", 150, full_height),
   ep_grid:create_panel(Grid5, "content", beside, "left sidebar", full_width, full_height).

test_left_sidebar_report() ->
   Grid = left_sidebar_report(),
   print_test_grid(Grid).


%%% *********************************************************      
%%% @doc Predefined grid - right_sidebar_report/0 
%%% *********************************************************      

-spec right_sidebar_report() -> map().

right_sidebar_report() ->
   Grid1 = ep_grid:create_grid("Right Sidebar Report", letter, letter),
   Grid2 = ep_grid:contain_content(Grid1, 72, 72, 72, 72),
   Grid3 = ep_grid:create_panel(Grid2, "header", left, top, full_width, 36),
   Grid4 = ep_grid:create_panel(Grid3, "footer", left, bottom, full_width, 36),
   Grid5 = ep_grid:create_panel(Grid4, "content", under, "header", 300, full_height),
   ep_grid:create_panel(Grid5, "right sidebar", beside, "content", full_width, full_height).

test_right_sidebar_report() ->
   Grid = right_sidebar_report(),
   print_test_grid(Grid).

%%% *********************************************************      
%%% @doc Predefined grid - two_column_report/0 
%%% *********************************************************      

-spec two_column_report() -> map().

two_column_report() ->
   N           = 2,
   GutterWidth = 10,
   Leading     = 10,
   Grid1 = ep_grid:create_grid("Two-Column Report", letter, letter),
   Grid2 = ep_grid:contain_content(Grid1, 72, 72, 72, 72),
   Grid3 = ep_grid:create_panel(Grid2, "header", left, top, full_width, 36),
   Grid4 = ep_grid:create_panel(Grid3, "footer", left, bottom, full_width, 36),
   ep_grid:align(Grid4, "header", N, under, "content", full_width, full_height, GutterWidth, Leading).

test_two_column_report() ->
   Grid = two_column_report(),
   print_test_grid(Grid).

%%% *********************************************************      
%%% @doc Predefined grid - three_column_report/0 
%%% *********************************************************      

-spec three_column_report() -> map().

three_column_report() ->
   N           = 3,
   GutterWidth = 10,
   Leading     = 10,
   Grid1 = ep_grid:create_grid("Three-Column Report", letter, letter),
   Grid2 = ep_grid:contain_content(Grid1, 72, 72, 72, 72),
   Grid3 = ep_grid:create_panel(Grid2, "header", left, top, full_width, 36),
   Grid4 = ep_grid:create_panel(Grid3, "footer", left, bottom, full_width, 36),
   ep_grid:align(Grid4, "header", N, under, "content", full_width, full_height, GutterWidth, Leading).

test_three_column_report() ->
   Grid = three_column_report(),
   print_test_grid(Grid).

%%% *********************************************************      
%%% @doc Predefined grid - six_column_report/0 
%%% *********************************************************      

six_column_report() ->
   N           = 5,
   GutterWidth = 10,
   Leading     = 10,
   Grid1 = ep_grid:create_grid("Six-Column Report", letter, letter),
   Grid2 = ep_grid:contain_content(Grid1, 72, 72, 72, 72),
   Grid3 = ep_grid:create_panel(Grid2, "header", left, top, full_width, 36),
   Grid4 = ep_grid:create_panel(Grid3, "footer", left, bottom, full_width, 36),
   Grid5 = ep_grid:create_panel(Grid4, "content", under, "header", 200, full_height),
   ep_grid:align(Grid5, "content", N, beside, "column", full_width, full_height, GutterWidth, Leading).

test_six_column_report() ->
   Grid = six_column_report(),
   print_test_grid(Grid).

%%% *********************************************************      
%%% @doc Predefined grid - newsletter/0 
%%% *********************************************************      

newsletter() ->
   N = 5,
   GutterWidth = 10,
   Leading = 10,
   Width   = 150,
   Grid1   = ep_grid:create_grid("Newsletter", letter, letter),
   Grid2   = ep_grid:contain_content(Grid1, 72, 72, 72, 72),
   Grid3   = ep_grid:create_panel(Grid2, "header", left, top, full_width, 36),
   Grid4   = ep_grid:create_panel(Grid3, "footer", left, bottom, full_width, 150),
   Grid5   = ep_grid:stack(Grid4, "article", N, under, "header", GutterWidth, Leading, Width),
   {X, Y}  = ep_grid:position_xy(Grid5, on_right, "stack 3"),
   Height  = ep_grid:vacant_below(Grid5, X, Y, full_width, full_height),
   ep_grid:align(Grid5, "column", 6, X, Y, full_width, Height, 10, 10).

test_newsletter() ->
   Grid = newsletter(),
   print_test_grid(Grid).



