%%% *********************************************************
%%%  ep_grid.erl
%%%
%%% Copyright:  {c) 2018 Lloyd R. Prentice
%%% Author:     Lloyd R. Prentice
%%% License: 
%%% File:       ep_grid.erl
%%% Description: 
%%%   Create page grids 
%%% *********************************************************      

-module (ep_grid).

% -export([create_grid/3, contain_content/5]).
% -export([create_panel/6, create_panel/8]).
% -export([name/1, get_panel/2, page_grid_header/2]).
% -export([paper_stock/1, position_xy/3, print_panels/2]).
% -export([vacant_below/5]).
% -export([make/4, move/4]).
% -export([align/9, stack/8]).
% -export([bleed/3, show_border/3, hide_border/2]).
% -export([border_color/3, background_color/3]).
% -export([update_margins/4]).
% -export([clone_panel/3, delete_panel/2]).
% -export([page_width/1, page_height/1]).
% -export([page_top/1, page_right/1, page_bottom/1, page_left/1]).
% -export([page_margins/1, page_dimensions/1]).
% -export([page_left_top/1, page_xy/1, page_right_top/1]).
% -export([page_right_bottom/1, page_left_bottom/1]).
% -export([print_grid/1]).
% -export([page_trim_spec/1]).

-compile([export_all]).

-include("../../include/ep.hrl").

-define(PAGE_GRID_DIR, "pdf/page_grids/").


%% *********************************************************      
%% @doc create_grid/3 -- create page grid on paper stock size 
%% suitable for desktop PDF printers.
%%
%% See ep_paper.erl paper_stock_dimensions/0 for standard sizes.
%%
%% See ep_paper.erl pagesize/1 for page bounding-box dimensions
%%
%% See ep_paper.erl page_dimensions/0 for page types
%%
%% NOTE: folio pages will not fit on letter paper stock
%% *********************************************************      

-spec  create_grid(GridName :: list(), letter | legal | 
     a4, PageType :: atom()) -> 
    map().

create_grid(GridName, letter, PageType) ->
   define_grid(GridName, letter, PageType);

create_grid(GridName, legal, PageType) ->
   define_grid(GridName, legal, PageType);

create_grid(GridName, a4, PageType) ->
   define_grid(GridName, a1, PageType).

%% *********************************************************      
%% @doc Define margins inside page type bounding box
%% *********************************************************      

-spec contain_content(Grid :: map(), Top :: integer(),
     Right :: integer(), Bottom :: integer(), Left :: integer()) -> map().

contain_content(Grid, Top, Right, Bottom, Left) ->
    PageMargins = {Top, Right, Bottom, Left},
    maps:put(content_margins, PageMargins, Grid).


%% *************************************************************      
%% @doc Define panel at left edge of content area 
%% *************************************************************      

-spec create_panel(Grid :: tuple(), 
                   PanelName :: string(),
                   X         :: left   | beside  | under | integer(),
                   Y         :: top    | bottom | string() | integer(), 
                   Width     :: full_width  | integer(),
                   Height    :: full_height | integer()) -> map().

%% left, top

create_panel(Grid, PanelName, left, top, full_width, full_height) ->
     {X, Y} = content_left_top(Grid),
      {Width, Height} = vacant_below(Grid, X, Y, full_width, full_height),
     create_panel(Grid, PanelName, X, Y, Width, Height);

create_panel(Grid, PanelName, left, top, full_width, Height) ->
     {X, Y} = content_left_top(Grid),
      {Width, Height1} = vacant_below(Grid, X, Y, full_width, Height),
     create_panel(Grid, PanelName, X, Y, Width, Height1);
  
create_panel(Grid, PanelName, left, top, Width, full_height) ->
     {X, Y} = content_left_top(Grid),
      {Width1, Height} = vacant_below(Grid, X, Y, Width, full_height),
     create_panel(Grid, PanelName, X, Y, Width1, Height);
     
create_panel(Grid, PanelName, left, top, Width, Height) ->
     {X, Y} = content_left_top(Grid),
      {Width1, Height1} = vacant_below(Grid, X, Y, Width, Height),
     create_panel(Grid, PanelName, X, Y, Width1, Height1);

%% left, bottom 

create_panel(Grid, PanelName, left, bottom, full_width, full_height) ->
     X = content_left(Grid),
     Y = content_bottom(Grid),
     {OpenWidth, OpenHeight} = vacant_above(Grid, X, Y, full_width, full_height),
     Y1 = Y - OpenHeight,
     create_panel(Grid, PanelName, X, Y1, OpenWidth, OpenHeight);

create_panel(Grid, PanelName, left, bottom, full_width, Height) ->
     X = content_left(Grid),
     Y = content_bottom(Grid),
     {OpenWidth, OpenHeight} = vacant_above(Grid, X, Y, full_width, Height),
     Y1 = Y - min(Height, OpenHeight),
     create_panel(Grid, PanelName, X, Y1, OpenWidth, OpenHeight);

create_panel(Grid, PanelName, left, bottom, Width, full_height) ->
     X = content_left(Grid),
     Y = content_bottom(Grid),
     {OpenWidth, OpenHeight} = vacant_above(Grid, X, Y, Width, full_height),
     Y1 = Y - OpenHeight,
     create_panel(Grid, PanelName, X, Y1, OpenWidth, OpenHeight);

create_panel(Grid, PanelName, left, bottom, Width, Height) ->
     X = content_left(Grid),
     Y = content_bottom(Grid),
     {OpenWidth, OpenHeight} = vacant_above(Grid, X, Y, Width, Height),
     Y1 = Y - min(Height, OpenHeight),
     create_panel(Grid, PanelName, X, Y1, OpenWidth, OpenHeight);

%% left Y

create_panel(Grid, PanelName, left, Y, full_width, full_height) ->
     X = content_left(Grid),
      {Width, Height} = vacant_below(Grid, X, Y, full_width, full_height),
     create_panel(Grid, PanelName, X, Y, Width, Height);

create_panel(Grid, PanelName, left, Y, full_width, Height) ->
     X = content_left(Grid),
      {Width, Height} = vacant_below(Grid, X, Y, full_width, Height),
     create_panel(Grid, PanelName, X, Y, Width, Height);

create_panel(Grid, PanelName, left, Y, Width, full_height) ->
     X = content_left(Grid),
      {Width, Height} = vacant_below(Grid, X, Y, Width, full_height),
     create_panel(Grid, PanelName, X, Y, Width, Height);

create_panel(Grid, PanelName, left, Y, Width, Height) ->
     X = content_left(Grid),
      {Width, Height} = vacant_below(Grid, X, Y, Width, Height),
     create_panel(Grid, PanelName, X, Y, Width, Height);

%% right top

create_panel(Grid, PanelName, right, top, full_width, full_height) ->
    {X, Y} = content_right_top(Grid),
    Width = content_width(Grid),
    X1 = X - Width, 
    {OpenWidth, OpenHeight} = vacant_below(Grid, X1, Y, full_width, full_height),
    create_panel(Grid, PanelName, X, Y, OpenWidth, OpenHeight);

create_panel(Grid, PanelName, right, top, full_width, Height) ->
    {X, Y} = content_right_top(Grid),
    Width = content_width(Grid),
    X1 = X - Width, 
    {OpenWidth, OpenHeight} = vacant_below(Grid, X1, Y, full_width, Height),
    create_panel(Grid, PanelName, X, Y, OpenWidth, OpenHeight);

create_panel(Grid, PanelName, right, top, Width, Height) ->
    {X, Y} = content_right_top(Grid),
    X1 = X - Width, 
    {OpenWidth, OpenHeight} = vacant_below(Grid, X1, Y, Width, Height),
    create_panel(Grid, PanelName, X1, Y, OpenWidth, OpenHeight);

%% right Y

create_panel(Grid, PanelName, right, Y, full_width, full_height) ->
    X = content_right(Grid),
    {Width, Height} = content_dimensions(Grid),
    X1 = X - Width, 
    {OpenWidth, OpenHeight} = vacant_below(Grid, X1, Y, Width, Height),
    create_panel(Grid, PanelName, X1, Y, OpenWidth, OpenHeight);

create_panel(Grid, PanelName, right, Y, full_width, Height) ->
    X = content_right(Grid),
    Width = content_width(Grid),
    X1 = X - Width, 
    {OpenWidth, OpenHeight} = vacant_below(Grid, X1, Y, Width, Height),
    create_panel(Grid, PanelName, X, Y, OpenWidth, OpenHeight);

create_panel(Grid, PanelName, right, Y, Width, full_height) ->
    X = content_right(Grid),
    Height = content_height(Grid),
    X1 = X - Width, 
    {OpenWidth, OpenHeight} = vacant_below(Grid, X1, Y, Width, Height),
    create_panel(Grid, PanelName, X, Y, OpenWidth, OpenHeight);

create_panel(Grid, PanelName, right, Y, Width, Height) ->
    X = content_right(Grid),
    X1 = X - Width, 
    {OpenWidth, OpenHeight} = vacant_below(Grid, X1, Y, Width, Height),
    create_panel(Grid, PanelName, X1, Y, OpenWidth, OpenHeight);

%% beside RefPanel
    
create_panel(Grid, PanelName,  beside, RefPanelName, full_width, full_height) ->
     {X, Y} = position_beside(Grid, RefPanelName),
     {Width, Height} = vacant_below(Grid, X, Y, full_width, full_height),
     create_panel(Grid, PanelName, X, Y, Width, Height);

create_panel(Grid, PanelName, beside, RefPanelName, full_width, Height) ->
     {X, Y} = position_beside(Grid, RefPanelName),
     {Width, Height1} = vacant_below(Grid, X, Y, full_width, Height),
     create_panel(Grid, PanelName, X, Y, Width, Height1);

create_panel(Grid, PanelName, beside, RefPanelName, Width, full_height) ->
     {X, Y} = position_beside(Grid, RefPanelName),
     {Width1, Height} = vacant_below(Grid, X, Y, Width, full_height),
     create_panel(Grid, PanelName, X, Y, Width1, Height);

create_panel(Grid, PanelName, beside, RefPanelName, Width, Height) ->
     {X, Y} = position_beside(Grid, RefPanelName),
     {Width1, Height1} = vacant_below(Grid, X, Y, Width, Height),
     create_panel(Grid, PanelName, X, Y, Width1, Height1);

%% under RefPanel

create_panel(Grid, PanelName, under, RefPanelName, full_width, full_height) ->
     {X, Y} = position_under(Grid, RefPanelName),
     {Width, Height} = vacant_below(Grid, X, Y, full_width, full_height),
     create_panel(Grid, PanelName, X, Y, Width, Height);

create_panel(Grid, PanelName, under, RefPanelName, full_width, Height) ->
     {X, Y} = position_under(Grid, RefPanelName),
     {Width, Height1} = vacant_below(Grid, X, Y, full_width, Height),
     create_panel(Grid, PanelName, X, Y, Width, Height1);

create_panel(Grid, PanelName, under, RefPanelName, Width, full_height) ->
     {X, Y} = position_under(Grid, RefPanelName),
     {Width1, Height} = vacant_below(Grid, X, Y, Width, full_height),
     create_panel(Grid, PanelName, X, Y, Width1, Height);

create_panel(Grid, PanelName, under, RefPanelName, Width, Height) ->
     {X, Y} = position_under(Grid, RefPanelName),
     {Width1, Height1} = vacant_below(Grid, X, Y, Width, Height),
     create_panel(Grid, PanelName, X, Y, Width1, Height1);

%% center Y

create_panel(Grid, PanelName, center, Y, Width, Height) ->
    Span = content_right(Grid) - content_left(Grid), 
    Span1 = (Span div 2) + content_left(Grid),
    X = Span1 - (Width div 2),
    {Width1, Height1} = vacant_below(Grid, X, Y, Width, Height),
    create_panel(Grid, PanelName, X, Y, Width1, Height1);

%% X Y

create_panel(Grid, PanelName, X, Y, full_width, full_height) ->
    {Width1, Height1} = vacant_below(Grid, X, Y, full_width, full_height),
    create_panel(Grid, PanelName, X, Y, Width1, Height1);

create_panel(Grid, PanelName, X, Y, full_width, Height) ->
    {Width1, Height1} = vacant_below(Grid, X, Y, full_width, Height),
    create_panel(Grid, PanelName, X, Y, Width1, Height1);

create_panel(Grid, PanelName, X, Y, Width, full_height) ->
    {Width1, Height1} = vacant_below(Grid, X, Y, Width, full_height),
    create_panel(Grid, PanelName, X, Y, Width1, Height1);

create_panel(Grid, PanelName, X, Y, Width, Height) -> 
    Index   = next_index(Grid),
    Panel = ep_panel:create(Index, PanelName, X, Y, Width, Height),
    update_panels(Grid, Panel).


%% *************************************************************      
%% @doc create_panel with given gutter width and leading
%% *************************************************************      

-spec create_panel(Grid        :: tuple(), 
                   PanelName   :: string(),
                   X           :: integer(),
                   Y           :: integer(), 
                   Width       :: integer(),
                   Height      :: integer(),
                   GutterWidth :: integer(),
                   Leading     :: integer()) -> map().
     
create_panel(Grid, PanelName, X, Y, Width, Height, GutterWidth, Leading) -> 
    Index   = next_index(Grid),
    {Width1, Height1} = vacant_below(Grid, X, Y, Width, Height),
    Panel = ep_panel:create(Index, PanelName, X, Y, Width1, Height1),
    Panel1 = ep_panel:update_gutter(Panel, GutterWidth),
    Panel2 = ep_panel:update_leading(Panel1, Leading),
    update_panels(Grid, Panel2).


   

%% *************************************************************      
%% @doc Update dimensions of panel; e.g. taller, shorter,
%%    wider, thinner. 
%% *************************************************************      

-spec make(Grid      :: map(), 
           PanelName :: string(), 
           Points    :: integer(),
           Dimension :: atom()) -> map().

make(Grid, PanelName, Points, taller) ->
   Height = panel_height(Grid, PanelName),
   Height1 = Height + Points,
   {_W, H} = content_dimensions(Grid),
   Height2 = min(Height1, H),
   update_panel_height(Grid, PanelName, Height2);

make(Grid, PanelName, Points, shorter) ->
   Height = panel_height(Grid, PanelName),
   io:format("Make: ~n~p ~p~n~n", [Height, Points]),
   Height1 = Height - Points,
   Height2 = max(Height1, 0),
   update_panel_height(Grid, PanelName, Height2);

make(Grid, PanelName, Points, wider) ->
   Width = panel_width(Grid, PanelName),
   Width1 = Width + Points,
   {W, _H} = content_dimensions(Grid),
   Width2 = min(Width1, W),
   update_panel_width(Grid, PanelName, Width2);

make(Grid, PanelName, Points, thinner) ->
   Width = panel_width(Grid, PanelName),
   Width1 = Width - Points,
   Width2 = max(Width1, 0),
   update_panel_width(Grid, PanelName, Width2).


%% *************************************************************      
%% @doc Update position of panel  
%% *************************************************************      

-spec move(Grid :: map(), 
           PanelName :: string(), 
           X :: atom() | integer(), 
           Y :: atom() | integer()) -> map().

move(Grid, PanelName, content_top, content_left) ->
   {X, Y} = content_left_top(Grid),
   update_panel_position(Grid, PanelName, X, Y);

move(Grid, PanelName, content_top, content_right) ->
   Y = content_top(Grid),
   RightX = content_right(Grid),
   Width = panel_width(Grid, PanelName),
   X1 = RightX - Width, 
   update_panel_position(Grid, PanelName, X1, Y);
   
move(Grid, PanelName, content_bottom, content_left) ->
   X = content_left(Grid),
   Y = content_bottom(Grid),
   Height = panel_height(Grid, PanelName),
   Y1 = Y - Height,
   update_panel_position(Grid, PanelName, X, Y1);

move(Grid, PanelName, content_bottom, content_right) ->
   RightX  = content_right(Grid),
   Width  = panel_width(Grid, PanelName),
   X = RightX - Width,
   BottomY = content_bottom(Grid),
   Height = panel_height(Grid, PanelName),
   Y = BottomY - Height,
   update_panel_position(Grid, PanelName, X, Y);
   
move(Grid, PanelName, content_left, Y) ->
   X = content_left(Grid),
   update_panel_position(Grid, PanelName, X, Y);

move(Grid, PanelName, content_right, Y) ->
   RightX  = content_right(Grid),
   Width  = panel_width(Grid, PanelName),
   X = RightX - Width,
   update_panel_position(Grid, PanelName, X, Y);

move(Grid, PanelName, under, RefPanelName) ->
   {X, Y} = panel_position(Grid, RefPanelName),
   TotalHeight = panel_total_height(Grid, RefPanelName),
   Y1 = Y + TotalHeight,
   update_panel_position(Grid, PanelName, X, Y1);

move(Grid, PanelName, beside, RefPanelName) ->
   {X, Y} = panel_position(Grid, RefPanelName),
   TotalWidth = panel_total_width(Grid, RefPanelName),
   X1 = X + TotalWidth,
   update_panel_position(Grid, PanelName, X1, Y);

move(Grid, PanelName, up, Points) ->
   {X, Y} = panel_position(Grid, PanelName),
   Y1 = Y - Points,
   Y2 = max(content_top(Grid), Y1),
   update_panel_position(Grid, PanelName, X, Y2);

move(Grid, PanelName, down, Points) ->
   {X, Y} = panel_position(Grid, PanelName),
   Y1 = Y + Points,
   YLimit = content_bottom(Grid) - panel_height(Grid, PanelName),
   Y2 = min(Y1, YLimit),
   update_panel_position(Grid, PanelName, X, Y2);

move(Grid, PanelName, left, Points) ->
   {X, Y} = panel_position(Grid, PanelName),
   X1 = X - Points,
   X2 = max(content_left(Grid), X1),
   update_panel_position(Grid, PanelName, X2, Y);

move(Grid, PanelName, right, Points) ->
   {X, Y} = panel_position(Grid, PanelName),
   X1 = X + Points,
   XLimit = content_right(Grid) - panel_width(Grid, PanelName),
   X2 = min(X1, XLimit),
   update_panel_position(Grid, PanelName, X2, Y);

move(Grid, PanelName, X, Y) ->
   update_panel_position(Grid, PanelName, X, Y).

%% *************************************************************      
%% @doc Align N panels at left edge and top of content area
%% *************************************************************      

-spec align(Grid        :: map(), 
            PanelName   :: string(), 
            N           :: integer(),
            X           :: left | under | beside | integer(), 
            Y           :: top | under | bottom | integer(),
            RowWidth    :: full_width | integer(), 
            RowHeight   :: full_height | integer(),
            GutterWidth :: integer(), 
            Leading     :: integer()) -> map().

%% left top

align(Grid, PanelName, N, left, top, full_width, full_height, GutterWidth, Leading) ->
    {X, Y} = content_left_top(Grid),
    {OpenWidth, OpenHeight} = vacant_below(Grid, X, Y, full_width, full_height),
    align(Grid, PanelName, N, X, Y, OpenWidth, OpenHeight, GutterWidth, Leading);

align(Grid, PanelName, N, left, top, full_width, Height, GutterWidth, Leading) ->
    {X, Y} = content_left_top(Grid),
    {OpenWidth, OpenHeight} = vacant_below(Grid, X, Y, full_width, Height),
    align(Grid, PanelName, N, X, Y, OpenWidth, OpenHeight, GutterWidth, Leading);

align(Grid, PanelName, N, left, top, Width, full_height, GutterWidth, Leading) ->
    {X, Y} = content_left_top(Grid),
    {OpenWidth, OpenHeight} = vacant_below(Grid, X, Y, Width, full_height),
    align(Grid, PanelName, N, X, Y, OpenWidth, OpenHeight, GutterWidth, Leading);

align(Grid, PanelName, N, left, top, Width, Height, GutterWidth, Leading) ->
    {X, Y} = content_left_top(Grid),
    {OpenWidth, OpenHeight} = vacant_below(Grid, X, Y, Width, Height),
    align(Grid, PanelName, N, X, Y, OpenWidth, OpenHeight, GutterWidth, Leading);

%% left bottom

align(Grid, PanelName, N, left, bottom, full_width, full_height, GutterWidth, Leading) ->
    {X, Y} = content_left_bottom(Grid),
    {OpenWidth, OpenHeight} = vacant_above(Grid, X, Y, full_width, full_height),
    io:format("Y: ~p; OpenHeight: ~p~n", [Y, OpenHeight]),
    Y1     = Y - OpenHeight,
    align(Grid, PanelName, N, X, Y1, OpenWidth, OpenHeight, GutterWidth, Leading);

align(Grid, PanelName, N, left, bottom, full_width, Height, GutterWidth, Leading) ->
    {X, Y} = content_left_bottom(Grid),
    {OpenWidth, OpenHeight} = vacant_above(Grid, X, Y, full_width, Height),
    io:format("Y: ~p; OpenHeight: ~p~n", [Y, OpenHeight]),
    Y1     = Y - OpenHeight,
    align(Grid, PanelName, N, X, Y1, OpenWidth, OpenHeight, GutterWidth, Leading);

align(Grid, PanelName, N, left, bottom, Width, full_height, GutterWidth, Leading) ->
    {X, Y} = content_left_bottom(Grid),
    {OpenWidth, OpenHeight} = vacant_above(Grid, X, Y, Width, full_height),
    io:format("Y: ~p; OpenHeight: ~p~n", [Y, OpenHeight]),
    Y1     = Y - OpenHeight,
    align(Grid, PanelName, N, X, Y1, OpenWidth, OpenHeight, GutterWidth, Leading);

align(Grid, PanelName, N, left, bottom, Width, Height, GutterWidth, Leading) ->
    {X, Y} = content_left_bottom(Grid),
    {OpenWidth, OpenHeight} = vacant_above(Grid, X, Y, Width, Height),
    io:format("Y: ~p; OpenHeight: ~p~n", [Y, OpenHeight]),
    Y1     = Y - OpenHeight,
    align(Grid, PanelName, N, X, Y1, OpenWidth, OpenHeight, GutterWidth, Leading);

%% left Y

align(Grid, PanelName, N, left, Y, full_width, full_height, GutterWidth, Leading) ->
    X = content_left(Grid),
    {OpenWidth, OpenHeight} = vacant_below(Grid, X, Y, full_width, full_height),
    align(Grid, PanelName, N, X, Y, OpenWidth, OpenHeight, GutterWidth, Leading);

align(Grid, PanelName, N, left, Y, full_width, Height, GutterWidth, Leading) ->
    X = content_left(Grid),
    {OpenWidth, OpenHeight} = vacant_below(Grid, X, Y, full_width, Height),
    align(Grid, PanelName, N, X, Y, OpenWidth, OpenHeight, GutterWidth, Leading);

align(Grid, PanelName, N, left, Y, Width, full_height, GutterWidth, Leading) ->
    X = content_left(Grid),
    {OpenWidth, OpenHeight} = vacant_below(Grid, X, Y, Width, full_height),
    align(Grid, PanelName, N, X, Y, OpenWidth, OpenHeight, GutterWidth, Leading);

align(Grid, PanelName, N, left, Y, Width, Height, GutterWidth, Leading) ->
    X = content_left(Grid),
    {OpenWidth, OpenHeight} = vacant_below(Grid, X, Y, Width, Height),
    align(Grid, PanelName, N, X, Y, OpenWidth, OpenHeight, GutterWidth, Leading);

%% right top

align(Grid, PanelName, N, right, top, full_width, full_height, GutterWidth, Leading) ->
    {X, Y} = content_left_top(Grid),
    {OpenWidth, OpenHeight} = vacant_below(Grid, X, Y, full_width, full_height),
    align(Grid, PanelName, N, X, Y, OpenWidth, OpenHeight, GutterWidth, Leading);

%% right y

%% center Y

align(Grid, PanelName, N, center, Y, Width, Height, GutterWidth, Leading) ->
    Span = content_right(Grid) - content_left(Grid), 
    Span1 = (Span div 2) + content_left(Grid),
    Span2 = Span1 - (Width div 2),
    X = Span2 + (GutterWidth div 2),
    {OpenWidth, OpenHeight} = vacant_below(Grid, X, Y, Width, Height),
    align(Grid, PanelName, N, X, Y, OpenWidth, OpenHeight, GutterWidth, Leading);

%% under RefPanel

align(Grid, PanelName, N, under, RefPanelName, full_width, full_height, GutterWidth, Leading) ->
     {X, Y} = position_under(Grid, RefPanelName),
     {OpenWidth, OpenHeight} = vacant_below(Grid, X, Y, full_width, full_height),
     align(Grid, PanelName, N, X, Y, OpenWidth, OpenHeight, GutterWidth, Leading);

align(Grid, PanelName, N, under, RefPanelName, full_width, Height, GutterWidth, Leading) ->
     {X, Y} = position_under(Grid, RefPanelName),
     {OpenWidth, OpenHeight} = vacant_below(Grid, X, Y, full_width, Height),
     align(Grid, PanelName, N, X, Y, OpenWidth, OpenHeight, GutterWidth, Leading);

align(Grid, PanelName, N, under, RefPanelName, Width, full_height, GutterWidth, Leading) ->
     {X, Y} = position_under(Grid, RefPanelName),
     {OpenWidth, OpenHeight} = vacant_below(Grid, X, Y, Width, full_height),
     align(Grid, PanelName, N, X, Y, OpenWidth, OpenHeight, GutterWidth, Leading);

align(Grid, PanelName, N, under, RefPanelName, Width, Height, GutterWidth, Leading) ->
     {X, Y} = position_under(Grid, RefPanelName),
     {OpenWidth, OpenHeight} = vacant_below(Grid, X, Y, Width, Height),
     align(Grid, PanelName, N, X, Y, OpenWidth, OpenHeight, GutterWidth, Leading);

%% beside RefPanel

align(Grid, PanelName, N, beside, RefPanelName, full_width, full_height, GutterWidth, Leading) ->
     {X, Y} = position_beside(Grid, RefPanelName),
     {OpenWidth, OpenHeight} = vacant_below(Grid, X, Y, full_width, full_height),
     align(Grid, PanelName, N, X, Y, OpenWidth, OpenHeight, GutterWidth, Leading);

align(Grid, PanelName, N, beside, RefPanelName, full_width, Height, GutterWidth, Leading) ->
     {X, Y} = position_beside(Grid, RefPanelName),
     {OpenWidth, OpenHeight} = vacant_below(Grid, X, Y, full_width, Height),
     align(Grid, PanelName, N, X, Y, OpenWidth, OpenHeight, GutterWidth, Leading);

align(Grid, PanelName, N, beside, RefPanelName, Width, full_height, GutterWidth, Leading) ->
     {X, Y} = position_beside(Grid, RefPanelName),
     {OpenWidth, OpenHeight} = vacant_below(Grid, X, Y, Width, full_height),
     align(Grid, PanelName, N, X, Y, OpenWidth, OpenHeight, GutterWidth, Leading);

align(Grid, PanelName, N, beside, RefPanelName, Width, Height, GutterWidth, Leading) ->
     {X, Y} = position_beside(Grid, RefPanelName),
     {OpenWidth, OpenHeight} = vacant_below(Grid, X, Y, Width, Height),
     align(Grid, PanelName, N, X, Y, OpenWidth, OpenHeight, GutterWidth, Leading);

%% X Y

align(Grid, PanelName, N, X, Y, full_width, full_height, GutterWidth, Leading) ->
    {OpenWidth, OpenHeight} = vacant_below(Grid, X, Y, full_width, full_height),
    align(Grid, PanelName, N, X, Y, OpenWidth, OpenHeight, GutterWidth, Leading);

align(Grid, PanelName, N, X, Y, full_width, Height, GutterWidth, Leading) ->
    {OpenWidth, OpenHeight} = vacant_below(Grid, X, Y, full_width, Height),
    align(Grid, PanelName, N, X, Y, OpenWidth, OpenHeight, GutterWidth, Leading);

align(Grid, PanelName, N, X, Y, Width, full_height, GutterWidth, Leading) ->
    {OpenWidth, OpenHeight} = vacant_below(Grid, X, Y, Width, full_height),
    align(Grid, PanelName, N, X, Y, OpenWidth, OpenHeight, GutterWidth, Leading);

align(Grid, PanelName, N, X, Y, OpenWidth, OpenHeight, GutterWidth, Leading) ->
   Accum = N,
   PanelWidth = row_panel_width(N, OpenWidth, GutterWidth),
    align_panels(Grid, N, PanelName, Accum, X, Y, PanelWidth, OpenHeight, GutterWidth, Leading).

%% *************************************************************      
%% @doc Create and stack N panels 
%% *************************************************************      
   
-spec stack(Grid        :: map(), 
            PanelName   :: string(), 
            N           :: integer(),
            X           :: atom() | integer(), 
            Y           :: atom() | integer(),
            Width       :: integer(),  
            GutterWidth :: integer(), 
            Leading     :: integer()) -> map(). 

%% left top

stack(Grid, PanelName, N, left, top, full_width, GutterWidth, Leading) ->
   {X, Y} = content_left_top(Grid),
   Width = content_width(Grid),
   stack(Grid, PanelName, N, X, Y, Width, GutterWidth, Leading);
  
stack(Grid, PanelName, N, left, top, Width, GutterWidth, Leading) ->
   {X, Y} = content_left_top(Grid),
   stack(Grid, PanelName, N, X, Y, Width, GutterWidth, Leading);
 
%% left Y
 
stack(Grid, PanelName, N, left, Y, full_width, GutterWidth, Leading) ->
   X = content_left(Grid),
   Width = content_width(Grid),
   stack(Grid, PanelName, N, X, Y, Width, GutterWidth, Leading);
  
stack(Grid, PanelName, N, left, Y, Width, GutterWidth, Leading) ->
   X = content_left(Grid),
   stack(Grid, PanelName, N, X, Y, Width, GutterWidth, Leading);
 
%% right top
 
stack(Grid, PanelName, N, right, top, Width, GutterWidth, Leading) ->
   X = content_right(Grid) - Width,
   Y = content_top(Grid),
   stack(Grid, PanelName, N, X, Y, Width, GutterWidth, Leading);

%% right Y 

stack(Grid, PanelName, N, right, Y, Width, GutterWidth, Leading) ->
   X = content_right(Grid) - Width,
   stack(Grid, PanelName, N, X, Y, Width, GutterWidth, Leading);

%% beside RefPanel 

stack(Grid, PanelName, N, beside, RefPanelName, full_width, GutterWidth, Leading) ->
     {X, Y} = position_beside(Grid, RefPanelName),
     Width = content_width(Grid),
     stack(Grid, PanelName, N, X, Y, Width, GutterWidth, Leading);

stack(Grid, PanelName, N, beside, RefPanelName, Width, GutterWidth, Leading) ->
     {X, Y} = position_beside(Grid, RefPanelName),
     stack(Grid, PanelName, N, X, Y, Width, GutterWidth, Leading);

%% under RefPanel

stack(Grid, PanelName, N, under, RefPanelName, full_width, GutterWidth, Leading) ->
     {X, Y} = position_under(Grid, RefPanelName),
     Width = content_width(Grid),
     stack(Grid, PanelName, N, X, Y, Width, GutterWidth, Leading);

stack(Grid, PanelName, N, under, RefPanelName, Width, GutterWidth, Leading) ->
     {X, Y} = position_under(Grid, RefPanelName),
     stack(Grid, PanelName, N, X, Y, Width, GutterWidth, Leading);


%% center top

stack(Grid, PanelName, N, center, top, Width, GutterWidth, Leading) ->
    ContentWidth = content_width(Grid), 
    Span = ContentWidth - Width,
    X = (Span div 2) + content_left(Grid),
    Y = content_top(Grid),
%    {OpenWidth, OpenHeight} = vacant_below(Grid, X, Y, Width, Height),
    stack(Grid, PanelName, N, X, Y, Width, GutterWidth, Leading);

%% center X 

stack(Grid, PanelName, N, center, Y, Width, GutterWidth, Leading) ->
    ContentWidth = content_width(Grid), 
    Span = ContentWidth - Width,
    X = (Span div 2) + content_left(Grid),
%    {OpenWidth, OpenHeight} = vacant_below(Grid, X, Y, Width, Height),
    stack(Grid, PanelName, N, X, Y, Width, GutterWidth, Leading);

%% X top

stack(Grid, PanelName, N, X, top, full_width, GutterWidth, Leading) ->
   Y = content_top(Grid),
   Width = content_width(Grid),
   stack(Grid, PanelName, N, X, Y, Width, GutterWidth, Leading);
  
stack(Grid, PanelName, N, X, top, Width, GutterWidth, Leading) ->
   Y = content_top(Grid),
   stack(Grid, PanelName, N, X, Y, Width, GutterWidth, Leading);
 
%% X Y
 
stack(Grid, PanelName, N, X, Y, Width, GutterWidth, Leading) ->
   Accum = N,
   Height = stack_panel_height(Grid, N, X,Y, Leading),
   {OpenOnRight, OpenBelow} = open_below(Grid, X, Y),
   Width1  = min(Width, OpenOnRight),
   Height1 = min(Height, OpenBelow),
   stack_panels(Grid, PanelName, N, Accum, X, Y, Width1, Height1, GutterWidth, Leading).


%% *************************************************************      
%% @doc Define panel border 
%% *************************************************************      

-spec bleed(Grid      :: map(), 
            PanelName :: string(), 
            Edge      :: atom()) -> map().

bleed(Grid, PanelName, left) ->
   BleedFactor = bleed_factor(Grid),
   {X, _Y}     = page_left_top(Grid),
   Panel       = get_panel(Grid, PanelName),
   Panel1      = ep_panel:update_x(Panel, X -  BleedFactor),
   replace_panel(Grid, PanelName, Panel1);

bleed(Grid, PanelName, top) ->
   BleedFactor  = bleed_factor(Grid),
   {_X, Y}      = page_left_top(Grid),
   Panel        = get_panel(Grid, PanelName),
   Panel1       = ep_panel:update_y(Panel, Y -  BleedFactor),
   replace_panel(Grid, PanelName, Panel1);


bleed(Grid, PanelName, right) ->
   BleedFactor = bleed_factor(Grid),
   X           = page_right(Grid),
   Panel       = get_panel(Grid, PanelName),
   Panel1      = ep_panel:update_x(Panel, X +  BleedFactor),
   replace_panel(Grid, PanelName, Panel1);

bleed(Grid, PanelName, bottom) ->
   BleedFactor = bleed_factor(Grid),
   Y           = page_bottom(Grid),
   Panel       = get_panel(Grid, PanelName),
   Panel1      = ep_panel:update_y(Panel, Y +  BleedFactor),
   replace_panel(Grid, PanelName, Panel1).

%% *************************************************************      
%% @doc bleed/3 helper 
%% *************************************************************      

-spec bleed_factor(Grid :: map()) -> integer().

bleed_factor(Grid) ->
   PageType     = page_type(Grid),
   DesktopStock = ep_paper:desktop_printer_stock(),
   Flag         = lists:member(PageType, DesktopStock),
   case Flag of
     true  -> 0;
     false -> 5
   end.

%% *************************************************************      
%% @doc Set border width; 0 - 5 
%% *************************************************************      

-spec show_border(Grid      :: map(), 
                  PanelName :: string(), 
                  Points    :: integer()) -> map().

show_border(Grid, PanelName, Points) when Points >= 0, Points < 5 ->
   Panel = get_panel(Grid, PanelName),
   Panel1 = ep_panel:update_border(Panel, Points), 
   replace_panel(Grid, PanelName, Panel1).

%% *************************************************************      
%% @doc Set border width to 0
%% *************************************************************      

-spec hide_border(Grid      :: map(), 
                  PanelName :: string()) -> map().

hide_border(Grid, PanelName) ->
   Panel = get_panel(Grid, PanelName),
   Panel1 = ep_panel:update_border(Panel, 0), 
   replace_panel(Grid, PanelName, Panel1).

%% *************************************************************      
%% @doc Define border color 
%% *************************************************************      

-spec border_color(Grid      :: map(), 
                   PanelName :: string(),
                   Color     :: atom()) -> map().

border_color(Grid, PanelName, Color) ->
   Panel = get_panel(Grid, PanelName),
   Panel1 = ep_panel:update_border_color(Panel, Color), 
   replace_panel(Grid, PanelName, Panel1).

%% *************************************************************      
%% @doc Define background color 
%% *************************************************************      

-spec background_color(Grid      :: map(), 
                       PanelName :: string(),
                       Color     :: atom()) -> map().

background_color(Grid, PanelName, Color) ->
   Panel = get_panel(Grid, PanelName),
   Panel1 = ep_panel:update_background_color(Panel, Color), 
   replace_panel(Grid, PanelName, Panel1).

%% *************************************************************      
%% @doc Update gutter and leading 
%% *************************************************************      

-spec update_margins(Grid :: map(), 
                     PanelName :: string,
                     GutterWidth :: integer(), 
                     Leading :: integer()) -> map().

update_margins(Grid, PanelName, GutterWidth, Leading) ->
    update_gutter(Grid, PanelName, GutterWidth),
    update_leading(Grid, PanelName, Leading).

%% *************************************************************      
%% @doc Update gutter 
%% *************************************************************      

-spec update_gutter(Grid :: map(), PanelName :: string,
     GutterWidth :: integer()) -> map().

update_gutter(Grid, PanelName, Gutter) ->
   Panel       = get_panel(Grid, PanelName),
   Panel1      = ep_panel:update_gutter(Panel, Gutter),
   replace_panel(Grid, PanelName, Panel1).
   
%% *************************************************************      
%% @doc Update leading 
%% *************************************************************      

-spec update_leading(Grid :: map(), PanelName :: string,
     Leading :: integer()) -> map().

update_leading(Grid, PanelName, Leading) ->
   Panel       = get_panel(Grid, PanelName),
   Panel1      = ep_panel:update_leading(Panel, Leading),
   replace_panel(Grid, PanelName, Panel1).
   
%% *************************************************************      
%% @doc Create copy of panel with new name and index
%% *************************************************************      

-spec clone_panel(Grid         :: tuple(), 
                  PanelName    :: string(),
                  NewPanelName :: string()) -> map().

clone_panel(Grid, PanelName, NewPanelName) ->
   Panel  = get_panel(Grid, PanelName),
   Index = next_index(Grid),
   NewPanel = {Index, NewPanelName, Panel},
   put_panel(Grid, NewPanel).
   
%% *************************************************************      
%% @doc Create copy of panel with new name and index
%% *************************************************************      

-spec delete_panel(Grid      :: map(), 
                   PanelName :: string()) -> map().

delete_panel(Grid, PanelName) ->
   Panels = panels(Grid),
   Panels1 = lists:keydelete(2, PanelName, Panels),
   maps:put(panels, Panels1, Grid).

%%% *********************************************************      
%%% @doc Print grid 
%%% *********************************************************      

 -spec print_grid(Grid :: map()) -> ok.

print_grid(Grid) ->
    Name  = name(Grid),
    OFile = Name ++ ".pdf",
    PDF = eg_pdf:new(),
    page_grid_header(PDF, Grid),
    print_panels(PDF, Grid),
   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?PAGE_GRID_DIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF),
    ok.
         
%%% *********************************************************      
%%% *********************************************************      
%%% Helper functions 
%%% *********************************************************      
%%% *********************************************************      

%% *************************************************************      
%% Create panel/6
%% *************************************************************      

%% *************************************************************      
%% Create panel including definition of gutterwidth and leading  
%% *************************************************************      

%% *************************************************************      
%% vacant_space/5  
%% *************************************************************      
 
vacant_below(Grid, X, Y, full_width, full_heidht) ->
    open_below(Grid, X, Y);

vacant_below(Grid, X, Y, full_width, Height) ->
    {OpenWidth, OpenHeight} = open_below(Grid, X, Y),
    Width  = OpenWidth,
    Height1 = min(Height, OpenHeight),
    {Width, Height1};

vacant_below(Grid, X, Y, Width, full_width) ->
    {OpenWidth, OpenHeight} = open_below(Grid, X, Y),
    Width1  = min(Width, OpenWidth),
    Height = OpenHeight,
    {Width1, Height};

vacant_below(Grid, X, Y, Width, Height) ->
    {OpenWidth, OpenHeight} = open_below(Grid, X, Y),
    Width1  = min(Width, OpenWidth),
    Height1 = min(Height, OpenHeight),
    {Width1, Height1}.

%% *************************************************************      
%% vacant_space_above/5  
%% *************************************************************      
 
vacant_above(Grid, X, Y, full_width, full_height) ->
    open_above(Grid, X, Y);

vacant_above(Grid, X, Y, full_width, Height) ->
    {OpenWidth, OpenHeight}  = open_above(Grid, X, Y),
    Width  = OpenWidth,
    Height1 = min(Height, OpenHeight),
    {Width, Height1};

vacant_above(Grid, X, Y, Width, full_height) ->
    {OpenWidth, OpenHeight} = open_above(Grid, X, Y),
    Width1 = min(Width, OpenWidth),
    Height = max(0, OpenHeight),
    {Width1, Height};

vacant_above(Grid, X, Y, Width, Height) ->
    {OpenWidth, OpenHeight} = open_above(Grid, X, Y),
    Width1  = min(Width, OpenWidth),
    Height1  = min(Height, OpenHeight),
%    Height1 = max(0, OpenHeight),
    {Width1, Height1}.


%% *************************************************************      






-spec define_grid(Name :: string(), PaperStock :: atom(), PageType :: atom()) -> map().

define_grid(Name, PaperStock, PageType) ->
   #{name            =>  Name,
     paper_stock     =>  PaperStock,
     page_type       =>  PageType,
     content_margins =>  {0, 0, 0, 0}, % Top, Right, Bottom, Left
     panels          =>  []
    }.

%% *********************************************************      
%% Paper stock functions 
%% ********************************************************* 

%% Return paper stock

-spec paper_stock(Grid :: map()) -> atom().

paper_stock(Grid) ->
    maps:get(paper_stock, Grid).

%% Return paper stock width and height

-spec stock_dimensions(Grid :: map()) -> tuple().

stock_dimensions(Grid) ->
    PaperStock = paper_stock(Grid),
    ep_paper:paper_stock_points(PaperStock).

-spec stock_width(Grid :: map()) -> integer().

stock_width(Grid) ->
    {StockWidth, _StockHeight} = stock_dimensions(Grid),
    StockWidth. 

-spec stock_height(Grid :: map()) -> integer().

stock_height(Grid) ->
    {_StockWidth, StockHeight} = stock_dimensions(Grid),
    StockHeight. 

%% *********************************************************      
%% Page functions 
%% ********************************************************* 

%% Return page type

-spec page_type(Grid :: map()) -> atom().

page_type(Grid) ->
    maps:get(page_type, Grid).

%% Return page dimensions

-spec page_dimensions(Grid :: map()) -> tuple().

page_dimensions(Grid) ->
    PageType = page_type(Grid),
    ep_paper:pagesize(PageType).

-spec page_width(Grid :: map()) -> integer().

page_width(Grid) ->
    {PageWidth, _PageHeight} = page_dimensions(Grid),
    PageWidth.

-spec page_height(Grid :: map()) -> integer().

page_height(Grid) ->
    {_PageWidth, PageHeight} = page_dimensions(Grid),
    PageHeight.




-spec page_trim_spec(Grid :: map()) -> tuple().

page_trim_spec(Grid) ->
    {X, Y} = page_xy(Grid),
    Width = page_width(Grid),
    Height = page_height(Grid),
    {X, Y, Width, Height}.


%% Given paper stock, return margins around page in points 
%% NOTE: Assumes letter or folio stock on desktop PDF printer

%% Return top of page in points 

-spec page_top(Grid :: map()) -> integer().

page_top(Grid) ->
   StockHeight = stock_height(Grid),
   {_PageWidth, PageHeight}  = page_dimensions(Grid),
   (StockHeight - PageHeight) div 2.

%% Return top edge of page in points 

-spec page_right(Grid :: map()) -> integer().

page_right(Grid) ->
   StockWidth = stock_width(Grid),
   PageWidth  = page_width(Grid),
   (StockWidth - PageWidth) div 2.

%% Return bottom of page in points 

-spec page_bottom(Grid :: map()) -> integer().

page_bottom(Grid) ->
   page_top(Grid). 

%% Return edge edge of page in points 

-spec page_left(Grid :: map()) -> integer().

page_left(Grid) ->
   page_right(Grid).


-spec page_margins(Grid :: map()) -> integer().

page_margins(Grid) ->
   {page_top(Grid), 
    page_right(Grid), 
    page_bottom(Grid), 
    page_left(Grid)
   }.
   

%% Return upper-left corner of page

-spec page_left_top(Grid :: map()) -> tuple().

page_left_top(Grid) ->
    LeftCoord = page_left(Grid),
    TopCoord  = page_top(Grid),
    {LeftCoord, TopCoord}.

%% Alias

page_xy(Grid) ->
   page_left_top(Grid).


%% Return upper-right corner of page

-spec page_right_top(Grid :: map()) -> tuple().

page_right_top(Grid) ->
    RightCoord = page_right(Grid),
    TopCoord   = page_top(Grid),
    {RightCoord, TopCoord}.

%% Return upper-left corner of page in points

-spec page_left_bottom(Grid :: map()) -> tuple().

 page_left_bottom(Grid) ->
    X = page_left(Grid),
    Y = page_bottom(Grid), 
    {X, Y}.

%% Return lower-right corner of page in points

-spec page_right_bottom(Grid :: map()) -> tuple().

page_right_bottom(Grid) ->
    X = page_right(Grid),
    Y = page_left(Grid), 
    {X, Y}.

%% *********************************************************      
%% Content functions 
%% ********************************************************* 

%% Given page type, return space around content; 
%% {Top, Right, Bottom, Left}

-spec content_margins(Grid :: map()) -> tuple().

content_margins(Grid) ->
    maps:get(content_margins, Grid).

%% Return top of content area in points

-spec content_top(Grid :: map()) -> integer().

content_top(Grid) ->
   PageTop = page_top(Grid),
   TopMargin = element(1, content_margins(Grid)),
   PageTop + TopMargin.   
   
%% Return right edge of content area in points

-spec content_right(Grid :: map()) -> integer().

content_right(Grid) ->
   PageRight = page_right(Grid),
   RightMargin = element(2, content_margins(Grid)),
   PageRight - RightMargin.   
   
%% Return bottom of content area in points

-spec content_bottom(Grid :: map()) -> integer().

content_bottom(Grid) ->
   PageBottom = page_bottom(Grid),
   BottomMargin = element(3, content_margins(Grid)),
   PageBottom - BottomMargin.   

%% Return left edge of content area in points

content_left(Grid) ->
   PageLeft = page_left(Grid),
   LeftMargin = element(4, content_margins(Grid)),
   PageLeft + LeftMargin.   
   
%% Given grid, return upper left corner of content area 

-spec content_left_top(Grid :: map()) -> tuple().

content_left_top(Grid) ->
    Left = content_left(Grid),
    Top  = content_top(Grid),
    {Left, Top}.

%% Given grid, return lower left corner of content area 

-spec content_left_bottom(Grid :: map()) -> tuple().

content_left_bottom(Grid) ->
    Left    = content_left(Grid),
    Bottom  = content_bottom(Grid),
    {Left, Bottom}.

%% Given grid, return lower left corner of content area 

-spec content_right_top(Grid :: map()) -> tuple().

content_right_top(Grid) ->
    Right = content_right(Grid),
    Top   = content_top(Grid),
    {Right, Top}.

%% Given grid, return width of content area 

-spec content_width(Grid :: map()) -> integer().

content_width(Grid) ->
    ContentRight = content_right(Grid),
    ContentLeft  = content_left(Grid),
    ContentRight - ContentLeft.

%% Given grid, return height of content area 

-spec content_height(Grid :: map()) -> integer().

content_height(Grid) ->
    {_PageWidth, PageHeight} = page_dimensions(Grid),
    {Top, _Right, Bottom, _Left} = content_margins(Grid),
    PageHeight - Top - Bottom.

%% Given grid, return width and height of content area 

-spec content_dimensions(Grid :: map()) -> tuple().

content_dimensions(Grid) ->
    Width  = content_width(Grid),
    Height = content_height(Grid),
    {Width, Height}.

%% *********************************************************      
%% Content boundary functions 
%% ********************************************************* 

%% Returns true if X is in content area 

-spec x_inbounds(Grid :: map(), X :: integer()) -> boolean().

x_inbounds(Grid, X) ->
    (X >= content_left(Grid)) and 
    (X =< content_right(Grid)).

%% Returns true if Y is in contentt area 

-spec y_inbounds(Grid :: map(), Y :: integer()) -> boolean().

y_inbounds(Grid, Y) ->
    (Y >= content_top(Grid)) and 
    (Y =< content_bottom(Grid)).

%% Returns true if X and Y are in contentt area 

-spec position_inbounds(Grid :: map(), X :: integer(), Y :: integer()) -> boolean().

position_inbounds(Grid, X, Y) ->
    x_inbounds(Grid, X) and
    y_inbounds(Grid, Y).

%% *********************************************************      
%% Grid functions 
%% ********************************************************* 

%% Return grid name

-spec name(Grid :: map()) -> string().

name(Grid) ->
    maps:get(name, Grid).

%% Return area of open space to right and below coordinate xy 

-spec open_below(Grid :: map(), X :: integer(), Y :: integer()) -> tuple().

open_below(Grid, X, Y) ->
   Flag = position_inbounds(Grid, X, Y),
   case Flag of
       true  -> Right = open_space_right(Grid, X, Y),
                Below = open_space_below(Grid, X, Y),
                {Right, Below};
       false -> {error, coordinate_out_of_bounds}
    end.

%% Return area of open space to right and above coordinate xy 

-spec open_above(Grid :: map(), X :: integer(), Y :: integer()) -> tuple().

open_above(Grid, X, Y) ->
   Flag = position_inbounds(Grid, X, Y),
   case Flag of
      true  -> Right = open_space_right(Grid, X, Y),
               Below = open_space_above(Grid, X, Y),
               {Right, Below};
      false -> {error, coordinate_out_of_bounds}
   end.


%% Return extent of open space to right of coordinate xy 

-spec open_space_right(Grid :: map(), X :: integer(), Y :: integer()) -> integer().

open_space_right(Grid, X, Y) ->
   Panels = panels_right(Grid, X, Y),
   Panels1 = [ep_panel:x(Panel) || Panel <- Panels],
   case Panels of
      []     -> content_right(Grid) - X;
      _      -> lists:min(Panels1) - X
   end.

%% Return extent of open space below coordinate xy 

-spec open_space_below(Grid :: map(), X :: integer(), Y :: integer()) -> integer().

open_space_below(Grid, X, Y) ->
   Panels = panels_below(Grid, X, Y),
   Panels1 = [ep_panel:y(Panel) || Panel <- Panels],
   case Panels of
      []     -> content_bottom(Grid) - Y;
      _      -> lists:min(Panels1) - Y
   end.

%% Return extent of open space above coordinate xy 

-spec open_space_above(Grid :: map(), X :: integer(), Y :: integer()) -> integer().

open_space_above(Grid, X, Y) ->
   Panels = panels_above(Grid, X, Y),
   List = [{ep_panel:y(Panel), ep_panel:name(Panel)} || Panel <- Panels],
   case Panels of
      []     -> Y - content_top(Grid);
      _      -> % Get value of largest Y in panels
                Tuple = lists:max(List),
                % Get name of panel with largest Y
                Name = element(2, Tuple),
                Panel = lists:keyfind(Name, 2, Panels),
                % Get values to compute space above
                Y1 = ep_panel:y(Panel),
                Height = ep_panel:height(Panel),
                GutterWidth = ep_panel:gutter(Panel),
                Y - (Y1 + Height + GutterWidth)
   end.

%% *********************************************************      
%% Open space helpers 
%% ********************************************************* 

%% Return list of panels to right of coordinate xy

-spec panels_right(Grid :: map(), X :: integer(), Y :: integer()) -> list().

panels_right(Grid, X, Y) ->
   Panels = y_within_panels(Grid, Y),
   [Panel || Panel <- Panels, ep_panel:x(Panel) > X].

%% Return list of panels below coordinate xy

-spec panels_below(Grid :: map(), X :: integer(), Y :: integer()) -> list().

panels_below(Grid, X, Y) ->
   Panels = x_within_panels(Grid, X),
   [Panel || Panel <- Panels, ep_panel:y(Panel) > Y].

%% Return list of panels above coordinate xy

-spec panels_above(Grid :: map(), X :: integer(), Y :: integer()) -> list().

panels_above(Grid, X, Y) ->
   Panels = x_within_panels(Grid, X),
   [Panel || Panel <- Panels, ep_panel:y(Panel) < Y].

%% Return list of panels trnasected by vertical line at x 

-spec x_within_panels(Grid :: map(), X :: integer()) -> list().

x_within_panels(Grid, X) ->
   Panels = panels(Grid),
   [Panel || Panel <- Panels, ep_panel:x_within(Panel, X)].

%% Return list of panels trnasected by horizontal line at y 

-spec y_within_panels(Grid :: map(), Y :: integer()) -> list().

y_within_panels(Grid, Y) ->
   Panels = panels(Grid),
   [Panel || Panel <- Panels, ep_panel:y_within(Panel, Y)].


%% *********************************************************      
%% Panel functions 
%% ********************************************************* 

%% Return list of panels assigned to page grid

-spec panels(Grid :: map()) -> list().

panels(Grid) ->
    maps:get(panels, Grid).

%% Return panel

-spec get_panel(Grid :: map(), PanelName :: string()) -> tuple().

get_panel(Grid, PanelName) ->
   Panels = panels(Grid),
   lists:keyfind(PanelName, 2, Panels).

%% Place panel in grid

-spec put_panel(Grid :: map(), Panel :: map()) ->
    map().

put_panel(Grid, Panel) ->
    update_panels(Grid, Panel).

-spec replace_panel(Grid :: map(), PanelName :: map(),
    Panel :: map()) -> map().

replace_panel(Grid, PanelName, Panel) ->
    Panels  = panels(Grid),
    Panels1 = lists:keyreplace(PanelName, 2, Panels, Panel),
    maps:put(panels, Panels1, Grid).

-spec update_panels(Grid :: map(), Panel :: tuple()) -> map().

update_panels(Grid, Panel) ->
    Panels = panels(Grid),
    Panels1 = [Panel | Panels],
    maps:put(panels, Panels1, Grid).

-spec panel_left(Grid :: map(), PanelName :: string()) -> integer().

panel_left(Grid, PanelName) ->
   Panel = get_panel(Grid, PanelName),
   ep_panel:x(Panel).

-spec panel_right(Grid :: map(), PanelName :: string()) -> integer().

panel_right(Grid, PanelName) ->
   Panel = get_panel(Grid, PanelName),
   ep_panel:end_x(Panel).

-spec panel_top(Grid :: map(), PanelName :: string()) -> integer().

panel_top(Grid, PanelName) ->
   Panel = get_panel(Grid, PanelName),
   ep_panel:y(Panel).

-spec panel_bottom(Grid :: map(), PanelName :: string()) -> integer().

panel_bottom(Grid, PanelName) ->
   Panel = get_panel(Grid, PanelName),
   ep_panel:end_y(Panel).

-spec panel_position(Grid :: map(), PanelName :: string()) -> tuple().

panel_position(Grid, PanelName) ->
   Panel = get_panel(Grid, PanelName),
   ep_panel:position(Panel).

-spec panel_width(Grid :: map(), PanelName :: string()) -> integer().

panel_width(Grid, PanelName) ->
   Panel = get_panel(Grid, PanelName),
   ep_panel:width(Panel).

-spec panel_gutter(Grid :: map(), PanelName :: string()) -> integer().

panel_gutter(Grid, PanelName) ->
   Panel = get_panel(Grid, PanelName),
   ep_panel:gutter(Panel).

-spec panel_total_width(Grid :: map(), PanelName :: string()) -> integer().

panel_total_width(Grid, PanelName) ->
   io:format("PanelName: ~p~n", [PanelName]),
   Panel  = get_panel(Grid, PanelName),
   Width  = ep_panel:width(Panel),
   Gutter = ep_panel:gutter(Panel),
   Width + Gutter.

-spec panel_height(Grid :: map(), PanelName :: string()) -> integer().

panel_height(Grid, PanelName) ->
   Panel = get_panel(Grid, PanelName),
   ep_panel:height(Panel).

-spec panel_leading(Grid :: map(), PanelName :: string()) -> integer().

panel_leading(Grid, PanelName) ->
   Panel = get_panel(Grid, PanelName),
   ep_panel:leading(Panel).

-spec panel_total_height(Grid :: map(), PanelName :: string()) -> integer().

panel_total_height(Grid, PanelName) ->
   Panel = get_panel(Grid, PanelName),
   Height  = ep_panel:height(Panel),
   Leading = ep_panel:leading(Panel),
   Height + Leading.

% -spec panel_dimensions(Grid :: map(), PanelName :: string()) -> tuple().

% panel_dimensions(Grid, PanelName) ->
%   Width = panel_width(Grid, PanelName),
%   Height = panel_height(Grid, PanelName),
%   {Width, Height}.



-spec position_beside(Grid :: map(), PanelName :: string()) -> tuple().

position_beside(Grid, RefPanel) ->
   {X, Y}      = panel_position(Grid, RefPanel),
   RefWidth    = panel_total_width(Grid, RefPanel),
   {X + RefWidth, Y}.




-spec position_under(Grid :: map(), PanelName :: string()) -> tuple().

position_under(Grid, RefPanel) ->
   {X, Y}      = panel_position(Grid, RefPanel),
   io:format("Y: ~p~n", [Y]),
   RefHeight    = panel_total_height(Grid, RefPanel),
   {X, Y + RefHeight}.






%% Return next panel index

-spec next_index(Grid :: map()) -> integer().

next_index(Grid) ->
   Panels = panels(Grid),
   Len = length(Panels),
   Len + 1.

%% *********************************************************      
%% Panel  positioning functions 
%% ********************************************************* 

-spec position_xy(Grid :: map(), Relation :: atom(),
   RefPanelName :: string()) -> tuple().

position_xy(Grid, beside, RefPanelName) ->
   RightX     = panel_right(Grid, RefPanelName),
   Gutter   = panel_gutter(Grid, RefPanelName),
   X        = RightX + Gutter,
   Y        = panel_top(Grid, RefPanelName),
   {X, Y};

position_xy(Grid, under, RefPanelName) ->
   X        = panel_left(Grid, RefPanelName),
   BottomY  = panel_bottom(Grid, RefPanelName),
   Leading  = panel_leading(Grid, RefPanelName),
   Y        = BottomY + Leading,
   {X, Y};

position_xy(Grid, _, _) ->
   content_left_top(Grid).

%% *********************************************************      
%% Panel sequence helpers 
%% *********************************************************      

%% Return total height of stack of panels 

-spec stack_panel_height(Grid :: map(), N :: integer(), X :: integer(),
           Y :: integer(), Leading :: integer()) -> integer().

stack_panel_height(Grid, N, X,Y, Leading) ->
    OpenSpace = open_space_below(Grid, X, Y),
    TotalHeight = (OpenSpace + Leading) div N,
    Height = TotalHeight - Leading,
    Height.

%% Return total widht of row of panels 

-spec row_panel_width(N :: integer(), OpenWidth :: integer(), 
      Leading :: integer()) -> integer().

row_panel_width(N, OpenWidth, GutterWidth) ->
    io:format("OpenWidth: ~p; N: ~p~n", [OpenWidth, N]),
    TotalWidth = (OpenWidth + GutterWidth) div N,
    Width = TotalWidth - GutterWidth,
    Width.

-spec align_panels(Grid :: map(), N :: integer(), PanelName :: string(),
    Accum :: integer(), X :: integer(), Y :: integer(), Width :: integer(),
    Height :: integer(), GutterWidth :: integer(), Leading :: integer()) ->
    map().

align_panels(Grid, _N, _PanelName, 0, _X, _Y, _Width, _Height, _GutterWidth, _Leading) ->
    Grid;

align_panels(Grid, N, PanelName, Accum, X, Y, Width, Height, GutterWidth, Leading) ->
    io:format("Width: ~p~n", [Width]),
    Accum1   = Accum - 1,
    Name = PanelName ++ "-" ++ integer_to_list(N - Accum1),
    Grid1    = create_panel(Grid, Name, X, Y, Width, Height, GutterWidth, Leading),
    X1 = X + Width + GutterWidth,
    align_panels(Grid1, N, PanelName, Accum1, X1, Y, Width, Height, GutterWidth, Leading). 

%% *********************************************************      
%% Stack helpers -- stack_panels/10
%% *********************************************************      

-spec stack_panels(Grid :: map(), 
    PanelName :: string(),
    N :: integer(), 
    Accum :: integer(), 
    X :: integer(), 
    Y :: integer(), 
    Width :: integer(),
    Height :: integer(),
    GutterWidth :: integer(), 
    Leading :: integer()) -> map().

stack_panels(Grid, _PanelName, _N, 0, _X, _Y, _Width,_Height, _GutterWidth, _Leading) ->
    Grid;

stack_panels(Grid, PanelName, N, Accum, X, Y, Width, Height, GutterWidth, Leading) ->
    Accum1 = Accum - 1,
    Name = PanelName ++ "-" ++ integer_to_list(N - Accum1),
    Grid1    = create_panel(Grid, Name, X, Y, Width, Height, GutterWidth, Leading),
    {X1, Y1}   = position_xy(Grid1, under, Name),
    stack_panels(Grid1, PanelName, N, Accum1, X1, Y1, Width, Height, GutterWidth, Leading).

%% *********************************************************      
%% Stack helpers -- update_panel_position/4
%% *********************************************************      


-spec update_panel_position(Grid :: map(), 
    PanelName :: string(),
    X :: integer(), 
    Y :: integer()) -> map().

update_panel_position(Grid, PanelName, X, Y) ->
   Panel  = get_panel(Grid, PanelName),
   Panel1 = ep_panel:update_x(Panel, X),
   Panel2 = ep_panel:update_y(Panel1, Y),
   replace_panel(Grid, PanelName, Panel2).

-spec update_panel_width(Grid :: map(), PanelName :: string(),
    Width :: integer()) -> map().

update_panel_width(Grid, PanelName, Width) ->
   Panel = get_panel(Grid, PanelName),
   Panel1 = ep_panel:update_width(Panel, Width),
   replace_panel(Grid, PanelName, Panel1).

-spec update_panel_height(Grid :: map(), PanelName :: string(),
    Height :: integer()) -> map().

update_panel_height(Grid, PanelName, Height) ->
   Panel  = get_panel(Grid, PanelName),
   Panel1 = ep_panel:update_height(Panel, Height),
   replace_panel(Grid, PanelName, Panel1).


%%% *********************************************************      
%%% Print grid helpers 
%%% *********************************************************      

%%  Format page grid header

page_grid_header(PDF, Grid) ->
   Name      = "*" ++ name(Grid) ++ "*",
   PageType  = atom_to_list(page_type(Grid)),
   PageTypeStr  = "*" ++ PageType ++ "*",
   Text      = "Grid Name: " ++ Name ++ " -- Page Type: " ++ PageTypeStr,
   io:format("******* ep_grid page_grid_header/1 Text: ~p~n", [Text]),
   Text1     = ep_lib:today(),
   io:format("******* ep_grid page_grid_header/1 Text1: ~p~n", [Text1]),
   TypeTag   = page_grid_label,
   TypeTag1  = page_number,
   JumpList  = label_box(Grid),
   ep_fitcopy:paste_text(PDF, Text, TypeTag, JumpList),
   ep_fitcopy:paste_text(PDF, Text1, TypeTag1, JumpList),
   underline(PDF, Grid).


-spec label_box(Grid :: map())-> list().

label_box(Grid) ->
   Indent = 36,
   TopMargin = 0,
   BoxHeight = 30,
   {Width, Height} = stock_dimensions(Grid),
   BoxWidth = Width - (3 * Indent),
   Box = ep_box:create(Indent, Height - TopMargin, BoxWidth, BoxHeight),
   [Box].


%% Format underline for page grid header 

underline(PDF, Grid) ->
   {Width, Height} = stock_dimensions(Grid),
   Indent = 36,
   TopMargin = 21,
   {X1, Y1} = {Indent, Height - TopMargin},
   {X2, Y2} = {Width - (2 * Indent), Height - TopMargin},
   Line = ep_line:new(X1, Y1, X2, Y2),
   ep_line:line(PDF, Line).


%% ***********************************************************
%% Print panels 
%% ***********************************************************

%% Print panels positioned on grid 

print_panels(PDF, Grid) ->
   PaperStock = paper_stock(Grid),
   Panels = panels(Grid),
   Panels1 = lists:reverse(Panels),
   ep_show_grid:show_grid(PDF, PaperStock),
   [print_panel(PDF, Panel, PaperStock) || Panel <- Panels1].


%% Print panel 

print_panel(PDF, Panel, PaperStock) ->
   Name = element(2, Panel),
   {X, Y1}  = ep_panel:position(Panel),
   EndX     = ep_panel:end_x(Panel),
   EndY1    = ep_panel:end_y(Panel),
   Y        = ep_lib:v_flip(Y1, PaperStock),
   EndY     = ep_lib:v_flip(EndY1, PaperStock),
   eg_pdf:begin_text(PDF),
   eg_pdf:set_stroke_color(PDF, black),
   eg_pdf:set_font(PDF, "Times-Italic", 12),
   eg_pdf:set_text_pos(PDF, X + 5, EndY + 5),
   eg_pdf:text(PDF, Name),
   eg_pdf:end_text(PDF),
   HLine1 = ep_line:new(X, Y, EndX, Y),
   HLine2 = ep_line:new(X, EndY, EndX, EndY),
   VLine1 = ep_line:new(X, Y, X, EndY),
   VLine2 = ep_line:new(EndX, Y, EndX, EndY),
   ep_line:line(PDF, HLine1),
   ep_line:line(PDF, HLine2),
   ep_line:line(PDF, VLine1),
   ep_line:line(PDF, VLine2).


