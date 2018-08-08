%%==========================================================================
%%% {c) 2018    Lloyd R. Prentice
%%% Author:     Lloyd R. Prentice
%%% License: 
%%% File:       ep_orphans.erl
%%% Description: 
%%%   Orphanded page layout functions 

%%% NOTE: We many or may not need these functions

%%==========================================================================


-module (ep_orphans).

-include("../include/eg.hrl").

% -export ([]).

-compile(export_all).

-define(PDF_DIR, "./pdf/").
-define(PAGE_GRID_DIR, "./pdf/").


%% *********************************************************      
%% @doc Page bleed dimensions 
%% *********************************************************      

-spec bleed(Grid :: map(), PaperStock :: atom(),
            Edge :: top | right | bottom | left) -> tuple().

bleed(Grid, PaperStock, top) ->
   Bleed = bleed_factor(PaperStock),  % pts
   {X, Y, Width, Height} = ep_grid:page_trim_spec(Grid),
   Top = Y - Bleed,
   Right = X + Width,
   Bottom = Y + Height,
   Left   = X,
   {Top, Right, Bottom, Left};

bleed(Grid, PaperStock,  right) ->
   Bleed = bleed_factor(PaperStock),  % pts
   {X, Y, Width, Height} = ep_grid:page_trim_spec(Grid),
   Top = Y,
   Right = X + Width + Bleed,
   Bottom = Y + Height,
   Left   = X - Bleed,
   {Top, Right, Bottom, Left};

bleed(Grid, PaperStock, bottom) ->
   Bleed = bleed_factor(PaperStock),  % pts
   {X, Y, Width, Height} = ep_grid:page_trim_spec(Grid),
   Top = Y,
   Right = X + Width,
   Bottom = Y + Height + Bleed,
   Left   = X,
   {Top, Right, Bottom, Left};

bleed(Grid, PaperStock, left) ->
   Bleed = bleed_factor(PaperStock),  % pts
   {X, Y, Width, Height} = page_trim_spec(Grid),
   Top = Y,
   Right = X + Width,
   Bottom = Y + Height,
   Left   = X - Bleed,
   {Top, Right, Bottom, Left};

bleed(Grid, PaperStock, all) ->
   Bleed = bleed_factor(PaperStock),  % pts
   {X, Y, Width, Height} = page_trim_spec(Grid),
   Top = Y - Bleed,
   Right = X + Width + Bleed,
   Bottom = Y + Height + Bleed,
   Left   = X - Bleed,
   {Top, Right, Bottom, Left}.


%% @doc Return page x and y coordinates, width, and height
%%      relative to paper stock

%% CAUTION: Test this carefully; may another function

%% @doc Return upper-left corner of page

-spec page_xy(Grid :: map()) -> tuple().

page_xy(Grid) ->
    {TopMargin, _RightMargin, _BottomMargin, LeftMargin} = ep_grid:page_margins(Grid),
    {LeftMargin, TopMargin}.


-spec page_trim_spec(Grid :: map()) -> tuple().

page_trim_spec(Grid) ->
    {X, Y} = ep_grid:page_xy(Grid),
    {Width, Height} = ep_page:page_dimensions(Grid),
    {X, Y, Width, Height}.





bleed_factor(PaperStock) ->
    StockList = ep_paper:desktop_printer_stock(),
    case lists:member(PaperStock, StockList) of
        true  -> 0;
        false -> 10
    end.






% Project
% Page grids
% Text

flip_panel(Panel, PaperStock) ->
   Y = ep_panel:y(Panel),
   Y1 = ep_lib:v_flip(PaperStock, Y),
   Cur_Y = ep_panel:y(Panel),
   Cur_Y1 = ep_lib:v_flip(PaperStock, Cur_Y),
   Panel1 = ep_panel:update_y(Panel, Y1),
   ep_panel:update_y(Panel1, Cur_Y1).


layout1() ->
%   Grid = ep_grid:report(),
%   Panel = ep_grid:get_panel(Grid, "body"),
   {ok, Text}  = file:read_file("/home/lloyd/EP/user_guide/user_guide.md"),
%   Text = ep_copyfit:paragraph(),
%   Box = element(3, Panel),
%   Box1 = flip_panel(Box, letter), 
   Box = ep_fitcopy:box4(),
   JumpList = [Box],
   {Text, JumpList}.

ug1(PDF) ->
   {Text, JumpList} = layout1(),
   TypeTag = report,
   paste_up(PDF, Text, TypeTag, JumpList).



paste_up(PDF, Text, TypeTag, JumpList) ->
   ep_fitcopy:paste_text(PDF, Text, TypeTag, JumpList).



paste_layout() ->
    OFile = "paste_user_guide.pdf",
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF, letter),
    eg_pdf:set_page(PDF, 1),
    ug1(PDF),
   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?PDF_DIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).





