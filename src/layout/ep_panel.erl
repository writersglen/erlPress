%%% ==========================================================================
%%% ep_panel.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:
%%%   File:         ep_panel.erl
%%%   Description:  Create panel map 
%%%
%%%                 Panels wrap boxes (ep_box.erl) in tupleis to facilitate 
%%%                 page design and layout functions
%%%
%%%                 Panels "contain" content elements, either text or images.
%%%
%%%                 Sets of panels are composed into a "page grids" to
%%%                  organize content and direct "eye path." 
%%%
%%%                 Position = {X,Y}
%%%                 X = upper left X coordinate of panel in points  
%%%                 Y = upper left Y coordinate of panel in points
%%%                 Size = {Width, Height}
%%%                 Width = width of box in points
%%%                 Height = height of box in pnts 
%%%                 id = {page number, panel index, name}
%%%
%%%   IMPORTANT NOTE:
%%%
%%%                 PDF assumes that 0, 0 XY coordinates are at lower-left
%%%                 This seems unnatural for folks who read top-to_bottom
%%%                 So for convenience, we'll assume that 0, 0 XY is
%%%                 at upper-left of the panel.  This means that y will 
%%%                 have to be inverted relative to paper stock height when we 
%%%                 print.
%%% @end
%%% ==========================================================================



-module (ep_panel).

-export([create/3, panel/3]).
-export([get_id/1, get_page_number/1, get_panel_index/1, get_panel_name/1]).
-export([get_position/1, get_text_position/1, get_size/1, get_radius/1, get_next_line/1]).
-export([get_available/1, get_nlines/3, get_border/1]).
-export([get_border_style/1, get_border_color/1, get_margin/1]).
-export([get_measure/1, get_indent/1]).
-export([get_jump_prompt/1]).

-export([update_id/2, update_position/2, update_size/2, update_radius/2]).
-export([update_next_line/2]).
-export([update_border/2, update_border_style/2, update_border_color/2]).
-export([update_background_color/2, update_margin/2, update_jump_prompt/2]).
-export([default_panel/0]).


-define(ID, {1, 1, top}).
-define(POSITION, {72, 72}).
-define(SIZE, {350, 500}).
-define(RADIUS, 10).
-define(NEXT_LINE, 72).
-define(BORDER, 1).
-define(BORDER_STYLE, solid).
-define(BORDER_COLOR, black).
-define(BACKGROUND_COLOR, eg_pdf_op:color(gainsboro)).
-define(MARGIN, 20).
-define(INDENT, 30).
-define(TYPESTYLE, report).
-define(JUMP_PROMPT, "No jump").



% -compile(export_all).

-include("../../include/ep.hrl").

%% ***********************************************************
%% Create panel map 
%% ***********************************************************

%% @doc Create panel map

-spec create(ID       :: tuple(),
             Position :: tuple(),
             Size     :: tuple()) -> map().

create(ID, Position, Size) ->
    #{ id                => ID
     , position          => Position 
     , size              => Size
     , radius            => ?RADIUS
     , next_line         => init_next_line(Position) 
     , border            => ?BORDER
     , border_style      => ?BORDER_STYLE
     , border_color      => ?BORDER_COLOR
     , background_color  => ?BACKGROUND_COLOR
     , margin            => ?MARGIN
     , indent            => ?INDENT
     , jump_prompt       => ?JUMP_PROMPT
     }. 

init_next_line(Position) ->
   {_X, Y} = Position,
   Y.


%% ***********************************************************
%% Display panel 
%% ***********************************************************

panel(PDF, Job, PanelMap) ->
   PanelName = get_panel_name(PanelMap),
   io:format("~nPasting panel -  Name: ~p~n", [PanelName]),
   Position        = ep_job:flip_box(Job, PanelMap),
   Size            = maps:get(size, PanelMap),
   
   Radius          = maps:get(radius, PanelMap),
   Border          = maps:get(border, PanelMap),
   BorderStyle     = maps:get(border_style, PanelMap),
   BorderColor     = maps:get(border_color, PanelMap),
   BackgroundColor = maps:get(background_color, PanelMap),
   eg_pdf:set_line_width(PDF, Border),
   eg_pdf:set_dash(PDF, BorderStyle),
   eg_pdf:set_stroke_color(PDF, BorderColor),
   eg_pdf:set_fill_color(PDF, BackgroundColor),
   eg_pdf:round_rect(PDF, Position, Size, Radius),
   eg_pdf:path(PDF, fill_stroke),
   io:format("Leaving panel/3 - Panel Name: ~p~n~n", [PanelName]),
   ok.


%% ***********************************************************
%% Get panel id 
%% ***********************************************************

%% @doc Get panel id

-spec get_id(PanelMap :: map()) -> tuple().

get_id(PanelMap) ->
   maps:get(id, PanelMap).


%% ***********************************************************
%% Get page number 
%% ***********************************************************

%% @doc Get page number

-spec get_page_number(PanelMap :: map()) -> tuple().

get_page_number(PanelMap) ->
   PageNumber = maps:get(id, PanelMap),
   element(1, PageNumber).


%% ***********************************************************
%% Get panel index 
%% ***********************************************************

%% @doc Get panel index

-spec get_panel_index(PanelMap :: map()) -> tuple().

get_panel_index(PanelMap) ->
   PanelIndex= maps:get(id, PanelMap),
   element(2, PanelIndex).


%% ***********************************************************
%% Get panel name 
%% ***********************************************************

%% @doc Get panel name  

-spec get_panel_name(PanelMap :: map()) -> tuple().

get_panel_name(PanelMap) ->
   PanelName= maps:get(id, PanelMap),
   element(3, PanelName).


%% ***********************************************************
%% Get position 
%% ***********************************************************

%% @doc Get position

-spec get_position(PanelMap :: map()) -> tuple().

get_position(PanelMap) ->
   maps:get(position, PanelMap).


%% ***********************************************************
%% Get text position 
%% ***********************************************************

%% @doc Get text position

-spec get_text_position(PanelMap :: map()) -> tuple().

get_text_position(PanelMap) ->
   {X, _Y} = maps:get(position, PanelMap),
   NextLine = maps:get(next_line, PanelMap),
   ep_lib:impose_text({X, NextLine}, letter).


%% ***********************************************************
%% Get size 
%% ***********************************************************

%% @doc Get size

-spec get_size(PanelMap :: map()) -> tuple().

get_size(PanelMap) ->
   maps:get(size, PanelMap).


%% ***********************************************************
%% Get radius 
%% ***********************************************************

%% @doc Get radius

-spec get_radius(PanelMap :: map()) -> tuple().

get_radius(PanelMap) ->
   maps:get(radius, PanelMap).


%% ***********************************************************
%% Get next line 
%% ***********************************************************

%% @doc Get next line

-spec get_next_line(PanelMap :: map()) -> integer().

get_next_line(PanelMap) ->
   maps:get(next_line, PanelMap).


%% ***********************************************************
%% Get available 
%% ***********************************************************

%% @doc Get available 

-spec get_available(PanelMap :: map()) -> integer().

get_available(PanelMap) ->
   {_X, Y} = maps:get(position, PanelMap),
   {_Width, Height} = maps:get(size, PanelMap),
   NextLine         = maps:get(next_line, PanelMap),
   (Y + Height) - NextLine.

   
%% ***********************************************************
%% Get NLines 
%% ***********************************************************

%% @doc Get NLines 

-spec get_nlines(TypeStyle  :: atom(),
                 Tag        :: atom(),
                  PanelMap  :: map()) -> integer().

get_nlines(TypeStyle, Tag, PanelMap) ->
   Available = get_available(PanelMap),
   Leading   = ep_typespec:get_leading(TypeStyle, Tag),
   Available div Leading.


   


%% ***********************************************************
%% ***********************************************************
%% Get border 
%% ***********************************************************

%% @doc Get border

-spec get_border(PanelMap :: map()) -> tuple().

get_border(PanelMap) ->
   maps:get(border, PanelMap).


%% ***********************************************************
%% Get border style 
%% ***********************************************************

%% @doc Get border style

-spec get_border_style(PanelMap :: map()) -> tuple().

get_border_style(PanelMap) ->
   maps:get(border_style, PanelMap).


%% ***********************************************************
%% Get border color 
%% ***********************************************************

%% @doc Get border color

-spec get_border_color(PanelMap :: map()) -> tuple().

get_border_color(PanelMap) ->
   maps:get(border_color, PanelMap).


%% ***********************************************************
%% Get margin 
%% ***********************************************************

%% @doc Get margin

-spec get_margin(PanelMap :: map()) -> tuple().

get_margin(PanelMap) ->
   maps:get(margin, PanelMap).


%% ***********************************************************
%% Get measure 
%% ***********************************************************

%% @doc Get measure 

-spec get_measure(PanelMap :: map()) -> integer().

get_measure(PanelMap) ->
   {Width, _Height} =maps:get(size, PanelMap),
   Margin  = maps:get(margin, PanelMap),
   Width - (Margin * 2).


%% ***********************************************************
%% Get indent 
%% ***********************************************************

%% @doc Get indent 

-spec get_indent(PanelMap :: map()) -> integer().

get_indent(PanelMap) ->
   maps:get(indent, PanelMap).


%% ***********************************************************





%% ***********************************************************
%% Get jump prompt 
%% ***********************************************************

%% @doc Get jump prompt 

-spec get_jump_prompt(PanelMap :: map()) -> tuple().

get_jump_prompt(PanelMap) ->
   maps:get(jump_prompt, PanelMap).


%% ***********************************************************
%% Update id 
%% ***********************************************************

%% @doc Update panel id

-spec update_id(Id       :: tuple(),
                PanelMap :: map()) -> map().

update_id(ID, PanelMap) ->
   maps:put(id, ID, PanelMap).


%% ***********************************************************
%% Update 
%% ***********************************************************

%% @doc Update position

-spec update_position(Position       :: tuple(),
                      PanelMap :: map()) -> map().

update_position(Position, PanelMap) ->
   maps:put(id, Position, PanelMap).


%% ***********************************************************
%% Update size 
%% ***********************************************************

%% @doc Update size

-spec update_size(Size     :: tuple(),
                  PanelMap :: map()) -> map().

update_size(Size, PanelMap) ->
   maps:put(size, Size, PanelMap).


%% ***********************************************************
%% Update radius 
%% ***********************************************************

%% @doc Update radius

-spec update_radius(Radius   :: tuple(),
                    PanelMap :: map()) -> map().

update_radius(Radius, PanelMap) ->
   maps:put(radius, Radius, PanelMap).


%% ***********************************************************
%% Update next line 
%% ***********************************************************

%% @doc Update next line 

-spec update_next_line(NextLine  :: tuple(),
                       PanelMap  :: map()) -> map().

update_next_line(NextLine, PanelMap) ->
   maps:put(next_line, NextLine, PanelMap).


%% ***********************************************************
%% Update border 
%% ***********************************************************

%% @doc Update border

-spec update_border(Border   :: tuple(),
                    PanelMap :: map()) -> map().

update_border(Border, PanelMap) ->
   maps:put(border, Border, PanelMap).


%% ***********************************************************
%% Update border style 
%% ***********************************************************

%% @doc Update border style

-spec update_border_style(BorderStyle  :: tuple(),
                          PanelMap     :: map()) -> map().

update_border_style(BorderStyle, PanelMap) ->
   maps:put(border_style, BorderStyle, PanelMap).


%% ***********************************************************
%% Update border color 
%% ***********************************************************

%% @doc Update border color

-spec update_border_color(BorderColor  :: tuple(),
                          PanelMap     :: map()) -> map().

update_border_color(BorderColor, PanelMap) ->
   maps:put(border_color, BorderColor, PanelMap).


%% ***********************************************************
%% Update background color 
%% ***********************************************************

%% @doc Update background color

-spec update_background_color(BackgroundColor  :: tuple(),
                              PanelMap         :: map()) -> map().

update_background_color(BackgroundColor, PanelMap) ->
   maps:put(background_color, BackgroundColor, PanelMap).


%% ***********************************************************
%% Updatte margin 
%% ***********************************************************

%% @doc Update margin

-spec update_margin(Margin   :: tuple(),
                    PanelMap :: map()) -> map().

update_margin(Margin, PanelMap) ->
   maps:put(margin, Margin, PanelMap).


%% ***********************************************************
%% Update jump prompt 
%% ***********************************************************

%% @doc Update jump prompt 

-spec update_jump_prompt(JumpPrompt  :: tuple(),
                         PanelMap    :: map()) -> map().

update_jump_prompt(JumpPrompt, PanelMap) ->
   maps:put(jump_prompt, JumpPrompt, PanelMap).


%% ***********************************************************
%% Default panel 
%% ***********************************************************

%% @doc Create default panel 

-spec default_panel() -> map().

default_panel() ->
   Id   = {1, 1, top},
   Position = {72, 72},
   Size     = {450, 300},
   create(Id, Position, Size).

