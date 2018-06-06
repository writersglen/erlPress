%%% ==========================================================================
%%% ep_text_block.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:
%%%   File:         ep_text_block.erl
%%%   Description:  Typseset paragraphs 
%%% @end

%%% ==========================================================================



-module (ep_text_block).

-export ([create/3, text_block/3]).

% -compile(export_all).

-define(RADIUS, 10).
-define(MARGIN, 10).
-define(BORDER, 1).
-define(BORDER_STYLE, solid).
-define(BORDER_COLOR, black).
-define(BACKGROUND_COLOR, eg_pdf_op:color(gainsboro)).
-define(FONT_SIZE, 14).
-define(LEADING, ?FONT_SIZE + (?FONT_SIZE div 2)).
-define(TYPESPEC, ep_typespec:default(14)).
-define(NLINES, 10).
-define(JUSTIFICATION, justified).   

%% NOTE: See XXXX for color selections
%%       Justification options: justified, centered, ragged, ragged_left

-include("../../include/ep.hrl").

%% ***********************************************************
%% Text block
%% ***********************************************************



%% @doc Create text block map

-spec create(Text      :: list(),
             Position  :: tuple(),
             Measure   :: integer()) -> map().

create(Text, Position, Measure) ->
   #{ text              => Text 
    , position          => Position
    , measure           => Measure 
    , nlines            => ?NLINES 
    , leading           => ?LEADING 
    , radius            => ?RADIUS
    , border            => ?BORDER
    , border_style      => ?BORDER_STYLE
    , border_color      => ?BORDER_COLOR
    , background_color  => ?BACKGROUND_COLOR 
    , margin            => ?MARGIN
    , typespec          => ?TYPESPEC 
    , justification     => ?JUSTIFICATION 
    }.

%% NOTE: If text is too long, it will spill over bottom
%%       of box 


%% ***********************************************************
%% Text Block 
%% ***********************************************************

%% @doc Display text block

-spec text_block(PDF      :: identifier(),
                 Job      :: map(),
                 BlockMap :: map()) -> ok. 

text_block(PDF, Job, BlockMap) ->
    {X, Y}          = maps:get(position, BlockMap),
    {X1, Y1}        = ep_job:flip_y(Job, BlockMap),
    NLines           = maps:get(nlines, BlockMap),
    Leading          = maps:get(leading, BlockMap),
    Measure          = maps:get(measure, BlockMap),
    Margin           = maps:get(margin, BlockMap),
    BoxWidth         = Measure + (2 * Margin),
    TextHeight       = Leading * NLines, 
    BoxHeight        = TextHeight + (2 * Margin),
    BoxSize          = {BoxWidth, BoxHeight},
    Radius           = maps:get(radius, BlockMap),
    Text             = maps:get(text, BlockMap),
    Justification    = maps:get(justification, BlockMap),
    TypeSpec         = maps:get(typespec, BlockMap),
    BoxMap           = ep_round_rect:create({X, Y + BoxHeight}, 
                                             BoxSize, Radius),
    BoxMap1          = inherit_values(BlockMap, BoxMap),
    % Draw box
    ep_round_rect:round_rect(PDF, Job, BoxMap1),
    % Draw text
    BackgroundColor   = maps:get(background_color, BlockMap),
     ep_block:block(PDF, BackgroundColor, Text,  X1, Y1, Measure, 10, 
                          Leading, NLines, Justification, TypeSpec),
    ok.

%% @doc Transfer values from BlockMap to BoxMap

-spec inherit_values(BlockMap :: map(),
                     BoxMap   :: map()) -> map().

inherit_values(BlockMap, BoxMap) ->
    Border           = maps:get(border, BlockMap),
    BorderStyle      = maps:get(border_style, BlockMap),
    BorderColor      = maps:get(border_color, BlockMap),
    BackgroundColor  = maps:get(background_color, BlockMap),
    Margin           = maps:get(margin, BlockMap),
    BoxMap1          = maps:put(border, Border, BoxMap),
    BoxMap2          = maps:put(border_style, BorderStyle, BoxMap1),
    BoxMap3          = maps:put(border_color, BorderColor, BoxMap2),
    BoxMap4          = maps:put(background_color, BackgroundColor, BoxMap3),
    maps:put(margin, Margin, BoxMap4).

