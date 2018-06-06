%%% ==========================================================================

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:
%%%   File:         ep_poly.erl
%%%   Description:  Display polygon 
%%% @end

%%% ==========================================================================

-module (ep_poly).

-export ([create/1]). 
-export([polygon/3]).

-include("../../include/ep.hrl").

-define(OUTLINE_WIDTH, 1).
-define(DASH, solid).
-define(OUTLINE_COLOR, black).
-define(FILL_COLOR, white).


%% ***********************************************************
%% Create polygon map 
%% ***********************************************************

%% @doc Create polygon map

-spec create(Vertices :: list()) -> map().

create(Vertices) ->
   #{ vertices      => Vertices 
    , outline       => ?OUTLINE_WIDTH
    , dash          => ?DASH
    , outline_color => ?OUTLINE_COLOR
    , fill_color    => ?FILL_COLOR
    }.


%% ***********************************************************
%% Polygon to pdf  
%% ***********************************************************

%% @doc Display polygon

-spec polygon(PDF        :: identifier(),
              Job        :: map(),
              PolygonMap :: map()) -> ok.


polygon(PDF, Job, PolygonMap) ->
    {PaperStock, PagePosition} = ep_job:stock_position(Job),
    Vertices       = maps:get(vertices, PolygonMap),
    Outline        = maps:get(outline, PolygonMap),
    Dash           = maps:get(dash, PolygonMap),
    OutlineColor   = maps:get(outline_color, PolygonMap),
    FillColor      = maps:get(fill_color, PolygonMap),
    Vertices1      = [ep_lib:impose_xy(Vertice, PagePosition, PaperStock) ||
                                        Vertice <- Vertices],
    eg_pdf:save_state(PDF),
    eg_pdf:poly(PDF, Vertices1),
    eg_pdf:set_line_width(PDF, Outline),
    eg_pdf:set_dash(PDF, Dash),
    eg_pdf:set_stroke_color(PDF, OutlineColor),
    eg_pdf:set_fill_color(PDF, FillColor),
    eg_pdf:path(PDF, close),
    eg_pdf:path(PDF, fill_stroke),
    eg_pdf:restore_state(PDF),
    ok.

    
