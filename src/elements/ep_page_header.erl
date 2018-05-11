%%==========================================================================
%% ep_page_header.erl

%% @copyright  2018 Lloyd R. Prentice
%% @author     Lloyd R. Prentice
%% @doc
%% License: 
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to permit
%% persons to whom the Software is furnished to do so, subject to the
%% following conditions:
%% 
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
%% USE OR OTHER DEALINGS IN THE SOFTWARE.
%% File:       ep_grid.erl
%%
%% File:       ep_page_header.erl
%% Description: 
%%    Position page header 
%% @end
%%==========================================================================

-module (ep_page_header).

-export ([create/3]). 

-export ([position/1, text/1, font/1, font_size/1, format/1]).
-export([update_position/3, update_text/2, update_font/2]).
-export([update_font_size/2]).
-export([page_header/2]).

% -compile(export_all).

-include("../../include/ep.hrl").

-define(DEFAULT_RENDERING_MODE, fill_then_stroke).
-define(DEFAULT_FORMAT, letter).
-define(DEFAULT_FONT, "Times-Roman").
-define(DEFAULT_FONT_SIZE, 14).

%% Rendering mode options: file, stroke, fill_then_stroke
%% Font options: eg_pdf:inBuiltFonts/0

%% Examples:  1, page 1, Page 1

%% ***********************************************************
%% Create circle map 
%% ***********************************************************

%% @doc Create 

-spec create(X    :: integer(),
             Y    :: integer(),
             Text :: string()) -> map().

create(X, Y, Text) ->
   #{ position      => {X, Y}
    , text          => Text 
    , font          => ?DEFAULT_FONT 
    , font_size     => ?DEFAULT_FONT_SIZE
    , format        => ?DEFAULT_FORMAT
    }.


%% ***********************************************************
%% Get text attributes 
%% ***********************************************************


%% @doc Return position 

-spec position(PageNoMap :: map()) -> tuple().

position(PageNoMap) ->
   maps:get(position, PageNoMap).


%% @doc Return text 

-spec text(PageNoMap :: map()) -> tuple().

text(PageNoMap) ->
   maps:get(text, PageNoMap).

%% @doc Return font 

-spec font(PageNoMap :: map()) -> tuple().

font(PageNoMap) ->
   maps:get(font, PageNoMap).


%% @doc Return font size

-spec font_size(PageNoMap :: map()) -> tuple().

font_size(PageNoMap) ->
   maps:get(font_size, PageNoMap).


%% @doc Return format 

-spec format(PageNoMap :: map()) -> tuple().

format(PageNoMap) ->
   maps:get(format, PageNoMap).


%% ***********************************************************
%% Update text parameters 
%% ***********************************************************


%% @doc Update position 

-spec update_position(X :: integer(),
                      Y :: integer(),
                      PageNoMap :: map()) -> tuple().

update_position(X, Y, PageNoMap) ->
    maps:put(position, {X, Y}, PageNoMap).


%% @doc Update text 

-spec update_text(Text :: string(),
                  PageNoMap :: map()) -> tuple().

update_text(Text, PageNoMap) ->
    maps:put(text, Text, PageNoMap).


%% @doc Update font 

-spec update_font(Font :: string(),
                  PageNoMap :: map()) -> tuple().

update_font(Font, PageNoMap) ->
    maps:put(font, Font, PageNoMap).


%% @doc Update font size

-spec update_font_size(FontSize :: integer(),
                  PageNoMap :: map()) -> tuple().

update_font_size(FontSize, PageNoMap) ->
    maps:put(font_size, FontSize, PageNoMap).


%% ***********************************************************
%% Page header to PDF  
%% ***********************************************************


page_header(PDF, PageNoMap) ->
    {X, Y}   = position(PageNoMap),
    Format   = format(PageNoMap),
    Y1       = ep_lib:v_flip(Y, Format),
    Text     = text(PageNoMap),
    eg_pdf:save_state(PDF),
    eg_pdf:begin_text(PDF),
    eg_pdf:set_font(PDF, ?DEFAULT_FONT, ?DEFAULT_FONT_SIZE),
    eg_pdf:set_text_pos(PDF, X, Y1),
    eg_pdf:text(PDF, Text),
    eg_pdf:end_text(PDF),
    eg_pdf:restore_state(PDF). 




