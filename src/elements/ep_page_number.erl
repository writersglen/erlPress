%%==========================================================================
%% ep_page_number.erl

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
%% File:       ep_page_number.erl
%% Description: 
%%    Position page number 
%% @end
%%==========================================================================

-module (ep_page_number).

-export ([create/2]). 

-export ([position/1, text/1, font/1, font_size/1, format/1]).
-export([update_position/2, update_text/2, update_font/2]).
-export([update_font_size/2]).
-export([page_number/3]).

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

-spec create(From :: tuple(),
             Text :: string()) -> map().

create(From, Text) ->
   #{ from         =>  From
    , text          => Text 
    , font          => ?DEFAULT_FONT 
    , font_size     => ?DEFAULT_FONT_SIZE
    , format        => ?DEFAULT_FORMAT
    }.


%% ***********************************************************
%% Get text attributes 
%% ***********************************************************


%% @doc Return position 

-spec position(PageNumberMap :: map()) -> tuple().

position(PageNumberMap) ->
   maps:get(position, PageNumberMap).


%% @doc Return text 

-spec text(PageNumberMap :: map()) -> tuple().

text(PageNumberMap) ->
   maps:get(text, PageNumberMap).

%% @doc Return font 

-spec font(PageNumberMap :: map()) -> tuple().

font(PageNumberMap) ->
   maps:get(font, PageNumberMap).


%% @doc Return font size

-spec font_size(PageNumberMap :: map()) -> tuple().

font_size(PageNumberMap) ->
   maps:get(font_size, PageNumberMap).


%% @doc Return format 

-spec format(PageNumberMap :: map()) -> tuple().

format(PageNumberMap) ->
   maps:get(format, PageNumberMap).


%% ***********************************************************
%% Update text parameters 
%% ***********************************************************


%% @doc Update position 

-spec update_position(From          :: tuple(),
                      PageNumberMap :: map()) -> tuple().

update_position(From, PageNumberMap) ->
    maps:put(position, From, PageNumberMap).


%% @doc Update text 

-spec update_text(Text          :: string(),
                  PageNumberMap :: map()) -> tuple().

update_text(Text, PageNumberMap) ->
    maps:put(text, Text, PageNumberMap).


%% @doc Update font 

-spec update_font(Font          :: string(),
                  PageNumberMap :: map()) -> tuple().

update_font(Font, PageNumberMap) ->
    maps:put(font, Font, PageNumberMap).


%% @doc Update font size

-spec update_font_size(FontSize :: integer(),
                  PageNumberMap :: map()) -> tuple().

update_font_size(FontSize, PageNumberMap) ->
    maps:put(font_size, FontSize, PageNumberMap).


%% ***********************************************************
%% Page number to PDF  
%% ***********************************************************



page_number(PDF, PageMap, PageNumberMap) ->
    PaperStock   = maps:get(paper_stock, PageMap),
    PageNumber   = maps:get(page_number, PageMap),
    PageNumber1  = integer_to_list(PageNumber),
    [PageXY]     = maps:get(page_xy, PageMap),
    From         = maps:get(from, PageNumberMap),
    Text         = maps:get(text, PageNumberMap),
    From1        = ep_lib:impose_xy(From, PageXY, PaperStock), 
    Text1        = Text ++ PageNumber1,
    {X, Y}       = From1,
    eg_pdf:save_state(PDF),
    eg_pdf:begin_text(PDF),
    eg_pdf:set_font(PDF, ?DEFAULT_FONT, ?DEFAULT_FONT_SIZE),
    eg_pdf:set_text_pos(PDF, X, Y),
    eg_pdf:text(PDF, Text1),
    eg_pdf:end_text(PDF),
    eg_pdf:restore_state(PDF). 




