%%% ==========================================================================
%%% ep_block.eerl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:
%%%   File:         ep_block.erl
%%%   Description:  Display text blocks 
%%% @end

%%% ==========================================================================



-module (ep_block).

-export([block/3]).
-export([paragraph/5]).

% -export([parse_xml/1, normalise_xml/2, rich_text/1, widths/3, offsets/3]).
% -export([break_rich_text/3]).
    
-include("../include/eg.hrl").


%% ******************************************************************************
%% Block/3
%% ******************************************************************************


%% @doc Process content elements into PDf

block(PDF, Job, BlockMap) ->
    Text          = maps:get(text, BlockMap),
    {X, Y}        = ep_job:flip_y(Job, BlockMap),
    Xml = parse_xml(Text),
    inner_block(PDF, Xml, X, Y, BlockMap).


%% ******************************************************************************
%% Block/3 helpers
%% ******************************************************************************

%% @doc process a parsed XML block of content into a block of PDf text with a blank background

inner_block(PDF, [{raw, Xml}], X, Y, BlockMap) ->
   block2(PDF, [{xml, Xml}], X, Y, BlockMap),
   ok;

inner_block(PDF, [{xml, Xml}], X, Y, BlockMap) ->
   block2(PDF, [{xml, Xml}], X, Y, BlockMap),
   ok;

inner_block(PDF, [{xml, Xml} | T], X, Y, BlockMap) ->

   Height = block2(PDF, [{xml, Xml}], X, Y, BlockMap),
   inner_block(PDF, T, X, Y - Height, BlockMap).

    
block2(PDF, [{xml, Xml}], X, Y, BlockMap) ->
    TypeSpec = maps:get(typespec, BlockMap),
    ensure_fonts_are_loaded(PDF, TypeSpec),
    Norm = normalise_xml(Xml, TypeSpec),
    {Tag, RichText} = rich_text(Norm),

    %% Possibly hook all content display functions here

    case Tag of
      p -> paragraph(PDF, X, Y, RichText, BlockMap);
      _ -> io:format("ep_block:block2/5 Tag: ~p~n", [Tag])
    end.

%% ******************************************************************************
%% Display content elements
%% ******************************************************************************


paragraph(PDF, X, Y, RichText, BlockMap) ->
   Measure       = maps:get(measure, BlockMap),
   Margin        = maps:get(margin, BlockMap),
   Indent        = maps:get(indent, BlockMap), 
   NLines        = maps:get(nlines, BlockMap),
   Justification = maps:get(justification, BlockMap),
   Leading       = maps:get(leading, BlockMap),
    Widths = widths(Margin, Measure, NLines),

    Off = offsets(Indent, Margin, NLines),

    MaybeLines = break_rich_text(RichText, Justification, Widths),
    case MaybeLines of
	impossible ->
	    io:format("Cannot break line are widths ok~n");
	{Lines,_,_} ->
	    Code = ep_richText2pdf:richText2pdf(PDF, X, Y, Justification, 0, Lines, 
						Leading, Widths, Off),
	    eg_pdf:begin_text(PDF),
	    eg_pdf:append_stream(PDF, Code),
	    eg_pdf:end_text(PDF),
	    length(Lines) * Leading
    end.  


%% ******************************************************************************
%% Helpers
%% ******************************************************************************


-spec parse_xml(TaggedText :: list()) -> list().  % [{xml, Tag, [], XML]

parse_xml(TaggedText) ->
    eg_xml_lite:parse_all_forms(TaggedText).


-spec normalise_xml(Xml :: list(), TypeSpec :: list()) -> list().

normalise_xml(Xml, TypeSpec) ->
    eg_xml2richText:normalise_xml(Xml, TypeSpec).


-spec rich_text(NormalizedXml :: list()) -> tuple(). % {Tag, RichText}

rich_text(NormalisedXml) ->
    {Tag, _, RichText} = NormalisedXml,
    {Tag, RichText}.


widths(Margin, Len, NLines) ->
    Margins = 2 * Margin,
    [Len - Margins|lists:duplicate(NLines - 1, Len)].


-spec offsets(Indent :: integer(), 
              Margin :: integer(),
              NLines :: integer()) -> list().

offsets(Indent, Margin, NLines) ->
    [Indent|lists:duplicate(NLines-1, Margin)].



break_rich_text(RichText, Justification, Widths) ->
    ep_line_break:break_richText(RichText, { Justification, Widths}).    


ensure_fonts_are_loaded(PDF, {_, TypeSpec}) ->
    lists:foreach(fun({_,Face}) ->
			  FontHandler = eg_richText:fontFromFace(Face),
			  Font = FontHandler:fontName(),
			  eg_pdf:ensure_font_gets_loaded(PDF, Font)
		  end, TypeSpec).
