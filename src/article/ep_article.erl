%%% ==========================================================================
%%% ep_article.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:
%%%   File:         ep_article.erl
%%%   Description:  Typseset article that may spill across 
%%%                  one or more panels (beads)
%%%                  See Adobe reference manual Ver. 1.4 p
%%%                   
%%% @end

%%% ==========================================================================



-module (ep_article).

-export ([create/2, article/0, article/3]).
-export ([get_article/1, get_beads/1, get_typestyle/1, get_panel/1]).
-export([ update_article/2, update_beads/2, update_typestyle/2]).

-export([copy_specs/3, line_specs/3, get_nlines/3, rich_text/2]).
-export([lines/1, pdf_code/5, update_panel/3]).
-export([xml2lines/5]).

%% -compile(export_all).

-define(TYPESTYLE, report). 

-include("../../include/ep.hrl").



%% ***********************************************************
%% ***********************************************************
%% Create article map 
%% ***********************************************************
%% ***********************************************************r


%% @doc Create article map

-spec create(Article   :: list(),
             Beads     :: list()) -> map().          

create(Article, Beads) ->
   #{ article           => Article 
    , beads             => Beads 
    , typestyle         => ?TYPESTYLE
    }.


%% ***********************************************************
%% ***********************************************************
%% Paste article and beads
%% ***********************************************************
%% ***********************************************************

article() ->
   io:format("Article pasted!~n~n").

%% @doc Display article 

-spec article(PDF        :: identifier(),
              Job        :: map(),
              ArticleMap :: map()) -> ok. 

article(PDF, Job, ArticleMap) ->
    TypeStyle = get_typestyle(ArticleMap),
    Copy  = get_article(ArticleMap),
    XML   = ep_block:parse_xml(Copy), 
    Beads = get_beads(ArticleMap),
    [PanelMap | _MoreBeads] = Beads,
    ep_panel:panel(PDF, Job, PanelMap), 
    paste_panels(PDF, Job, TypeStyle, XML, Beads, []),
    ok.


%% *************************************************************
%% Get typestyle
%% *************************************************************

%% @doc Get typestyle

-spec get_typestyle(ArticleMap :: map()) -> atom().

get_typestyle(ArticleMap) ->
    maps:get(typestyle, ArticleMap).



%% *************************************************************
%% Get article 
%% *************************************************************

%% @doc Get article 

-spec get_article(ArticleMap :: map()) -> list().

get_article(ArticleMap) ->
    maps:get(article, ArticleMap).


%% *************************************************************
%% Get beads 
%% *************************************************************

%% @doc Get beads 

-spec get_beads( ArticleMap :: map()) -> list().

get_beads(ArticleMap) ->
    maps:get(beads, ArticleMap).



%% ***********************************************************
%% ***********************************************************
%% Paste Panels 
%% ***********************************************************
%% ***********************************************************

paste_panels(_PDF, _Job, _TypeStyle, [], _Beads, _SpillOver) ->
   io:format("Out of copy!~n~n"),
   article();

paste_panels(_PDF, _Job, _TypeStyle, _XML, [], _SpillOver) ->
   io:format("Out of beads!~n~n"),
   article();

paste_panels(PDF, Job, TypeStyle, XML, Beads, Spill) ->
   [PanelMap | MoreBeads] = Beads,
                          io:format("PanelMap: ~p~n", [PanelMap]),
                          Name = ep_panel:get_panel_name(PanelMap),
                          io:format("Panel - Name: ~p~n~n", [Name]),
   [{xml, Xml} | MoreXML] = XML,
   Tag = element(1, Xml), 
   Lines = xml2lines(TypeStyle, Tag, Xml, PanelMap, Spill),
   Result = measure_copy(PDF, Job, TypeStyle, Tag, Lines, PanelMap),
   case Result of
      fits   -> % Paste and get next Xml 
                PanelMap1 = paste_lines(PDF, TypeStyle, Tag, Lines, PanelMap),
                [_Xml | MoreXML] = XML ,
                Beads1 = [PanelMap1 | MoreBeads], 
                paste_panels(PDF, Job, TypeStyle, MoreXML, Beads1, []);

      spills -> % Split lines, paste what lines  we can, get new bead, 

                    io:format("~n~n ################  spills ############~n~n"),                

                % Paste all lines that fit 
                NLines          = get_nlines(TypeStyle, Tag, PanelMap),
                Pastable        = NLines - length(Lines),
                    io:format("PanelMap: ~p~n", [PanelMap]),
                    io:format("Pastable: ~p~n", [Pastable]),
                    io:format("Lines: ~p~n", [Lines]),
                    io:format("Length Lines: ~p~n", [length(Lines)]),
                {Paste, Spill1} = lists:split(Pastable, Lines),
                    io:format("Paste: ~p~n", [Paste]),
                    io:format("Spill1: ~p~n", [Spill1]),
                    paste_lines(PDF, TypeStyle, Tag, Paste, PanelMap),

                % Paste new bead, 
                [PanelMap1 | _MoreBeads] = MoreBeads,
                    io:format("New PanelMap1: ~p~n", [PanelMap1]),
                ep_panel:panel(PDF, Job, PanelMap1), 

                % Paste spill lines 
               
                io:format("XML: ~p~n", [XML]),
                io:format("MoreXML: ~p~n", [MoreXML]),
                io:format("Beads: ~p~n", [Beads]),
                io:format("MoreBeads: ~p~n", [MoreBeads]),

                paste_panels(PDF, Job, TypeStyle, MoreXML, MoreBeads, Spill1 )
   end.   


   
%% ********************************************************************
%% xml2lines/5
%% ********************************************************************

%% @doc Given Xml, generate rich text for new panel; preppend spillover   
%%      rich text from previous panel, and break rich text into lines
%%      to fit 

-spec xml2lines(TypeStyle :: atom(),
                Tag       :: atom(),
                Xml       :: list(),
                PanelMap  :: map(),
                Spill     :: list()) -> list().

xml2lines(TypeStyle, Tag, Xml, PanelMap, []) ->
   {Widths, _Offsets}         = copy_specs(TypeStyle, Tag, PanelMap),
   Justify                    = ep_typestyle:report_justify(Tag),
   RichText                   = rich_text(TypeStyle, Xml),
   MaybeLines                 = ep_block:break_rich_text(RichText, Justify, Widths),
   Lines                      = lines(MaybeLines);

xml2lines(TypeStyle, Tag, Xml, PanelMap, Spill) ->
   {Widths, _Offsets}         = copy_specs(TypeStyle, Tag, PanelMap),
   Justify                    = ep_typestyle:report_justify(Tag),
   RichText                   = rich_text(TypeStyle, Xml),
   RichText1                  = [Spill | RichText],
   MaybeLines                 = ep_block:break_rich_text(RichText1, Justify, Widths),
   Lines                      = lines(MaybeLines),
   Lines.

   

%% ********************************************************************
%% paste_copy/5
%% ********************************************************************


measure_copy(PDF, Job, TypeStyle, Tag, Lines, PanelMap) ->
   NLines      = get_nlines(TypeStyle, Tag, PanelMap),
   Vacant      = NLines - length(Lines),
   io:format("Vacant: ~p~n", [Vacant]),
   case Vacant > 1 of
      true  -> fits;
      false -> spills 
   end.





%% ********************************************************************
%% paste_copy/5 helpers
%% ********************************************************************


get_nlines(TypeStyle, Tag, PanelMap) ->
    ep_panel:get_nlines(TypeStyle, Tag, PanelMap).



paste_lines(PDF, TypeStyle, Tag, [], PanelMap) -> 
   [];

%% @doc Paste lines and update panel map 

paste_lines(PDF, TypeStyle, Tag, Lines, PanelMap) -> 
   io:format("%%%%%%% Entering paste_lines/5 - Tag: ~p~n", [Tag]),            
   io:format("Lines: ~p~n", [length(Lines)]),
   Code      = pdf_code(PDF, TypeStyle, Tag, Lines, PanelMap),
   ok        = paste(PDF, Code),
       Leading           = ep_typestyle:report_leading(Tag),
       Consumed = Leading * length(Lines),
       NextLine = ep_panel:get_next_line(PanelMap) + Consumed,
       Name = ep_panel:get_panel_name(PanelMap),
       io:format("%%%%%%% NextLine ~p Pasted: ~p~n~n", [NextLine, Name]),            
   update_panel(Tag, Lines, PanelMap).


   




%% ***********************************************************
%% Lines
%% ***********************************************************

lines(impossible)    ->
  io:format("Cannot break line; are widths ok?~n");

lines({Lines, _, _}) ->
   Lines.

%% ***********************************************************
%% Paste lines helpers
%% ***********************************************************

pdf_code(PDF, TypeStyle, Tag, Lines, PanelMap) ->
       Name = ep_panel:get_panel_name(PanelMap),
       io:format("pdf_code/5 - PanelMap: ~p~n~n", [Name]),
   {TextX, TextY}    = ep_panel:get_text_position(PanelMap),
   Justify           = ep_typestyle:report_justify(Tag),
   Leading           = ep_typestyle:report_leading(Tag),
   {Widths, Offsets} = copy_specs(TypeStyle, Tag, PanelMap),
   ep_richText2pdf:richText2pdf(PDF, TextX, TextY, Justify, 0, Lines, Leading, Widths, Offsets).

paste(PDF, Code) ->
  eg_pdf:begin_text(PDF),
  eg_pdf:append_stream(PDF, Code),
  eg_pdf:end_text(PDF),
  ok.


%% ***********************************************************
%% Update panel 
%% ***********************************************************

update_panel(Tag, Lines, PanelMap) ->
   Leading  = ep_typestyle:report_leading(Tag),
   Consumed = Leading * length(Lines),
   NextLine = ep_panel:get_next_line(PanelMap) + Consumed,
   ep_panel:update_next_line(NextLine, PanelMap).


%% ***********************************************************
%% Chope richText 
%% ***********************************************************

chop_richText(RichText, Lines, NLines) ->
   io:format("copy_richText/3 - RichText: ~p~n~n", [RichText]),
   Counts = [size(Line) || Line <- Lines],
   CountList = lists:sublist(Counts, NLines),
   Total     = lists:sum(CountList),
   lists:nthtail(Total, RichText).


            




rich_text(TypeStyle, Xml) ->
   Tag = element(1, Xml),
   FontMap         = ep_typespec:get_fontmap(TypeStyle, Tag),
   Norm            = ep_block:normalise_xml(Xml, FontMap),
   {_, RichText}   = ep_block:rich_text(Norm),
   RichText.

copy_specs(TypeStyle, Tag, PanelMap) ->
   NLines          = get_nlines(TypeStyle, Tag, PanelMap),
   Indent          = ep_typestyle:report_indent(Tag),
   Measure         = ep_panel:get_measure(PanelMap),
   Margin          = ep_panel:get_margin(PanelMap),
   Widths          = ep_block:widths(Indent, Measure, NLines),
   Offsets         = ep_block:offsets(Indent, Margin, NLines),
   {Widths, Offsets}.
  

line_specs(TypeStyle, Tag, PanelMap) -> 
   Justify = ep_typespec:get_justify(TypeStyle, Tag),
   Indent  = ep_typespec:get_indent(TypeStyle, Tag),
   Measure = ep_panel:get_measure(PanelMap),
   {Justify, Indent, Measure}.



%% *************************************************************
%% Get current panel 
%% *************************************************************

%% @doc Get current panel 

-spec get_panel( ArticleMap :: map()) -> tuple().

get_panel(ArticleMap) ->
    Beads = get_beads(ArticleMap),
    [Panel | _Remaining] = Beads,
    Panel.
    



%% *************************************************************
%% Update article 
%% *************************************************************

%% @doc Update article 

-spec update_article(Article :: list(),
                     ArticleMap :: map()) -> map().

update_article(Article, ArticleMap) ->
    maps:put(article, Article, ArticleMap).


%% *************************************************************
%% Update beads 
%% *************************************************************

%% @doc Update beads 

-spec update_beads(Beads :: list(),
                   ArticleMap :: map()) -> map().

update_beads(Beads, ArticleMap) ->
    maps:put(beads, Beads, ArticleMap).


%% *************************************************************
%% Update typestyle
%% *************************************************************

%% @doc Update typestyle

-spec update_typestyle(TypeStyle  :: atom(),
                       ArticleMap :: map()) -> map().

update_typestyle(TypeStyle, ArticleMap) ->
    maps:get(typestyle, TypeStyle, ArticleMap).



