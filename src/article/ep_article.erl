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

% -export ([create/2, article/3]).
% -export ([update_article/2, update_beads/2, update_typestyle/2]).

-compile(export_all).

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

%% @doc We're done!

article() ->
   io:format("Article pasted!~n~n").


%% @doc Display article 

-spec article(PDF        :: identifier(),
              Job        :: map(),
              ArticleMap :: map()) -> ok. 

article(PDF, Job, ArticleMap) ->
    TypeStyle = get_article_typestyle(ArticleMap),
    Copy      = get_article(ArticleMap),
    XML       = ep_block:parse(Copy), 
    Beads     = get_beads(ArticleMap),
    PanelMap  = get_panelmap(Beads),
    paste_panel(PDF, Job, PanelMap),
    Beads1    = [ep_panel:update_typestyle(TypeStyle, Bead) || Bead <- Beads],
    paste_panels(PDF, Job, XML, Beads1),
    ok.


%% ***********************************************************
%% ***********************************************************
%% Artivle helpers 
%% ***********************************************************
%% ***********************************************************


%% *************************************************************
%% Get article typestyle
%% *************************************************************

%% @doc Get article typestyle

-spec get_article_typestyle(ArticleMap :: map()) -> atom().

get_article_typestyle(ArticleMap) ->
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



%% ***********************************************************
%% ***********************************************************
%% Paste up article 
%% ***********************************************************
%% ***********************************************************


%% @doc Paste up article

-spec paste_panels(PDF :: identifier(),
                   Job :: map(),
                   XML       :: list(),
                   Beads     :: list()) -> none().

paste_panels(_PDF, _Job, [], _Beads) ->
   io:format("Out of copy!~n~n"),
   article();

paste_panels(_PDF, _Job, _XML, []) ->
   io:format("Out of beads!~n~n"),
   article();

paste_panels(PDF, Job, XML, Beads) ->
   io:format("!!!!!!!!!!!! Entering paste_panels/4~n~n"),
   Tag               = get_tag(XML),
   Lines             = get_lines(XML, Beads),
   PanelMap          = get_panelmap(Beads),
   Result            = measure_lines(Tag, Lines, PanelMap),
   {XML1, Beads1}    = paste_lines(PDF, Job, Tag, Lines, XML, Beads, Result),
   io:format("!!!!!!!!!!!! Length XML: ~p~n", [length(XML)]),
   io:format("!!!!!!!!!!!! recurse~n~n"),
   paste_panels(PDF, Job, XML1, Beads1).


%% ***********************************************************************
%% paste_panel/4 helpers
%% ***********************************************************************



paste_panel(PDF, Job, PanelMap) ->
    ep_panel:panel(PDF, Job, PanelMap). 

get_lines(XML, Beads) ->
   TypeStyle          = get_typestyle(Beads),
   Tag                = get_tag(XML),
   PanelMap           = get_panelmap(Beads),
   NLines             = get_nlines(TypeStyle, Tag, PanelMap),
   {Widths, _Offsets} = line_specs(PanelMap, NLines),
   Justify            = ep_typestyle:report_justify(Tag),
   RichText           = rich_text(XML, Beads),
   MaybeLines         = ep_block:break_rich_text(RichText, Justify, Widths),
   Lines              = lines(MaybeLines),
   {Tag, Lines}.


measure_lines(Tag, Lines, PanelMap) ->
   TypeStyle = ep_panel:get_typestyle(PanelMap),
   NLines    = get_nlines(TypeStyle, Tag, PanelMap),
   Vacant   = NLines > length(Lines),
   case Vacant of
      true  -> {Lines, PanelMap, fits};
      false -> {Lines, PanelMap, spills} 
   end.
   

%% Ok to here

%% ***********************************************************************
%% Work space
%% ***********************************************************************

%% Working on paste_lines



paste_lines(PDF, _Job, Tag, Lines, XML, Beads, fits)   ->
    [PanelMap | MoreBeads] = Beads,   
    TypeStyle = ep_panel:get_typestyle(PanelMap),
    PanelMap1 = paste_up(PDF, TypeStyle, Tag, Lines, PanelMap),
    Beads1 = [PanelMap1 | MoreBeads], 
    io:format("paste_lines/7 - length XML: ~p~n~n", [length(XML)]),
    [_Xml | MoreXML] = XML,
    {MoreXML, Beads1}; 

paste_lines(PDF, Job, Tag, Lines, XML, Beads, spills) ->
   % Paste what we can
   [PanelMap | MoreBeads] = Beads,   
   TypeStyle = ep_panel:get_typestyle(PanelMap),
   NLines    = get_nlines(TypeStyle, Tag, PanelMap),
   {Paste, Spill} = lists:split(NLines, Lines),
    paste_up(PDF, TypeStyle, Tag, Paste, PanelMap),

   % Paste spill 

   [PanelMap1 | MoreBeads1] = MoreBeads,
   ep_panel:panel(PDF, Job, PanelMap1), 
   Result = measure_lines(Tag, Spill, PanelMap1),
   paste_lines(PDF, Job, Tag, Spill, XML, MoreBeads1, Result).





get_Xml(XML) ->
   [Xml | _Rest] = XML,
   Xml.
    

get_tag(XML) ->
   {Tag, _Object, _MoreXML} = get_object(XML),
   Tag.





%% @docPaste up lines to PDF

-spec paste_up(PDF         :: identifier(),
               TypeStyle   :: atom(),
               Tag         :: atom(),
               Lines       :: list(),
               PanelMap    :: map()) -> map().

paste_up(_PDF, _TypeStyle, _Tag, [], _PanelMap) -> 
   [];

paste_up(PDF, TypeStyle, Tag, Lines, PanelMap) -> 
   Code      = pdf_code(PDF, TypeStyle, Tag, Lines, PanelMap),
   ok        = paste(PDF, Code),
   update_panel(Tag, Lines, PanelMap).


get_object(XML) ->
   [{xml, Xml} | MoreXML] = XML,
   Tag = element(1, Xml),
   Object = element(3, Xml),
   {Tag, Object, MoreXML}.

get_panel(Beads) ->
   [PanelMap | MoreBeads] = Beads,
   PanelId   = ep_panel:get_id(PanelMap),
   TypeStyle = ep_panel:get_typestyle(PanelMap),
   {PanelMap, PanelId, TypeStyle, MoreBeads}.

get_panelmap(Beads) ->
   {PanelMap, _PanelId, _TypeStyle, _MorePanels}   = get_panel(Beads),
   PanelMap.

get_typestyle(Beads) ->
   {_PanelMap, _PanelId, TypeStyle, _MorePanels}   = get_panel(Beads),
   TypeStyle.



%% ********************************************************************
%% Return line widths and offsets for a given panel 
%% ********************************************************************

% copy_specs(TypeStyle, Tag, PanelMap) ->
%   NLines          = get_nlines(TypeStyle, Tag, PanelMap),
%   Indent          = ep_typestyle:report_indent(Tag),
%   Measure         = ep_panel:get_measure(PanelMap),
%   Margin          = ep_panel:get_margin(PanelMap),
%   Widths          = ep_block:widths(Indent, Measure, NLines),
%   Offsets         = ep_block:offsets(Indent, Margin, NLines),
%   {Widths, Offsets} = ep_block:line_specs
 

%% @doc Return line widths and offsets for a given panel

-spec line_specs(PanelMap  :: map(),
                 NLines    :: integer()) -> tuple().

line_specs(PanelMap, NLines) ->
    Measure = ep_panel:get_measure(PanelMap),
    Indent = ep_panel:get_indent(PanelMap),
    Widths = [Measure - Indent|lists:duplicate(NLines - 1, Measure)],
    Offsets = [Indent|lists:duplicate(NLines - 1, 0)],
    {Widths, Offsets}.
 

%% ********************************************************************
%% Transform Xml into rich text; e.g. tagged text tokens 
%% ********************************************************************


%% @doc Transform Xml into rich text; e.g. tagged text tokens 

-spec rich_text(XML   :: list(),
                Beads :: list()) -> list().        

rich_text(XML, Beads) ->
   Xml            = get_Xml(XML),
   PanelMap       = get_panelmap(Beads),
   TypeStyle      = ep_panel:get_typestyle(PanelMap),       
   Tag            = get_tag(XML),
   FontMap        = ep_typespec:get_fontmap(TypeStyle, Tag),
   Norm           = ep_block:normalise_xml(XML, FontMap),
   {_, RichText}  = ep_block:rich_text(Norm),
   RichText.


%% ********************************************************************
%% Verify that we have valid lines 
%% ********************************************************************

%% @doc Verify that we have valid lines 

-spec lines(MaybeLines :: tuple()) -> list().

lines(impossible) ->
   io:format("Cannot break line; are widths ok?~n");

lines({Lines, _, _}) ->
    Lines.


%% ********************************************************************
%% Return number of lines that fit in panel 
%% ********************************************************************


%% @doc Return number of lines that fit in panel 

-spec get_nlines(TypeStyle :: atom(),
                 Tag       :: atom(),
                 PanelMap  :: map()) -> integer().

get_nlines(TypeStyle, Tag, PanelMap) ->
    ep_panel:get_nlines(TypeStyle, Tag, PanelMap).


%% ********************************************************************
%% Transform lines to PDF code
%% ********************************************************************

   
%% @doc Transform lines to PDF code

-spec pdf_code(PDF            :: identifier(),
               TypeStyle      :: atom(),
               Tag            :: atom(),
               Lines          :: list(),
               PanelMap       :: map()) -> string().

pdf_code(PDF, TypeStyle, Tag, Lines, PanelMap) ->
       io:format("Entering pdf_code/5~n"),
       io:format("Tag: ~p~n", [Tag]),
       io:format("Length Lines: ~p~n~n", [length(Lines)]),
       Name = ep_panel:get_panel_name(PanelMap),
       io:format("pdf_code/5 - PanelMap: ~p~n", [Name]),
   {TextX, TextY}    = ep_panel:get_text_position(PanelMap),
   Justify           = ep_typestyle:report_justify(Tag),
   Leading           = ep_typestyle:report_leading(Tag),
   NLines             = get_nlines(TypeStyle, Tag, PanelMap),
   {Widths, Offsets} = line_specs(PanelMap, NLines),
       io:format("Justfify: ~p~n", [Justify]),
       io:format("Leading: ~p~n", [Leading]),

   ep_richText2pdf:richText2pdf(PDF, TextX, TextY, Justify, 0, Lines, Leading, Widths, Offsets).



%% ***********************************************************
%% Update panel 
%% ***********************************************************

update_panel(Tag, Lines, PanelMap) ->
   Leading  = ep_typestyle:report_leading(Tag),
   Consumed = Leading * length(Lines),
   NextLine = ep_panel:get_next_line(PanelMap) + Consumed,
   ep_panel:update_next_line(NextLine, PanelMap).




%% ********************************************************************
%% Append PDF code to text stream in PDF
%% ********************************************************************


%% @doc Append PDF code to text stream in PDF

-spec paste(PDF  :: identifier(),
            Code :: string()) -> ok.

paste(PDF, Code) ->
  eg_pdf:begin_text(PDF),
  eg_pdf:append_stream(PDF, Code),
  eg_pdf:end_text(PDF),
  ok.


%% *****************************************************************
%% Archive
%% *****************************************************************

%% Need unique measure procedure for each object type
%% Need unique paste procedure for each object type


%% Given Tag, Lines, and PanelMap
%% Do lines fit?
%%    Yes -- Paste them up
%%           Update PanelMap
%%           Return {XML1, Beads1}

%%    No  -- Split lines
%%           Paste what we can
%%           Get next panelmap
%%           Recompute lines  
%%           Paste spill
%%           Update panelmap 
%%           Return {XML1, Beads1}
 
%% Note1: if object is a headline, 2 x lines x leading
%% Note2: Object gets transformed into lines


p_rules(Paste, Spill) ->
   io:format("&&&&&& Entering p_rules~n"),
   {Char, LineCount} = last_line(Paste),
   io:format("LineCount: ~p~n~n", [LineCount]), 
   Action            = verdict(Char, LineCount),
   io:format("Action: ~p~n~n", [Action]), 
   dispatch_line(Paste, Spill, Action).   



last_line(Paste) ->
   LineCount   = length(Paste),     
   LastLine    = lists:last(Paste),
   Tokens      = element(2, LastLine),
   TokenSpec   = lists:last(Tokens),
   MaybeSpace  = size(TokenSpec),     % If size == 3, then token is a space, else word
   case MaybeSpace of
       3      -> Char = space;
       _      -> Size = MaybeSpace,
                 Word = element(Size, TokenSpec),
                 Char = lists:last(Word)
   end,
   {Char, LineCount}.

verdict(space, 1) ->  spill;   % Blank line -- spill
verdict($-, 1)    ->  spill;   % Hyphenated line -- spill
verdict( _, 1)    ->  paste.   % Valid line - paste


 dispatch_line(_Paste, _Spill, spill) ->
   io:format("dispatch_line/3 - spill~n~n");
 
 dispatch_line(_Paste, _Spill, paste) ->  
   io:format("dispatch_line/3 - paste~n~n").



last_paste_token(Paste, _Spill) ->
   Line        = lists:last(Paste),
   Tokens      = element(2, Line),
   TokenSpec   = lists:last(Tokens),
   Size        = size(TokenSpec),


   Token       = element(Size, TokenSpec),
   
   case Token of

      true  -> Char = lists:last(Token),
               Char == $-;
      false -> false
   end.
    






%   [PanelMap | MoreBeads] = Beads,
%                          % io:format("PanelMap: ~p~n", [PanelMap]),
%                          Name = ep_panel:get_panel_name(PanelMap),
%                          io:format("Panel - Name: ~p~n~n", [Name]),


%  io:format("LLLLLLLLLLLL Lines: ~p~n~n", [Lines]),

%   Result = measure_copy(TypeStyle, Tag, Lines, PanelMap),

%   case Result of
%      fits   -> % Paste and get next Xml 
%                io:format("~n################  fits ############~n~n"),
%          {MoreXML, Beads1} = lines_fit(PDF, TypeStyle, Tag, Lines, PanelMap, MoreXML, MoreBeads ),
%                io:format("********* Recurse~n~n~n"),  
%           paste_panels(PDF, Job, MoreXML, Beads1, []);

%      spills -> % Split lines, paste what lines  we can, get new bead, 
%                io:format("~n################  spills ############~n~n"),

%           % need to paste panel

%           {Paste1, Spill1}  = paste_rules(TypeStyle, Tag, Lines, PanelMap), 
%           LengthPaste1 = length(Paste1),
%           case LengthPaste1 of
%               0 -> Spill1;
%               _ -> paste_lines(PDF, TypeStyle, Tag, Paste1, PanelMap),
%                    Spill1
%           end, 
%                io:format("********* Recurse~n~n~n"),  
%           [PanelMap1 | _More] = MoreBeads,

%           ep_panel:panel(PDF, Job, PanelMap1), 
%           paste_panels(PDF, Job, MoreXML, MoreBeads, Spill1)
%   end.   


%% ********************************************************************
%% Fit lines to panel
%% ********************************************************************

%% @doc Given Xml, generate rich text for new panel; preppend spillover   
%%      rich text from previous panel, and break rich text into lines
%%      to fit 

% -spec xml2lines(TypeStyle :: atom(),
%                Tag       :: atom(),
%                Xml       :: list(),
%                PanelMap  :: map(),
%                Spill     :: list()) -> list().

% xml2lines(TypeStyle, Tag, Xml, PanelMap, []) ->
%   io:format("XXXXXXXXXX Entering xml2lines/5 - 1~n~n"),
%   {Widths, _Offsets}         = copy_specs(TypeStyle, Tag, PanelMap),
%   Justify                    = ep_typestyle:report_justify(Tag),
%   RichText                   = rich_text(TypeStyle, Xml),
%   MaybeLines                 = ep_block:break_rich_text(RichText, Justify, Widths),
%   Lines                      = lines(MaybeLines);

% xml2lines(TypeStyle, Tag, Xml, PanelMap, Spill) ->
%   io:format("Entering xml2lines/5 - 2~n~n"),
%   {Widths, _Offsets}         = copy_specs(TypeStyle, Tag, PanelMap),
%   Justify                    = ep_typestyle:report_justify(Tag),
%   RichText                   = rich_text(TypeStyle, Xml),
%   MaybeLines                 = ep_block:break_rich_text(RichText, Justify, Widths),
%   Lines                      = lines(MaybeLines),
%   io:format("PanelMap: ~p~n", [PanelMap]),
%   io:format("2222222222 Spill: ~p~n", [Spill]),
%   io:format("2222222222 Lines: ~p~n~n", [Lines]),
%   Spill ++ Lines.

%% **********************************

% preview(XML) ->
%   [{xml, Xml} | MoreXML] = XML,
%   io:format("Xml: ~p~n~n", [Xml]),
%   [{xml, Xml1} | _Rest]  = MoreXML,
%   io:format("Xml1: ~p~n~n", [Xml1]),
%   Tag1 = element(1, Xml),
%   Tag2 = element(1, Xml1),
%   {Tag1, Tag2}.
   
% text(Xml) ->
   % Only apply if length of Text phrases is 1
%   {Tag,[],[{raw,Text}]} = Xml,
%   Text1 = string:strip(Text),
%   {Tag, Text1}.










   
%% ********************************************************************
%% Determine if lines fit in paenl 
%% ********************************************************************


%% @doc Determine if lines fit in paenl 

% -spec measure_copy(TypeStyle :: atom(),
%                   Tag       :: atom(),
%                   Lines     :: list(),
%                   PanelMap  :: map()) -> atom().


% paste_rules(PDF, Job, TypeStyle, Tag, Lines, PanelMap) -> 


% measure_copy(TypeStyle, Tag, Lines, PanelMap) ->
%   NLines      = get_nlines(TypeStyle, Tag, PanelMap),
%   Vacant      = NLines > length(Lines),
%        io:format("@@@@@ Measuring copy~n"), 
%        io:format("Vacant: ~p~n", [Vacant]),
%        io:format("Length Lines: ~p~n", [length(Lines)]),
%        io:format("@@@@@ Leaving measure_copy~n"),
%   case Vacant of
%      true  -> fits;
%      false -> spills 
%   end.





%% ********************************************************************
%% The lines will fit, so paste 'em up 
%% ********************************************************************


%% @doc The lines will fit, so paste 'em up 

% -spec lines_fit(PDF       :: identifier(),
%                TypeStyle :: atom(),
%                Tag       :: atom(),
%                Lines     :: list(),
%                PanelMap  :: map(),
%                MoreXML   :: list(),
%                MoreBeads :: list()) -> tuple().

% lines_fit(PDF, TypeStyle, Tag, Lines, PanelMap, MoreXML, MoreBeads) ->
%    PanelMap1 = paste_lines(PDF, TypeStyle, Tag, Lines, PanelMap),
%    Beads1 = [PanelMap1 | MoreBeads], 
%    {MoreXML, Beads1}. 



%% ********************************************************************
%% Enforce line allocation when lines spill over from one panel to next 
%% ********************************************************************


%% @doc Enforce line allocation when lines spill over from one panel to next 

% -spec paste_rules(TypeStyle   :: atom(),
%                  Tag         :: atom(),
%                  Lines       :: list(),
%                  PanelMap    :: map()) -> tuple().

% paste_rules(TypeStyle, Tag, Lines, PanelMap) -> 
%     io:format("============ Entering paste_rules~n"),
%     NLines          = get_nlines(TypeStyle, Tag, PanelMap),
%     {Paste, Spill}  = split_Lines(TypeStyle, Tag, Lines, PanelMap),
%     io:format("Tag: ~p~n~n", [Tag]),
%     case Tag of
%        p  -> {Paste1, Spill1} = p_rules(Paste, Spill);
%        _  -> {Paste1, Spill1} = spill_over(Paste, Spill)
%     end,
%     {Paste1, Spill1}.


%% ********************************************************************
%% Paste up lines to PDF
%% ********************************************************************


% fit_lines(Tag, Lines, XML, Beads, fits)   ->
% lines_fit(PDF, TypeStyle, Tag, Lines, PanelMap, MoreXML, MoreBeads) ->
%    PanelMap1 = paste_lines(PDF, TypeStyle, Tag, Lines, PanelMap),
%    Beads1 = [PanelMap1 | MoreBeads], 
%    {MoreXML, Beads1}. 



%  fit_lines(Tag, Lines, XML, Beads, spills) ->


%% @docv Paste up lines to PDF

% -spec paste_lines(PDF         :: identifier(),
%                  TypeStyle   :: atom(),
%                  Tag         :: atom(),
%                  Lines       :: list(),
%                  PanelMap    :: map()) -> map().

% paste_lines(PDF, TypeStyle, Tag, [], PanelMap) -> 
%   [];

%% @doc Paste lines and update panel map 

% paste_lines(PDF, TypeStyle, Tag, Lines, PanelMap) -> 
%   io:format("+++++++++ Entering paste_lines - Tag: ~p~n", [Tag]),            
%   io:format("Length Lines: ~p~n", [length(Lines)]),
%   io:format("Lines: ~p~n", [Lines]),

%   Code      = pdf_code(PDF, TypeStyle, Tag, Lines, PanelMap),
%   ok        = paste(PDF, Code),
%       Leading           = ep_typestyle:report_leading(Tag),
%       Consumed = Leading * length(Lines),
%       NextLine = ep_panel:get_next_line(PanelMap) + Consumed,
%           Name = ep_panel:get_panel_name(PanelMap),
%           io:format("Pasted: ~p~n", [Name]),
%           io:format("++++++++++ NextLine ~p~n~n", [NextLine]),            
%   update_panel(Tag, Lines, PanelMap).






%% ********************************************************************
%%  paste_rules/6
%% ********************************************************************


% split_Lines(TypeStyle, Tag, Lines, PanelMap) ->
%   NLines          = get_nlines(TypeStyle, Tag, PanelMap),
%   Pastable        = NLines - length(Lines),
%   {Paste, Spill}  = lists:split(Pastable, Lines),
%   {Paste, Spill}.


% spill_over(Paste, Spill) ->
%   LengthPaste = length(Paste),
%   case LengthPaste of
%      0  -> 
%            {Paste1, Spill1} ={Paste, Spill},
%            {Paste1, Spill1};
             
%      _  -> 
%             [Line | Paste1 ] = Paste,
%            Spill1           = [Line | Spill],
%            {Paste1, Spill1}
%   end, 
%   {Paste1, Spill1}.




   






%% ********************************************************************
%% paste_copy/5 helpers
%% ********************************************************************






%% Paste lines helpers
%% ***********************************************************


%% ***********************************************************
%% Chope richText 
%% ***********************************************************

% chop_richText(RichText, Lines, NLines) ->
%   io:format("copy_richText/3 - RichText: ~p~n~n", [RichText]),
%   Counts = [size(Line) || Line <- Lines],
%   CountList = lists:sublist(Counts, NLines),
%   Total     = lists:sum(CountList),
%   lists:nthtail(Total, RichText).


            





% line_specs(TypeStyle, Tag, PanelMap) -> 
%   Justify = ep_typespec:get_justify(TypeStyle, Tag),
%   Indent  = ep_typespec:get_indent(TypeStyle, Tag),
%   Measure = ep_panel:get_measure(PanelMap),
%   {Justify, Indent, Measure}.



%% *************************************************************
%% Get current panel 
%% *************************************************************

%% @doc Get current panel 

% -spec get_panel( ArticleMap :: map()) -> tuple().

% get_panel(ArticleMap) ->
%    Beads = get_beads(ArticleMap),
%    [Panel | _Remaining] = Beads,
%    Panel.
    





