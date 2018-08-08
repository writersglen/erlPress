 
%%% ==========================================================================
%%% ep_article.erl
%%%
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
-define(WIDOW, 3).
-define(MIN_LINESPACE, 2).

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
    XML       = ep_block:parse_xml(Copy), 
    Beads     = get_beads(ArticleMap),
    PanelMap  = get_panelmap(Beads),
    paste_panel(PDF, Job, PanelMap),
    Beads1    = [ep_panel:update_typestyle(TypeStyle, Bead) || Bead <- Beads],
%    paste_panels(PDF, Job, XML, Beads1),
    ok.


%% ************* Test functions -- Remove  

xml() ->
   Copy = ep_sample_text:article(),
   ep_block:parse_xml(Copy). 

beads() ->
   Panel1 = ep_panel:create({1, 1, top}, {72, 150}, {400, 200}),
   Panel2 = ep_panel:create({1, 2, continue}, {72, 350}, {195, 200}),
   Panel3 = ep_panel:create({1, 3, final}, {277, 350}, {195, 200}),
   [Panel1, Panel2, Panel3].

 

trial_paste_li() ->
   L                         = "<li>This is a list item</li>", 
   L1                        = ep_block:parse_xml(L),
   PanelMap                  = ep_panel:create({1, 1, top}, {72, 150}, {400, 200}),
   {Paste, Cost, PanelMap1}  = assign_content(L1, PanelMap),
   trial_paste(Paste, Cost, PanelMap1, "li"). 
   

trial_paste_ci() ->
   C                = "<ci>This is a checklist item</ci>", 
   C1               = ep_block:parse_xml(C),
   PanelMap         = ep_panel:create({1, 1, top}, {72, 150}, {400, 200}),
   {Paste, Cost, PanelMap1} = assign_content(C1, PanelMap),
   trial_paste(Paste, Cost, PanelMap1, "ci"). 
   








trial_paste_spill(Tag, Spill, Cost, PanelMap) ->
    PDF                = eg_pdf:new(), 
    Job                = ep_job:create("Trial Paste", "LRP"),
    PanelMap1          = ep_panel:reveal(PanelMap),
    ok                 = paste_panel(PDF, Job, PanelMap1),
    {Cost1, PanelMap2} = paste_spill(PDF, Tag, Spill, Cost, PanelMap),
    OutFile   = "./pdf/galleys/" ++ 
                 ?MODULE_STRING ++ " 
                 spill.trial.pdf",
    save(PDF, OutFile),
    {Cost1, PanelMap2}.





trial_paste(Paste, Cost, PanelMap, TestType) ->
    PDF       = eg_pdf:new(),
    Job       = ep_job:create("Trial Paste", "LRP"),
 
    Paste1    = lists:reverse(Paste),
    PanelMap1 = ep_panel:reveal(PanelMap),
    NameString = ep_panel:get_name_string(PanelMap1),
    NameString1 = NameString ++ "_" ++ TestType,
    ok        = paste_panel(PDF, Job, PanelMap1),
    ok        = paste(PDF, Paste1, Cost, PanelMap1),
    ok        = paste_if_li(PDF, Paste1, PanelMap1),
    ok        = paste_if_ci(PDF, Paste, PanelMap1),
    OutFile   = "./pdf/galleys/" ++ 
                 ?MODULE_STRING ++ 
                 "_" ++ 
                 NameString1 ++ 
                 ".trial.pdf",
    save(PDF, OutFile).



%    ok        = paste(PDF, Paste1, Cost, PanelMap1),




%% @doc Paste text elements into panel

-spec paste(PDF      :: identifier(),
            Paste    :: list(),
            Cost     :: integer(),
            PanelMap :: map()) -> ok.        

paste(_PDF, [], _Cost,  _PanelMap) ->
    ok;

paste(PDF, Paste, Cost, PanelMap) ->
    io:format("====================================== Entering paste/4~n"),
    [Paste1 | MorePaste] = Paste,
    io:format("================ Paste1: ~p~n~n", [Paste1]),
        % io:format("trial_paste3 - MorePaste: ~p~n~n", [MorePaste]),
    Tag                = element(1, Paste1),
    Lines              = element(2, Paste1),
    io:format("================ Tag: ~p~n~n", [Tag]),
    io:format("================ Lines: ~p~n~n", [Lines]),
    {Cost1, PanelMap1} = paste_lines(PDF, Tag, Lines, Cost, PanelMap),
    Paste2             = MorePaste,
    io:format("====================================== Exiting paste/4~n"),
    paste(PDF, Paste2, Cost1, PanelMap1).


%% ***********************************************************



paste_spill(PDF, Spill, Cost, PanelMap) ->
    io:format("================================= Entering paste_spill4~n"),
    io:format("================ Spill: ~p~n~n", [Spill]),
%        % io:format("trial_paste3 - MorePaste: ~p~n~n", [MorePaste]),
    Spill1             = hd(Spill),
    Tag                = element(1, Spill1),
    Lines              = element(2, Spill1),
    io:format("================ Tag: ~p~n~n", [Tag]),
    io:format("================ Lines: ~p~n~n", [Lines]),
    {Cost1, PanelMap1} = paste_spill(PDF, Tag, Lines, Cost, PanelMap),
    io:format("================================= Eexiting paste_spill4~n"),
    {Cost1, PanelMap1}. 



%    {Cost1, PanelMap1} = paste_lines(PDF, Tag, Lines, Cost, PanelMap),
%    {Cost1, PanelMap1} = paste_spill(PDF, Tag, Lines, Cost, PanelMap),





%% ************* End Test functions 


%% ***********************************************************
%% ***********************************************************
%% Article helpers 
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
%% Paste panel 
%% *************************************************************

%% @doc Paste panel

-spec paste_panel(PDF   :: identifier(),
                  Job   :: map(),
                  PanelMap :: map()) -> ok.

paste_panel(PDF, Job, PanelMap) ->
    ep_panel:panel(PDF, Job, PanelMap). 



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
%% Paste panel 
%%
%% Copyfits XML to a panel
%% ***********************************************************
%% ***********************************************************



%% ***********************************************************
%% ***********************************************************
%% Fit copy into top panel 
%%
%% Copyfits XML to a panel
%%
%% NOTE: XML is a list of tagged content elements
%%       Beads is a list of panel maps
%%
%% Returns {Paste, Cost, Spill} e.g,
%%          Paste:. lines to paste into panel
%%          Cost: pixels that need to be squeezed out of panel
%%          Spill: lines that need to pasted into next panel
%% ***********************************************************
%% ***********************************************************


%% @doc Copyfit content to panel

-spec copyfit_top_panel(XML   :: list(),
                        Beads :: list()) -> tuple().


copyfit_top_panel(XML, Beads) ->
   [PanelMap | MoreBeads]     = Beads,
   {Content, XML1, PanelMap1} = assign_content(XML, PanelMap),
   {Paste, Cost, Spill}       = fill_gap(Content, XML1, Beads),
   Spill1                     = copyfit_spill(XML1, MoreBeads, Spill),
   {Paste, Cost, Spill1}.

%% Now we need to paste Paste and Spill.
%% Do we need Cost

copyfit_spill(XML1, MoreBeads, Spill) ->
   io:format("+++++++++++++++++++++++++++ Entering copyfit_spill/3~n"),
   Xml      = get_Xml(XML1),
   Tag      = get_tag(Xml),
   PanelMap = hd(MoreBeads),
   io:format("+++++++++++++++++++ Tag: ~p~n" , [Tag]),
   io:format("+++++++++++++++++++ Xml: ~p~n" , [Xml]),
   io:format("+++++++++++++++++++ PanelMap: ~p~n~n" , [PanelMap]),
   io:format("+++++++++++++++++++++++++++ Leaving copyfit_spill/3 for recompute_spill/3~n"),
   recompute_spill(Tag, PanelMap, Spill).

      
   


% recompute_spill(Tag, PanelMap, Spill) ->
% But how do I get Tag and PanelMap?

%   Tag             = get_tag(Xml),
%   PanelMap        = Beads1 = tl(Beads),

%% ***********************************************************
%% ***********************************************************
%% copyfit_top_panel/2 helpers 
%% ***********************************************************
%% ***********************************************************


%% ***********************************************************
%% copyfit_top_panel/2 helper - Assign content to panel 
%% ***********************************************************


%% @doc Assign content to panel

-spec assign_content(XML      :: list(),
                     PanelMap :: map()) -> tuple().

assign_content(XML, PanelMap) ->
   assign_content([], XML, PanelMap, true).


assign_content(Content, [], PanelMap, _Continue) ->
    io:format("Out of copy~n~n"),
    {Content, [], PanelMap};


assign_content(Content, XML, PanelMap, false) ->
    {Content, XML, PanelMap};


assign_content(Content, XML, PanelMap, true) ->
     [X | XML1] = XML,
     Xml           = element(2, X),
     Tag          = get_tag(Xml),
     io:format("XXXXXXXXX assign_content/4 Xml: ~p~n~n", [Xml]),
     Lines        = xml2lines(Xml, PanelMap),
     Vacancies    = vacancies(Tag, PanelMap),
     Continue     = Vacancies >= length(Lines),
     case Continue of
        true  -> 
                 Content1    = [{Tag, Lines} | Content],
                 PanelMap1   = update_panel(Tag, Lines, PanelMap),
                 assign_content(Content1, XML1, PanelMap1, Continue);
        false -> assign_content(Content, XML, PanelMap, Continue)
     end.
        


%% ***********************************************************
%% copyfit_top_panel/2 helper - Fill gap in panel 
%% ***********************************************************

%% @doc Fill gap in panel; returns {Paste, Cost, Spill}

-spec fill_gap(Content :: list(),
               XML     :: list(),
               Beads   :: list()) -> tuple().

fill_gap(Content, XML, Beads) ->
   Xml                     = get_Xml(XML),
   PanelMap                = hd(Beads),
   NextPanel               = next_panel(Beads),
   Tag                     = get_tag(Xml),
   CanSqueezeIn            = can_squeeze_in(Tag, Content, NextPanel),
   move_up_lines(CanSqueezeIn, Content, Xml, PanelMap).



%% ***********************************************************
%% XML functions 
%% ***********************************************************


%    XML = ep_block:parse_xml(Copy), 


%% @doc Return Xml

-spec get_Xml(XML :: list()) -> tuple().

get_Xml(XML) ->
   [X | _MoreXML]  = XML,
   element(2, X).


%% @doc Given content elment, return tag 

-spec get_tag(Xml :: tuple()) -> atom().

get_tag(Xml) ->
   element(1, Xml).


get_phrase(Xml) ->
   element(3, Xml).


%% @doc Transform Xml into lines copyfitted into panel

-spec xml2lines(Xml      :: tuple(),
                PanelMap :: map()) -> list().        

xml2lines(Xml, PanelMap) ->
   Tag      = get_tag(Xml),   
   RichText = rich_text(Xml, PanelMap),
   get_lines(Tag, RichText, PanelMap).

   






get_first_line(XML) ->
   Xml = get_Xml(XML),
   hd(element(3, Xml)).





get_object(XML) ->
   [{xml, Xml} | MoreXML] = XML,
   Tag = element(1, Xml),
   Object = element(3, Xml),
   {Tag, Object, MoreXML}.


%% ***********************************************************
%% RichText functions 
%% ***********************************************************


%% @doc rich_text/2 helper 

-spec rich_text(Xml      :: tuple(),
                PanelMap :: map()) -> list().        

rich_text(Xml, PanelMap) ->
   TypeStyle      = ep_panel:get_typestyle(PanelMap),       
   Tag            = element(1, Xml),
   FontMap        = ep_typespec:get_fontmap(TypeStyle, Tag),
   Norm           = ep_block:normalise_xml(Xml, FontMap),
   {_, _, RichText}  = Norm,
   RichText.


%% ***********************************************************
%% Lines 
%% ***********************************************************


%% @doc Transform xml to lines to fit panel

-spec get_lines(Tag      :: atom(),
                RichText :: list(),
                PanelMap :: list()) -> list().

get_lines(Tag, RichText, PanelMap) ->
   io:format("================================= Entering get_lines/3~n"),
   io:format("!!!!!!!!!!!!!!!!! get_lines/3 - Tag: ~p~n~n", [Tag]),
   io:format("!!!!!!!!!!!!!!!!! get_lines/3 - PanelMapt: ~p~n~n", [PanelMap]),
   Vacancies          = vacancies(Tag, PanelMap),
   {Widths, _Offsets} = line_specs(Tag, PanelMap),
   io:format("!!!!!!!!!!!!!!!!! get_lines/3 - Widths: ~p~n~n", [Widths]),
   Justify            = ep_typestyle:report_justify(Tag),
   MaybeLines         = ep_line_break:break_richText(RichText, {Justify, Widths}),
   Lines              = lines(MaybeLines),
   io:format("================================= Exiting get_lines/3~n"),
   Lines.











%% ***********************************************************
%% assign_content/2 helpers
%%
%%  NOTE: XML is a list of tagged content elements
%%        Xml is a tagged content element; example:
%%            {h1,[],[{raw,"This is a headline"}]} 
%%        Beads is a list of panel maps
%% ***********************************************************



%% @doc Return number of lines that fit in panel 

-spec vacancies(Tag      :: list(),
                PanelMap :: map()) -> integer().

vacancies(Tag, PanelMap) ->
   TypeStyle = ep_panel:get_typestyle(PanelMap),
   ep_panel:get_nlines(TypeStyle, Tag, PanelMap).



%% @doc Update content cursor (NextLine) in panel

-spec update_panel(Tag      :: atom(),
                   Lines    :: list(),
                   PanelMap :: map()) -> map().

update_panel(Tag, Lines, PanelMap) ->
   Leading  = ep_typestyle:report_leading(Tag),
   Consumed = Leading * length(Lines),
   NextLine = ep_panel:get_next_line(PanelMap) + Consumed,
   ep_panel:update_next_line(NextLine, PanelMap).
   


%% ***********************************************************
%% fill_gap/3 helpers
%% ***********************************************************


%% @doc Return second panel in list of beads

-spec next_panel(Beads :: list()) -> map().

next_panel(Beads) ->
   [_PanelMap | MoreBeads ] = Beads,
   [NextPanel | _MB]        = MoreBeads,
   NextPanel. 


%% @doc How many paragraph lines can we squeeze into panel?

-spec can_squeeze_in(Tag      :: atom(),
                     Content  :: list(),
                     PanelMap :: map()) -> integer().

can_squeeze_in(Tag, Content, PanelMap) ->
   MaxGap       = max_gap(Tag, Content, PanelMap),
   FontSize     = ep_typestyle:report_leading(Tag),
   MinLeading   = FontSize + ?MIN_LINESPACE,
   MaxGap div MinLeading.


%% ***********************************************************
%% max_gap/3 helpers
%% ***********************************************************


%% @doc Return maximum space we can 
%       open up by reducing leading

-spec max_gap(Tag      :: atom(),
              Content  :: list(),
              PanelMap :: map()) -> integer(). 

max_gap(Tag, Content, PanelMap) ->
   TotalLines   = total_lines(Content),
   Leading      = ep_typestyle:report_leading(Tag),
   FontSize     = ep_typestyle:report_fontsize(Tag),
   LineSpacing  = Leading - FontSize,    % How many pixels can we take from each line       
   Allowance    = LineSpacing - ?MIN_LINESPACE,
   TotalGain    = TotalLines * Allowance,  
   Gap          = gap(Content, PanelMap),
   TotalGain + Gap.
   

%% ***********************************************************
%% fill_gap/3 Move lines up from panel below the break 
%% Returns {Paste, Cost, Spill 
%% ***********************************************************


%% @doc Move lines up from second panel in beads list

-spec move_up_lines(LineCount   :: integer(),
                    Content     :: list(),
                    Xml         :: tuple(),
                    PanelMap    :: map()) -> tuple().

move_up_lines(LineCount, Content, Xml, PanelMap) ->
   Tag             = get_tag(Xml),
   Gap             = gap(Content, PanelMap),
   {Cost, Count}   = cost_of_moving(Tag, Gap, LineCount),
   {Take, Spill}   = get_lines_below_break(Count, Tag, Xml, PanelMap),
   Take1           = [{Tag, Take}],
   Take2           = lists:reverse(Take1),
   Paste           = prepend_lines(Take2, Content),
   {Paste, Cost, Spill}.                

   
                      

%% ***********************************************************
%% move_up_lines/4 helper - calculate cost in pixels of moving
%% lines.
%% 
%% Returns cost of move: e.g. {Cost, Lines}, e.g.,
%%       Cost: number of pixels that need to be adjusted
%%             to fit copy into panel
%%       Lines: number of lines that can be moved   
%% ***********************************************************


%% @doc Return number of pixels to add or deduct from current panl
%%      and optimum number of lines to move up from next panel

-spec cost_of_moving(Tag   :: atom(),
                     Gap   :: integer(),
                     LineCount :: integer()) -> tuple().

cost_of_moving(Tag, Gap, LineCount) ->
   cost_of_moving(Tag, Gap, LineCount, []).

cost_of_moving(_Tag, _Gap, 0, Cost) ->
   lists:min(Cost);

cost_of_moving(Tag, Gap, LineCount, Cost) ->
   Leading    = ep_typestyle:report_leading(Tag),
   Cost1      = Gap - (LineCount * Leading),
   Cost2      = [{Cost1, LineCount} | Cost],
   LineCount1 = LineCount - 1,
   cost_of_moving(Tag, Gap, LineCount1, Cost2).


%% @doc Returns two lists: lines that can be moved into
%%      panel and lines left over

-spec get_lines_below_break(Count      :: integer(),
                            Tag        :: atom(),
                            Xml        :: tuple(),
                            PanelMap   :: map()) -> tuple().

get_lines_below_break(Count, Tag, Xml, PanelMap) ->
   Lines = xml2lines(Xml, PanelMap),
   io:format("+++++++++++ Length Lines: ~p~n", [length(Lines)]),
   lists:split(Count, Lines).


%% @doc Prepend lines to content

-spec prepend_lines(Lines   :: list(),
                    Content :: list()) -> list().
prepend_lines(Lines, Content) ->
   Reverse = lists:reverse(Lines),
   lists:append(Reverse, Content).


%% ***********************************************************
%% max_gap/3 helper - gap/2 
%% ***********************************************************


%% @doc Return total number of lines in content list

-spec total_lines(Content :: list()) -> integer().

total_lines(Content) ->
   length(Content).


%% @doc Gap in pixels at bottom of panel following execution 
%%      of assign_content/2

-spec gap(Content   :: list(),
          PanelMap  :: map()) -> integer().

gap(Content, PanelMap) ->
   PanelHeight   = panel_height(PanelMap),
   Consumed      = total_consumed(Content),
   PanelHeight - Consumed.


%% ***********************************************************
%% gap/2 helpers  
%% ***********************************************************


%% @doc Return panel height 

-spec panel_height(PanelMap :: map()) -> integer().

panel_height(PanelMap) ->
   {_W, Height}            = ep_panel:get_size(PanelMap),
   Height.


%% @doc Return number of pixels consumed in panel by content

-spec total_consumed(Content :: list()) -> integer().

total_consumed(Content) ->
   ConsumptionList = [consumed(ContentItem) || ContentItem <- Content],
   lists:sum(ConsumptionList).


%% @doc Return number of pixels consumed in panel by content item

-spec consumed(ContentItem :: tuple()) -> integer().

consumed(ContentItem) ->
   Tag = element(1, ContentItem),
   Lines = element(2, ContentItem),
   Leading = ep_typestyle:report_leading(Tag),
   LineCount = length(Lines),
   Leading * LineCount.


%% ***********************************************************
%% xml2lines/2 helpers
%% ***********************************************************


%% @doc Return updated content list

recompute_spill(Tag, PanelMap, Spill) ->
   io:format("===============================  Entering recompute_spill/3~n"),
   Vacancies          = vacancies(Tag, PanelMap),
   RichText           = hd(Spill),
   {Widths, _Offsets} = spill_specs(Tag, PanelMap),
   Justify            = ep_typestyle:report_justify(Tag),
   MaybeLines         = ep_line_break:break_richText(RichText, {Justify, Widths}),
   Lines              = lines(MaybeLines),
   io:format("================================= Exiting recompute_spill/3~n~n~n"),
   [{Tag, Lines} | []].



%% @doc Return line widths and offsets for a given panel

-spec line_specs(Tag         :: atom(),
                 PanelMap    :: map()) -> tuple().

line_specs(Tag, PanelMap) ->
    io:format("===================================== Entering line_specs/2~n"),
    Measure   = ep_panel:get_measure(PanelMap),
    Margin    = ep_panel:get_margin(PanelMap),
    Indent    = ep_panel:get_indent(PanelMap),
    Vacancies = vacancies(Tag, PanelMap),
    case Tag of
        p     -> Widths  = [Measure - Indent|lists:duplicate(Vacancies - 1, Measure)],
                 Offsets = [Margin + Indent|lists:duplicate(Vacancies - 1, Margin)];
        li    -> Widths  = [Measure - Indent|lists:duplicate(Vacancies - 1, Measure - Indent)],
                 Offsets = [Margin + Indent|lists:duplicate(Vacancies - 1, Margin + Indent)];
        ci    -> Widths  = [Measure - Indent|lists:duplicate(Vacancies - 1, Measure - Indent)],
                 Offsets = [Margin + Indent|lists:duplicate(Vacancies - 1, Margin + Indent)];
        _     -> Widths  = [Measure|lists:duplicate(Vacancies - 1, Margin)],
                 Offsets = [Margin|lists:duplicate(Vacancies - 1, Margin)]
    end, 
    io:format("########################## line_specs/2 - Widths: ~p~n~n", [Widths]),
    io:format("########################## line_specs/2 - Offsets: ~p~n~n", [Offsets]),
    io:format("===================================== Exiting line_specs/2~n"),
    {Widths, Offsets}.


spill_specs(Tag, PanelMap) ->
    io:format("===================================== Entering spill_specs/2~n"),
    Vacancies = vacancies(Tag, PanelMap),
    Measure   = ep_panel:get_measure(PanelMap),
    Margin    = ep_panel:get_margin(PanelMap),
    Widths    = [Measure|lists:duplicate(Vacancies -1, Measure)],
    Offsets   = [Margin|lists:duplicate(Vacancies - 1, Margin)],
    io:format("================== spill_specs/2 - Tag: ~p~n", [Tag]),
    io:format("================== spill_specs/2 - Measure: ~p~n", [Measure]),
    io:format("================== spill_specs/2 - Margin: ~p~n", [Margin]),
    io:format("================== spill_specs/2 - PanelMap: ~p~n", [PanelMap]),
    io:format("================== spill_specs/2 - Widths: ~p~n", [Widths]),
    io:format("================== spill_specs/2 - Offsets: ~p~n", [Offsets]),
    io:format("===================================== Exiting spill_specs/2~n"),
    {Widths, Offsets}.




%% ***********************************************************
%% paste/4 - Paste text elements into panel  
%% ***********************************************************

%% paste/4 helpers  
%% ***********************************************************

%% @doc Paste lines into panel

-spec paste_lines(PDF     :: identifier(),
                  Tag     :: atom(),
                  Lines   :: list(),
                  Cost    :: integer(),
                  PanelMap :: map()) -> tuple().


paste_lines(PDF, Tag, Lines, [], PanelMap) ->
    {Widths, Offsets}  = line_specs(Tag, PanelMap),
    Code               = pdf_code(PDF, Tag, Lines, Widths, Offsets, PanelMap),
    ok                 = paste(PDF, Code),
    PanelMap1          = update_panel(Tag, Lines, PanelMap),
    io:format("====================================== Exiting paste_lines/5~n"),
    {[], PanelMap1};

paste_lines(PDF, Tag, Lines, Cost, PanelMap) ->
    io:format("====================================== Entering paste-lines/5~n"),
        % io:format("Lines: ~p~n ~n", [Lines]),
    {Cost1, PanelMap1} = impose_cost(-1, Cost, PanelMap),
    {Widths, Offsets}  = line_specs(Tag, PanelMap),
    Code               = pdf_code(PDF, Tag, Lines, Widths, Offsets, PanelMap1),
    ok                 = paste(PDF, Code),

    PanelMap2          = update_panel(Tag, Lines, PanelMap1),
    {Cost1, PanelMap2}.


paste_spill(PDF, Tag, Lines, Cost, PanelMap) ->
    io:format("====================================== Entering paste_spill/5~n"),
        % io:format("Lines: ~p~n ~n", [Lines]),
    {Widths, Offsets}  = spill_specs(Tag, PanelMap),
    Code               = pdf_code(PDF, Tag, Lines, Widths, Offsets, PanelMap),
    ok                 = paste(PDF, Code),

    PanelMap1          = update_panel(Tag, Lines, PanelMap),
    io:format("====================================== Exiting paste-spill/5~n"),
    {Cost, PanelMap1}.



impose_cost(Adjust, Cost, PanelMap) ->
    Cost1     = reduce_cost(Adjust, Cost),
    PanelMap1 = reposition_next_line(Adjust, PanelMap),
    {Cost1, PanelMap1}. 

reduce_cost(Cost, Adjust) ->
    Cost + Adjust.

reposition_next_line(Adjust, PanelMap) ->
    NextLine = ep_panel:get_next_line(PanelMap),
    NextLine1 = NextLine + Adjust,
    ep_panel:update_next_line(NextLine1, PanelMap).



%% @doc Transform lines to PDF code

-spec pdf_code(PDF            :: identifier(),
               Tag            :: atom(),
               Lines          :: list(),
               Widths         :: list(),
               Offsets        :: list(),
               PanelMap       :: map()) -> string().


pdf_code(PDF, Tag, Lines, Widths, Offsets, PanelMap) ->
       io:format("========================== Entering pdf_code/6~n"),
       io:format("============== Tag: ~p~n", [Tag]),
       Margin = ep_panel:get_margin(PanelMap),


   {TextX, TextY}    = ep_panel:get_text_position(PanelMap),
             io:format("&&&&&&&&&&& pdf_code/6- TextX: ~p~n", [TextX]),
             io:format("&&&&&&&&&&& pdf_code/6 - TextY: ~p~n", [TextY]),
             io:format("&&&&&&&&&&& pdf_code/6 - Margin: ~p~n", [Margin]),


   Justify           = ep_typestyle:report_justify(Tag),
   Vacancies         = vacancies(Tag, PanelMap), 
   TypeStyle         = ep_panel:get_typestyle(PanelMap),
   Leading           = ep_typestyle:report_leading(Tag),
       io:format("============== pdf_code/5 - Widths1: ~p~n", [Widths]),
       io:format("============== pdf_code/5 - Offsets1: ~p~n", [Offsets]),
       io:format("========================== Exiting pdf_code/6~n~n"),
   Code = ep_richText2pdf:richText2pdf(PDF, 
                                       TextX, 
                                       TextY, 
                                       Justify,  
                                       0, 
                                       Lines, 
                                       Leading, 
                                       Widths, 
                                       Offsets),
    Code.







paste_if_li(PDF, Paste, PanelMap) ->
   [Paste1 | MorePaste] = Paste,
   Tag     = get_tag(Paste1),  
   Margin  = ep_panel:get_margin(PanelMap), 
   Indent  = ep_panel:get_indent(PanelMap) div 2, 
   Radius  = 2,
   Diff    = ep_typestyle:report_leading(Tag) - Radius, 
   case Tag of
      li  -> {TextX, TextY} = ep_panel:get_text_position(PanelMap),
             DiffX = TextX + Margin + Indent,
             DiffY = TextY - Diff,
             eg_pdf:save_state(PDF),
             eg_pdf:set_line_width(PDF, 1),
             eg_pdf:set_dash(PDF, solid),
             eg_pdf:set_stroke_color(PDF, black),
             eg_pdf:set_fill_color(PDF, black),
             eg_pdf:circle(PDF, {DiffX, DiffY}, Radius),
             eg_pdf:path(PDF, fill_stroke),
             eg_pdf:restore_state(PDF),
             ok;
        _  -> ok
    end.


paste_if_ci(PDF, Paste, PanelMap) ->
   [Paste1 | MorePaste] = Paste,
   Tag     = get_tag(Paste1),  
   Size    = ep_typestyle:report_fontsize(Tag),
   Margin  = ep_panel:get_margin(PanelMap), 
   Diff    = ep_typestyle:report_leading(Tag), 
   case Tag of
      ci  -> {TextX, TextY} = ep_panel:get_text_position(PanelMap),
             DiffX = TextX + Margin,
             DiffY = TextY  - Diff,
             eg_pdf:save_state(PDF),
             eg_pdf:set_line_width(PDF, 1),
             eg_pdf:set_dash(PDF, solid),
             eg_pdf:set_stroke_color(PDF, black),
             eg_pdf:set_fill_color(PDF, white),
             eg_pdf:rectangle(PDF, {DiffX, DiffY}, {Size, Size}),
             eg_pdf:path(PDF, fill_stroke),
             eg_pdf:restore_state(PDF),
             ok;
        _  -> ok
    end.





delete_indent(Widths) ->
   Width = lists:last(Widths),
   [Width | tl(Widths)].


%% ********************************************************************
%% paste/2 - Append PDF code to text stream in PDF
%% ********************************************************************


%% @doc Append PDF code to text stream in PDF

-spec paste(PDF  :: identifier(),
            Code :: string()) -> ok.

paste(PDF, Code) ->
       io:format("----------- Entering paste/2~n"),
  eg_pdf:begin_text(PDF),
  eg_pdf:append_stream(PDF, Code),
  eg_pdf:end_text(PDF),
       io:format("----------- Exiting paste/2~n~n"),
  ok.


save(PDF, OutFile) ->
    ep_job:save_job(PDF, OutFile).



save(PDF) ->
    OutFile = "./pdf/galleys/" ++ ?MODULE_STRING ++ ".trial.pdf",
    ep_job:save_job(PDF, OutFile).





name(Beads) ->
   PanelMap = hd(Beads),
   ep_panel:get_name(PanelMap).




lines_below_break(XML, Beads) ->
  {Xml, PanelMap} = content_below_break(XML, Beads), 
  xml2lines(Xml, PanelMap).


content_below_break(XML, Beads) ->
  [Xml | _MoreXML] = XML,
  [PanelMap | MoreBeads] = Beads, 
  {Xml, PanelMap}.
 

total_lines_in_panel(Paste) ->
   LineCounts = [count_lines(Block) || Block <- Paste],
   lists:sum(LineCounts).


count_lines(TextBlock) ->
    Lines = element(2, TextBlock),
    length(Lines).


adjust_leading(Cost, Leading) when Cost > 0 ->
    io:format("Cost: ~p~n", [Cost]),
    io:format("Lower TextY~n"),
    Leading1 = Leading + 1,
    io:format("Leading: ~p~n~n", [Leading1]),
    Cost1  = Cost - 1,
    Cost2  = max(Cost1, 0),
    {Cost2, Leading1}; 

adjust_leading(Cost, Leading) when Cost == 0 ->
    io:format("Stet Leading~n"),
    {Cost, Leading};

adjust_leading(Cost, Leading) when Cost < 0 ->
    io:format("Cost: ~p~n", [Cost]),
    io:format("Leading: ~p~n", [Leading]),
    io:format("Raise Leading~n"),
    Leading1 = Leading - 1,
    io:format("Leading1: ~p~n~n", [Leading1]),
    Cost1  = Cost + 1,
    Cost2  = min(Cost1, 0), 
    {Cost2, Leading1}. 


%% These may be duplicate functions for unsed

last_item(PanelContent) ->
   Content = element(1, PanelContent),
   hd(Content).


last_element(PanelContent) ->
   Items = spill(PanelContent),
   hd(Items).



content(PanelContent) ->
   Content = element(1, PanelContent),
   [element(4, Lines) || Lines <- Content].








pastable(PanelContent, XML) ->
   Element   = last_element(PanelContent),
   lists:takewhile(fun(Item) -> Item /= Element end, XML).



%% @doc XML1

spill(PanelContent) ->
   LastItem = last_item(PanelContent),
   Items = element(4, LastItem),
   Items.





%% ********************************************************************
%% ********************************************************************
%% paste_panels/4 helpers  
%% ********************************************************************
%% ********************************************************************


%% ********************************************************************


%% @doc Split lines to fit panel

-spec allocate_lines(Lines :: list(),
                     Vacancies :: integer()) -> tuple().
                         
allocate_lines(Lines, Vacancies) -> 
   LengthLines = length(Lines),
   if
      Vacancies > LengthLines ->   Paste  = Lines,
                                   Spill  = [],
                                   Action = paste;
      Vacancies == LengthLines -> {Paste, Spill} = lists:split(Vacancies, Lines),
                                   Action = dispatch;
                                  
      Vacancies  < LengthLines ->  {Paste, Spill} = lists:split(Vacancies, Lines),
                                   Action = spill 
   end,
   {Paste, Spill, Action}. 



%   [PanelMap1 | MoreBeads]. 

%% ********************************************************************
%% paste_h3 - Paste up subhead
%% ********************************************************************


%% @doc Paste up subhead

-spec paste_h3(PDF    :: identifier(),
               Job    :: map(),
               XML    :: list(),
               Beads  :: list()) -> tuple().

paste_h3(PDF, Job, XML, Beads) ->
    [_PanelMap | Beads1] = Beads,
    [PanelMap1 | _More ]= Beads1,
    paste_panel(PDF, Job, PanelMap1),
    {XML, Beads1}.



%% ********************************************************************
%% ********************************************************************
%% Second level helpers 
%% ********************************************************************
%% ********************************************************************







get_panelmap(Beads) ->
   {PanelMap, _PanelId, _TypeStyle, _MorePanels}   = get_panel(Beads),
   PanelMap.




%% ********************************************************************
%% Verify that we have valid lines 
%% ********************************************************************

%% @doc Verify that we have valid lines 

-spec lines(MaybeLines :: tuple()) -> list().

lines(impossible) ->
   io:format("Cannot break line; are widths ok?~n");

lines({Lines, _, _}) ->
    Lines.




trim_richText(Paste, RichText) ->
   Trim = measure_trim(Paste),
   Tokens = element(2, RichText),
   {_A, B} = lists:split(Trim, Tokens),
   {richText, B}.

measure_trim(Paste) ->
   Tokens = [tokens(Line) || Line <- Paste],
   lists:sum(Tokens).
 

tokens(Line) ->
   Tokens = element(2, Line),
   length(Tokens).


get_panel(Beads) ->
   [PanelMap | MoreBeads] = Beads,
   PanelId   = ep_panel:get_id(PanelMap),
   TypeStyle = ep_panel:get_typestyle(PanelMap),
   {PanelMap, PanelId, TypeStyle, MoreBeads}.




get_last_line(Lines) ->
   Reverse = lists:reverse(Lines),
   hd(Reverse).






maybe_drop_last_line(Paste) ->
   IsHyphenated = is_hyphenated(Paste),
   case IsHyphenated of
      true  -> Reverse = lists:reverse(Paste),
               [_Line | MoreLines] = Paste,
               lists:reverse(MoreLines);
      false -> Paste
   end.


last_rich_token(Paste) -> 
   Reverse   = lists:reverse(Paste),
   LastLine  = hd(Reverse),
   Tokens    = element(2, LastLine),
   ReverseT  = lists:reverse(Tokens),
   hd(ReverseT).

token_is_hyphenated(Token) ->
    ReverseToken     = lists:reverse(Token),
    hd(ReverseToken) == $-.



%% ***********************************************************************
%% paste_panels/4 helpers
%% ***********************************************************************


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



%% *************************************************************
%% *************************************************************
%% Delete from here down
%% *************************************************************
%% *************************************************************


paste_rules(Action) ->
   case Action of
        paste  -> paste;
        review -> spill 
   end. 















%% ***********************************************************
%% ***********************************************************
%% Report proof results 
%% ***********************************************************
%% ***********************************************************



%% @doc Report proof results 

-spec report(Content :: list(),
             XML     :: list(),
             Beads   :: list()) -> ok.

report(Content, XML, Beads) ->
   PanelName     = name(Beads),
   PanelHeight   = panel_height(Beads),
   Lines         = total_lines(Content),
   Consumed      = total_consumed(Content),
   Gap           = gap(Content, Beads),
   LastTag       = last_tag(Content),
   Widow         = is_widow(Content),
   Hyphenated    = is_hyphenated(Content),
   NextPanel     = panel_below_break(Beads),
   NextTag       = tag_below_break(XML),
   Available     = movable_lines_below_break(XML, Beads),
   io:format("*******************************************~n"),
   io:format("* PANEL REPORT: ~p~n", [PanelName]),
   io:format("*******************************************~n"),
   io:format("*    Height: ~p~n", [PanelHeight]),
   io:format("*     Lines: ~p~n", [Lines]),
   io:format("*  Consumed: ~p~n", [Consumed]),
   io:format("*       Gap: ~p~n", [Gap]),
   io:format("*  Last Tag: ~p~n", [LastTag]),
   io:format("*     Widow: ~p~n", [Widow]),
   io:format("*    Hyphen: ~p~n", [Hyphenated]),  
   io:format("*******************************************~n"),
   io:format("* NEXT PANEL: ~p~n", [NextPanel]),
   io:format("*******************************************~n"),
   io:format("*  Next Tag: ~p~n", [NextTag]),
   io:format("*     Lines: ~p~n", [Available]),
   io:format("*******************************************~n"),
   layout_issues(Content, XML, Beads),
   ok.


%% ***********************************************************
%% Report panel layout issues
%% ***********************************************************

layout_issues(Content, XML, Beads) ->
   io:format("~n*******************************************~n"),
   io:format("*******************************************~n"),
   io:format("* !!!!!!!!!! PANEL LAYOUT ISSUES !!!!!!!!!!~n"),
   io:format("*******************************************~n"),
   io:format("*******************************************~n~n"),
   last_line_is_head(Content, XML, Beads),
   last_line_is_hyphenated(Content),
   last_line_is_hyphenated(Content),
   widow_issue_solutions(Content),
   ok.


last_line_is_head(Content, XML, Beads) ->
   Tag = last_tag(Content),
   Flag    = lists:member(Tag, [h1, h2, h3, h4, h5, h6]),
   case Flag of
      true  -> io:format("* Last item in panel is a subhead.~n"),
               io:format("*******************************************~n"),
               io:format("* POSSIBLE SOLUTIONS~n"),
               io:format("*******************************************~n"),
               % head_issue_solutions(Content, XML, Beads),
               hyphenated_issue_solutions(Content);
      false -> ok
   end,
   ok.


last_line_is_hyphenated(Content) ->
   Flag = is_hyphenated(Content),
   case Flag of
      true  -> io:format("* Last tem in panel is hyphenated.~n"),
               io:format("********************************************~n"),
               io:foramt("* POSSIBLE SOLUTIONS~n"),
               io:format("********************************************~n");
      false -> ok
   end,
   ok.

last_line_is_widowed(Content) ->
   Flag = is_widow(Content),
   case Flag of
      true  -> io:format("* Last tem in panel is widowed.~n"),
               io:format("********************************************~n"),
               io:foramt("* POSSIBLE SOLUTIONS~n"),
               io:format("********************************************~n");
      false -> ok
   end,
   ok.



%% ***********************************************************
%% Report possible panel layout solutions 
%% ***********************************************************



head_issue_solutions(Content, XML, Beads) ->
   Tag = last_tag(Content),
   Flag    = lists:member(Tag, [h1, h2, h3, h4, h5, h6]),
   case Flag of
     true  -> io:format("* 1. Move subhead to next panel and~n"), 
              io:format("*    increase leading in items above~n"),
              io:format("*~n"),
              io:format("* 2. Reduce leading in lines above~n"), 
              io:format("*    and bring up one or more lines~n"),
              io:format("*    from next panel~n"),
              io:format("*~n"),
              io:format("********************************************~n"),
              io:format(" DESIGN/EDITORIAL SOLUTIONS~n"),
              io:format("********************************************~n"),
              io:format("* 3. Move subhead to next panel and~n"), 
              io:format("*    add copy to items above~n"),
              io:format("*~n"),
              io:format("* 4. Reduce height of panel and ~n"), 
              io:format("     adjust position of all panels below~n"),
              io:format("********************************************~n"),
              io:format("* TRYING Solution 1~n"),
              io:format("********************************************~n");
              % trying_head_solution_one(Content, XML, Beads);
      false -> ok
   end,
   ok.


hyphenated_issue_solutions(Content) ->
   Flag = is_hyphenated(Content),
   case Flag of
      true  -> io:format("* 1. Cut copy in last line~n"), 
               io:format("*~n"),
               io:format("* 2. Move last line to next panel and~n"), 
               io:format("*    increase leading in items above~n"),
               io:format("*~n"),
               io:format("* 3. Reduce height of panel and ~n"), 
               io:format("     adjust position of all panels below~n"),
               io:format("********************************************~n");
       false -> ok
   end,
   ok.



widow_issue_solutions(Content) ->
   Flag = is_widow(Content),
   case Flag of
      true  -> io:format("* 1. Add copy to last line~n"), 
               io:format("*~n"),
               io:format("* 2. Move last line to next panel and~n"), 
               io:format("*    increase leading in items above~n"),
               io:format("*~n"),
               io:format("* 3. Reduce height of panel and ~n"), 
               io:format("     adjust position of all panels below~n"),
               io:format("********************************************~n");
      false -> ok
   end,
   ok.







%% **********************************************************
%% Scrap 
%% **********************************************************

%% If last line of paste is hypenated, then we 
%% drop it from paste

%% Need to deal with li, checkbox, etc. 
%% We can do this in line_specs by passing Tag; making line_specs/2
%% line_specs/3

%% Need to allocate pixel penalty
%% Penalty = moving_cost/3
%% E.g. {-6, 3}
%% Given Penalty, allocate across lines

%% Make penalty list [1, 1, 1, 0, 0]
%% Where 1s represent lines with leading increased or decreased
%% And 0s represent no change to leading
%% If abolute value of penalty is greater than length(Content)
%% Then [2,2,2,1,1]
%% Revise paste/4, paste_up/4, pdf_code/4 to allocate penalty


%% @doc Break lines 
%% Needs more analyasis to generalize 


    %  PanelMap1 = paste_up(PDF, Paste, XML, Beads),
    % MoreBeads = tl(Beads),
    % Beads1 = [PanelMap1 | MoreBeads], 


%% ********************************************************************
%% dispatch/6 - Dispatch lines for processing by tag 
%% ********************************************************************


%% @doc Dispatch lines for processing by tag

% -spec dispatch(PDF  ::  identifier(),
%               Job  ::  map(),
%               Paste :: list(),
%               XML   :: list(),
%               Beads :: list()) -> tuple().

% dispatch(PDF, Job,  Paste, XML, Beads)   ->
%        io:format("^^^^^^^^^^^^^^^^^^^^^^^^^^^ Entering dispatch/4~n~n"),
%        FirstLine = get_first_line(XML),
%        io:format("++++++++ first_line: ~p~n", [FirstLine]),
%        io:format("++++++++ Length Paste: ~p~n~n", [length(Paste)]),
%   Tag = get_tag(XML),
%   case Tag of
%      p   -> {XML1, Beads1} = paste_paragraph(PDF, Paste, XML, Beads),
%             Recurse = recurse;
%      h3  -> A = paste_h3(PDF, Job, XML, Beads),
%             io:format("Review A: ~p~n~n", [A]),
             
%             {XML1, Beads1} = paste_h3(PDF, Job, XML, Beads),
%             Recurse = recurse;

%      _   -> XML1 = XML,
%             Beads1 = Beads,
%             Recurse = stumped 
%   end,
%        io:format("^^^^^^^^^^^^^^^^^^^^^^^^^^^ Exiting dispatch/4~n~n"),
%   {Recurse, XML1, Beads1}.


% spill(PDF, Job,  Paste, Spill, XML, Beads) ->
       % paste panel
       % get next panel
%   {PanelMap, Beads1} = paste_up(PDF, Paste, XML, Beads),
   % update rich text

%   RichText  = rich_text(XML, Beads),
%        io:format("spill/6 - RichText: ~p~n~n", [RichText]),
%        io:format("spill/6 - Paste: ~p~n~n", [Paste]),
%   RichText1  = trim_richText(Paste, RichText),
%        io:format("spill/6 - RichText1: ~p~n~n", [RichText1]),
   
%   Lines                  = get_lines(RichText1, XML, Beads1),
%   io:format("spill/6 - Lines: ~p~n~n", [Lines]),
%   Vacancies              = vacancies(XML, Beads1),
%   io:format("spill/6 - Vacancies: ~p~n~n", [Vacancies]),

   % recurse 
%   {recurse, XML, Beads1}. 

  

%   {_PM | MoreBeads] = Beads1,
%% ********************************************************************
%% paste_paragraph/5 - Paste up paragraph 
%% ********************************************************************


%% @doc Paste up paragraph

% -spec paste_paragraph(PDF    :: identifier(),
%                      Paste  :: list(),
%                      XML    :: list(),
%                      Beads  :: list()) -> tuple().
 
% paste_paragraph(PDF, Paste, XML, Beads) ->
%   {PanelMap, Beads1} = paste_up(PDF, Paste, XML, Beads),
   
   % [_PanelMap | Beads1] = Beads,

   % RichText  = rich_text(XML, Beads),
   % RichText1 = trim_richText(Paste, RichText),
%   {XML, Beads1}.



%    Cost1 = Cost - LineCount,
%    if  Cost1 =< 0 -> Cost2 = 0;
%        Cost1 < 0  -> Cost2 = tCost
%    end,
%    Cost2;

% reduce_cost(Cost, _Lines) when Cost == 0 ->
%    Cost;

% reduce_cost(Cost, Lines) when Cost < 0 ->
%    LineCount = length(Lines),
%    Cost1 = Cost + LineCount,
%    if  Cost1 >= 0 -> Cost2 = 0;
%        Cost1 < 0  -> Cost2 = Cost1
%    end,
%    Cost2.
       




%% **********************************************************
%% This is old paste-up routing. It will be replaced
%% **********************************************************

%% @doc Return all lines that fit in current panel

% panel_content(XML, Beads) ->
%    PDF = eg_pdf:new(),
%    panel_content(PDF, XML, Beads, true).

% panel_content(PDF, _XML, [], true) ->
%    save(PDF),
%    io:format("Out of beads. Done!~n~n");

% panel_content(PDF, [], _Beads, true) ->
%    save(PDF),
%    io:format("Out of copy. Done!~n~n");

% panel_content(PDF, XML, Beads, false) ->
%    io:format("!!!!!!!!!!!!!!!!!!!! Current panel is out of space. On to next panel!~n~n"),
%    [PanelMap | Beads1] = Beads,
%    io:format(" XML: ~p~n", [XML]),
%    io:format(" Beads1: ~p~n", [Beads1]),
%    panel_content(PDF, XML, Beads1, true);

% panel_content(PDF, XML, Beads, Continue) ->
%   io:format(" ===========================================Entering panel_content/4~n~n"),
%   Vacancies          =vacancies(XML, Beads), 
%   Lines = xml2lines(XML, Beads),
%   ContinueOn         = Vacancies >= length(Lines),
%   io:format(" ContinueOn: ~p~n", [ContinueOn]),
%   case ContinueOn of
%      true   -> 
%         PanelMap     = paste_up(PDF, Lines, XML, Beads),
%         Beads1       = revise_beads(PanelMap, Beads), 
%         XML1         = revise_xml(XML),
%         io:format(" =======================================================recurse~n~n"),
%         panel_content(PDF, XML1, Beads1, ContinueOn);
%       false -> 
%         panel_content(PDF, XML, Beads, ContinueOn)   
%     end.

 
      

%% Next step we'll need to;
%% Deduct pixels from Paste,
%% Paste it up
%% Re-fit Leave
%% And recurse to next panel     


% pixel_penalties(LineCount, Content, XML, Beads) ->
%    {Paste, _Leave} = move_up_lines(LineCount, Content, XML, Beads),
%    {Cost, _Count} = move_up_cost(LineCount, Content, XML, Beads),
%    TotalLines      = count_lines_in_panel(Paste).
    
    
%% Need to adjust cost based on number of lines
%% Also need to limit limit imposition of cost
%% And revise update_next_line




%% @doc Attempt to move subhead to next panel and increase leading 
%%      in items above


% trying_head_solution_one(Content, XML, Beads) ->
%   Tag          = last_tag(Content),  
%   MaxLineSpace = ep_typestyle:report_max_linespace(Tag),
%   Leading      = ep_typestyle:report_leading(Tag),
%   Gap          = gap(Content, Beads),
%   TotalLines   = total_lines(Content),
%   Adjust       = (Leading + Gap) / TotalLines,
%   Flag         = Adjust =< MaxLineSpace,
%   case Flag of
%      true  -> io:format("* Solution 1 FAILS~n"), 
%               io:format("* Creates excessive space between lines~n"), 
%               io:format("********************************************~n"),
%               trying_head_solution_two(Content, XML, Beads),
%               io:format("********************************************~n");
%      false -> io:format(" SUCCESS!~n"),
%               io:format(" Testing next panel~n"),
%               io:format("********************************************~n")
%   end.


%% @doc Trying to reduce leading in lines above and bring up 
%%      one or more lines from next panel


% trying_head_solution_two(Content, XML, Beads) ->
%   Flag  = can_move_lines(Content, XML, Beads),
%   case Flag of
%      true  -> io:format(" TRYING Solution 2~n"),
%               io:format("********************************************~n"),
%               io:format("* We can move lines up!~n"), 
%               io:format("* Let's try it~n"),
%               move_lines(Content, XML, Beads);
%      false -> io:format("* Solution 2 FAILS~n"), 
%               io:format("* Creates excessive space between lines~n"), 
%               io:format("* Requires design/editorial solution ~n"),
%               io:format("********************************************~n")
%    end.


% can_move_lines(Content, XML, Beads) ->
%   CanSqueezeIn  = can_squeeze_in(Content, Beads),
%   Available     = movable_lines_below_break(XML, Beads),
%   Available > 0.


% move_lines(Content, XML, Beads) ->
%   CanSqueezeIn  = can_squeeze_in(Content, Beads),
%   Available     = movable_lines_below_break(XML, Beads),
%   CanMove       = Available - CanSqueezeIn,
%   Flag = CanMove > 0,
%   case Flag of
%      true  -> io:format("* Movable lines: ~p ~n", [CanMove]);
%      false -> io:format("* Solution 2 FAILS~n"), 
%               io:format("* No lines to move ~n"), 
%               io:format("********************************************~n")
%    end.


%% Move lines up

%% Is the first element in the next panel a paragraph?
%% Yes
%%     How many lines are available?
%%     How many do we need?


%% ***********************************************************
%% ***********************************************************
%% Proof sensors and helpers 
%% ***********************************************************
%% ***********************************************************



%% @doc Return number of lines that fit in panel 

% -spec vacancies(XML :: list(),
%                Beads :: list()) -> integer().

% vacancies(XML, Beads) ->
%   Tag               = get_tag(XML),
%   PanelMap          = get_panelmap(Beads),
%   vacant_lines(Tag, PanelMap).
   

% vacant_lines(Tag, PanelMap) ->
%   TypeStyle = ep_panel:get_typestyle(PanelMap),
%   ep_panel:get_nlines(TypeStyle, Tag, PanelMap).


% get_tag(XML) ->
%   Xml = get_xml(XML),
%   element(1, Xml).





% update_panel(Lines, XML, Beads) ->
%   Tag      = get_tag(XML),
%   PanelMap = get_panelmap(Beads),
%   update_next_line(Tag, Lines, PanelMap).








% panel_height(Beads) ->
%   [PanelMap | _MoreBeads] = Beads,
%   {_W, Height}            = ep_panel:get_size(PanelMap),
%   Height.


% gap(Content, Beads) ->
%   PanelHeight   = panel_height(Beads),
%   Consumed      = total_consumed(Content),
%   PanelHeight - Consumed.

   
last_tag(Content) ->   
   Element       = hd(Content), 
   element(1, Element).

   
is_widow(Content) ->
   Element       = hd(Content), 
   Lines         = element(2, Element),
   Line          = hd(Lines),
   RichTokens    = element(2, Line),
   length(RichTokens) < ?WIDOW.


is_hyphenated(Content) ->
   Element       = hd(Content), 
   RichTokens    = element(2, Element),
   LastRichToken = lists:last(RichTokens),
   RichTokenSize = size(LastRichToken),
   case RichTokenSize of
      4  -> Token = element(4, LastRichToken),
            Hyphenated  = token_is_hyphenated(Token);
      _  -> Hyphenated  = false
   end,
   Hyphenated.


panel_below_break(Beads) ->
   [_P | MoreBeads] = Beads,
   name(MoreBeads).
          


%% NOTE: Use output of proof_panel/2


movable_lines_below_break(XML, Beads) ->
   Tag = tag_below_break(XML),
   case Tag of
      p   -> Lines = lines_below_break(XML, Beads),
             length(Lines);
      _   -> 0
   end.


tag_below_break(XML) ->
  Element = hd(XML),
  element(1, element(2, Element)).
%% ***********************************************************
%% move_up_lines/4 helpers 
%% ***********************************************************




%% ********************************************************************
%% get_lines/3 - Transform xml to lines to fit panel
%% ********************************************************************


%% ********************************************************************
%% allocate_lines/2 - Split lines to fit panel









%% ***********************************************************


%% ********************************************************************
%% vacanies/2 -- Return number of lines that fit in current panel 
%% ********************************************************************






%% ***********************************************************
%% ***********************************************************
%% Paste panels 
%%
%% NOTE: Beads are simply a list of panels
%% ***********************************************************
%% ***********************************************************


% paste_up_panel(XML, Beads) ->
%   PDF       = eg_pdf:new(),
%   Job       = ep_job:create("Trial Paste", "LRP"),
%   {Content, XML1, Beads1} = proof_panel(XML, Beads),

%   Tag                     = get_tag(XML1),
%   Gap                     = gap(Content, Beads1),

%   CanSqueezeIn            = can_squeeze_in(Content, Beads),

%   Available               = movable_lines_below_break(XML1, Beads1),

%   {Cost, LineCount}       = cost_of_moving(Tag, Gap, CanSqueezeIn),
%   {Paste, Cost, Spill}    = move_up_lines(LineCount, Content, Xml, PanelMap),
%   [PanelMap | MoreBeads]  = Beads,
%   Paste1                  = lists:reverse(Paste),
%   ok                      = paste(PDF, Paste1, Cost, PanelMap),
   % trial_paste(Paste, Cost, PanelMap),
%   [PanelMap1 | MB]        = MoreBeads,
%   Lines                   = recompute_spill(Tag, PanelMap1, Spill),
%   Paste2                  = {Tag, Lines},
%   paste(PDF, Paste2, 0, PanelMap1),
%   OutFile   = "./pdf/galleys/" ++ ?MODULE_STRING ++ ".trial.pdf",
%   save(PDF, OutFile),
%   {MB}.


   
%% ***********************************************************
%% Trial paste lines  
%% ***********************************************************




% paste(PDF, Paste, Cost, PanelMap)



% proof_panel(XML, Beads) -- {Content, XML1, Beads1}
%       NOTE: Content is a list of RichText lines

% get_tag(XML1) --- Tag

% gap(Content, Beads1) --- Gap

%   CanSqueezeIn  = can_squeeze_in(Content, Beads),
%   Available     = movable_lines_below_break(XML, Beads),

 
% cost_of_moving(Tag, Gap, CanSqueezeIn) --- {Cost, LineCount1}

% move_up_lines(LineCount, Content, XML1, Beads1) --- {Paste, Cost, Leave} 

% trial_paste(Paste, Cost, PanelMap) --- ok 
% paste(PDF, Paste, Cost, PanelMap) --- ok

















%% @doc Paste panels 

% -spec paste_panels(PDF :: identifier(),
%                   Job :: map(),
%                   XML       :: list(),
%                   Beads     :: list()) -> none().

% paste_panels(_PDF, _Job, [], _Beads) ->
%   io:format("Out of copy!~n~n"),
%   article();

% paste_panels(_PDF, _Job, _XML, []) ->
%   io:format("Out of beads!~n~n"),
%   article();

% paste_panels(PDF, Job, XML, Beads) ->
%   RichText               = rich_text(XML, Beads),
%   Lines                  = get_lines(RichText, XML, Beads),
%   Vacancies              = vacancies(XML, Beads),
%   {Paste, Spill, Action} = allocate_lines(Lines, Vacancies), 
%   io:format("!!!!!!!!!! Action: ~p~n", [Action]),
%    case Action of
%      paste    ->  {Recurse, XML1, Beads1} = paste(PDF, Paste, XML, Beads);
%      dispatch ->  {Recurse, XML1, Beads1} = dispatch(PDF, Job,  Paste, XML, Beads);
%      spill    ->  {Recurse, XML1, Beads1} = spill(PDF, Job, Paste, Spill, XML, Beads)
%    end,
%    case Recurse of
%       recurse -> paste_panels(PDF, Job, XML1, Beads1);
%       stumped -> io:format("I'm stumped~~n~n")
%    end.






%% ********************************************************************
%% paste/4 - Paste lines into panel 
%% ********************************************************************

%% @doc Paste lines into panel

% -spec paste(PDF :: identifier(),
%            Paste :: list(),
%            XML   :: list(),
%            Beads :: list()) -> tuple().

% paste(PDF, Paste, XML, Beads)   ->
%        io:format("========================== Entering paste/4~n~n"),
%        FirstLine = get_first_line(XML),
%        io:format("++++++++ first_line: ~p~n", [FirstLine]),
%        io:format("++++++++ Length Paste: ~p~n~n", [length(Paste)]),
%    {PanelMap, Beads1} = paste_up(PDF, Paste, XML, Beads),




%    [_Xml | MoreXML ] = XML,
%        io:format("============================ Exiting paste/4~n~n"),
%    {recurse, MoreXML, Beads1}. 

% -spec pdf_code(PDF            :: identifier(),

%% @doc Transform lines to PDF code

% -spec pdf_code(PDF            :: identifier(),
%               Lines          :: list(),
%               XML            :: list(),
%               Beads          :: list()) -> string().


% pdf_code(PDF, Lines, XML, Beads) ->
%       io:format("----------- Entering pdf_code/5~n"),
%   Tag               = get_tag(XML),
%   PanelMap          = get_panelmap(Beads),
       % Name = ep_panel:get_panel_name(PanelMap),
       % io:format("pdf_code/5 - PanelMap: ~p~n", [Name]),
%   {TextX, TextY}    = ep_panel:get_text_position(PanelMap),
%   Justify           = ep_typestyle:report_justify(Tag),
%   Leading           = ep_typestyle:report_leading(Tag),
%   Vacancies         = vacancies(XML, Beads),
%   {Widths, Offsets} = line_specs(Tag, PanelMap, Vacancies),
%   ep_richText2pdf:richText2pdf(PDF, TextX, TextY, Justify, 0, Lines, Leading, Widths, Offsets).



% gap(Tag, Leading, Vacancies) ->
%   {Tag, Leading * Vacancies}.


%% Helpers



% vacancies(XML, Beads) ->
%   vacancies(XML, Beads),
%   {Vacancies, Lines}.



% break_lines(Vacancies, Lines) ->
%   ContinueOn = length(Lines) > Vacancies,
%   case ContinueOn of
%      true  -> Vacancies;
%      false -> ok
%   end.
     






% paste_up(PDF, Lines, XML, Beads)
% snip_last_element(PanelContnet),



% revise_beads(PanelMap, Beads) -> 
% [_PM | MoreBeads] = Beads,
%   Beads1 = [PanelMap | MoreBeads]. 

%% @doc Given panel content, we need apply jump rules.

%% Jump rules
%% If last line is a head, move it next panel,




%% @doc If tag of last item is not p, strip last_element from
   



  


%% Snip first element of content
%% Prepend it to XML1 






% gap(PanelContent) ->
%   Content = element(1, PanelContent),
%   LastItem = hd(Content),
%   {Tag, Leading, Vacancies, _} = LastItem,
%   {Tag, Leading * Vacancies}.

%   {PanelMap, Beads1}. 

% panel_content/2 now returns:
%  [{Tag, XML, Lines},
%    ...
%  ]

% We can test if element(1, hd(PanelContent)) /= p
% If true
%      drop it
%      reverse the list
%      paste
%      snap XML
%      recurse  

% At some point, need to:
% Measure gap, distribute through leading


%    Vacancy  = VacantLines > length(Lines),


%   Vacancy           = paste_rules(Tag, Lines, VacantLines), 
%   case Vacancy of
%      true  -> fits;
%      false -> spills 
%   end.
  
% Need fits, break, spills

% if p and last line hyphenated, 
% if p -> VacantLines >= length(Lines)
% if _ -> VacantLines > (2 * length(Lines))





%   Vacancy.
%   case Tag of
%      p  = 


%% ***********************************************************************
%% Work space
%% ***********************************************************************

%% Working on paste_lines


% paste_lines(PDF, _Job, Lines, XML, Beads, fits)   ->
%    io:format("=============Entering paste_lines5 - fits~n~n"),
%        FirstLine = get_first_line(XML),
%    io:format("=============first_line: ~p~n", [FirstLine]),
%    Tag       = get_tag(XML),
%    PanelMap  = get_panelmap(Beads),
%    MoreBeads = tl(Beads),
%    TypeStyle = ep_panel:get_typestyle(PanelMap),

%    PanelMap1 = paste_up(PDF, TypeStyle, Tag, Lines, PanelMap),
%    Beads1 = [PanelMap1 | MoreBeads], 
%    MoreXML = tl(XML),
%    {MoreXML, Beads1}; 

% paste_lines(PDF, Job, Lines, XML, Beads, spills) ->
%    io:format("=============Entering paste_lines5 - spills~n~n"),
%        FirstLine = get_first_line(XML),
%    io:format("=============first_line: ~p~n", [FirstLine]),

   % Paste what we can
%   Tag           = get_tag(XML),
%   [PanelMap | MoreBeads] = Beads,   
%       Name = ep_panel:get_panel_name(PanelMap),
%      io:format("========== PanelMap - Name: ~p~n~n", [Name]),
%   TypeStyle      = ep_panel:get_typestyle(PanelMap),





% allocate_lines(Lines, Vacancies) -> 
%   LengthLines = length(Lines),
%   Vacancy = Vacancies - LengthLines,
%   if
%      Vacancy >= LengthLines -> Paste = Lines,
%                                Spill = [];
%      Vacancy < LengthLines  -> {Paste, Spill} = lists:split(VacantLines, Lines)
%   end,
%   {Paste, Spill}. 
 
      

% paste_rules(Paste, Spill, RichText) ->
%   case Spill == [] of
%      true  -> paste;
%      false -> dispatch
%   end. 



% paste(PDF, _Job,  Paste, XML, Beads)   ->
%    io:format("=============Entering paste_lines5 - fits~n~n"),
%        FirstLine = get_first_line(XML),
%    io:format("=============first_line: ~p~n", [FirstLine]),
%    Tag       = get_tag(XML),
%    PanelMap  = get_panelmap(Beads),
%    MoreBeads = tl(Beads),
%    TypeStyle = ep_panel:get_typestyle(PanelMap),

%    PanelMap1 = paste_up(PDF, TypeStyle, Tag, Paste, PanelMap),
%    Beads1 = [PanelMap1 | MoreBeads], 
%    MoreXML = tl(XML),
%    {MoreXML, Beads1}. 





%    Vacancy  = VacantLines > length(Lines),


%   Vacancy           = paste_rules(Tag, Lines, VacantLines), 
%   case Vacancy of
%      true  -> fits;
%      false -> spills 
%   end.
  
% Need fits, break, spills

% if p and last line hyphenated, 
% if p -> VacantLines >= length(Lines)
% if _ -> VacantLines > (2 * length(Lines))





%   Vacancy.
%   case Tag of
%      p  = 


%% ***********************************************************************
%% Work space
%% ***********************************************************************

%% Working on paste_lines


% paste_lines(PDF, _Job, Lines, XML, Beads, fits)   ->
%    io:format("=============Entering paste_lines5 - fits~n~n"),
%        FirstLine = get_first_line(XML),
%    io:format("=============first_line: ~p~n", [FirstLine]),
%    Tag       = get_tag(XML),
%    PanelMap  = get_panelmap(Beads),
%    MoreBeads = tl(Beads),
%    TypeStyle = ep_panel:get_typestyle(PanelMap),

%    PanelMap1 = paste_up(PDF, Lines, XML, Beads),
%    Beads1 = [PanelMap1 | MoreBeads], 
%    MoreXML = tl(XML),
%    {MoreXML, Beads1}; 

% paste_lines(PDF, Job, Lines, XML, Beads, spills) ->
%    io:format("=============Entering paste_lines5 - spills~n~n"),
%        FirstLine = get_first_line(XML),
%    io:format("=============first_line: ~p~n", [FirstLine]),

%   % Paste what we can
%   Tag           = get_tag(XML),
%   [PanelMap | MoreBeads] = Beads,   
%       Name = ep_panel:get_panel_name(PanelMap),
%      io:format("========== PanelMap - Name: ~p~n~n", [Name]),
%   TypeStyle      = ep_panel:get_typestyle(PanelMap),
%   Vacancies    = vacancies(XML, Beads),
%       io:format("Vacancies: ~p~n", [Vacancies]),
%   {Paste, Spill} = lists:split(Vacancies, Lines),


%         io:format("++++++++ Paste: ~p~n~n", [Paste]),
         % Lengths = [length(Line) || Line <- Paste],
         % Tokens  = lists:sum(Lengths),
         % io:format("++++++++ Tokens: ~p~n~n", [Tokens]),





%    paste_up(PDF, TypeStyle, Tag, Paste, PanelMap),

   % Paste spill 

%   [PanelMap1 | _MoreBeads1] = MoreBeads,
%       Name1 = ep_panel:get_panel_name(PanelMap1),
%       io:format("========== PanelMap - Name1: ~p~n~n", [Name1]),
%   io:format("######### paste_lines - PanelMap1: ~p~n~n", [ PanelMap1]),
%   ep_panel:panel(PDF, Job, PanelMap1), 
%   io:format("######### paste_lines - Spill: ~p~n~n", [Spill]),
%   case Spill > 0 of
%      true  -> paste_up(PDF, TypeStyle, Tag, Spill, PanelMap1);
%      false -> ok
%   end,
%   XML1 = tl(XML),
%   {XML1, MoreBeads}.
   




%% @doc Useful function for debugging


%% We can paste li and checkbox elements here


%% ********************************************************************
%% Return line widths and offsets for a given panel 
%% ********************************************************************

% copy_specs(TypeStyle, Tag, PanelMap) ->
%   VacantLines     = get_vacantlines(TypeStyle, Tag, PanelMap),
%   Indent          = ep_typestyle:report_indent(Tag),
%   Measure         = ep_panel:get_measure(PanelMap),
%   Margin          = ep_panel:get_margin(PanelMap),
%   Widths          = ep_block:widths(Indent, Measure, VacantLines),
%   Offsets         = ep_block:offsets(Indent, Margin, VacantLines),
%   {Widths, Offsets} = ep_block:line_specs



%% If we pass in tag, we can adjust specs for li, checkbox, etc 
%% We do this by increasing margin and decreasing measure

 






%% ********************************************************************
%% Transform lines to PDF code
%% ********************************************************************

   




%% @doc Dispatch paste


% dispatch_paste(Paste, RichText) ->
  









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
%   VacantLines  = get_vacant_lines(TypeStyle, Tag, PanelMap),
%   Vacant       = VacantLines > length(Lines),
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
%     VacantLines     = get_vacant_lines(TypeStyle, Tag, PanelMap),
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
%   VacantLines     = get_vacant_lines(TypeStyle, Tag, PanelMap),
%   Pastable        = VacantLines - length(Lines),
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

% chop_richText(RichText, Lines, VacantLines) ->
%   io:format("copy_richText/3 - RichText: ~p~n~n", [RichText]),
%   Counts = [size(Line) || Line <- Lines],
%   CountList = lists:sublist(Counts, VacantLines),
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
    





