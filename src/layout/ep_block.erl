%%==========================================================================
%% ep_block.erl
%% Copyright (C) 2016 Lloyd R. Prentice
%%
%% Extensive modifications based on erlguten eg_block.erl:
%% 
%%  Copyright (C) 2004 Sean Hinde
%%  Copyright (C) 2010 Carl Wright
%%
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
%%
%% Authors: Lloyd R. Prentice <lloyd@writersglen.com>, 
%%          Sean Hinde <sean.hinde@mac.com>, 
%%          Carl Wright <wright@servicelevel.net>
%% Purpose: API call to paste up copy in a page grid.
%%==========================================================================


-module (ep_block).

-export([block/3, line_specs/2, normalize/2 ]).
-export([delete_nl/1, parse/1, tag/1, get_tag/1]).
-export([parsed_text2xml/1]).
% -export([block/11]).
 
-include("../include/eg.hrl").


%% 
%% @doc block() processes one or more paragraphs correctly into PDF content
%%
%%  parameters to control its output
%%
%% PDF  = the PID of pdf process for the document
%%
%% Color  = the color of the box that the block of text is in
%%
%%  Sample  = the content to format as a block of text i.e. "<p> a para with <em> big </em> thoughts </p>"
%%
%%  X = the X coordinate of the top left corner of the block of text
%%
%%  Y = the Y coordinate of the top left corner of the block of text 
%%
%%  Measure = the width of the block holding the text in points
%%
%%  PtSize  = the size in points of the font
%%
%%  Leading = the distance in points from the bottom of one line to the bottom of the next 
%%
%%  Vacancies  = the number of lines allowed in text box 
%%
%%  Justification = an atom expressing the text justification required ()
%%
%%  TagMap  = the tagmap describing how to handle differnet XML tags
%%
%% the tag map is formatted like the following:
%%
%% <pre>
%% default_tagMap(Pts) -> 
%%     {[p],
%%      [{default,eg_richText:mk_face("Times-Roman", Pts, true, default, 0)}, 
%%       {em,     eg_richText:mk_face("Times-Italic", Pts, true, default, 0)}, 
%%       {red,    eg_richText:mk_face("ZapfChancery-MediumItalic", Pts, true,  {1,0,0},0)},  
%%       {blue,   eg_richText:mk_face("ZapfChancery-MediumItalic", Pts, true, {0,0,1},0)},  
%%       {code,   eg_richText:mk_face("Courier", Pts, false, default, 0)},  
%%       {b,      eg_richText:mk_face("Times-Bold", Pts, true, default, 0)}, 
%%       {hb,     eg_richText:mk_face("Helvetica-Bold", Pts, true, default, 0)},  
%%       {helv,   eg_richText:mk_face("Helvetica", Pts, true, default, 0)}  
%%      ]}.
%% </pre>


%% process an XML block of content into a block of PDF text with a color background

wrap_copy(WrapFlag, Copy) ->
    case WrapFlag of
       wrap    -> Copy; % "<p>" ++ Copy ++ "</p>";
       no_wrap -> Copy
    end. 
   


block(PDF, Copy, Boxes) ->
   {WrapFlag, TypeSpec} = ep_typespec:typespec(Copy),
   {_, Text} = Copy,
   Text1 = wrap_copy(WrapFlag, Text),
   block(PDF, Text1, TypeSpec, Boxes).

%% ************************************************************
%% Block/3 helpers 
%% ************************************************************

block(PDF, FileName, TypeSpec, Boxes) ->
   % Result = eg_xml_lite:parse_all_forms(Copy),
   ParsedText = md_parse:parse(FileName),

   ParsedText1 = parsed_text2xml(ParsedText),   
   Result = [{xml, {p, [], ParsedText1}}],
   case Result of
       [{xml, Xml}] -> block2(PDF, [{xml, Xml}], TypeSpec, Boxes);
%       [{raw, Xml}] -> block2(PDF, [{xml, Xml}], TypeSpec, Boxes);
%       Error        -> {error, Error}
       _        -> io:format("Error: ~p~n", [Result]) 
   end.


parsed_text2xml(ParsedText) ->
   %% Assumes parsed by md_parse.erl
   [convert2xml(Span) || Span <- ParsedText].

convert2xml(Span) ->
   {Tag, Text}    = Span,      %% This assumes md_parse output
   case Tag of
      p    -> {raw, Text};
      h1   -> {h1, Text};
      em   -> {em, [], [{raw, Text}]};
      b    -> {b, [], [{raw, Text}]};
      code -> {code, [], [{raw, Text}]};
      _    -> {error}
   end.


block2(PDF, [{xml, Xml}], TypeSpec, Boxes) ->
   [Box | _] = Boxes,
   {Justification, TagMap} = type_params(TypeSpec),
%   Justification = ep_typespec:justification(TypeSpec),
%   TagMap        = ep_typespec:tag_map(TypeSpec),
    ensure_fonts_are_loaded(PDF, TagMap),

    Norm = eg_xml2richText:normalise_xml(Xml, TagMap),




    {p, _, RichText} = Norm,
   Vacancies = ep_panel:available_lines(Box, TypeSpec),

   {Widths, _Off} = line_specs(Box, Vacancies),
   case ep_line_break:break_richText(RichText, { Justification, Widths}) of
        impossible  -> {error, cannot_break_line};
        {Lines,_,_} -> paste_up(PDF, Boxes, Lines, TypeSpec, [])
   end.

%% block2/4 helpers

type_params(TypeSpec) ->
   Justification = ep_typespec:justification(TypeSpec),
   TagMap        = ep_typespec:tagmap(TypeSpec),
   {Justification, TagMap}.

ensure_fonts_are_loaded(PDF, {_,TagMap}) ->
    lists:foreach(fun({_,Face}) ->
                          FontHandler = eg_richText:fontFromFace(Face),
                          Font = FontHandler:fontName(),
                          eg_pdf:ensure_font_gets_loaded(PDF, Font)
    end, TagMap).





normalize(Xml, TagMap) ->
    Norm = eg_xml2richText:normalise_xml(Xml, TagMap),
%    io:format("Norm: ~p~n~n", [Norm]),
    Norm.






line_specs(Box, Vacancies) ->
    {_, _, Measure, _} = ep_box:text_box(Box),
%    {_, _, Measure, _} = ep_panel:text_box(Box),
    Indent = ep_box:indent(Box),
%    Indent = ep_panel:indent(Box),
    Widths = [Measure - Indent|lists:duplicate(Vacancies - 1, Measure)],
    Off = [Indent|lists:duplicate(Vacancies - 1, 0)],
    {Widths, Off}.

paste_up(PDF, Boxes, Lines, TypeSpec, CodeList) ->
            [Box | JumpList] = Boxes,
            Vacancies = ep_panel:available_lines(Box, TypeSpec),
            {Lines1, MoreLines} = split_lines(Lines, Vacancies),
            % Box1 = ep_box:dec_lines(length(Lines1), Box, TypeSpec),
            Code = ep_richText2pdf:richText2pdf(PDF, Lines1, Box, TypeSpec),
            CodeList1 = [Code | CodeList],
            case MoreLines == [] of
               true  -> box_to_pdf(PDF, Box, CodeList1);
               false -> box_to_pdf(PDF, Box, CodeList1),
                        paste_up(PDF, JumpList, MoreLines, TypeSpec, CodeList1)
            end.

%% paste_up helpers

split_lines(Lines, Vacancies) ->
    Flag = length(Lines) > Vacancies,
    case Flag of
        true  -> lists:split(Vacancies, Lines);
        false -> {Lines, []}
    end.



box_to_pdf(PDF, Box, CodeList) ->
   Flag = ep_panel:if_background(Box),
   case Flag of
      true  -> render_box(PDF, Box),
               render_text(PDF, CodeList);
      false -> render_text(PDF, CodeList)
   end.




%% box_to_pdf/3 helpers

render_box(PDF, Box) ->
   {LineWidth, Stroke, StrokeColor, FillColor} = ep_panel:background(Box),
   {X, Y} = ep_box:position(Box),
   {W, H} = ep_box:dimensions(Box),
    eg_pdf:set_line_width(PDF, LineWidth),
    eg_pdf:set_stroke_color(PDF, StrokeColor),
    eg_pdf:set_fill_color(PDF, FillColor),
    eg_pdf:rectangle(PDF,{X, Y},{W,H}),
    eg_pdf:path(PDF, Stroke),
    eg_pdf:set_fill_color(PDF, black).

render_text(PDF, CodeList) ->
            lists:reverse(CodeList),
            eg_pdf:begin_text(PDF),

            [eg_pdf:append_stream(PDF, Code) || Code <- CodeList],
            eg_pdf:end_text(PDF).

%% *****************************************

delete_nl(Copy) ->
    re:replace(Copy, [$\n], [$\s], [global, {return, list}]).

parse(Copy) ->
   Copy1 = re:replace(Copy, [$\n], [$\s], [global, {return, list}]),
   eg_xml_lite:parse_all_forms(Copy1).

tag(Item) ->
   {_,{Tag, _, Text}} = Item,
   {Tag, Text}.

get_tag(Item) ->
   {_,{Tag, _, _}} = Item,
   Tag.





% block(PDF, Color, Sample, X, Y, Measure, PtSize, Leading, Vacancies, Justification, TagMap) ->
%    Width = Measure + 20,
%    Ht = (Vacancies * Leading) + 20,
%    box(PDF, Color, X, Y-Ht+10, Width, Ht),
%    block(PDF, Sample, X+10, Y , Measure, PtSize, Leading, Vacancies, Justification, TagMap).

%% @doc process a parsed XML block of content into a block of PDf text with a color background 
   
% colored_inner_block(PDF, Color, Sample, X, Y, Measure, PtSize, Leading, Vacancies, Justification, TagMap) ->
%    Width = Measure + 20,
%    Ht = (Vacancies * Leading) + 20,
%    box(PDF, Color, X, Y-Ht+10, Width, Ht),
%    inner_block(PDF, Sample, X+10, Y-10, Measure, PtSize, Leading, Vacancies, Justification, TagMap).

%% @doc process an XML block of content into a block of PDf text with a blank background
       
% block(PDF, Sample, X, Y, Measure, PtSize, Leading, Vacancies, Justification, TagMap) ->
%    inner_block(PDF, eg_xml_lite:parse_all_forms(Sample),X, Y, Measure, PtSize, Leading, Vacancies, Justification, TagMap).

%% @doc process a parsed XML block of content into a block of PDf text with a blank background

% inner_block(PDF, [{raw, Xml}], X, Y, Len, PtSize, Leading, Vacancies, Justification, TagMap) ->
%   block2(PDF, [{xml, Xml}], X, Y, Len, PtSize, Leading, Vacancies, Justification, TagMap),
%   ok;
% inner_block(PDF, [{xml, Xml}], X, Y, Len, PtSize, Leading, Vacancies, Justification, TagMap) ->
%   block2(PDF, [{xml, Xml}], X, Y, Len, PtSize, Leading, Vacancies, Justification, TagMap),
%   ok;
% inner_block(PDF, [{xml, Xml} | T], X, Y, Len, PtSize, Leading, Vacancies, Justification, TagMap) ->
%  Height = block2(PDF, [{xml, Xml}], X, Y, Len, PtSize, Leading, Vacancies, Justification, TagMap),
%   inner_block(PDF, T, X, Y - Height, Len, PtSize, Leading, Vacancies, Justification, TagMap).

    
% block2(PDF, [{xml, Xml}], X, Y, Len, _PtSize, Leading, Vacancies, Justification, TagMap) ->
%    ensure_fonts_are_loaded(PDF, TagMap),
%    Norm = eg_xml2richText:normalise_xml(Xml, TagMap),
    %% io:format("Norm=~p~n",[Norm]),
%    {p, _, RichText} = Norm,
%    Widths = [Len-20|lists:duplicate(Vacancies-1, Len)],
%    Off = [20|lists:duplicate(Vacancies-1, 0)],
%    case eg_line_break:break_richText(RichText, { Justification, Widths}) of
%	impossible ->
%	    io:format("Cannot break line are widths ok~n");
%	{Lines,_,_} ->

 %           {Lines1, _} = lists:split(Vacancies, Lines),

  %          io:format("Lines1: ~p~n", [Lines1]),
%	    Code = eg_richText2pdf:richText2pdf(PDF, X, Y, justified, 0, Lines1, 
%						Leading, Widths, Off),
%	    eg_pdf:begin_text(PDF),
%	    eg_pdf:append_stream(PDF, Code),
%	    eg_pdf:end_text(PDF),
%	    length(Lines) * Leading

 %   end.  
    

% box(PDF, Color, X, Y, W, H) ->
%    eg_pdf:set_fill_color(PDF, Color), 
%    eg_pdf:rectangle(PDF,{X, Y},{W,H}),
%    eg_pdf:path(PDF,fill),
%    eg_pdf:set_fill_color(PDF,black).
    

% ensure_fonts_are_loaded(PDF, {_,TagMap}) ->
%    lists:foreach(fun({_,Face}) ->
%			  FontHandler = eg_richText:fontFromFace(Face),
%			  Font = FontHandler:fontName(),
%			  eg_pdf:ensure_font_gets_loaded(PDF, Font)
% end, TagMap).


