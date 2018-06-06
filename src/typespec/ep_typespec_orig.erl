%%% ==========================================================================
%%% ep_typespec.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:
%%%   File:         ep_typespec.erl
%%%   Description:  Type specification 
%%% @end

%%% ==========================================================================


-module (ep_typespec_orig).

-export([typespec_test1/0, typespec_test2/0]).

-export([ specify/2
        , spec_report/1
        , report_spec/1
        , code_block/1
        , default/1
        , times_serif_center/2
        , times_serif_ragged/2
        , times_right_justify/2
        , helvetica_sans_justify/2
        , helvetica_sans_ragged/2
        , courier_monospace_justify/2
        , courier_monospace_center/2
        , spec/1
        , pt_size/1
        , leading/1
        , justification/1
        , tagmap/1
        , justify_serif/1
        , typespec/1 ]).



-define(LEADING_FACTOR, 1.25).


typespec_test1() ->
   Format = report,
   ContentBlocks = md_parse:test_parse2(),
   specify(Format, ContentBlocks).

typespec_test2() ->
   Format = page_grid_label,
   ContentBlocks = md_parse:parse("page_grid_label.md"),
   specify(Format, ContentBlocks).



-spec specify(atom(), list()) -> list().

%% @doc Apply type specs to content blocks
%%      where a Block is a text element or an image 



specify(report, ContentBlocks) -> 
   [spec_report(ContentBlock) || ContentBlock <- ContentBlocks];

specify(page_grid_label, ContentBlocks) ->
   [spec_page_grid_label(ContentBlock) || ContentBlock <- ContentBlocks];

specify(page_number, ContentBlocks) ->
   [spec_page_number(ContentBlock) || ContentBlock <- ContentBlocks].



%% *****************************************************
%% Type styles
%% *****************************************************

spec_report(ContentBlock) ->
   TypeSpec = report_spec(ContentBlock),
   {TypeSpec, ContentBlock}.

report_spec(Block) ->
   Tag = element(1, Block),
   case Tag of
      code -> courier_monospace_ragged(Tag, 12);
      h1   -> helvetica_sans_center(Tag, 36);
      h2   -> helvetica_sans_center(Tag, 24);
      h3   -> helvetica_sans_center(Tag, 18);
      h4   -> helvetica_sans_center(Tag, 12);
      h5   -> helvetica_sans_center(Tag, 10);
      h6   -> helvetica_sans_center(Tag, 8);
      _    -> times_serif_justify(Tag, 12)
   end.


% page_grid_label(Text) ->
%    TaggedText = ep_tag_elements:tag(Text),
%   [spec_page_grid_label(Item) || Item <- TaggedText].

spec_page_grid_label(TaggedText) ->
   Typespec = page_grid_label_spec(TaggedText),
   {Typespec, TaggedText}.

page_grid_label_spec(Copy) ->
   Tag = element(1, Copy),
   case Tag of
      p -> {times_serif_justify(Tag, 14)};
      _ -> {times_serif_justify(Tag, 14)}
   end.
        

spec_page_number(TaggedText) ->
   Typespec = page_number_spec(TaggedText),
   {Typespec, TaggedText}.

page_number_spec(Copy) ->
   Tag = element(1, Copy),
   case Tag of
      p -> {times_right_justify(Tag, 14)};
      _ -> {times_right_justify(Tag, 14)}
   end.




code_block(Copy) ->
   Tag = element(1, Copy),
   case Tag of
      code -> courier_monospace_ragged(Tag, 12)
   end.


%% Font, PointSize, Breakable, Color, Voffset

default(Pts) ->
    {[p],
     [{default,eg_richText:mk_face("Times-Roman", Pts, true, default, 0)},
      {em,     eg_richText:mk_face("Times-Italic", Pts, true, default, 0)},
      {code,   eg_richText:mk_face("Courier", Pts, false, default, 0)},
      {b,      eg_richText:mk_face("Times-Bold", Pts, true, default, 0)},
      {hb,     eg_richText:mk_face("Helvetica-Bold", Pts, true, default, 0)},
      {helv,   eg_richText:mk_face("Helvetica", Pts, true, default, 0)}
     ]}.


%% ************************************************
%% Type specifications 
%% ************************************************

times_serif_justify(Tag, Pts) ->
   new(Pts, justified, times_roman(Tag, Pts)).

times_serif_center(Tag, Pts) ->
   new(Pts, centered, times_roman(Tag, Pts)).

times_serif_ragged(Tag, Pts) ->
   new(Pts, left_justified, times_roman(Tag, Pts)).

times_right_justify(Tag, Pts) ->
   new(Pts, right_justified, times_roman(Tag, Pts)).


helvetica_sans_justify(Tag, Pts) ->
   new(Pts, justified, helvetica(Tag, Pts)).

helvetica_sans_center(Tag, Pts) ->
   new(Pts, centered, helvetica(Tag, Pts)).

helvetica_sans_ragged(Tag, Pts) ->
   new(Pts, left_justified, helvetica(Tag, Pts)).


%% Provisional -- may need to debug

courier_monospace_justify(Tag, Pts) ->
   new(Pts, left_justified, courier(Tag, Pts)).

courier_monospace_center(Tag, Pts) ->
   new(Pts, centered, courier(Tag, Pts)).

courier_monospace_ragged(Tag, Pts) ->
   new(Pts, left_justified, courier(Tag, Pts)).


serif_tagMap(Pts) ->
    {[p],
     [{default,eg_richText:mk_face("Times-Roman", Pts, true, default, 0)},
      {em,     eg_richText:mk_face("Times-Italic", Pts, true, default, 0)},
      {b,      eg_richText:mk_face("Times-Bold", Pts, true, default, 0)}
     ]}.

sans_serif_tagMap(Pts) ->
    {[p],
     [{default,eg_richText:mk_face("Helvetica", Pts, true, default, 0)},
      {em,     eg_richText:mk_face("Helvetica-Oblique", Pts, true, default, 0)},
      {b,      eg_richText:mk_face("Helvetica-Bold", Pts, true, default, 0)},
      {hb,      eg_richText:mk_face("Helvetica-Bold", Pts, true, default, 0)},
      {h1,   eg_richText:mk_face("Helvetica", Pts, true, default, 0)},
      {h2,   eg_richText:mk_face("Helvetica", Pts, true, default, 0)},
      {h3,   eg_richText:mk_face("Helvetica", Pts, true, default, 0)}
     ]}.

monospace_tagMap(Pts) ->
    {[p],
     [{default,eg_richText:mk_face("Courier", Pts, true, default, 0)},
      {em,     eg_richText:mk_face("Courier-Oblique", Pts, true, default, 0)},
      {b,      eg_richText:mk_face("Courier-Bold", Pts, true, default, 0)}
     ]}.




justify_serif(Pts) ->
   new(Pts, justified, serif_tagMap(Pts)).

center_sans_serif(Pts) ->
   new(Pts, centered, sans_serif_tagMap(Pts)).

ragged_monospace(Pts) ->
   new(Pts, ragged, monospace_tagMap(Pts)).





%% ************************************************
%% Create typespec 
%% ************************************************

new(PtSize, Justification, TagMap) ->
   Leading = round(?LEADING_FACTOR * PtSize),
   #{pt_size       => PtSize,
     leading       => Leading,
     justification => Justification,
     tagmap        => TagMap
    }.


%% ************************************************
%% Times Roman tag map
%% ************************************************

times_roman(Tag, Pts) ->
   mk_times_roman(Tag, Pts, default, 0).

mk_times_roman(Tag, Pts, Color, Voffset) ->
   {[Tag],
    [{default,eg_richText:mk_face("Times-Roman", Pts, true, Color, Voffset)},
     {em,     eg_richText:mk_face("Times-Italic", Pts, true, Color, Voffset)},
     {b,      eg_richText:mk_face("Times-Bold", Pts, true, Color,  Voffset)},
     {code,   eg_richText:mk_face("Courier", Pts, true, Color,  Voffset)}
    ]}.

%% ************************************************
%% Helvetica tag map 
%% ************************************************

helvetica(Tag, Pts) ->
   mk_helvetica(Tag, Pts, default, 0).

mk_helvetica(Tag, Pts, Color, Voffset) ->
   {[Tag],
    [{default,eg_richText:mk_face("Helvetica", Pts, true, Color, Voffset)},
     {em,     eg_richText:mk_face("Helvetica-Oblique", Pts, true, Color, Voffset)},
     {b,      eg_richText:mk_face("Helvetica-Bold", Pts, true, Color, Voffset)},
     {code,   eg_richText:mk_face("Courier", Pts, true, Color,  Voffset)}
    ]}.

%% ************************************************
%% Courier tag map
%% ************************************************


courier(Tag, Pts) ->
    mk_courier(Tag, Pts, default, 0).

mk_courier(Tag, Pts, Color, Voffset) ->
   {[Tag],
    [{default,eg_richText:mk_face("Courier", Pts, true, Color, Voffset)},
     {em,     eg_richText:mk_face("Courier-Oblique", Pts, true, Color, Voffset)},
     {b,      eg_richText:mk_face("Courier-Bold", Pts, true, Color, Voffset)},
     {code,   eg_richText:mk_face("Courier", Pts, false, Color, Voffset)}
    ]}.


%% ************************************************
%% Typespec 
%% ************************************************

spec(Map) ->
  PtSize        = maps:get(pt_size, Map),
  Leading       = maps:get(leading, Map),
  Justification = maps:get(justification, Map),
  TagMap        = maps:get(tagmap, Map),
  {PtSize, Leading, Justification, TagMap}.

%% ************************************************
%% Type attributes
%% ************************************************

pt_size(TypeSpec) ->
%   Map = element(1, TypeSpec),
   maps:get(pt_size, TypeSpec).

leading(TypeSpec) ->
%   Map = element(1, TypeSpec),
   maps:get(leading, TypeSpec).

justification(TypeSpec) ->
    io:format("**** ep_typespec:justication/1 TypeSpec: ~p~n", [TypeSpec]),
%   Map = element(1, TypeSpec),
   maps:get(justification, TypeSpec).

tagmap(TypeSpec) ->
%   Map = element(1, TypeSpec),
   maps:get(tagmap, TypeSpec).




%% Tag, PtSize, LineHight, Characters 
%% p    12      1.25       60-75
%% h1   36      1.25
%% h2   24      1.25
%% h3   18      1.25
%% h4   14      1.25
%% h5   10      1.25
%% h6    8      1.25


%% NOTE:4/4/18 Not sure if we need this, but leave for now

typespec(Copy) ->
   Flag = string:left(Copy, 6) == "<code>",
   case Flag of
       true  ->  {wrap, ragged_monospace(12)};
       _     ->  Tag = string:left(Copy, 4),
                 case Tag of
                    "<h1>"   -> {wrap, center_sans_serif(36)};
                    "<h2>"   -> {wrap, center_sans_serif(24)};
                    "<h3>"   -> {wrap, center_sans_serif(18)};
                    "<h4>"   -> {wrap, center_sans_serif(12)};
                    "<h5>"   -> {wrap, center_sans_serif(10)};
                    "<h6>"   -> {wrap, center_sans_serif(8)};
                    _        -> {no_wrap, justify_serif(12)}
                 end
   end.





