
%%% *********************************************************
%%% {c) 2016 Lloyd R. Prentice
%%% Author:     Lloyd R. Prentice
%%% License: 
%%% File:       ep_boxes.erl
%%% Description: 
%%%    Experimental print routines 
%%% *********************************************************   


-module (ep_exper).

-export([test/1, text/0, type_spec/0, boxes/0]).

% -export ([new/4, put_page_dim/2]).
% -export ([put_x/2, put_y/2, put_z/2, put_xyz/4]).
% -export([put_pad_left/2, put_pad_right/2, put_pad_top/2, put_pad_bottom/2]). 
% -export([put_line_height/2, put_background/3]).
%-export([report/1, consume/2, from_here/2, print_box/1]).

-define(IMAGE_DIR, "../test/images/").
-define(PDF_DIR, "./pdf/").


%% *************************************************************
%% Print tests
%% *************************************************************

test(1) ->
   PDF = eg_pdf:new(),
   eg_pdf:set_pagesize(PDF, letter),
   eg_pdf:set_page(PDF, 1),

   test_jump(PDF), 

   {Serialized, _PageNo}  = eg_pdf:export(PDF),
   file:write_file("./jump.pdf", [Serialized]),
   eg_pdf:delete(PDF);

test(2) ->
   PDF = eg_pdf:new(),
   eg_pdf:set_pagesize(PDF, letter),
   eg_pdf:set_page(PDF, 1),

   test_job_order(PDF), 

   {Serialized, _PageNo}  = eg_pdf:export(PDF),
   file:write_file("./job_order.pdf", [Serialized]),
   eg_pdf:delete(PDF);


test(Box) ->
   PDF = eg_pdf:new(),
   eg_pdf:set_pagesize(PDF, letter),
   eg_pdf:set_page(PDF, 1),

   test_paste_up(PDF, Box),

   {Serialized, _PageNo}  = eg_pdf:export(PDF),
   file:write_file("./paste_up.pdf", [Serialized]),
   eg_pdf:delete(PDF).


%% *************************************************************
%% Jump test
%% *************************************************************


test_jump(PDF) ->
   Text = text(),
   {_, TypeSpec} = ep_typespec:justify_serif(12, 14),
   {PtSize, Leading, Justification, TagMap} = ep_typespec:spec(TypeSpec),
   eg_block:block(PDF, Text, 100, 700, 500, PtSize, Leading, 5, Justification, TagMap).

%% *************************************************************
%% Test Job Order
%% *************************************************************

test_job_order(PDF) ->
    [do_item(PDF, CopyItem) || CopyItem <- copy_spec()]. 

do_item(PDF, CopyItem) ->
    {Copy, X, Y, EndY, Measure, PtSize, Leading, NLines, Justified, TagMap} =
       CopyItem,
    ep_block:block(PDF, Copy, X, Y, EndY, Measure, PtSize, Leading, NLines, Justified, TagMap).
  
job_order_head() ->
   "<p>JOB ORDER</p>".

job_order_item(Param1, Param2) ->
   "<p>" ++ Param1 ++ ": " ++ "<em>" ++ Param2 ++ "</em></p>".


copy_spec() ->
  PtSize14 = 14,
  TagMap14 = tagmap(14),
  PtSize12 = 12,
  TagMap12 = tagmap(12),
  [{job_order_head(), 100, 620, 500, 200, PtSize14, 14, 5, justified, TagMap14},
   {job_order_item("ID", "123"), 100, 600, 500, 200, PtSize12, 14, 5, justified, TagMap12},
   {job_order_item("CLIENT", "LRP"), 100, 586, 500, 200, PtSize12, 14, 5, justified, TagMap12},
   {job_order_item("DEADLINE", "January 1, 2017"), 100, 572, 500, 200, PtSize12, 14, 5, justified, TagMap12},
   {job_order_item("TITLE", "Freein' Pancho Postcard"), 100, 558, 500, 200, PtSize12, 14, 5, justified, TagMap12}, 
   {job_order_item("AUTHOR", "Lloyd R. Prentice"), 100, 544, 500, 200, PtSize12, 14, 5, justified, TagMap12},
   {job_order_item("STOCK", "letter"), 100, 530, 500, 200, PtSize12, 14, 5, justified, TagMap12},
   {job_order_item("DESCRIPTION", "4 x 6-inch postcard"), 100, 516, 500, 200, PtSize12, 14, 5, justified, TagMap12}
  ].

%% *************************************************************
%% Test paste-up
%% *************************************************************


test_paste_up(PDF, Box) ->
   Text = text(),
   TypeSpec = type_spec(),
   paste_up(PDF, Text, TypeSpec, Box).

% paste_up(PDF, Text, TypeSpec, Box) ->
%   {PtSize, Leading, Justification, TagMap} = TypeSpec,
%   {X, Y, EndY, Measure, Height, _, _} = ep_box:get_dimensions(Box), 
%   NLines = Height div Leading,
%  ep_block:block(PDF, Text, X, Y, EndY, Measure, PtSize, Leading, NLines, Justification, TagMap).


paste_up(PDF, Text, TypeSpec, Box) ->
%   {PtSize, Leading, Justification, TagMap} = TypeSpec,
%   {X, Y, EndY, Measure, Height, _, _} = ep_box:get_dimensions(Box), 
%   NLines = Height div Leading,
   ep_block:block3(PDF, {xml, Text}, Box, TypeSpec).



%% *************************************************************
%% Sample text
%% *************************************************************

text() ->
    "<p>This is normal text, set 5 picas wide in 12/14 Times Roman.
I even allow some <em>emphasised term,</em> set in Times-Italic. The TeX
hyphenation algorithm is also implemented.
I have also some <em>cursive text</em> and an example of
an Erlang term. The term <code>{person, \"Joe\"}</code> is an Erlang term.
The variable <code>X</code>, was immediately followed by
a comma. The justification algorithm does proper <em>kerning</em>,
which is more than <em>Microsoft Word</em> can do. AWAY again is
correctly kerned! Erlang terms <code>{like, this}</code>
are typeset in <em>courier.</em></p>".

%% *************************************************************
%% Type spec 
%% *************************************************************

type_spec() ->
   {ok, TypeSpec} = ep_typespec:justify_serif(12, 14),
   TypeSpec.
%   TagMap = tagmap(12),
%   {12, 14, justified, TagMap}.

%% *************************************************************
%% TagMap
%% *************************************************************

tagmap(PtSize) ->
    eg_xml2richText:default_tagMap(PtSize).

%% *************************************************************
%% Test boxes 
%% *************************************************************

boxL() -> ep_box:new(72, 720, 180, 144).

boxR() -> ep_box:new(270, 720, 180, 144).

boxes() -> [boxL(), boxR()].

