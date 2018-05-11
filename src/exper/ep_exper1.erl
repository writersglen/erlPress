
%%% *********************************************************
%%% {c) 2016 Lloyd R. Prentice
%%% Author:     Lloyd R. Prentice
%%% License: 
%%% File:       ep_boxes.erl
%%% Description: 
%%%    Experimental print routines 
%%% *********************************************************   



-module (ep_exper1).

% -export([test/2, boxes/0, text/0, text1/0, text2/0, text3/0, text4/0, text5/0, typespec/1, tagmap/1]).

-compile([export_all]).

% -export ([new/4, put_page_dim/2]).
% -export ([put_x/2, put_y/2, put_z/2, put_xyz/4]).
% -export([put_pad_left/2, put_pad_right/2, put_pad_top/2, put_pad_bottom/2]). 
% -export([put_line_height/2, put_background/3]).
%-export([report/1, consume/2, from_here/2, print_box/1]).

-define(IMAGE_DIR, "../test/images/").
-define(PDF_DIR, "./pdf/boxes/").


%% *************************************************************
%% Print Box 
%% *************************************************************

test(Copy, Boxes) ->
   PDF = eg_pdf:new(),
   eg_pdf:set_pagesize(PDF, letter),
   eg_pdf:set_page(PDF, 1),

   ep_block:block(PDF, Copy, Boxes), 

   {Serialized, _PageNo}  = eg_pdf:export(PDF),
   file:write_file("?PDF_DIR" ++ "exper_box.pdf", [Serialized]),
   eg_pdf:delete(PDF).


boxes() ->
   [box1(), box2()].


box1() ->
   Box = ep_box:create(72, 720, 144, 200),
   Box1 = ep_box:update_border_color(black, Box),
   Box2 = ep_box:update_background_color(yellow, Box1),
   Box3 = ep_box:update_stroke(fill_stroke, Box2),
   ep_box:set_bg_flag(Box3).

box2() ->
   Box = ep_box:create(236, 720, 144, 200),
   Box1 = ep_box:update_border_color(blue, Box),
   Box2 = ep_box:update_background_color(white, Box1),
   Box3 = ep_box:update_stroke(fill_stroke, Box2),
   Box4 = ep_box:update_indent(0, Box3),
   ep_box:reset_bg_flag(Box4).


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

text1() ->
    "<h1>Hello!</h1>".

text2() ->
    "<p><h2>How are you?</h2></p>".

text3() ->
    "<h1>Hello!</h1>
     <h2>How are you?</h2>
    <p>This is normal text, set 5 picas wide in 12/14 Times Roman.
I even allow some <em>emphasised term,</em> set in Times-Italic. The TeX
hyphenation algorithm is also implemented.
I have also some <em>cursive text</em> and an example of
an Erlang term. The term <code>{person, \"Joe\"}</code> is an Erlang term.
The variable <code>X</code>, was immediately followed by
a comma. The justification algorithm does proper <em>kerning</em>,
which is more than <em>Microsoft Word</em> can do. AWAY again is
correctly kerned! Erlang terms <code>{like, this}</code>
are typeset in <em>courier.</em></p>".

text4() ->
   "<h1>The quick brown fox</h1>".

text5() ->
   "<p>The quick brown fox</p>".

%% *************************************************************
%% Type spec
%% *************************************************************

typespec(Copy) ->
   {WrapFlag, TypeSpec} = ep_typespec:typespec(Copy),
   case WrapFlag of
      wrap    -> {"<p>" ++ Copy ++ "</p>", TypeSpec};
      no_wrap -> {Copy, TypeSpec}
   end. 

%% *************************************************************
%% TagMap
%% *************************************************************

tagmap(PtSize) ->
    eg_xml2richText:default_tagMap(PtSize).


