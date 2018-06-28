%%% ==========================================================================
%%% ep_sample_text.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:
%%%   File:         ep_sample_text.erl
%%%   Description:  Sample text blocks 
%%% @end

%%% ==========================================================================

-module (ep_sample_text).

-export ([times_14/0, helvetica_10/0, the_road_not_taken/0, article/0]).

%% @doc  Display Times-Roman 14pt text
%% NOTE: Must apply ep_block:default_14/0 for this to 
%%       show advertised font size 


times_14() ->
"<p>This is normal text set in 14/21 Times Roman.
It includes <em>emphasized terms,</em> set in Times-Italic. The TeX
hyphenation algorithm is implemented.  The term <code>{person, 
\"Joe\"}</code> is an Erlang term.
The variable <code>X</code>, was immediately followed by
a comma. The justification algorithm does proper <em>kerning</em>,
which is more than <em>Microsoft Word</em> can do. AWAY is
correctly kerned! Erlang terms <code>{like, this}</code>
are typeset in <em>courier.</em></p>".


%% @doc  Display Helvetica 10pt text
%% NOTE: Must apply ep_typespec:default_helvetics(10) for this to 
%%       show advertised font size 

helvetica_10() ->
    "<p>This is normal text set in 10/15 Helvetica.
It includes <em>emphasized terms,</em> set in Helvetica-Oblique. The TeX
hyphenation algorithm is implemented.  The term <code>{person, 
\"Joe\"}</code> is an Erlang term.
The variable <code>X</code>, was immediately followed by
a comma. The justification algorithm does proper <em>kerning</em>,
which is more than <em>Microsoft Word</em> can do. AWAY is
correctly kerned! Erlang terms <code>{like, this}</code>
are typeset in <em>courier.</em></p>".


the_road_not_taken() ->
"<p>  </p>
<p>Two roads diverged in a yellow wood,
And sorry I could not travel both
And be one traveler, long I stood
And looked down one as far as I could
To where it bent in the undergrowth;</p>
     <p> - <em>Robert Frost</em></p>".


article() ->
"<h1>This is a headline</h1>
<h2>This is a deck</h2>
<p>    </p>
<p>This is normal text set in 14/21 Times Roman.
It includes <em>emphasized terms,</em> set in Times-Italic. The TeX
hyphenation algorithm is implemented.  The term <code>{person, 
\"Joe\"}</code> is an Erlang term.</p>
<p>    </p>
<h3>This is a subhead</h3>
<p>The variable <code>X</code>, was immediately followed by
a comma. The justification algorithm does proper <em>kerning</em>,
which is more than <em>Microsoft Word</em> can do. AWAY is
correctly kerned! Erlang terms <code>{like, this}</code>
are typeset in <em>courier.</em></p>".



