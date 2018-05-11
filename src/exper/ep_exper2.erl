
%%% *********************************************************
%%% {c) 2016 Lloyd R. Prentice
%%% Author:     Lloyd R. Prentice
%%% License: 
%%% File:       ep_boxes.erl
%%% Description: 
%%%    Experimental print routines 
%%% *********************************************************   



-module (ep_exper2).

% -export([text/0, tag/1]).
% -export([parse/1, drop_spaces/1, font_info/1]).

-compile([export_all]).

%% ***************************************************************
%% Sample text rendered in markdown
%% https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet
%% ***************************************************************

text() ->
   "# H1

    ## H2 

    ### H3 

    #### H4 

    ##### H5 

    ###### H6 

    The quick 
brown fox.".

%% ***************************************************************
%% Parse markdown text
%% ***************************************************************

parse(Text) ->
    Text1 = prep_text(Text),
    List = string:tokens(Text1, "$|"),
    [tag(Item) || Item <- List].   


%% parse(text()) returns:

% [{h,1,"The quick brown fox."},
%  {h,2,"The quick brown fox."},
%  {h,3,"The quick brown fox."},
%  {p,0,"The quick brown fox."}]


%% ***************************************************************
%% Prep text 
%% ***************************************************************

prep_text(Text) ->
    Text1 = drop_spaces(Text),
    Text2 = string:strip(Text1),
    Text3 = re:replace(Text2, [$\n, $#], [$\n,$\n,$#], [global, {return, list}]),
    Text4 = re:replace(Text3, [$\n, $\s], [$\n,$\n,$#], [global, {return, list}]),
    Text5 = re:replace(Text4, [$\n,$\n], "|", [global, {return, list}]),
    re:replace(Text5, [$\n], "", [global, {return, list}]).


drop_spaces(String) ->
   drop_spaces(String, []).

drop_spaces([], NewString) ->
   lists:reverse(NewString);

drop_spaces(String, NewString) ->
   String1 = string:strip(String),
   [Char | Rest] = String1,
   case Char of
      $\s  -> LastChar = hd(NewString),
              case LastChar of
                 $\s  -> drop_space(LastChar, Rest, NewString);
                 _    -> drop_spaces(Rest, [Char | NewString])
              end; 
      _    -> drop_spaces(Rest, [Char | NewString])
   end. 

drop_space(Char, String, NewString) ->
   case Char of
        $\s  -> drop_spaces(String, NewString);
        _    -> drop_spaces(String, [Char | NewString])
   end.

%% ***************************************************************
%% Tag text 
%% ***************************************************************


tag(String) ->
   tag(String, 0).

tag([], N) ->
   {N, undefined};

tag(String, N) ->
   String1 = string:strip(String),
   [First | Rest ] = String1,
   case First of
      $# -> tag(Rest, N + 1);
      _  -> index_tag(N, String1)
   end. 


index_tag(N, String) -> 
  case N > 0 of
     true  -> {h, N, String};
     false -> {p, 0, String}
  end.
 

%% ***************************************************************
%% Sample tag_map 
%% ***************************************************************



font_info({Tag, Index, Text}) ->
   FullTag = atom_to_list(Tag) ++ integer_to_list(Index),
   case FullTag of 
      "h1" -> {{face,eg_font_7,36,0,default,true}, Text};
      "h2" -> {{face,eg_font_7,24,0,default,true}, Text};
      "h3" -> {{face,eg_font_7,18,0,default,true}, Text};
      "p0" -> {{face,eg_font_13,12,0,default,true}, Text}
   end.



%% 237> PT.
%% "# The quick brown fox.\n\n    ## The quick brown fox.\n\n    ### The quick brown fox.\n\n    The quick \nbrown fox."
   
% 235> [ep_exper2:font_info(Block) || Block <- ep_exper2:parse(PT) ].
% [{{face,eg_font_7,36,0,default,true},"The quick brown fox."},
% {{face,eg_font_7,24,0,default,true},"The quick brown fox."},
% {{face,eg_font_7,18,0,default,true},"The quick brown fox."},
% {{face,eg_font_13,12,0,default,true},
%  "The quick brown fox."}]


%% Scan text word specs.

% Need function to get word length.

% 239> eg_richText:width_of(eg_font_7, 36, "brown").
% 97488

% 243> eg_richText:mk_space({face,eg_font_13,12,0,default,true}).
% {space,3000,{face,eg_font_13,12,0,default,true}}


%% We would like to go to:

% {p,[],
%         {richText,[{word,62028,{face,eg_font_7,36,0,default,true},"The"},
%                    {space,10008,{face,eg_font_7,36,0,default,true}},
%                    {word,83304,{face,eg_font_7,36,0,default,true},"quick"},
%                    {space,10008,{face,eg_font_7,36,0,default,true}},
%                    {word,97488,{face,eg_font_7,36,0,default,true},"brown"},
%                    {space,10008,{face,eg_font_7,36,0,default,true}},
%                    {word,45864,{face,eg_font_7,36,0,default,true},"fox"}]}}










  
    
%% ******************************************************

% 189> eg_xml_lite:parse_all_forms(Text1).
% [{xml,{p,[],
%         [{raw,"This is normal text, set 5 picas wide in 12/14 Times Roman.\nI even allow some "},
%          {em,[],[{raw,"emphasised term,"}]},
%          {raw," set in Times-Italic. The TeX\nhyphenation algorithm is also implemented.\nI have also some "},
%          {em,[],[{raw,"cursive text"}]},
%          {raw," and an example of\nan Erlang term. The term "},
%          {code,[],[{raw,"{person, \"Joe\"}"}]},
%          {raw," is an Erlang term.\nThe variable "},
%          {code,[],[{raw,"X"}]},
%          {raw,", was immediately followed by\na comma. The justification algorithm does proper "},
%          {em,[],[{raw,"kerning"}]},
%          {raw,",\nwhich is more than "},
%          {em,[],[{raw,"Microsoft Word"}]},
%          {raw," can do. AWAY again is\ncorrectly kerned! Erlang terms "},
%          {code,[],[{raw,"{like, this}"}]},
%          {raw,"\nare typeset in "},
%          {em,[],[{raw,"courier."}]}]}}]

% 191> Text2 = ep_exper1:text1().
% "<h1>Hello!</h1>"

%% 192> eg_xml_lite:parse_all_forms(Text2).
%% [{xml,{h1,[],[{raw,"Hello!"}]}}]

% 193> Text3 = ep_exper1:text3().
%"<h1>Hello!</h1>\n     <h2>How are you?</h2>\n    <p>This is normal text, set 5 picas wide in 12/14 Times Roman.\nI even allow some <em>emphasised term,</em> set in Times-Italic. The TeX\nhyphenation algorithm is also implemented.\nI have also some <em>cursive text</em> and an example of\nan Erlang term. The term <code>{person, \"Joe\"}</code> is an Erlang term.\nThe variable <code>X</code>, was immediately followed by\na comma. The justification algorithm does proper <em>kerning</em>,\nwhich is more than <em>Microsoft Word</em> can do. AWAY again is\ncorrectly kerned! Erlang terms <code>{like, this}</code>\nare typeset in <em>courier.</em></p>"

% 194> eg_xml_lite:parse_all_forms(Text3).
% [{xml,{h1,[],[{raw,"Hello!"}]}},
% {xml,{h2,[],[{raw,"How are you?"}]}},
% {xml,{p,[],
%         [{raw,"This is normal text, set 5 picas wide in 12/14 Times Roman.\nI even allow some "},
%          {em,[],[{raw,"emphasised term,"}]},
%          {raw," set in Times-Italic. The TeX\nhyphenation algorithm is also implemented.\nI have also some "},
%          {em,[],[{raw,"cursive text"}]},
%          {raw," and an example of\nan Erlang term. The term "},
%          {code,[],[{raw,"{person, \"Joe\"}"}]},
%          {raw," is an Erlang term.\nThe variable "},
%          {code,[],[{raw,"X"}]},
%          {raw,", was immediately followed by\na comma. The justification algorithm does proper "},
%          {em,[],[{raw,"kerning"}]},
%          {raw,",\nwhich is more than "},
%          {em,[],[{raw,"Microsoft Word"}]},
%          {raw," can do. AWAY again is\ncorrectly kerned! Erlang terms "},
%          {code,[],[{raw,"{like, this}"}]},
%          {raw,"\nare typeset in "},
%          {em,[],[{raw,...}]}]}}]

%% ********************************************************************

%% We want to get from:


%  {word,3996,{face,eg_font_13,12,0,default,true},"I"},
%                    {space,3000,{face,eg_font_13,12,0,default,true}},
%                    {word,22176,{face,eg_font_13,12,0,default,true},"even"},
%                    {space,3000,{face,eg_font_13,12,0,default,true}},
%                    {word,26364,{face,eg_font_13,12,0,default,true},"allow"},
%                    {space,3000,{face,eg_font_13,12,0,default,true}},
%                    {word,25332,{face,eg_font_13,12,0,default,true},"some"},
 
%                   {space,3000,{face,eg_font_13,12,0,default,true}},
%                    {word,55992,
%                          {face,eg_font_10,12,0,default,true},
%                          "emphasised"},
%                    {space,3000,{face,eg_font_10,12,0,default,true}},
%                    {word,24996,{face,eg_font_10,12,0,default,true},"term,"},
%                    {space,3000,{face,eg_font_13,12,0,default,true}},
%                    {word,13332,{face,eg_font_13,12,0,default,true},"set"},
%                    {space,3000,{face,eg_font_13,12,0,default,true}},
%                    {word,9336,{face,eg_font_13,12,0,default,true},"in"},
%                    {space,3000,{face,eg_font_13,12,0,default,true}},
%                    {word,61236,
%                          {face,eg_font_13,12,0,default,true},
%                          "Times-Italic."},


