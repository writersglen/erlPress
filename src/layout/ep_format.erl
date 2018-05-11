
%%% ********************************************************* 
%%% {c) 2018    Lloyd R. Prentice
%%% Author:     Lloyd R. Prentice
%%% License: 
%%% File:       ep_format.erl
%%% Description: 
%%%   Define and specify print formats 
%%% ********************************************************* 

-module (ep_format).

-export([formats/0]).
-export([pagesize_points/1, pagesize_inches/1, pagesize_picas/1]).
-export([page_width/1, page_height/1]).
-export([page_offsets/2]).


%% @doc Return list of page formats 

formats() ->
     [element(1, Format) || Format <- format_list()].

%% @doc Return width and height of standard page formats 
%%      in points

-spec pagesize_points(Format :: atom()) -> tuple().

pagesize_points(Format) -> 
   FormatList = format_list(),
   Tuple = lists:keyfind(Format, 1, FormatList),
   element(2, Tuple).

%% @doc Return width and height of standard page formats 
%%      in inches
 
-spec pagesize_inches(Format :: atom()) -> tuple().

pagesize_inches(Format) ->
   {Width, Height} = pagesize_points(Format),
   {Width / 72, Height / 72}.

%% @doc Return width and height of standard page formats 
%%      in picas

-spec pagesize_picas(Format :: atom()) -> tuple().

pagesize_picas(Format) ->
   {Width, Height} = pagesize_points(Format),
   {Width / 6, Height / 6}.

page_width(Format) ->
   {Width, _Height} = pagesize_points(Format),
   Width.

page_height(Format) ->
   {_Width, Height} = pagesize_points(Format),
   Height.



%% @doc Return list of standard formats 

-spec format_list() -> list().

format_list() ->
    [{a0,               {2380, 3368}}, 
     {a1,               {1684, 2380}},
     {a2,               {1190, 1684}},
     {a3,               {842, 1190}},
     {a4,               {595, 842}},
     {a5,               {421, 595}},
     {a6,               {297, 421}},
     {a7,               {210, 297}},
     {a8,               {148, 210}},
     {a9,               {105, 148}},
     {b0,               {2836, 4008}},
     {b1,               {2004, 2836}},
     {b2,               {1418, 2004}},
     {b3,               {1002, 1418}},
     {b4,               {709, 1002}},
     {b5,               {501, 709}},
     {b6,               {355, 501}},
     {b7,               {250, 355}},
     {b8,               {178, 250}},
     {b9,               {125, 178}},
     {b10,              {89, 125}},
     {avery_5164,       {288, 252}, "Avery 5164 - mailing"},
     {avery_8168,       {252, 360}, "Avery 8168 - media kit"},
     {book1,            {378, 576}, "Createspace"},
     {book4,            {396, 612}, "Createspace"},
     {book3,            {432, 648}, "Createspace"},
     {book4,            {504, 720}, "Createspace"},
     {book5,            {576, 720}, "Createspace"},
     {book6,            {594, 432}, "Createspace"},
     {book7,            {594, 594}, "Createspace"},
     {bookmark,         {180, 450}, "Check with printer"},
     {business_card,    {252, 144}, "US"},
     {envelope_no10,    {297, 684}},
     {invoice,          {612, 792}},
     {ledger,           {1224, 792}},
     {legal,            {612, 1008}},
     {letter,           {612, 792}},
     {postcard1,        {360, 252}, "5 x 3.5-inches"},
     {postcard2,        {432, 306}, "6 x 4.25-inches"},
     {postcard3,        {504, 360}, "7 x 5-inches"},
     {report,           {612, 792}},
     {tabloid,          {792, 1224}}
    ].

%% For business card dimensions see: 
%% http://designerstoolbox.com/designresources/postcards/




%% ****************************************************************
%% @doc Upper left corner offset of page impression relative to paperstock;
%%      Offsets may be determined by printer preference or placement of
%%      impression relative to 8.5 x 11-inch stock
%%
%%      NOTE: Need list of page sizes
%% ****************************************************************

page_offsets(letter, bookmark) ->
   {144, 144};

page_offsets(letter, label_5164_1) ->   % Position 1 Avery 8.25 x 11 Avery sheet
   {0, 0};

page_offsets(letter, label_5164_2) ->   
   {0, 0};

page_offsets(letter, label_5164_3) ->
   {0, 0};

page_offsets(letter, label_5164_4) ->
   {0, 0};

page_offsets(letter, label_5164_5) ->
   {0, 0};

page_offsets(letter, label_5164_6) ->
   {0, 0};

page_offsets(letter, label_8168_1) ->
   {0, 0};

page_offsets(letter, label_8168_2) ->
   {0, 0};

page_offsets(letter, label_8168_3) ->
   {0, 0};

page_offsets(letter, label_8168_4) ->
   {0, 0};

page_offsets(letter, postcard_5) ->
   {0, 0};

page_offsets(letter, postcard_6) ->
   {0, 0};

page_offsets(letter, postcard_7) ->
   {0, 0};

page_offsets(letter, letter) ->
   {0, 0}.





