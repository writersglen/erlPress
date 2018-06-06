%%% ==========================================================================
%%% ep_typespec.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version    0.01
%%% @title      
%%% @doc
%%%   License:
%%%   File:         ep_typespec.erl
%%%   Description:  Type specification functions 
%%%   Note:         erlguten refers to "tagmaps." Since they are not
%%%                 maps in the contemporary Erlang since, we're 
%%%                 changed the term "tagmap" to "typespec"
%%% @end

%%% ==========================================================================


-module (ep_typespec).

-export ([default/1, default_helvetica/1]). 
-export ([tag_list/1, tag_map/1, get_face/2, get_font/2]).


%% ***********************************************************************
%% Type specifications
%% ***********************************************************************

%% @doc Default type specification

-spec default(FontSize :: integer()) -> tuple().

default(FontSize) ->
   {[p],
    [tag_face(default, "Times-Roman", FontSize),
     tag_face(em,      "Times-Italic", FontSize),
     tag_face(code,    "Courier", FontSize), 
     tag_face(b,       "Times-Bold", FontSize),
     tag_face(hb,      "Helvetica-Bold", FontSize),
     tag_face(helv,    "Helvetica", FontSize)
    ]}.


%% @doc Default helvetica type specification

-spec default_helvetica(FontSize :: integer()) -> tuple().


default_helvetica(FontSize) ->
   {[p],
    [tag_face(default, "Helvetica", FontSize),
     tag_face(em,      "Helvetica-Oblique", FontSize),
     tag_face(code,    "Courier", FontSize), 
     tag_face(b,       "Helvetica-Bold", FontSize),
     tag_face(hb,      "Helvetica-Bold", FontSize),
     tag_face(helv,    "Helvetica", FontSize)
    ]}.


%% ***********************************************************************
%% ***********************************************************************
%% Type specification functions
%% ***********************************************************************


%% @doc Return tagged type face

-spec tag_face(Tag :: atom(), Font :: string(),
               FontSize :: integer()) -> tuple().

tag_face(Tag, Font, FontSize) ->
    FaceMap = ep_face:create(Font, FontSize),
    {Tag, ep_face:make_face(FaceMap)}.


%% @doc Given type specification, return list of tagged type faces

-spec tag_list(TypeSpec :: tuple()) -> list().

tag_list(TypeSpec) ->
    element(2, TypeSpec).


%% @doc Convert list of type specifications into a tag map

-spec tag_map(TypeSpec :: tuple()) -> map().

tag_map(TypeSpec) ->
    TagList = tag_list(TypeSpec),
    maps:from_list(TagList).


%% @doc Given tag and type specification, return type face

-spec get_face(Tag :: atom(), TypeSpec :: tuple()) -> tuple().
 
get_face(Tag, TypeSpec) ->
    TagMap = tag_map(TypeSpec),
    Result = maps:find(Tag, TagMap),
    case Result of
        error -> default_face();
        _     -> maps:get(Tag, TagMap)
    end.


default_face() ->
   eg_pdf:default_face().


%% @doc Given tag and type specification, return font

-spec get_font(Tag :: atom(), TypeSpec :: tuple()) -> string().

get_font(Tag, TypeSpec) ->
    Face = get_face(Tag, TypeSpec),
    element(2, Face).








