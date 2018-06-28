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
%%%                 maps in the contemporary Erlang sense, we're 
%%%                 changed the term "tagmap" to "typespec"
%%% @end

%%% ==========================================================================


-module (ep_typespec).

-export([update_panelmap/3]).
-export([get_spec/2, get_fontmap/2, list_faces/2, get_face/3]).
-export([get_face/2]).
-export([face_map/2, get_leading/2, get_justify/2, get_indent/2]).
-export ([get_font/3, get_fontsize/3]).



%% @doc Return typefaces, leading, jusitification, and indentation

-spec update_panelmap(Typestyle :: tuple(),
                      Tag       :: atom(),
                      BeadMap   :: map()) -> map().

update_panelmap(TypeStyle, Tag, PanelMap) ->
   {Faces, Leading, Justify, Indent} = get_spec(TypeStyle, Tag), 
   PanelMap1     = maps:put(faces, Faces, PanelMap),
   PanelMap2     = maps:put(leading, Leading, PanelMap1),
   PanelMap3     = maps:put(justify, Justify, PanelMap2),
   maps:put(indent, Indent, PanelMap3).
   

%% @doc Return typefaces, leading, jusitification, and indentation

-spec get_spec(Typestyle :: tuple(),
              Tag       :: atom()) -> tuple().

get_spec(TypeStyle, Tag) ->
   case TypeStyle of
      report  -> ep_typestyle:report(Tag);
      _       -> ep_typestyle:report(Tag)
   end.

%% @doc Return list of faces

-spec get_fontmap(Typestyle :: tuple(),
                  Tag       :: atom()) -> tuple().

get_fontmap(TypeStyle, Tag) ->
   Typespecs = get_spec(TypeStyle, Tag),
   element(1, Typespecs).

% -spec list_faces(TypeSpec :: tuple()) -> list().

list_faces(TypeStyle, Tag) ->
    Faces = get_fontmap(TypeStyle, Tag),
    element(2, Faces).

%% @doc Given type style, style tag and face tag, return typeface 

-spec get_face(TypeStyle :: tuple(),
               StyleTag  :: atom(),
               FaceTag   :: atom()) -> tuple().

get_face(TypeStyle, StyleTag, FaceTag) ->
   FaceList = list_faces(TypeStyle, StyleTag),
   TaggedFace = lists:keyfind(FaceTag, 1, FaceList),
   element(2, TaggedFace).



get_face(Tag, FontMap) ->
   FaceList = element(2, FontMap),
   TaggedFace = lists:keyfind(Tag, 1, FaceList),
   element(2, TaggedFace).


%% @doc Return face map 

-spec face_map(TypeSyle :: tuple(),
               Tag      :: atom()) -> map().

face_map(TypeStyle, Tag) ->
    FaceList = list_faces(TypeStyle, Tag),
    maps:from_list(FaceList).


%% @doc Return leading 

-spec get_leading(TypeSyle :: tuple(),
                  Tag      :: atom()) -> map().

get_leading(TypeStyle, Tag) ->
   Typespecs = get_spec(TypeStyle, Tag),
   element(2, Typespecs).


%% @doc Return justification 

-spec get_justify(TypeSyle :: tuple(),
                  Tag      :: atom()) -> map().

get_justify(TypeStyle, Tag) ->
   Typespecs = get_spec(TypeStyle, Tag),
   element(3, Typespecs).


%% @doc Return indent 

-spec get_indent(TypeSyle :: tuple(),
                 Tag      :: atom()) -> map().

get_indent(TypeStyle, Tag) ->
   Typespecs = get_spec(TypeStyle, Tag),
   element(4, Typespecs).


%% @doc Return erlPress font 

-spec get_font(TypeStyle :: tuple(),
               StylTag   :: atom(),
               FaceTag   :: atom())-> atom().

get_font(TypeStyle, StyleTag, FaceTag) ->
    Face = get_face(TypeStyle, StyleTag, FaceTag),
    element(2, Face).


%% @doc Return fontsize

-spec get_fontsize(TypeStyle :: tuple(),
                   StylTag   :: atom(),
                   FaceTag   :: atom()) -> atom().

get_fontsize(TypeStyle, StyleTag, FaceTag) ->
    Face = get_face(TypeStyle, StyleTag, FaceTag),
    element(3, Face).




