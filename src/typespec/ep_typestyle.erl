%%% ==========================================================================
%%% ep_typestyle.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version    0.01
%%% @title      
%%% @doc
%%%   License:
%%%   File:         ep_typespec.erl
%%%   Description:  Define type styles 
%%% @end

%%% ==========================================================================


-module (ep_typestyle).

-export ([report/1, report_faces/1]). 
-export([default/1, default_helvetica/1]). 
-export([times/2, helvetica/2, courier/2]).
-export ([tag_face/3, default_face/0]). 
-export ([report_leading/1, report_justify/1, report_indent/1]).



%% ***********************************************************************
%% Type style - report
%% ***********************************************************************


%% @doc Return type specifications

-spec report(Tag :: atom()) -> tuple().

report(Tag) ->
   Faces   = report_faces(Tag),
   Leading = report_leading(Tag),
   Justify = report_justify(Tag),
   Indent  = report_indent(Tag),
   {Faces, Leading, Justify, Indent}.

%% Report helpers 


%% @doc Return report typefaces 

-spec report_faces(Tag :: atom()) -> tuple().

report_faces(Tag) ->
   case Tag of
      h1 -> helvetica(h1, 36);
      h2 -> helvetica(h2, 24);
      h3 -> helvetica(h3, 18);
      h4 -> helvetica(h4, 14);
      h5 -> helvetica(h5, 12);
      h6 -> helvetica(h6, 10);
      p  -> times(p, 12);
      _  -> times(p, 12)
    end.


%% @doc Return leading 

-spec report_leading(Tag :: atom()) -> tuple().


report_leading(Tag) ->
  Faces = report_faces(Tag),
  FaceList = element(2, Faces),
  Face     = lists:keyfind(Tag, 1, FaceList),
  FaceSpec = element(2, Face),
  FontSize = element(3, FaceSpec),
  case Tag of
     p -> round(FontSize * 1.5);
     _ -> round(FontSize * 1.25)
  end. 


%% @doc Return justification 

-spec report_justify(Tag :: atom()) -> tuple().

report_justify(Tag) ->
   case Tag of
      h1 -> centered;
      h2 -> centered;
      h3 -> ragged;
      p  -> justified;
      _  -> ragged
   end.


%% @doc Return indentation 

-spec report_indent(Tag :: atom()) -> tuple().

report_indent(Tag) ->  
   case Tag of
      p  -> 30;
      _  -> 0
   end.


%% ***********************************************************************
%% Typeface Specifications 
%% ***********************************************************************


%% @doc Return times typefaces

-spec times(Tag      :: atom(),
            FontSize :: integer()) -> tuple().

times(Tag, FontSize) ->
   {[Tag],
    [tag_face(default, "Times-Roman", FontSize),
     tag_face(Tag,     "Times-Roman", FontSize),
     tag_face(em,      "Times-Italic", FontSize),
     tag_face(b,       "Times-Bold", FontSize),
     tag_face(code,    "Courier", FontSize) 
    ]}.


%% @doc Return helvetica typefaces

-spec helvetica(Tag      :: atom(),
                FontSize :: integer()) -> tuple().

helvetica(Tag, FontSize) ->
   {[Tag],
    [tag_face(default, "Helvetica", FontSize),
     tag_face(Tag,     "Helvetica", FontSize),
     tag_face(em,      "Helvetica_Oblique", FontSize),
     tag_face(b,       "Helvetica-Bold", FontSize),
     tag_face(code,    "Courier", FontSize) 
    ]}.


%% @doc Return courier typefaces

-spec courier(Tag      :: atom(),
              FontSize :: integer()) -> tuple().

courier(Tag, FontSize) ->
   {[Tag],
    [tag_face(default, "Courier", FontSize),
     tag_face(Tag,     "Courier", FontSize),
     tag_face(em,      "Courier_Italic", FontSize),
     tag_face(b,       "Courier-Bold", FontSize),
     tag_face(code,    "Courier", FontSize) 
    ]}.


%% @doc Default type specification

-spec default(FontSize :: integer()) -> tuple().

default(FontSize) ->
   {[h1],
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
%% Default typeface 
%% ***********************************************************************


%% @doc Return default typeface 

-spec default_face() -> tuple().

default_face() ->
   eg_pdf:default_face().


%% ***********************************************************************
%% Type specification functions
%% ***********************************************************************


%% @doc Create tagged type face

-spec tag_face(Tag :: atom(), 
               Font :: string(),
               FontSize :: integer()) -> tuple().

tag_face(Tag, Font, FontSize) ->
    FaceMap = ep_face:create(Font, FontSize),
    {Tag, ep_face:make_face(FaceMap)}.


