%% @author Danielle E. Sucher <dsucher@gmail.com>
%% @copyright 2013 Danielle E. Sucher

-module(static_resource).
-export([init/1, content_types_provided/2, resource_exists/2, content/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {root, filepath}).

init(Options) ->
    {ok, #context{root=proplists:get_value(root, Options)}}.

content_types_provided(ReqData, Context) ->
    Mime = webmachine_util:guess_mime(uri(ReqData)),
    {[{Mime, content}], ReqData, Context}.

resource_exists(ReqData, Context=#context{root=Root}) ->
    FilePath = filename:join([Root|clean_path(uri(ReqData))]),
    case filelib:is_regular(FilePath) of
      true ->
        {true, ReqData, Context#context{filepath=FilePath}};
      _ ->
        {false, ReqData, Context}
    end.

content(ReqData, Context=#context{filepath=FilePath}) ->
    {ok, Data} = file:read_file(FilePath),
    {Data, ReqData, Context}.

clean_path(Path) ->
    Fun = fun("..", [])       -> [];     % no shallower
             ("..", [_|Rest]) -> Rest;   % one shallower
             (P,    Acc)      -> [P|Acc] % one deeper
          end,
    lists:reverse(lists:foldl(Fun, [], string:tokens(Path, "/"))).

uri(ReqData) ->
  Uri = wrq:disp_path(ReqData),
  case Uri of
    "" -> "index.html";
    _ -> Uri
  end.
