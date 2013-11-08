%% @author Danielle E. Sucher <dsucher@gmail.com>
%% @copyright 2013 Danielle E. Sucher

-module(regex_resource).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
    Text = wrq:get_qs_value("text", "", ReqData),
    Pattern = wrq:get_qs_value("pattern", "", ReqData),
    {Match, Groups} = match(Text, Pattern),
    {ok, Content} = results_dtl:render([{match, Match}, {groups, Groups}]),
    {Content, ReqData, State}.


match(Text, Pattern) ->
  VimResult = vim:handle_regex(Text, Pattern),
  Tokens = string:tokens(binary_to_list(VimResult), "VRMG"),
  case Tokens of
    [_|[]] ->
      {Tokens, ""};
    _ ->
      split_highlighted_and_groups(Tokens)
  end.


split_highlighted_and_groups(Tokens) ->
  IsMatchGroup = fun([A,B|_]) -> [A] =:= "<" andalso B =:= $b; (_) -> false end,
  {Groups, Match} = lists:partition(IsMatchGroup, Tokens),
  {string:join(Match, ""), string:join(Groups, "<br>")}.
