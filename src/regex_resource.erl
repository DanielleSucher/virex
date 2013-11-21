%% @author Danielle E. Sucher <dsucher@gmail.com>
%% @copyright 2013 Danielle E. Sucher

-module(regex_resource).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
    Text = wrq:get_qs_value("text", "", ReqData),
    Pattern = wrq:get_qs_value("pattern", "", ReqData),
    {Match, Groups} = vim:handle_regex(Text, Pattern),
    {ok, Content} = results_dtl:render([{match, Match}, {groups, Groups}]),
    {Content, ReqData, State}.
