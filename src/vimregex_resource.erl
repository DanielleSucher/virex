%% @author Danielle E. Sucher <dsucher@gmail.com>
%% @copyright 2013 Danielle E. Sucher

-module(vimregex_resource).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
    Text = wrq:get_qs_value("text", "", ReqData),
    Pattern = wrq:get_qs_value("pattern", "", ReqData),
    Highlighted = highlight(Text, Pattern),
    {ok, Content} = highlighted_dtl:render([{highlighted, Highlighted}]),
    {Content, ReqData, State}.


highlight(Text, Pattern) ->
  Filename = mktemp(),
  file:write_file(Filename, Text),
  os:cmd(vim_command(Filename, Pattern)),
  {ok, Highlighted} = file:read_file(Filename),
  file:delete(Filename),
  Highlighted.


mktemp() ->
  string:strip(os:cmd("mktemp -t vimregextmp.XXXXXXX"), both, $\n).


vim_command(Filename, Pattern) ->
  SafePattern = escape_quotes_for_vim(Pattern),
  Substitute = " -c \"%s/" ++ SafePattern ++ "/<span class='highlight'>\\0<\\/span>/g\"",
  "vim -X " ++ Filename ++ Substitute ++ " -c \"x\"".


escape_quotes_for_vim(Pattern) ->
  re:replace(Pattern, "\"", [92,92,92,34], [{return, list}]).
