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
  VimCommand = vim_command(Filename, Pattern),
  Port = open_port({spawn, VimCommand}, [exit_status, stderr_to_stdout]),
  loop_until_vim_is_done(Filename, Port).


mktemp() ->
  string:strip(os:cmd("mktemp -t vimregextmp.XXXXXXX"), both, $\n).


vim_command(Filename, Pattern) ->
  SafePattern = escape_quotes_for_vim(Pattern),
  Substitute = " -c \"%s/" ++ SafePattern ++ "/<span class='highlight'>\\0<\\/span>/g\"",
  "vim -X " ++ Filename ++ Substitute ++ " -c \"x\"".


escape_quotes_for_vim(Pattern) ->
  re:replace(Pattern, "\"", [92,92,92,34], [{return, list}]).


loop_until_vim_is_done(Filename, Port) ->
  receive
    {Port, {data, _}} ->
      loop_until_vim_is_done(Filename, Port);
    {Port, {exit_status, 0}} ->
      {ok, Highlighted} = file:read_file(Filename),
      file:delete(Filename),
      Highlighted;
    _ ->
      error
  end.
