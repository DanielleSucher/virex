%% @author Danielle E. Sucher <dsucher@gmail.com>
%% @copyright 2013 Danielle E. Sucher

-module(vimregex_resource).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

-define(SUBSTITUTION, "/<span class='highlight'>\\0<\\/span><br><br>" ++ 
  "Match groups:<br><br>\\&#92;1: \1<br>\\&#92;2: \2<br>\\&#92;3: \3<br>" ++
  "\\&#92;4: \4<br>\\&#92;5: \5<br>\\&#92;6: \6<br>\\&#92;7: \7/g\"").

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
    Text = wrq:get_qs_value("text", "", ReqData),
    Pattern = wrq:get_qs_value("pattern", "", ReqData),
    Highlighted = highlight(Text, Pattern),
    {Highlighted, ReqData, State}.


highlight(Text, Pattern) ->
  Filename = mktemp(),
  file:write_file(Filename, Text),
  VimCommand = vim_command(Filename, Pattern),
  Port = open_port({spawn, VimCommand}, [exit_status, stderr_to_stdout]),
  loop_until_vim_is_done(Filename, Port).


mktemp() ->
  string:strip(os:cmd("mktemp -t vimregextmp.XXXXXXX"), both, $\n).


vim_command(Filename, Pattern) ->
  Substitute = " -c \"%s/" ++ escape_quotes_for_vim(Pattern) ++ ?SUBSTITUTION,
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
