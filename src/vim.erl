%% @author Danielle E. Sucher <dsucher@gmail.com>
%% @copyright 2013 Danielle E. Sucher

-module(vim).
-export([handle_regex/2]).

-define(SUBSTITUTION, "/<span class='highlight'>\\0<\\/span>" ++
  "VRMG<br>\\&#92;1: \\1<br>\\&#92;2: \\2<br>\\&#92;3: \\3VRMG/g\"").


handle_regex(Text, Pattern) ->
  Filename = mktemp(),
  file:write_file(Filename, Text),
  VimCommand = substitution_command(Filename, Pattern),
  Port = open_port({spawn, VimCommand}, [exit_status, stderr_to_stdout]),
  loop_until_vim_is_done(Filename, Port).


substitution_command(Filename, Pattern) ->
  SafePattern = re:replace(Pattern, "\"", [92,92,92,34], [{return, list}]),
  Substitute = " -c \"%s/" ++ SafePattern ++ ?SUBSTITUTION,
  "vim -X " ++ Filename ++ Substitute ++ " -c \"x\"".


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


mktemp() ->
  string:strip(os:cmd("mktemp -t virextmp.XXXXXXX"), both, $\n).

