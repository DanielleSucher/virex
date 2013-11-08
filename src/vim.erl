%% @author Danielle E. Sucher <dsucher@gmail.com>
%% @copyright 2013 Danielle E. Sucher

-module(vim).
-export([handle_regex/2]).

-define(SUBSTITUTION, "/<span class='highlight'>\\0<\\/span>" ++
  "VRMG<br>\\&#92;1: \\1<br>\\&#92;2: \\2<br>\\&#92;3: \\3VRMG/g\"").


handle_regex(Text, Pattern) ->
  Filename = mktemp(),
  file:write_file(Filename, Text),
  case substitution_command(Filename, Pattern) of
    rejected ->
      <<"Your pattern has been rejected. Please email me with your use case.">>;
    {ok, VimCommand} ->
      Port = open_port({spawn, VimCommand}, [exit_status, stderr_to_stdout]),
      loop_until_vim_is_done(Filename, Port)
  end.


substitution_command(Filename, Pattern) ->
  case secure_pattern(Filename, Pattern) of
    rejected ->
      rejected;
    SafePattern ->
      Substitute = " -c \"%s/" ++ SafePattern ++ ?SUBSTITUTION,
      {ok, "vim -X " ++ Filename ++ Substitute ++ " -c \"x\""}
  end.


secure_pattern(Filename, Pattern) when erlang:length(Pattern) > 80 ->
  reject_pattern(Filename);
secure_pattern(Filename, Pattern) ->
  case re:run(Pattern, "\{[0-9]{3,}|[0-9]{3,}\\\\*\}") of
    nomatch ->
      re:replace(Pattern, "\"", [92,92,92,34], [global, {return, list}]);
    {match, _} ->
      reject_pattern(Filename)
  end.


reject_pattern(Filename) ->
  file:delete(Filename),
  rejected.


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
