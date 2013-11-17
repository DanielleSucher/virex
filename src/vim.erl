%% @author Danielle E. Sucher <dsucher@gmail.com>
%% @copyright 2013 Danielle E. Sucher

-module(vim).
-export([handle_regex/2]).

-define(SUBSTITUTION, "/VIREXSTARTH\\0VIREXSTOPH" ++
  "VRMGVIREXBR1: \\1VIREXBR2: \\2VIREXBR3: \\3VRMG/g\"").

-define(HTMLFORMAT, " -c \"%s/<\\|>\\|&\\|VIREXSTARTH\\|" ++
  "VIREXSTOPH\\|VIREXBR/\\={'&' : '&amp;', '<' : '&lt;', '>' : '&gt;', " ++
  "'VIREXSTARTH' : '<span class=''highlight''>', 'VIREXSTOPH' : \\\"<\\/" ++
  "span>\\\", 'VIREXBR' : '<br>&#92;'}[submatch(0)]/g \"").

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
      Substitute = " -c \"%s/" ++ SafePattern ++ ?SUBSTITUTION ++ ?HTMLFORMAT,
      {ok, "vim -X " ++ Filename ++ Substitute ++ " -c \"x\""}
  end.


secure_pattern(Filename, Pattern) when erlang:length(Pattern) > 80 ->
  reject_pattern(Filename);
secure_pattern(Filename, Pattern) ->
  case re:run(Pattern, "\{-?[0-9]{3,}|[0-9]{3,}\\\\?\}|([^\\\\]|^)(\\\\\\\\)*/") of
    {match, _} ->
      reject_pattern(Filename);
    nomatch ->
      escape_pattern_quotes(Pattern)
  end.


reject_pattern(Filename) ->
  file:delete(Filename),
  rejected.


escape_pattern_quotes(Pattern) ->
  re:replace(Pattern, "\"", "\\\\\\\"", [global, {return, list}]).


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
