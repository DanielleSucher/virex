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


handle_regex(TestString, Pattern) ->
  case secure_pattern(Pattern) of
    rejected ->
      <<"Your pattern has been rejected. Please email me with your use case.">>;
    SafePattern ->
      Filename = create_test_file(TestString),
      VimCommand = substitution_command(Filename, SafePattern),
      Port = open_port({spawn, VimCommand}, [exit_status, stderr_to_stdout]),
      loop_until_vim_is_done(Filename, Port)
  end.


substitution_command(Filename, Pattern) ->
  Substitute = " -c \"%s/" ++ Pattern ++ ?SUBSTITUTION ++ ?HTMLFORMAT,
  "vim -X " ++ Filename ++ Substitute ++ " -c \"x\"".


secure_pattern(Pattern) ->
  case safe(Pattern) of
    true ->
      escape_pattern_quotes(Pattern);
    false ->
      rejected
  end.


safe(Pattern) when erlang:length(Pattern) > 80 ->
  false;
safe(Pattern) ->
  DangerousRegex = "\{-?[0-9]{3,}|[0-9]{3,}\\\\?\}|([^\\\\]|^)(\\\\\\\\)*/",
  re:run(Pattern, DangerousRegex) =:= nomatch.


escape_pattern_quotes(Pattern) ->
  re:replace(Pattern, "\"", "\\\\\\\"", [global, {return, list}]).


loop_until_vim_is_done(Filename, Port) ->
  receive
    {Port, {data, _}} ->
      loop_until_vim_is_done(Filename, Port);
    {Port, {exit_status, 0}} ->
      {ok, VimResult} = file:read_file(Filename),
      file:delete(Filename),
      VimResult;
    _ ->
      error
  end.


mktemp() ->
  string:strip(os:cmd("mktemp -t virextmp.XXXXXXX"), both, $\n).

create_test_file(TestString) ->
  Filename = mktemp(),
  file:write_file(Filename, TestString),
  Filename.
