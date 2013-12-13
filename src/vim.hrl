%% @author Danielle E. Sucher <dsucher@gmail.com>
%% @copyright 2013 Danielle E. Sucher

-define(SUBSTITUTION, "VIREXSTARTH\\0VIREXSTOPH").

-define(GROUP_SUBSTITUTION, "VRMG" ++
    "VIREXBR" ++
      "1: \\1" ++
    "VIREXBR" ++
      "2: \\2" ++
    "VIREXBR" ++
      "3: \\3" ++
  "VRMG").

-define(HTMLFORMAT, "%s/" ++
    "<" ++
    "\\|" ++
    ">" ++
    "\\|" ++
    "&" ++
    "\\|" ++
    "VIREXSTARTH" ++
    "\\|" ++
    "VIREXSTOPH" ++
    "\\|" ++
    "VIREXBR" ++
  "/" ++
    "\\={" ++
      "'&' : '&amp;', " ++
      "'<' : '&lt;', " ++
      "'>' : '&gt;', " ++
      "'VIREXSTARTH' : '<span class=''highlight''>', " ++
      "'VIREXSTOPH' : \"<\\/span>\", " ++
      "'VIREXBR' : '<br>&#92;'" ++
    "}" ++
    "[submatch(0)]" ++
  "/g").
