-define(I(__F,__A), io:format(user, __F, __A)).
-define(I(__F), ?I(__F, [])).

-define(PRIVDIR,
        (fun() ->
                 case code:priv_dir(weberlang) of
                     {error, bad_name} -> "priv";
                     PDir -> PDir
                 end
         end)()).

-define(pidtob64(__P),  base64:encode(term_to_binary(__P))).
-define(b64topid(__BP), binary_to_term(base64:decode(__BP))).
