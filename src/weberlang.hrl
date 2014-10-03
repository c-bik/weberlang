-define(I(__F,__A), io:format(user, __F, __A)).
-define(I(__F), ?I(__F, [])).

-define(PRIVDIR,
        (fun() ->
                 case code:priv_dir(weberlang) of
                     {error, bad_name} -> "priv";
                     PDir -> PDir
                 end
         end)()).
