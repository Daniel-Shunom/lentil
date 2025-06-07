%%--------------------------------------------------------------------
%% file: sanitizer.erl
%%--------------------------------------------------------------------
-module(sanitizer).
-author("Daniel Jeremiah <danielshunom2@gmail.com>").
-export([sanitize_text/1]).

-define(PROFANE_LIST_FILE, "priv/profane_words.txt").

-spec sanitize_text(string()) -> string().
sanitize_text(Text) when is_list(Text) ->
    %% 1) Load the profanity list at runtime
    {ok, Bin} = file:read_file(?PROFANE_LIST_FILE),
    Lines     = string:split(binary_to_list(Bin), "\n", all),
    Words     = [ string:trim(L) || L <- Lines, L =/= "" ],

    %% 2) Build the feck config
    BaseConfig = feck:configure([{blacklist, Words}]),
    FullConfig = feck:configure([{replacement, stars}], BaseConfig),

    %% 3) Sanitize and return
    feck:sanitize(Text, FullConfig).
