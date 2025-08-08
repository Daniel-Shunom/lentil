%%--------------------------------------------------------------------
%% file: sanitizer.erl
%%--------------------------------------------------------------------
-module(sanitizer).
-author("Daniel Jeremiah <danielshunom2@gmail.com>").
-export([sanitize_text/1]).

-define(PROFANE_LIST_FILE, "priv/profane_words.txt").

-spec sanitize_text(binary() | string()) -> binary().
sanitize_text(Text) when is_binary(Text) ->
    sanitize_text(binary_to_list(Text));
sanitize_text(Text) when is_list(Text) ->
    {ok, Bin} = file:read_file(?PROFANE_LIST_FILE),
    Lines     = string:split(binary_to_list(Bin), "\n", all),
    Words     = [ string:trim(L) || L <- Lines, L =/= "" ],

    BaseConfig = feck:configure([{blacklist, Words}]),
    % options are "garbled", "vowels", "stars" and others
    % we're just gonna use these options for now cause
    % they're better :)
    FullConfig = feck:configure([{replacement, garbled}], BaseConfig),

    SanitizedList = feck:sanitize(Text, FullConfig),

    list_to_binary(SanitizedList).
