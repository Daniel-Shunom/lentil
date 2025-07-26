%%--------------------------------------------------------------------
%% @doc
%%   Poll OS metrics and forward them to a Gleam actor subject.
%%--------------------------------------------------------------------
-module(osmon_gs).
-behaviour(gen_server).

-export([start_link/2, stop/1, ffi_start/2]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {
    subject    :: term(),           %% Gleam process.Subject(Msg)
    interval   :: non_neg_integer(),%% poll interval millis
    timer_ref  :: reference()       %% active timer reference
}).

-spec start_link(term(), non_neg_integer()) ->
          {ok, pid()} | {error, term()}.
start_link(GleamSubject, IntervalMs)
    when is_integer(IntervalMs), IntervalMs > 0 ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
                          {GleamSubject, IntervalMs}, []).

-spec stop(pid()) -> ok.
stop(ServerPid) when is_pid(ServerPid) ->
    gen_server:call(ServerPid, stop).

-spec ffi_start(term(), non_neg_integer()) -> pid() | no_return().
ffi_start(GleamSubject, IntervalMs)
    when is_integer(IntervalMs), IntervalMs > 0 ->
    case gen_server:start_link({local, ?MODULE}, ?MODULE,
                               {GleamSubject, IntervalMs}, []) of
        {ok, Pid} ->
            Pid;
        {error, Reason} ->
            erlang:error({start_failed, Reason})
    end.

-spec init({term(), non_neg_integer()}) ->
          {ok, #state{}} | {stop, term()}.
init({GleamSubject, Interval}) ->
    _ = application:ensure_all_started(os_mon),
    Ref = erlang:send_after(Interval, self(), poll),
    {ok, #state{subject   = GleamSubject,
                interval  = Interval,
                timer_ref = Ref}}.

% -type osmon_msg() :: {osmon_msg, integer(), integer(), integer()}.

-spec handle_info(poll, #state{}) -> {noreply, #state{}}.
handle_info(poll, State = #state{subject = Subj, interval = I}) ->
    {TotalMem, UsedMem} = sys_utils:get_mem(),
    CpuLoad             = sys_utils:get_load(),

    OsmonMsg = {osmon_msg, TotalMem, UsedMem, CpuLoad},

    Subj ! OsmonMsg,

    Ref2 = erlang:send_after(I, self(), poll),
    {noreply, State#state{timer_ref = Ref2}};

handle_info(_Other, State) ->
    {noreply, State}.

-spec handle_call(stop, {pid(), term()}, #state{}) ->
          {stop, normal, ok, #state{}}.
handle_call(stop, _From, State = #state{timer_ref = Ref}) ->
    _ = timer:cancel(Ref),
    {stop, normal, ok, State};
handle_call(_Req, _From, State) ->
    {reply, {error, unsupported}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), #state{}, term()) ->
          {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
