-module(thing_statem).
-behaviour(gen_statem).

%% API
-export([start_link/0]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([handle_event/4]).

%%%===================================================================
%%% Includes, defines, types and records
%%%===================================================================

-define(SERVER, ?MODULE).

% States
-define(ALIVE, alive).
-define(DIALOGUE, dialogue).
-define(DEAD, dead).

-record(data, {}).



%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
                      ignore |
                      {error, Error :: term()}.
start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

% TODO: Implement custom registry modules for rooms, objects, entities, and players.
% Or maybe just two separate ones, one for players and one for the rest.
% ServerName = {via, Module, Name}
% A benefit of this is that Names can be strings rather than atoms.
% https://chatgpt.com/share/c3e1bfe2-530f-42e7-a43d-78b938e2b74b
% start_link(Player_name) ->
%     gen_statem:start_link(ServerName, Module, Args, Opts).
%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Define the callback_mode() for this callback module.
%% @end
%%--------------------------------------------------------------------
-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> handle_event_function.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%% @end
%%--------------------------------------------------------------------
-spec init(Flags :: list()) -> gen_statem:init_result(term()).
init([]) ->
    process_flag(trap_exit, true),
    {ok, state_name, #data{}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for every event a gen_statem receives.
%% @end
%%--------------------------------------------------------------------
-spec handle_event('enter',
                   OldState :: term(),
                   State :: term(),
                   Data :: term()) ->
          gen_statem:state_enter_result(term());
                  (gen_statem:event_type(),
                   Msg :: term(),
                   State :: term(),
                   Data :: term()) ->
          gen_statem:event_handler_result(term()).

% Player is alive
handle_event({call,From}, _Msg, ?ALIVE, Data) ->
    {keep_state, Data, [{reply,From,ok}]};

% Player is speaking to NPC
handle_event({call,From}, _Msg, ?DIALOGUE, Data) ->
    {keep_state, Data, [{reply,From,ok}]};

% Player is dead
handle_event({call,From}, _Msg, ?DEAD, Data) ->
    {keep_state, Data, [{reply,From,ok}]};

% Any other event/state
handle_event({call,From}, _Msg, State, Data) ->
    {next_state, State, Data, [{reply,From,ok}]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: term(), Data :: term()) -> any().
terminate(_Reason, _State, _Data) ->
    void.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down,term()},
                  State :: term(),
                  Data :: term(),
                  Extra :: term()) ->
          {ok, NewState :: term(), NewData :: term()} |
          (Reason :: term()).
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
