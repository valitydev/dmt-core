-module(dmt).
-behaviour(application).
-behaviour(supervisor).

-export([start/2]).
-export([stop/1]).
-export([init/1]).

-export_type([version/0]).
-export_type([head/0]).
-export_type([ref/0]).
-export_type([snapshot/0]).
-export_type([commit/0]).
-export_type([operation/0]).
-export_type([history/0]).
-export_type([object_ref/0]).
-export_type([domain/0]).
-export_type([domain_object/0]).

-type version() :: dmsl_domain_config_thrift:'Version'().
-type head() :: dmsl_domain_config_thrift:'Head'().
-type ref() :: dmsl_domain_config_thrift:'Reference'().
-type snapshot() :: dmsl_domain_config_thrift:'Snapshot'().
-type commit() :: dmsl_domain_config_thrift:'Commit'().
-type operation() :: dmsl_domain_config_thrift:'Operation'().
-type history() :: dmsl_domain_config_thrift:'History'().
-type object_ref() :: dmsl_domain_thrift:'Reference'().
-type domain() :: dmsl_domain_thrift:'Domain'().
-type domain_object() :: dmsl_domain_thrift:'DomainObject'().

-spec start(normal, any()) ->
    {ok, pid()} | {error, any()}.

start(_StartType, _Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec stop(term()) -> ok.

stop(_State) ->
    ok.

%%

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    Cache = #{id => dmt_cache, start => {dmt_cache, start_link, []}, restart => permanent},
    {ok, {#{strategy => one_for_one, intensity => 10, period => 60}, [Cache]}}.
