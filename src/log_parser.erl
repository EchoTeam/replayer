-module(log_parser).

-export([
    behaviour_info/1
]).

% reader function should be of type
% -type continuation_fun() :: fun(() -> eof | {continuation_fun(), [replayer_utils:request()]}).
% -type option() :: {Key :: term(), Value :: term()}.
% -spec reader(File :: string(), Options :: [option()]) -> {continuation_fun(), [replayer_utils:request()]}.
behaviour_info(callbacks) -> [{reader, 2}];
behaviour_info(_Other) -> undefined.
