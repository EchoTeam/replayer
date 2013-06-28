-module(log_parser).
-export([behaviour_info/1]).

% reader function should be of type
% -type continuation_fun() :: fun(() -> eof | {continuation_fun(), [replayer_utils:request()]}).
% -spec reader(File :: string()) -> {continuation_fun(), [replayer_utils:request()]}.
behaviour_info(callbacks) -> [{reader, 1}];
behaviour_info(_Other) -> undefined.
