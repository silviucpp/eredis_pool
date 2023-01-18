
% logs

-include_lib("kernel/include/logger.hrl").

-define(LOG_PRINT(Format, Args),
    io:format(Format, Args)).

% types

-type reason() :: term().
-type return_value() :: eredis:return_value().
-type pipeline() :: eredis:pipeline().
-type redis_error() :: binary() | no_nodes_available | tuple().
