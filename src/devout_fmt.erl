-module(devout_fmt).

-include("devout.hrl").

-export([err/1]).

err({child_creation_failed, Child, SubReason}) when is_binary(Child) ->
    SubReasonBin = err(SubReason),
    <<"Failed to create child directory ", Child/binary, ": ", SubReasonBin/binary>>;
err(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8).