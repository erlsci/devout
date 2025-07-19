-module(devout_fmt).

error({child_creation_failed, Child, SubReason}) when is_binary(Child) ->
    SubReasonBin = error(SubReason),
    <<"Failed to create child directory ", Child/binary, ": ", SubReasonBin/binary>>;
error(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8).
