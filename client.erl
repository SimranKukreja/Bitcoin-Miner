-module(client).
-compile(export_all).

start(ServerAddress) ->
    % Request the server for parameters
    {server, ServerAddress} ! {clientRequestParams, self()},
    receive
        % Get NoOfZeros and WorkUnitSize from the server
        {clientParams, NoOfZeros, WorkUnitSize} ->
            NoOfActors = 10 * erlang:system_info(logical_processors_available),
            spawnActors(NoOfActors, NoOfZeros, ServerAddress, WorkUnitSize),
            exit(self())
    end.

mine(0) ->
    exit(self());
mine(WorkUnitSize) ->
    receive
        {mine, MasterPid, NoOfZeros, WorkerId, ServerAddress} ->
            {_, Output} = randomizer(9),
            Msg = "s.kukreja;" ++ Output,
            Hash = crypto:hash(sha256, Msg),
            EncodeHex = binary:encode_hex(Hash),
            Lower = string:lowercase(EncodeHex),
            ZeroString = getZeroString(NoOfZeros),
            SlicedString = string:slice(Lower, 0, NoOfZeros),
            ComparisonResult = string:equal(SlicedString, ZeroString),
            if
                % If bitcoin found, pass it to the server and continue mining
                ComparisonResult ->
                    {server, ServerAddress} ! {clientMinedBitcoin, MasterPid, Msg, Lower};
                true ->
                    % Else, continue mining
                    WorkerId ! {mine, MasterPid, NoOfZeros, WorkerId, ServerAddress}
            end,
            mine(WorkUnitSize - 1)
    end.

getZeroString(1) -> "0";
getZeroString(K) -> "0" ++ getZeroString(K - 1).

randomizer(Length) ->
    RandBytes = crypto:strong_rand_bytes(Length),
    Base64 = base64:encode(RandBytes),
    BinaryPart = binary_part(Base64, {0, Length}),
    Lower = string:lowercase(BinaryPart),
    {RandBytes, Lower}.

% For spawning actors recursively
spawnActors(0, NoOfZeros, ServerAddress, WorkUnitSize) ->
    WorkerPID = spawn(fun() -> mine(WorkUnitSize) end),
    WorkerPID ! {mine, self(), NoOfZeros, WorkerPID, ServerAddress},
    ok;
spawnActors(NoOfActors, NoOfZeros, ServerAddress, WorkUnitSize) ->
    WorkerPID = spawn(fun() -> mine(WorkUnitSize) end),
    WorkerPID ! {mine, self(), NoOfZeros, WorkerPID, ServerAddress},
    spawnActors(NoOfActors - 1, NoOfZeros, ServerAddress, WorkUnitSize).
