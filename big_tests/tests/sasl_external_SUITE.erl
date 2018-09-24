-module(sasl_external_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, fast_tls}].

groups() ->
    G = [{fast_tls, [], [cert_with_cn_xmpp_addresses_requested_correct_user]}],
    %ct_helper:repeat_all_until_all_ok(G).
    G.

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

cert_with_cn_xmpp_addresses_requested_correct_user(C) ->
    SSLDir = filename:join([path_helper:repo_dir(C), "tools", "ssl"]),

    AliceConfig = filename:join(?config(data_dir, C), "openssl-alice.cnf"),
    AliceKey = filename:join(?config(priv_dir, C), "alice_key.pem"),
    AliceCsr = filename:join(?config(priv_dir, C), "alice.csr"),

    Cmd = ["openssl", "req", "-config", AliceConfig, "-newkey", "rsa:2048", "-sha256", "-nodes",
	   "-out", AliceCsr, "-keyout", AliceKey, "-outform", "PEM"],
    ct:pal("Sign request"),
    {done, 0, Output} = erlsh:run(Cmd),

    AliceCert = filename:join(?config(priv_dir, C), "alice_cert.pem"),
    SignCmd = filename:join(?config(data_dir, C), "sign_cert.sh"),
    Cmd2 = [SignCmd, "--req", AliceCsr, "--out", AliceCert],
    ct:pal("Signinig by CA"),
    LogFile = filename:join(?config(priv_dir, C), "singing.log"),
    {done, 0, _} = erlsh:run(Cmd2, LogFile, SSLDir),

    UserSpec = [{username, <<"alice">>},
		{server, <<"localhost">>},
		{password, <<"break_me">>},
		{endpoint, {server, <<"alice@localhost">>}},
		{auth, {escalus_auth, auth_sasl_external}},
		{ssl_opts, [{certfile, AliceCert},
			    {keyfile, AliceKey}]},
		{starttls, required}],

    {ok, Client, _} = escalus_connection:start(UserSpec),

    escalus_connection:stop(Client).


