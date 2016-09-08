-module(mongoose_client_api).

-export([init/3]).
-export([content_types_provided/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([to_json/2]).
-export([rest_init/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _HandlerOpts) ->
    {ok, Req, #{}}.

is_authorized(Req, State) ->
    Auth = cowboy_req:parse_header(<<"authorization">>, Req),
    case Auth of
        {ok, undefined, _} ->
            make_unauthorized_response(Req, State);
        {ok, AuthDetails, Req2} ->
            do_authorize(AuthDetails, Req2, State)
    end.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, '*'}, to_json}
     ], Req, State}.

to_json(Req, User) ->
    {<<"{}">>, Req, User}.

do_authorize({<<"basic">>, {User, Password}}, Req, State) ->
    case jid:from_binary(User) of
        error ->
            make_unauthorized_response(Req, State);
        JID ->
            do_check_password(User, JID, Password, Req, State)
    end;
do_authorize(_, Req, State) ->
    make_unauthorized_response(Req, State).

do_check_password(RawUser, #jid{luser = User, lserver = Server} = JID,
                  Password, Req, State) ->
    case ejabberd_auth:check_password(User, Server, Password) of
        true ->
            {true, Req, State#{user => RawUser, jid => JID}};
        _ ->
            make_unauthorized_response(Req, State)
    end.

make_unauthorized_response(Req, State) ->
    {{false, <<"Basic realm=\"mongooseim\"">>}, Req, State}.

