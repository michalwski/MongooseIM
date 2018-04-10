-module(push_helper).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").

-define(NS_XDATA,               <<"jabber:x:data">>).

-export([enable_stanza/2, enable_stanza/3, enable_stanza/4,
         disable_stanza/1, disable_stanza/2,
         make_form/1, maybe_form/2]).

-export([become_unavailable/1]).
-export([wait_for/2]).

-export([ns_push/0, ns_pubsub_pub_options/0, push_form_type/0]).

ns_push() -> <<"urn:xmpp:push:0">>.
ns_pubsub_pub_options() -> <<"http://jabber.org/protocol/pubsub#publish-options">>.
push_form_type()-> <<"urn:xmpp:push:summary">>.


disable_stanza(JID, undefined) ->
    disable_stanza([
        {<<"xmlns">>, <<"urn:xmpp:push:0">>},
        {<<"jid">>, JID}
    ]);
disable_stanza(JID, Node) ->
    disable_stanza([
        {<<"xmlns">>, <<"urn:xmpp:push:0">>},
        {<<"jid">>, JID},
        {<<"node">>, Node}
    ]).
disable_stanza(JID) when is_binary(JID) ->
    disable_stanza(JID, undefined);
disable_stanza(Attrs) when is_list(Attrs) ->
    escalus_stanza:iq(<<"set">>, [#xmlel{name = <<"disable">>, attrs = Attrs}]).

enable_stanza(JID, Node) ->
    enable_stanza(JID, Node, undefined).
enable_stanza(JID, Node, FormFields) ->
    enable_stanza(JID, Node, FormFields, ns_pubsub_pub_options()).
enable_stanza(JID, Node, FormFields, FormType) ->
    escalus_stanza:iq(<<"set">>, [#xmlel{name = <<"enable">>, attrs = [
        {<<"xmlns">>, <<"urn:xmpp:push:0">>},
        {<<"jid">>, JID},
        {<<"node">>, Node}
    ], children = maybe_form(FormFields, FormType)}]).

maybe_form(undefined, _FormType) ->
    [];
maybe_form(FormFields, FormType) ->
    [make_form([{<<"FORM_TYPE">>, FormType} | FormFields])].

make_form(Fields) ->
    #xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"submit">>}],
           children = [make_form_field(Name, Value) || {Name, Value} <- Fields]}.

make_form_field(Name, Value) ->
    #xmlel{name = <<"field">>,
           attrs = [{<<"var">>, Name}],
           children = [#xmlel{name = <<"value">>, children = [#xmlcdata{content = Value}]}]}.

become_unavailable(Client) ->
    escalus:send(Client, escalus_stanza:presence(<<"unavailable">>)),
    true = wait_for(timer:seconds(20), fun() ->
        is_offline(escalus_utils:jid_to_lower(escalus_client:username(Client)),
                   escalus_utils:jid_to_lower(escalus_client:server(Client)))
    end). %% There is no ACK for unavailable status

is_offline(LUser, LServer) ->
    case catch lists:max(escalus_ejabberd:rpc(ejabberd_sm, get_user_present_pids, [LUser, LServer])) of
        {Priority, _} when is_integer(Priority), Priority >= 0 ->
            false;
        _ ->
            true
    end.

wait_for(TimeLeft, Fun) when TimeLeft < 0 ->
    Fun();
wait_for(TimeLeft, Fun) ->
    Step = 500,
    try
        case Fun() of
            ok -> ok;
            true -> true;
            {ok, _} = R -> R
        end
    catch
        _:_ ->
            timer:sleep(Step),
            wait_for(TimeLeft - Step, Fun)
    end.

