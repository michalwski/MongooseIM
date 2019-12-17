%%%-------------------------------------------------------------------
%%% @author Rafal Slota
%%% @copyright (C) 2017 Erlang Solutions Ltd.
%%% This software is released under the Apache License, Version 2.0
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Default plugin module for mod_event_pusher_push.
%%% This module allows for some dynamic customizations.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_event_pusher_push_plugin_defaults).
-behavior(mod_event_pusher_push_plugin).
-author('rafal.slota@erlang-solutions.com').

-include("jlib.hrl").
-include("mongoose.hrl").

%% Callback API
-export([should_publish/3, sender_id/2]).
-export([publish_notification/5]).

-define(PUSH_FORM_TYPE, <<"urn:xmpp:push:summary">>).

-type push_payload() :: [{Key :: binary(), Value :: binary()}].

%%--------------------------------------------------------------------
%% Callbacks
%%--------------------------------------------------------------------

%% Callback 'should_publish'
-spec should_publish(From :: jid:jid(), To :: jid:jid(), Packet :: exml:element()) ->
                            boolean().
should_publish(_From, To = #jid{luser = LUser, lserver = LServer}, Packet) ->
    try ejabberd_users:does_user_exist(LUser, LServer) of
        false ->
            false;
        true ->
            ejabberd_sm:is_offline(To) andalso has_body(Packet)
    catch
        _:_ ->
            ejabberd_sm:is_offline(To) andalso has_body(Packet)
    end.

%% Callback 'sender_id'
-spec sender_id(From :: jid:jid(), Packet :: exml:element()) -> SenderId :: binary().
sender_id(From, Packet) ->
    case exml_query:attr(Packet, <<"type">>) of
        <<"chat">> ->
            jid:to_binary(jid:to_bare(jid:to_lower(From)));
        <<"groupchat">> ->
            jid:to_binary(jid:to_lower(From))
    end.

-spec publish_notification(Acc :: mongooseim_acc:t(),
                           From :: jid:jid(),
                           To :: jid:jid(),
                           Packet :: exml:element(),
                           Services :: [mod_event_pusher_push:publish_service()]) ->
    mongooseim_acc:t().
publish_notification(Acc0, From, #jid{lserver = Host} = To, Packet, Services) ->
    {Acc1, MessageCount} = get_unread_count(Acc0, To),
    VirtualPubsubHosts = mod_event_pusher_push:virtual_pubsub_hosts(Host),
    PushPayload = push_content_fields(From, Packet, MessageCount),
    lists:foreach(fun({PubsubJID, _Node, _Form} = Service) ->
                          case lists:member(PubsubJID#jid.lserver, VirtualPubsubHosts) of
                              true ->
                                  publish_via_hook(Acc0, Host, To, Service, PushPayload);
                              false ->
                                  publish_via_pubsub(Host, To, Service, PushPayload)
                          end
                  end, Services),
    Acc1.

-spec publish_via_hook(Acc :: mongooseim_acc:t(),
                       Host :: jid:server(),
                       To :: jid:jid(),
                       Service :: mod_event_pusher_push:publish_service(),
                       PushPayload :: push_payload()) -> any().
publish_via_hook(Acc0, Host, To, {PubsubJID, Node, Form}, PushPayload) ->
    OptionMap = maps:from_list(Form),
    BareRecipient = jid:to_bare(To),
    HookArgs = [Host, [maps:from_list(PushPayload)], OptionMap],
    %% Acc is ignored by mod_push_service_mongoosepush, added here only for
    %% tracability purposes and push_SUITE code unification
    Acc = mongoose_acc:set(push_notifications, pubsub_jid, PubsubJID, Acc0),
    case ejabberd_hooks:run_fold(push_notifications, Host, Acc, HookArgs) of
        {error, device_not_registered} ->
            %% We disable the push node in case the error type is device_not_registered
            ejabberd_sm:remove_info(To#jid.luser, To#jid.lserver, To#jid.lresource,
                                    push_notifications),
            mod_event_pusher_push_backend:disable(BareRecipient, PubsubJID, Node);
        _ -> ok
    end.

-spec publish_via_pubsub(Host :: jid:server(),
                         To :: jid:jid(),
                         Service :: mod_event_pusher_push:publish_service(),
                         PushPayload :: push_payload()) -> any().
publish_via_pubsub(Host, To, {PubsubJID, Node, Form}, PushPayload) ->
    Stanza = push_notification_iq(Node, Form, PushPayload),
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => To#jid.lserver,
                              element => jlib:iq_to_xml(Stanza),
                              from_jid => To,
                              to_jid => PubsubJID }),

    ResponseHandler =
    fun(_From, _To, Acc1, Response) ->
            mod_event_pusher_push:cast(Host, handle_publish_response,
                                       [To, PubsubJID, Node, Response]),
            Acc1
    end,
    %% The IQ is routed from the recipient's server JID to pubsub JID
    %% This is recommended in the XEP and also helps process replies to this IQ
    NotificationFrom = jid:make(<<>>, Host, <<>>),
    mod_event_pusher_push:cast(Host, ejabberd_local, route_iq,
                               [NotificationFrom, PubsubJID, Acc, Stanza, ResponseHandler]).

-spec push_notification_iq(Node :: mod_event_pusher_push:pubsub_node(),
                           Form :: mod_event_pusher_push:form(),
                           PushPayload :: push_payload()) -> jlib:iq().
push_notification_iq(Node, Form, PushPayload) ->
    NotificationFields = [{<<"FORM_TYPE">>, ?PUSH_FORM_TYPE} | PushPayload ],

    #iq{type = set, sub_el = [
        #xmlel{name = <<"pubsub">>, attrs = [{<<"xmlns">>, ?NS_PUBSUB}], children = [
            #xmlel{name = <<"publish">>, attrs = [{<<"node">>, Node}], children = [
                #xmlel{name = <<"item">>, children = [
                    #xmlel{name = <<"notification">>,
                           attrs = [{<<"xmlns">>, ?NS_PUSH}],
                           children = [make_form(NotificationFields)]}
                ]}
            ]}
        ] ++ maybe_publish_options(Form)}
    ]}.

-spec push_content_fields(From :: jid:jid(),
                          Packet :: exml:element(),
                          MessageCount :: non_neg_integer()) -> push_payload().
push_content_fields(From, Packet, MessageCount) ->
    [
     {<<"message-count">>, integer_to_binary(MessageCount)},
     {<<"last-message-sender">>, sender_id(From, Packet)},
     {<<"last-message-body">>, exml_query:cdata(exml_query:subelement(Packet, <<"body">>))}
    ].

-spec maybe_publish_options(mod_event_pusher_push:form()) -> [exml:element()].
maybe_publish_options([]) ->
    [];
maybe_publish_options(FormFields) ->
    [#xmlel{name = <<"publish-options">>,
            children = [
                        make_form([{<<"FORM_TYPE">>, ?NS_PUBSUB_PUB_OPTIONS}] ++ FormFields)
                       ]}].

-spec make_form(mod_event_pusher_push:form()) -> exml:element().
make_form(Fields) ->
    #xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"submit">>}],
           children = [make_form_field(Field) || Field <- Fields]}.

-spec make_form_field(mod_event_pusher_push:form_field()) -> exml:element().
make_form_field({Name, Value}) ->
    #xmlel{name = <<"field">>,
           attrs = [{<<"var">>, Name}],
           children = [#xmlel{name = <<"value">>, children = [#xmlcdata{content = Value}]}]}.

get_unread_count(Acc, To) ->
    Acc0 = ejabberd_hooks:run_fold(inbox_unread_count, To#jid.lserver, Acc, [To]),
    UnreadCount = mongoose_acc:get(inbox, unread_count, 1, Acc0),
    {Acc0, UnreadCount}.

has_body(Packet) ->
    case exml_query:path(Packet, [{element, <<"body">>}, cdata]) of
        <<>> ->
            false;
        _ ->
            true
    end.
