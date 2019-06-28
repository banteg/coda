let component = ReasonReact.statelessComponent("Header");

module Styles = {
  open Css;
  open Theme;

  let header =
    merge([
      style([
        position(`fixed),
        top(`px(0)),
        left(`px(0)),
        right(`px(0)),
        height(Spacing.headerHeight),
        maxHeight(Spacing.headerHeight),
        minHeight(Spacing.headerHeight),
        display(`flex),
        alignItems(`center),
        justifyContent(`spaceBetween),
        color(black),
        fontFamily("IBM Plex Sans, Sans-Serif"),
        padding2(~v=`zero, ~h=Theme.Spacing.defaultSpacing),
        borderBottom(`px(1), `solid, Colors.borderColor),
        CssElectron.appRegion(`drag),
      ]),
      notText,
    ]);

  let logo =
    style([display(`flex), alignItems(`center), marginLeft(`px(4))]);

  let rightButtons =
    style([
      display(`flex),
      alignItems(`center),
      justifyContent(`spaceBetween),
    ]);

  let deactivatedSettings =
    merge([
      Link.Styles.greyLink,
      style([
        padding4(
          ~top=`rem(0.5),
          ~right=`rem(0.75),
          ~bottom=`rem(0.5),
          ~left=`rem(0.5),
        ),
        color(Theme.Colors.slateAlpha(0.5)),
        hover([
          backgroundColor(Theme.Colors.slateAlpha(0.15)),
          borderRadius(`px(6)),
          color(Theme.Colors.slate),
        ]),
      ]),
    ]);

  let activatedSettings =
    merge([
      deactivatedSettings,
      style([
        color(Theme.Colors.hyperlinkAlpha(0.8)),
        backgroundColor(Theme.Colors.hyperlinkAlpha(0.15)),
        borderRadius(`px(6)),
      ]),
    ]);
};

module SyncStatusQ = [%graphql
  {|
    query querySyncStatus {
      syncStatus
    }
  |}
];

module SyncStatusQuery = ReasonApollo.CreateQuery(SyncStatusQ);

module SyncStatus = {
  module SubscriptionGQL = [%graphql
    {|
      subscription syncStatus {
        newSyncUpdate
      }
    |}
  ];

  module Subscription = ReasonApollo.CreateSubscription(SubscriptionGQL);

  [@react.component]
  let make =
      (
        ~result,
        ~subscribeToMore as
          _:
            (
              ~document: ReasonApolloTypes.queryString,
              ~variables: Js.Json.t=?,
              ~updateQuery: ReasonApolloQuery.updateQuerySubscriptionT=?,
              ~onError: ReasonApolloQuery.onErrorT=?,
              unit
            ) =>
            unit,
      ) => {
    // TODO: Replace/remove when we fix/replace the current subscriptions
    /* let _ = */
    /*   React.useEffect0(() => { */
    /*     subscribeToMore(~document=Subscription.graphQLSubscriptionAST, ()); */
    /*     None; */
    /*   }); */
    switch ((result: SyncStatusQuery.response)) {
    | Loading => <Alert kind=`Warning message="Connecting" />
    | Error(_) => <Alert kind=`Danger message="Error" />
    | Data(response) =>
      switch (response##syncStatus) {
      | `OFFLINE => <Alert kind=`Danger message="Offline" />
      | `SYNCED => <Alert kind=`Success message="Synced" />
      | `BOOTSTRAP => <Alert kind=`Warning message="Syncing" />
      }
    };
  };
};

[@react.component]
let make = () => {
  let url = ReasonReact.Router.useUrl();
  let onSettingsPage =
    switch (url.path) {
    | ["settings", ..._] => true
    | _ => false
    };
  <header className=Styles.header>
    <div className=Styles.logo onClick={_ => ReasonReact.Router.push("/")}>
      <img src="CodaLogo.svg" alt="Coda logo" />
    </div>
    <div className=Styles.rightButtons>
      <SyncStatusQuery fetchPolicy="no-cache" partialRefetch=true>
        {response =>
           <SyncStatus
             result={response.result}
             subscribeToMore={response.subscribeToMore}
           />}
      </SyncStatusQuery>
      <Spacer width=0.75 />
      <a
        className={
          onSettingsPage
            ? Styles.activatedSettings : Styles.deactivatedSettings
        }
        onClick={_e =>
          ReasonReact.Router.push(onSettingsPage ? "/" : "/settings")
        }>
        <Icon kind=Icon.Settings />
        <Spacer width=0.25 />
        {React.string("Settings")}
      </a>
    </div>
  </header>;
};
