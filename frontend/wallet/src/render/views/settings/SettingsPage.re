open Tc;

module Styles = {
  open Css;

  let headerContainer =
    style([display(`flex), justifyContent(`spaceBetween)]);

  let versionText =
    merge([
      Theme.Text.Header.h6,
      style([
        display(`flex),
        textTransform(`uppercase),
        paddingTop(`rem(0.5)),
      ]),
    ]);

  let container =
    style([
      height(`percent(100.)),
      padding(`rem(2.)),
      borderTop(`px(1), `solid, white),
      borderLeft(`px(1), `solid, white),
      overflow(`scroll),
    ]);

  let label =
    merge([
      Theme.Text.Header.h3,
      style([
        margin2(~v=`rem(0.5), ~h=`zero),
        color(Theme.Colors.midnight),
      ]),
    ]);

  let walletItemContainer =
    style([
      display(`flex),
      flexDirection(`column),
      backgroundColor(`rgba((255, 255, 255, 0.8))),
      borderRadius(`px(6)),
      border(`px(1), `solid, Theme.Colors.slateAlpha(0.4)),
      width(`rem(28.)),
    ]);

  let walletItem =
    merge([
      Theme.Text.Body.regular,
      style([
        padding(`rem(1.)),
        color(Theme.Colors.midnight),
        display(`flex),
        alignItems(`center),
        borderBottom(`px(1), `solid, Theme.Colors.slateAlpha(0.25)),
        lastChild([borderBottomWidth(`zero)]),
        hover([
          backgroundColor(Theme.Colors.midnightAlpha(0.05)),
          selector("> :last-child", [color(Theme.Colors.hyperlink)]),
        ]),
      ]),
    ]);

  let walletName = style([width(`rem(12.5)), color(Theme.Colors.marine)]);

  let walletKey =
    merge([Theme.Text.Body.mono, style([color(Theme.Colors.midnightAlpha(0.7))])]);

  let walletChevron =
    style([display(`inlineFlex), color(Theme.Colors.tealAlpha(0.5))]);
};

module Version = {
  module QueryString = [%graphql
    {|
      query getVersion {
        version
      }
    |}
  ];

  module Query = ReasonApollo.CreateQuery(QueryString);

  [@react.component]
  let make = () => {
    let prettyVersion = v =>
      String.slice(v, ~from=0, ~to_=min(8, String.length(v)));

    <div className=Styles.versionText>
      <span className=Css.(style([color(Theme.Colors.slateAlpha(0.3))]))>
        {React.string("Version:")}
      </span>
      <Spacer width=0.5 />
      <span className=Css.(style([color(Theme.Colors.slateAlpha(0.7))]))>
        <Query>
          {response =>
             (
               switch (response.result) {
               | Loading => "..."
               | Error(err) => err##message
               | Data(data) =>
                 data##version
                 |> Option.map(~f=prettyVersion)
                 |> Option.withDefault(~default="Unknown")
               }
             )
             |> React.string}
        </Query>
      </span>
    </div>;
  };
};

module WalletSettingsItem = {
  [@react.component]
  let make = (~publicKey) => {
    let keyStr = PublicKey.toString(publicKey);
    let route = "/settings/" ++ Js.Global.encodeURIComponent(keyStr);
    <div
      className=Styles.walletItem
      onClick={_ => ReasonReact.Router.push(route)}>
      <div className=Styles.walletName> <WalletName pubkey=publicKey /> </div>
      <span className=Styles.walletKey>
        <Pill> {React.string(PublicKey.prettyPrint(publicKey))} </Pill>
      </span>
      <Spacer width=5.0 />
      <span className=Styles.walletChevron>
        <Icon kind=Icon.EmptyChevronRight />
      </span>
    </div>;
  };
};

module WalletsQueryString = [%graphql
  {|
    query getWallets {
      ownedWallets {
        publicKey @bsDecoder(fn: "Apollo.Decoders.publicKey")
      }
    }
  |}
];

module WalletsQuery = ReasonApollo.CreateQuery(WalletsQueryString);

[@react.component]
let make = () => {
  <div className=Styles.container>
    <div className=Styles.headerContainer>
      <div className=Theme.Text.Header.h3>
        {React.string("Node Settings")}
      </div>
      <Version />
    </div>
    <Spacer height=1. />
    <NetworkDropdown />
    <Spacer height=1. />
    <div className=Styles.label> {React.string("Wallet Settings")} </div>
    <Spacer height=0.5 />
    <div className=Styles.walletItemContainer>
      <WalletsQuery>
        {({result}) =>
           switch (result) {
           | Loading
           | Error(_) => React.null
           | Data(data) =>
             data##ownedWallets
             |> Array.map(~f=w =>
                  <WalletSettingsItem
                    key={PublicKey.toString(w##publicKey)}
                    publicKey=w##publicKey
                  />
                )
             |> React.array
           }}
      </WalletsQuery>
    </div>
  </div>;
};
