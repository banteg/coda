open Tc;

module Styles = {
  open Css;

  let container =
    style([
      height(`percent(100.)),
      padding(`rem(2.)),
      borderTop(`px(1), `solid, white),
      borderLeft(`px(1), `solid, white),
      overflow(`scroll),
    ]);

  let backHeader = style([display(`flex), alignItems(`center)]);

  let backIcon =
    style([
      display(`inlineFlex),
      color(Theme.Colors.hyperlink),
      hover([color(Theme.Colors.hyperlinkAlpha(0.5))]),
    ]);

  let backHeaderText =
    merge([Theme.Text.Header.h3, style([color(Theme.Colors.midnight)])]);

  let headerWalletName = style([fontSize(`rem(1.25))]);

  let label =
    merge([
      Theme.Text.Body.semiBold,
      style([color(Theme.Colors.midnight), marginBottom(`rem(0.25))]),
    ]);

  let deleteModalLabel = merge([label, style([alignSelf(`flexStart)])]);

  let deleteAlert = style([margin2(~v=`rem(0.5), ~h=`zero)]);

  let textBox = style([width(`rem(21.))]);

  let modalContainer =
    style([
      width(`rem(22.)),
      display(`flex),
      flexDirection(`column),
      alignItems(`center),
      margin(`auto),
    ]);

  let buttonWrapper = style([display(`flex)]);
};

module DeleteWallet = [%graphql
  {|
    mutation deleteWallet($key: PublicKey!) {
      deleteWallet(input: {publicKey: $key}) {
        publicKey
      }
    }
  |}
];

module DeleteWalletMutation = ReasonApollo.CreateMutation(DeleteWallet);

module DeleteButton = {
  type modalState = {
    text: string,
    error: option(string),
  };
  [@react.component]
  let make = (~publicKey) => {
    let (modalState, updateModal) = React.useState(() => None);
    let (addressBook, _) = React.useContext(AddressBookProvider.context);
    let walletName =
      switch (AddressBook.lookup(addressBook, publicKey)) {
      | Some(name) => name
      | None =>
        PublicKey.toString(publicKey) |> String.slice(~from=0, ~to_=5)
      };
    let warningMessage =
      "Are you sure you want to delete "
      ++ walletName
      ++ "? \
      This can't be undone, and you may lose the funds in this wallet.";
    <>
      <Link
        kind=Link.Red
        onClick={_ =>
          updateModal(x => Option.or_(Some({text: "", error: None}), x))
        }>
        {React.string("Delete wallet")}
      </Link>
      {switch (modalState) {
       | None => React.null
       | Some({text, error}) =>
         <Modal
           title="Delete Wallet" onRequestClose={_ => updateModal(_ => None)}>
           <div className=Styles.modalContainer>
             <div className=Styles.deleteAlert>
               <Alert kind=`Warning message=warningMessage />
             </div>
             {switch (error) {
              | Some(errorText) => <Alert kind=`Danger message=errorText />
              | None => React.null
              }}
             <div className=Styles.deleteModalLabel>
               {React.string("Type wallet name to confirm:")}
             </div>
             <TextField
               label="Name"
               value=text
               onChange={s => updateModal(_ => Some({text: s, error: None}))}
             />
             <Spacer height=2. />
             <div className=Styles.buttonWrapper>
               <Button
                 label="Cancel"
                 style=Button.Gray
                 onClick={_ => updateModal(_ => None)}
               />
               <Spacer width=1. />
               <DeleteWalletMutation>
                 (
                   (mutation, _) =>
                     <Button
                       label="Delete"
                       style=Button.Red
                       onClick={_ => {
                         let variables =
                           DeleteWallet.make(
                             ~key=Apollo.Encoders.publicKey(publicKey),
                             (),
                           )##variables;
                         let performMutation =
                           Task.liftErrorPromise(() =>
                             mutation(
                               ~variables,
                               ~refetchQueries=[|"getWallets"|],
                               (),
                             )
                           );
                         Task.attempt(
                           performMutation,
                           ~f=
                             fun
                             | Ok(Data(_))
                             | Ok(EmptyResponse) => {
                                 updateModal(_ => None);
                                 ReasonReact.Router.push("/settings");
                               }
                             | Ok(Errors(err)) => {
                                 let message =
                                   err
                                   |> Array.get(~index=0)
                                   |> Option.map(~f=e => e##message)
                                   |> Option.withDefault(
                                        ~default="Server error",
                                      );
                                 updateModal(_ =>
                                   Some({text, error: Some(message)})
                                 );
                               }
                             | Error(e) =>
                               updateModal(_ =>
                                 Some({
                                   text,
                                   error: Some(Js.String.make(e)),
                                 })
                               ),
                         );
                       }}
                       disabled={text != walletName}
                     />
                 )
               </DeleteWalletMutation>
             </div>
           </div>
         </Modal>
       }}
    </>;
  };
};

[@bs.scope "window"] [@bs.val] external showItemInFolder: string => unit = "";

module KeypathQueryString = [%graphql
  {|
    query ($publicKey: PublicKey!)  {
      wallet(publicKey: $publicKey) {
        privateKeyPath
      }
    }
  |}
];

module KeypathQuery = ReasonApollo.CreateQuery(KeypathQueryString);

[@react.component]
let make = (~publicKey) => {
  let (addressBook, updateAddressBook) =
    React.useContext(AddressBookProvider.context);

  let handleClipboard = () =>
    ignore(
      Bindings.Navigator.Clipboard.writeText(PublicKey.toString(publicKey)),
    );

  <div className=Styles.container>
    <div className=Styles.backHeader>
      <span
        className=Styles.backIcon
        onClick={_ => ReasonReact.Router.push("/settings")}>
        <Icon kind=Icon.BackArrow />
      </span>
      <Spacer width=0.5 />
      <span className=Styles.backHeaderText>
        <WalletName pubkey=publicKey className=Styles.headerWalletName />
        {React.string(" settings")}
      </span>
    </div>
    <Spacer height=1. />
    <div className=Styles.label> {React.string("Wallet name")} </div>
    <div className=Styles.textBox>
      <TextField
        label="Name"
        value={Option.withDefault(
          ~default="",
          AddressBook.lookup(addressBook, publicKey),
        )}
        placeholder="My Coda Wallet"
        onChange={value =>
          updateAddressBook(ab =>
            AddressBook.set(ab, ~key=publicKey, ~name=value)
          )
        }
      />
    </div>
    <Spacer height=1. />
    <div className=Styles.label> {React.string("Public key")} </div>
    <div className=Styles.textBox>
      <TextField
        label="Key"
        value={PublicKey.prettyPrint(publicKey)}
        mono=true
        onChange={_ => ()}
        button={
          <TextField.Button text="Copy" color=`Blue onClick=handleClipboard />
        }
      />
    </div>
    <Spacer height=1. />
    <div className=Styles.label> {React.string("Private key")} </div>
    <div className=Styles.textBox>
      <KeypathQuery
        variables=
          {KeypathQueryString.make(
             ~publicKey=Apollo.Encoders.publicKey(publicKey),
             (),
           )##variables}>
        {({result}) => {
           let path =
             switch (result) {
             | Loading
             | Error(_) => None
             | Data(data) =>
               Option.map(~f=w => w##privateKeyPath, data##wallet)
             };
           switch (path) {
           | Some(secretKeyPath) =>
             <TextField
               label="Path"
               value=secretKeyPath
               onChange=ignore
               button={
                 <TextField.Button
                   text="Open"
                   color=`Teal
                   onClick={_ => showItemInFolder(secretKeyPath)}
                 />
               }
             />
           | None =>
             <TextField
               label="Path"
               value=""
               onChange=ignore
               button={
                 <TextField.Button
                   text="Open"
                   disabled=true
                   color=`Teal
                   onClick=ignore
                 />
               }
             />
           };
         }}
      </KeypathQuery>
    </div>
    <Spacer height=1.5 />
    <ConsensusSettings />
    <Spacer height=1.5 />
    <DeleteButton publicKey />
  </div>;
};
