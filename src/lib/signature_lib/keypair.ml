open Core_kernel

module Stable = struct
  module V1 = struct
    module T = struct
      type t =
        { public_key: Public_key.Stable.V1.t
        ; private_key: Private_key.Stable.V1.t sexp_opaque }
      [@@deriving sexp, bin_io]
    end

    include T
  end

  module Latest = V1
end

module T = struct
  type t = Stable.Latest.t =
    { public_key: Public_key.Stable.V1.t
    ; private_key: Private_key.Stable.V1.t sexp_opaque }
  [@@deriving sexp]

  let compare {public_key= pk1; private_key= _}
      {public_key= pk2; private_key= _} =
    Public_key.compare pk1 pk2
end

include T
include Comparable.Make (T)

let of_private_key_exn private_key =
  let public_key = Public_key.of_private_key_exn private_key in
  {public_key; private_key}

let create () = of_private_key_exn (Private_key.create ())

let gen = Quickcheck.Generator.(map ~f:of_private_key_exn Private_key.gen)

module And_compressed_pk = struct
  module T = struct
    type t = T.t * Public_key.Compressed.t [@@deriving sexp]

    let compare ({public_key= pk1; private_key= _}, _)
        ({public_key= pk2; private_key= _}, _) =
      Public_key.compare pk1 pk2
  end

  include T
  include Comparable.Make (T)
end
