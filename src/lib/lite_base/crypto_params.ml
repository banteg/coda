[%%import
"../../config.mlh"]

[%%if
curve_size = 298]

module Tock0 = struct
  include Snarkette.Mnt6_80

  let fq_to_scalars (x : Fq.t) : N.t list = [Fq.to_bigint x]
end

[%%elif
curve_size = 753]

module Tock0 = struct
  include Snarkette.Mnt6753

  let () = assert (Fq.length_in_bits = 753)

  let fq_to_scalars (x : Fq.t) : N.t list =
    let k' = Fq.length_in_bits - 1 in
    let x = Fq.to_bigint x in
    let one = N.of_int 1 in
    let all_but_top = N.(shift_left one k' - one) in
    [N.log_and all_but_top x; N.(shift_right x k')]
end

[%%else]

[%%show
curve_size]

[%%error
"invalid value for \"curve_size\""]

[%%endif]

module Pedersen = Pedersen_lib.Pedersen.Make (Tock0.Fq) (Tock0.G1)

module Tock = struct
  include Tock0

  let bg_params =
    Group_map.Params.create
      (module Fq)
      ~a:G1.Coefficients.a ~b:G1.Coefficients.b

  module Bowe_gabizon = Tock0.Make_bowe_gabizon (Bowe_gabizon_hash.Make (struct
    module Field = struct
      include Fq

      let of_bits t = Core_kernel.Option.value_exn (of_bits t)
    end

    module Fqe = Fq3
    module G1 = G1
    module G2 = G2

    module Bigint = struct
      include N

      let of_field = Fq.to_bigint
    end

    let params = bg_params

    let init =
      lazy
        (Pedersen.State.salt
           (Lazy.force Lite_params.pedersen_params)
           (Hash_prefixes.bowe_gabizon_hash :> string))

    let pedersen t = Pedersen.digest_fold (Lazy.force init) t
  end))
end
