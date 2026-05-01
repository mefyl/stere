include Stere

module Lwt = struct
  module type S = S with module Args := Ident and module Io := Lwt

  module Augment
      (Args : MONAD)
      (Base : BASE with module Args := Args and module Io := Lwt) =
    Augment (Args) (Lwt) (Base)

  module Make (Base : BASE with module Args := Ident and module Io := Lwt) =
    Augment (Ident) (Base)

  let make src =
    let module Base = struct
      include (val Logs_lwt.src_log src)

      let msg level msgf =
        let msgf m = msgf (fun ?header ?tags fmt -> m ?header ?tags fmt) in
        msg level msgf

      let kmsg k level msgf = kmsg ?over:None k level msgf
    end in
    (module Make (Base) : S)
end
