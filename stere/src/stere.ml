(** A layer on top of Logs that provides:

    - Scoped [with_*] flavors. They emit a log just like their non-scoped
      counterpart, but can monitor the end of the scope, which can be used to
      provide logs indentation and opentelemetry scopes.

    - Builtin [component] tag support, to determine what part of the code is
      emitting logs. On top of its usefullness, it was also introduced because
      an additional argument was required to avoid death by a thousand 'weak
      cuts of value restriction.

    - Unified interface for additional arguments if specific libraries want to
      expose convenient way to augment the logs. *)

include Logs
module Context = Context

let uuid = Uuidm.v4_gen (Random.State.make_self_init ())

(** [combine_reporters r1 r2] is a reporter that reports to [r1] and [r2]. *)
let combine_reporters r1 r2 =
  let report src level ~over k msgf =
    let v = r1.Logs.report src level ~over:(fun () -> ()) k msgf in
    r2.Logs.report src level ~over (fun () -> v) msgf
  in
  { Logs.report }

module Tag = struct
  include Tag

  type def_list =
    | ( :: ) : ('a Logs.Tag.def * def_list) -> def_list
    | [] : def_list

  module List = struct
    type t =
      | ( :: ) : Logs.Tag.t * t -> t
      | [] : t
  end
end

type span = {
  depth : int;
  id : Uuidm.t;
  parent : span option;
  trace : Uuidm.t;
}

let _current_span : span Context.key = Context.create_key ()

module Tags = struct
  (** The component the log originates from. *)
  let component =
    Tag.def ~doc:"The component the log originates from." "component"
      Fmt.(string ++ const char ':' ++ sp)

  type span_event =
    | Start of { span : span }
    | End of {
        span : span;
        status : (unit, string) Result.t;
      }

  (** The span event associated to this log line. *)
  let span_event =
    Logs.Tag.def "trace.event" ~doc:"span lifecycle event" (fun fmt -> function
      | Start { span = { id; _ }; _ } -> Fmt.pf fmt "start %a" Uuidm.pp id
      | End { span = { id; _ }; _ } -> Fmt.pf fmt "end %a" Uuidm.pp id)
end

module type MONAD = sig
  type 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

module type IO = sig
  include MONAD

  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
end

(** Base logging primitives. *)
module type BASE = sig
  (** The additional arguments logging function accept. *)
  module Args : MONAD

  (** The possibly asynchronous IO monad. *)
  module Io : IO

  (** Log a message. See {!Logs.msg}. *)
  val msg : Logs.level -> (('a, unit Io.t) Logs.msgf -> unit Io.t) Args.t

  (** Log a message with continuation. See {!Logs.kmsg}. *)
  val kmsg :
    (unit -> 'b Io.t) -> Logs.level -> ('a, 'b Io.t) Logs.msgf -> 'b Io.t
end

module Ident = struct
  type 'a t = 'a

  let ( >>= ) v f = f v
  let return v = v
  let catch f catch = try f () with exn -> catch exn
end

(** Rich logging interface built from [BASE]. *)
module type S = sig
  include BASE

  type 'a msgf := ('a, unit Io.t) Logs.msgf
  type 'a log := ?component:string -> ('a msgf -> unit Io.t) Args.t

  val with_tag : 'tag Logs.Tag.def -> 'tag -> (unit -> 'result) -> 'result
  val msg : Logs.level -> 'a log
  val app : 'a log
  val err : 'a log
  val warn : 'a log
  val info : 'a log
  val debug : 'a log

  type ('a, 'b) with_log :=
    ?component:string -> ('a msgf -> (unit -> 'b Io.t) -> 'b Io.t) Args.t

  val with_msg : Logs.level -> ('a, 'b) with_log
  val with_app : ('a, 'b) with_log
  val with_debug : ('a, 'b) with_log
  val with_err : ('a, 'b) with_log
  val with_info : ('a, 'b) with_log
  val with_warn : ('a, 'b) with_log
end

let _context_tags : Logs.Tag.t list Context.key = Context.create_key ()
let current_span () = Context.get _current_span

let current_tag def =
  match Context.get _context_tags with
  | None -> None
  | Some tags ->
    let set =
      List.fold_left
        (fun set (Logs.Tag.V (d, v)) -> Logs.Tag.add d v set)
        Logs.Tag.empty tags
    in
    Logs.Tag.find def set

(** Create a rich logging interface from primitives. *)
module Augment
    (Args : MONAD)
    (Io : IO)
    (Base : BASE with module Args := Args and module Io := Io) :
  S with module Args := Args and module Io := Io = struct
  include Base

  let with_tag def tag f =
    let tags =
      match Context.get _context_tags with None -> [] | Some tags -> tags
    in
    Context.with_binding _context_tags (V (def, tag) :: tags) f

  let pp_indent fmt n =
    for _ = 1 to n do
      Fmt.string fmt "  "
    done

  let msg level ?component =
    Args.( >>= ) (msg level) (fun msg ->
        Args.return @@ fun msgf ->
        let msgf m =
          let m ?header ?tags fmt =
            let depth =
              match current_span () with
              | Some { depth; _ } -> depth + 1
              | None -> 0
            in
            let tags =
              match component with
              | None -> tags
              | Some component ->
                Some
                  (Option.value tags ~default:Logs.Tag.empty
                  |> Logs.Tag.add Tags.component component)
            in
            let tags =
              match Context.get _context_tags with
              | None | Some [] -> tags
              | Some context_tags ->
                Some
                  (List.fold_left
                     (fun tags (Logs.Tag.V (tag, value)) ->
                       Logs.Tag.add tag value tags)
                     (Option.value ~default:Logs.Tag.empty tags)
                     context_tags)
            in
            m ?header ?tags ("%a@[<2>" ^^ fmt ^^ "@]") pp_indent depth
          in
          msgf m
        in
        msg msgf)
  [@@warning "-unerasable-optional-argument"]

  let app ?component = msg App ?component
  [@@warning "-unerasable-optional-argument"]

  let debug ?component = msg Debug ?component
  [@@warning "-unerasable-optional-argument"]

  let err ?component = msg Error ?component
  [@@warning "-unerasable-optional-argument"]

  let info ?component = msg Info ?component
  [@@warning "-unerasable-optional-argument"]

  let warn ?component = msg Warning ?component
  [@@warning "-unerasable-optional-argument"]

  let with_msg level ?component =
    let ( let* ) = Io.( >>= ) in
    Args.( >>= ) (msg level ?component) (fun msg ->
        Args.return @@ fun msgf f ->
        let parent = current_span () in
        let depth, trace =
          match parent with
          | Some { depth; trace; _ } -> (depth + 1, trace)
          | None -> (0, uuid ())
        in
        let span = { depth; id = uuid (); parent; trace } in
        let send_start () =
          msg (fun m ->
              msgf (fun ?header ?tags fmt ->
                  let tags =
                    Option.value tags ~default:Logs.Tag.empty
                    |> Logs.Tag.add Tags.span_event (Start { span })
                  in
                  m ?header ~tags fmt))
        and send_end status =
          msg (fun m ->
              msgf (fun ?header ?tags fmt ->
                  let tags =
                    Option.value tags ~default:Logs.Tag.empty
                    |> Logs.Tag.add Tags.span_event (End { span; status })
                  in
                  m ?header ~tags fmt))
        in
        let* () = send_start () in
        let try' () =
          let* result = Context.with_binding _current_span span f in
          let* () = send_end (Ok ()) in
          Io.return result
        and catch' exn =
          let* () = send_end (Error (Printexc.to_string exn)) in
          raise exn
        in
        Io.catch try' catch')
  [@@warning "-unerasable-optional-argument"]

  let with_app ?component = with_msg App ?component
  [@@warning "-unerasable-optional-argument"]

  let with_debug ?component = with_msg Debug ?component
  [@@warning "-unerasable-optional-argument"]

  let with_err ?component = with_msg Error ?component
  [@@warning "-unerasable-optional-argument"]

  let with_info ?component = with_msg Info ?component
  [@@warning "-unerasable-optional-argument"]

  let with_warn ?component = with_msg Warning ?component
  [@@warning "-unerasable-optional-argument"]
end

module Sync = struct
  module Augment
      (Args : MONAD)
      (Base : BASE with module Args := Args and module Io := Ident) =
    Augment (Args) (Ident) (Base)

  module type S = S with module Args := Ident and module Io := Ident

  module Make (Base : BASE with module Args := Ident and module Io := Ident) =
    Augment (Ident) (Base)

  let make src = (module Make ((val Logs.src_log src)) : S)
end

let rec tags = function
  | Tag.List.[] -> Tag.empty
  | Tag.V (def, value) :: rest -> tags rest |> Tag.add def value

let tag def value = Tag.V (def, value)

(** Whether to send logs in the console at the end of scopes. *)
let _log_scopes =
  match Sys.getenv_opt "STERE_LOG_SCOPES" with
  | None | Some "" | Some "0" -> false
  | Some _ -> true

let scope_reporter reporter =
  let report src lvl ~over k msgf =
    let msgf m =
      msgf (fun ?header ?tags fmt ->
          let span_event =
            match tags with
            | None -> None
            | Some tags -> Logs.Tag.find Tags.span_event tags
          in
          match (_log_scopes, span_event) with
          | false, Some (End _) ->
            let k () =
              over ();
              k ()
            in
            Stdlib.Format.ikfprintf
              (fun _ppf -> k ())
              Stdlib.Format.std_formatter fmt
          | true, Some (End _) -> m ?header ?tags Stdlib.("◀️ " ^^ fmt)
          | true, Some (Start _) -> m ?header ?tags Stdlib.("▶️ " ^^ fmt)
          | _ -> m ?header ?tags fmt)
    in
    reporter.report src lvl ~over k msgf
  in
  { Logs.report }
