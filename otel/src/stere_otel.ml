(** OpenTelemetry Protocol (OTLP). *)

open Acid_lwt.Versions.V0_13_0
open Lwt.Infix

module Attributes = struct
  module Map = Map.M_printable (String)
  include Map

  type t = Schematic.Json.t Map.t [@@deriving eq, show]
end

let uuid_to_16_bytes uuid =
  Uuidm.to_binary_string uuid |> Bytes.unsafe_of_string_promise_no_mutation

let uuid_to_8_bytes uuid = uuid_to_16_bytes uuid |> Bytes.subo ~len:8

(** Protobuf conversion helpers for OpenTelemetry Protocol Buffer encoding. *)
module Proto_helpers = struct
  (** [time_to_nanos t] converts [t] to nanoseconds since Unix epoch. *)
  let time_to_nanos (t : Timmy.Time.t) : int64 =
    t
    |> Timmy.Time.to_ptime
    |> Ptime.to_float_s
    |> ( *. ) 1_000_000_000.
    |> Int64.of_float

  (** [status_to_proto status] converts a result status to OTLP protobuf status.
  *)
  let status_to_proto :
      (unit, string) Result.t -> Opentelemetry.Proto.Trace.status = function
    | Ok () -> Opentelemetry.Proto.Trace.{ code = Status_code_ok; message = "" }
    | Error msg ->
      Opentelemetry.Proto.Trace.{ code = Status_code_error; message = msg }

  (** [json_to_anyvalue] converts a JSON value to OTLP protobuf AnyValue. *)
  let rec json_to_anyvalue :
      Schematic.Json.t -> Opentelemetry.Proto.Common.any_value = function
    | `String s -> Opentelemetry.Proto.Common.String_value s
    | `Bool b -> Opentelemetry.Proto.Common.Bool_value b
    | `Float n ->
      if Float.is_integer n then
        Opentelemetry.Proto.Common.Int_value (Int64.of_float n)
      else Opentelemetry.Proto.Common.Double_value n
    | `A values ->
      Opentelemetry.Proto.Common.Array_value
        { values = List.map values ~f:json_to_anyvalue }
    | `O kvs ->
      Opentelemetry.Proto.Common.Kvlist_value
        {
          values =
            List.map kvs ~f:(fun (key, value) ->
                Opentelemetry.Proto.Common.
                  { key; value = Some (json_to_anyvalue value) });
        }
    | `Null ->
      (* Protobuf doesn't have null, use empty string *)
      Opentelemetry.Proto.Common.String_value ""

  (** [attributes_to_proto attrs] converts attributes map to OTLP protobuf
      key-value list. *)
  let attributes_to_proto (attrs : Attributes.t) :
      Opentelemetry.Proto.Common.key_value list =
    Map.to_alist attrs
    |> List.map ~f:(fun (key, value) ->
        Opentelemetry.Proto.Common.
          { key; value = Some (json_to_anyvalue value) })

  (** [severity_to_proto level] converts Logs.level to OTLP severity enum. *)
  let severity_to_proto : Logs.level -> Opentelemetry.Proto.Logs.severity_number
      = function
    | Logs.App -> Severity_number_info
    | Logs.Error -> Severity_number_error
    | Logs.Warning -> Severity_number_warn
    | Logs.Info -> Severity_number_info
    | Logs.Debug -> Severity_number_debug

  (** [severity_text level] converts Logs.level to text representation. *)
  let severity_text : Logs.level -> string = function
    | Logs.App -> "App"
    | Logs.Error -> "Error"
    | Logs.Warning -> "Warning"
    | Logs.Info -> "Info"
    | Logs.Debug -> "Debug"
end

(** Otel automatic logs and traces via a [Log.reporter].

    Nesting is not supported yet. *)
module Log = struct
  (** Log severity. *)
  module Severity = struct
    (** OTLP log severity.

        Each level supports 4 gradations (1-4), where the parameter adds to the
        base severity number. For example, [Debug 1] = 5, [Debug 2] = 6, etc.

        See
        https://opentelemetry.io/docs/specs/otel/logs/data-model/#field-severitynumber
    *)
    type t =
      | Trace of int  (** Base 0, range 1-4 *)
      | Debug of int  (** Base 4, range 5-8 *)
      | Info of int  (** Base 8, range 9-12 *)
      | Warn of int  (** Base 12, range 13-16 *)
      | Error of int  (** Base 16, range 17-20 *)
      | Fatal of int  (** Base 20, range 21-24 *)
    [@@deriving show, eq]

    (** [to_int severity] is the OTLP severity number of [severity]. *)
    let to_int = function
      | Trace n -> 0 + n
      | Debug n -> 4 + n
      | Info n -> 8 + n
      | Warn n -> 12 + n
      | Error n -> 16 + n
      | Fatal n -> 20 + n

    (** [of_int severity] is the severity corresponding to OTLP severity number
        [severity]. *)
    let of_int i =
      match i with
      | 1 | 2 | 3 | 4 -> Result.return (Trace i)
      | 5 | 6 | 7 | 8 -> Result.return (Debug (i - 4))
      | 9 | 10 | 11 | 12 -> Result.return (Info (i - 8))
      | 13 | 14 | 15 | 16 -> Result.return (Warn (i - 12))
      | 17 | 18 | 19 | 20 -> Result.return (Error (i - 16))
      | 21 | 22 | 23 | 24 -> Result.return (Fatal (i - 20))
      | _ -> Result.failf "invalid severity number: %d" i
  end

  let otel =
    Logs.Tag.def "otel" ~doc:"whether to genereate opentelemetry signal"
      (fun fmt enabled -> Fmt.bool fmt enabled)

  (** [reporting_otel] is whether we are in the process of sending an
      OpenTelemetry log record, in which case nested log lines must not be
      reported lest we end up with infinite recursion. *)
  let reporting_otel : bool Lwt.key = Lwt.new_key ()

  (** Empty bytes array. For the protobuf wire protocol. *)
  let bytes_empty = Bytes.create 0

  let tag_find def = function
    | None -> None
    | Some tags -> Logs.Tag.find def tags

  let attribute_list (defs : Stere.Tag.def_list) tags =
    let rec loop : Stere.Tag.def_list -> (string * Schematic.Json.t) list =
      let collect (type a) (def : a Logs.Tag.def) =
        match Logs.Tag.find def tags with
        | None -> None
        | Some value ->
          Some
            ( Logs.Tag.name def,
              `String (Fmt.str "%a" (Logs.Tag.printer def) value) )
      in
      function
      | Stere.Tag.[] -> []
      | Stere.Tag.(def :: defs) -> (
        match collect def with
        | None -> loop defs
        | Some attribute -> attribute :: loop defs)
    in
    Attributes.of_alist_exn @@ loop defs

  (** Pending batched OTLP signals that are regularly flushed. *)
  module Batched = struct
    type 'signal t = {
      mutable failures : int;
      name : string;
      mutable pending : 'signal list;
      mutable reported : bool;
    }

    let make name = { failures = 0; name; reported = false; pending = [] }

    (** Number of consecutive failures before we send a sentry report. *)
    let _report_threshold = 3

    let flush t f =
      match t with
      | { pending = []; _ } -> Lwt.return ()
      | { name; pending; _ } -> (
        f pending >>= function
        | Result.Ok () ->
          let () = t.failures <- 0
          and () = t.pending <- []
          and () = t.reported <- false in
          Lwt.return ()
        | Result.Error error ->
          let () = t.failures <- t.failures + 1 in
          let () =
            if (not t.reported) && t.failures >= _report_threshold then
              t.reported <- true
          in
          let () = Logs.err (fun m -> m "%s export failed: %s" name error) in
          Lwt.return ())

    let push t value = t.pending <- value :: t.pending
  end

  (** Simple HTTP POST for OTLP export. *)
  let http_post_protobuf ~endpoint ~body =
    let open Lwt.Syntax in
    let headers =
      Cohttp.Header.of_list [ ("Content-Type", "application/x-protobuf") ]
    in
    let body_stream = Cohttp_lwt.Body.of_string body in
    let* resp, resp_body =
      Cohttp_lwt_unix.Client.post ~headers ~body:body_stream endpoint
    in
    let status = Cohttp.Response.status resp in
    let* body_string = Cohttp_lwt.Body.to_string resp_body in
    if Cohttp.Code.is_success (Cohttp.Code.code_of_status status) then
      Lwt.return (Result.Ok ())
    else
      Lwt.return
        (Result.Error
           (Fmt.str "HTTP %d: %s"
              (Cohttp.Code.code_of_status status)
              body_string))

  (** [reporter ?batching ?resource ?traces_endpoints endpoints] is a
      Logs.reporter that sends OpenTelemetry signals to [endpoints] every
      [batching]. *)
  let reporter ?attributes_of_tags ?(batching = Timmy.Span.seconds 10) ?resource
      ?(traces_endpoints = []) endpoints =
    let open struct
      type log_entry = {
        attributes : Attributes.t;
        level : Logs.level;
        msg : string;
        span : Stere.span option;
        src : Logs.src;
        time : Timmy.Time.t;
      }

      type pending_span = {
        attributes : Attributes.t;
        name : string;
        span : Stere.span;
        src : Logs.src;
        start_time : Timmy.Time.t;
      }

      type span_entry = {
        src : Logs.src;
        span : Opentelemetry.Proto.Trace.span;
      }
    end in
    let logs_endpoints =
      List.map endpoints ~f:(fun endpoint ->
          Uri.with_path endpoint (Uri.path endpoint ^ "/v1/logs"))
    and traces_endpoints =
      List.map traces_endpoints ~f:(fun endpoint ->
          Uri.with_path endpoint (Uri.path endpoint ^ "/v1/traces"))
    in
    let logs_batched =
      List.map logs_endpoints ~f:(fun endpoint ->
          (endpoint, Batched.make (Fmt.str "OTLP logs to %a" Uri.pp endpoint)))
    and spans_batched =
      List.map traces_endpoints ~f:(fun endpoint ->
          (endpoint, Batched.make (Fmt.str "OTLP spans to %a" Uri.pp endpoint)))
    and pending_spans = ref (Map.empty (module Bytes))
    and resource =
      match resource with
      | None -> None
      | Some attributes ->
        Some
          Opentelemetry.Proto.Resource.
            {
              attributes = Proto_helpers.attributes_to_proto attributes;
              dropped_attributes_count = 0l;
            }
    in
    let _ticker =
      Timmy_lwt.Ticker.make ~period:batching @@ fun _time ->
      let () =
        let flush () =
          let open Lwt.Syntax in
          let flush_logs =
            Lwt_list.iter_p
              (fun (endpoint, batched) ->
                Batched.flush batched @@ fun logs ->
                let request =
                  (* Group logs by source name into scope_logs *)
                  let scope_logs =
                    Base.Sequence.of_list logs
                    |> Base.Sequence.map
                         ~f:(fun { attributes; level; msg; span; src; time } ->
                           let log_record =
                             let span_id, trace_id =
                               match span with
                               | None -> (bytes_empty, bytes_empty)
                               | Some { id; trace; _ } ->
                                 (uuid_to_8_bytes id, uuid_to_16_bytes trace)
                             in
                             Opentelemetry.Proto.Logs.
                               {
                                 attributes =
                                   Proto_helpers.attributes_to_proto attributes;
                                 time_unix_nano =
                                   Proto_helpers.time_to_nanos time;
                                 observed_time_unix_nano = Int64.zero;
                                 severity_number =
                                   Proto_helpers.severity_to_proto level;
                                 severity_text =
                                   Proto_helpers.severity_text level;
                                 body =
                                   Some
                                     (Opentelemetry.Proto.Common.String_value
                                        msg);
                                 dropped_attributes_count = 0l;
                                 flags = 0l;
                                 trace_id;
                                 span_id;
                               }
                           in
                           (Logs.Src.name src, log_record))
                    |> Map.of_sequence_multi (module String)
                    |> Map.to_sequence
                    |> Base.Sequence.map ~f:(fun (src_name, log_records) ->
                        Opentelemetry.Proto.Logs.
                          {
                            scope =
                              Some
                                Opentelemetry.Proto.Common.
                                  {
                                    name = src_name;
                                    version = "";
                                    attributes = [];
                                    dropped_attributes_count = 0l;
                                  };
                            log_records;
                            schema_url = "";
                          })
                    |> Base.Sequence.to_list
                  in
                  Opentelemetry.Proto.Logs_service.
                    {
                      resource_logs =
                        [
                          Opentelemetry.Proto.Logs.
                            { resource; scope_logs; schema_url = "" };
                        ];
                    }
                in
                let body =
                  let encoder = Pbrt.Encoder.create () in
                  let () =
                    Opentelemetry.Proto.Logs_service
                    .encode_pb_export_logs_service_request request encoder
                  in
                  Pbrt.Encoder.to_bytes encoder |> Bytes.to_string
                in
                Lwt.with_value reporting_otel (Some true) @@ fun () ->
                http_post_protobuf ~endpoint ~body)
              logs_batched
          and flush_spans =
            Lwt_list.iter_p
              (fun (endpoint, batched) ->
                Batched.flush batched @@ fun spans ->
                let request =
                  let scope_spans =
                    Base.Sequence.of_list spans
                    |> Base.Sequence.map ~f:(fun { src; span } ->
                        (Logs.Src.name src, span))
                    |> Map.of_sequence_multi (module String)
                    |> Map.to_sequence
                    |> Base.Sequence.map ~f:(fun (src_name, proto_spans) ->
                        Opentelemetry.Proto.Trace.
                          {
                            scope =
                              Some
                                Opentelemetry.Proto.Common.
                                  {
                                    name = src_name;
                                    version = "";
                                    attributes = [];
                                    dropped_attributes_count = 0l;
                                  };
                            spans = proto_spans;
                            schema_url = "";
                          })
                    |> Base.Sequence.to_list
                  in
                  Opentelemetry.Proto.Trace_service.
                    {
                      resource_spans =
                        [
                          Opentelemetry.Proto.Trace.
                            { resource; scope_spans; schema_url = "" };
                        ];
                    }
                in
                let body =
                  let encoder = Pbrt.Encoder.create () in
                  let () =
                    Opentelemetry.Proto.Trace_service
                    .encode_pb_export_trace_service_request request encoder
                  in
                  Pbrt.Encoder.to_bytes encoder |> Bytes.to_string
                in
                http_post_protobuf ~endpoint ~body)
              spans_batched
          in
          let+ () = flush_logs
          and+ () = flush_spans in
          ()
        in
        Lwt.dont_wait flush (fun exn ->
            Logs.err (fun m -> m "OTLP flush exception: %s" (Exn.to_string exn)))
      in
      true
    in
    let report src level ~over k msgf =
      msgf @@ fun ?header:_ ?tags fmt ->
      Fmt.kstr
        (fun msg ->
          let () =
            let inside_otel_reporting =
              Lwt.get reporting_otel |> Option.value ~default:false
            and otel_enabled =
              tag_find otel tags |> Option.value ~default:true
            in
            if (not inside_otel_reporting) && otel_enabled then
              let send_record, span =
                match tag_find Stere.Tags.span_event tags with
                | Some (Start { span }) ->
                  let () =
                    if not (List.is_empty spans_batched) then
                      let pending =
                        {
                          attributes = Map.empty (module String);
                          name = msg;
                          span;
                          src;
                          start_time = Timmy.Time.of_ptime (Ptime_clock.now ());
                        }
                      in
                      pending_spans :=
                        Map.set !pending_spans ~key:(uuid_to_8_bytes span.id)
                          ~data:pending
                  in
                  (true, Some span)
                | Some (End { span = { id; _ }; status }) ->
                  let () =
                    if not (List.is_empty spans_batched) then
                      let span_id_bytes = uuid_to_8_bytes id in
                      match Map.find !pending_spans span_id_bytes with
                      | None ->
                        Logs.warn (fun m ->
                            m "Span finish without start: %a" Uuidm.pp id)
                      | Some { attributes; name; span; src; start_time } ->
                        let span =
                          let parent_span_id =
                            match span.parent with
                            | None -> bytes_empty
                            | Some { id; _ } -> uuid_to_8_bytes id
                          in
                          Opentelemetry.Proto.Trace.
                            {
                              trace_id = uuid_to_16_bytes span.trace;
                              span_id = span_id_bytes;
                              parent_span_id;
                              name;
                              kind = Span_kind_internal;
                              start_time_unix_nano =
                                Proto_helpers.time_to_nanos start_time;
                              end_time_unix_nano =
                                Proto_helpers.time_to_nanos
                                  (Timmy.Time.of_ptime (Ptime_clock.now ()));
                              attributes =
                                Proto_helpers.attributes_to_proto attributes;
                              status =
                                Some (Proto_helpers.status_to_proto status);
                              events = [];
                              links = [];
                              dropped_attributes_count = 0l;
                              dropped_events_count = 0l;
                              dropped_links_count = 0l;
                              trace_state = "";
                            }
                        in
                        let span_entry = { src; span } in
                        let () =
                          List.iter spans_batched
                            ~f:(fun (_endpoint, batched) ->
                              Batched.push batched span_entry)
                        in
                        pending_spans := Map.remove !pending_spans span_id_bytes
                  in
                  (false, None)
                | None -> (true, None)
              in
              if send_record then
                let attributes =
                  match (attributes_of_tags, tags) with
                  | Some f, Some tags -> f tags
                  | _ -> Attributes.empty
                and span =
                  match span with
                  | Some span -> Some span
                  | None -> Stere.current_span ()
                in
                let log_entry =
                  {
                    attributes;
                    level;
                    msg;
                    span;
                    src;
                    time = Timmy.Time.of_ptime (Ptime_clock.now ());
                  }
                in
                List.iter logs_batched ~f:(fun (_endpoint, batched) ->
                    Batched.push batched log_entry)
          in
          let () = over () in
          k ())
        fmt
    in
    { Logs.report }
end
