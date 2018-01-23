(** Error reporting *)

(** Captures the backtrace and report an error. *)
val report
  :  Format.formatter
  -> exn
  -> unit

(** Report [true] if the backtrace was printed *)
val report_with_backtrace
  :  Format.formatter
  -> exn
  -> backtrace:Printexc.raw_backtrace
  -> bool

(**/**)
val map_fname : (string -> string) ref
