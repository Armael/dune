(** Fibers *)

open Import

(** {1 Generals} *)

(** Type of fiber. A fiber represent a suspended computation. Note
    that using the same fiber twice will execute it twice, which is
    probably not what you want. To share the result of a fiber, use an
    [Ivar.t].  *)
type 'a t

(** Create a fiber that has already terminated. *)
val return : 'a -> 'a t

module O : sig
  (** [>>>] is a sequencing operator. [a >>> b] is the fiber that
      first executes [a] and then [b]. *)
  val (>>>) : unit t -> 'a t -> 'a t

  (** [>>=] is similar to [>>>] except that the result of the first
      fiber is used to create the second one. *)
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

  (** [t >>| f] is the same as [t >>= fun x -> return (f x)] but
      slightly more efficient. *)
  val (>>|) : 'a t -> ('a -> 'b) -> 'b t
end

exception Already_reported

val memoize : 'a t -> 'a t

(** {1 Combining} *)

(** The following functions allow to combine two or more fibers. Note
    that when the execution of a fiber fails because of an exception,
    the other fibers will continue to run.

    This is why combining functions always take [unit -> _ t] functions rather than [_ t]
    values directly.
*)

val fork : (unit -> 'a t) -> (unit -> 'b t) -> ('a * 'b) t
val fork_unit : (unit -> unit t) -> (unit -> 'a t) -> 'a t

val nfork_map  : 'a list -> f:('a -> 'b   t) -> 'b list t
val nfork_iter : 'a list -> f:('a -> unit t) -> unit    t

(** {1 Local storage} *)

(** Variables local to a fiber *)
module Var : sig
  type 'a fiber = 'a t
  type 'a t

  (** Create a new variable *)
  val create : unit -> 'a t

  (** [get var] is a fiber that reads the value of [var] *)
  val get : 'a t -> 'a option fiber

  (** Same as [get] but raises if [var] is unset. *)
  val get_exn : 'a t -> 'a fiber

  (** [set var value fiber] sets [var] to [value] during the execution
      of [fiber].

      For instance, the following fiber always evaluate to [true]:

      {[
        set v x (get_exn v >>| fun y -> x = y)
      ]}
 *)
  val set : 'a t -> 'a -> 'b fiber -> 'b fiber
end with type 'a fiber := 'a t

(** {1 Error handling} *)

(** [fold_errors f ~init ~on_error] calls [on_error] for every
    exception raised during the execution of [f]. This include
    exception raised when calling [f ()] or during the execution of
    fibers after [f ()] has returned. *)
val fold_errors
  :  (unit -> 'a t)
  -> init:'b
  -> on_error:(exn -> 'b -> 'b)
  -> ('a, 'b) result t

(** [iter_errors f ~on_error] is:

    {[
      fold_errors f ~init:() ~on_error:(fun e () ->
        on_error e)
      ]}
*)
val iter_errors
  :  (unit -> 'a t)
  -> on_error:(exn -> unit)
  -> ('a, unit) result t

(** [catch_errors f] is:

    {[
      fold_errors f
        ~init:[]
        ~on_error:(fun e l -> e :: l)
    ]}
*)
val catch_errors
  :  (unit -> 'a t)
  -> ('a, exn list) result t

(** [finalize f ~finally] runs [finally] after [f ()] has terminated,
    whether it fails or succeeds. *)
val finalize
  :  (unit -> 'a t)
  -> finally:(unit -> unit t)
  -> 'a t

(** {1 Synchronization} *)

(** Write once variables *)
module Ivar : sig
  type 'a fiber = 'a t

  (** A ivar is a synchronization variable that can be written only
      once. *)
  type 'a t

  (** Create a new empty ivar. *)
  val create : unit -> 'a t

  (** Read the contents of the ivar. *)
  val read : 'a t -> 'a fiber

  (** Fill the ivar with the following value. This can only be called
      once for a given ivar. *)
  val fill : 'a t -> 'a -> unit fiber
end with type 'a fiber := 'a t

module Mutex : sig
  type 'a fiber = 'a t
  type t
  val create : unit -> t
  val with_lock : t -> (unit -> 'a fiber) -> 'a fiber
end with type 'a fiber := 'a t

(** {1 Running external programs} *)

type accepted_codes =
  | These of int list
  | All

(** How to handle sub-process failures *)
type ('a, 'b) failure_mode =
  | Strict : ('a, 'a) failure_mode
  (** Fail if the process exits with anything else than [0] *)
  | Accept : accepted_codes -> ('a, ('a, int) result) failure_mode
  (** Accept the following non-zero exit codes, and return [Error
      code] if the process exists with one of these codes. *)

(** Where to redirect standard output *)
type std_output_to =
  | Terminal
  | File        of string
  | Opened_file of opened_file

and opened_file =
  { filename : string
  ; desc     : opened_file_desc
  ; tail     : bool
  (** If [true], the descriptor is closed after starting the command *)
  }

and opened_file_desc =
  | Fd      of Unix.file_descr
  | Channel of out_channel

(** Why a Fiber.t was run *)
type purpose =
  | Internal_job
  | Build_job of Path.t list

(** [run ?dir ?stdout_to prog args] spawns a sub-process and wait for its termination *)
val run
  :  ?dir:string
  -> ?stdout_to:std_output_to
  -> ?stderr_to:std_output_to
  -> ?env:string array
  -> ?purpose:purpose
  -> (unit, 'a) failure_mode
  -> string
  -> string list
  -> 'a t

(** Run a command and capture its output *)
val run_capture
  :  ?dir:string
  -> ?env:string array
  -> ?purpose:purpose
  -> (string, 'a) failure_mode
  -> string
  -> string list
  -> 'a t
val run_capture_line
  :  ?dir:string
  -> ?env:string array
  -> ?purpose:purpose
  -> (string, 'a) failure_mode
  -> string
  -> string list
  -> 'a t
val run_capture_lines
  :  ?dir:string
  -> ?env:string array
  -> ?purpose:purpose
  -> (string list, 'a) failure_mode
  -> string
  -> string list
  -> 'a t

(** {1 Running fibers} *)

module Scheduler : sig
  (** [go ?log t] runs the following fiber until it terminates. *)
  val go : ?log:Log.t -> 'a t -> 'a

  (** Executes [f] before exiting, after all pending commands have
      finished *)
  val at_exit_after_waiting_for_commands : (unit -> unit) -> unit
end
