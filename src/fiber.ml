open Import

let ( += ) r x = r := !r + x

module Var0 = struct
  module Key = struct
    type 'a t = ..
  end

  module type T = sig
    type t
    type 'a Key.t += T : t Key.t
    val id : int
  end

  type 'a t = (module T with type t = 'a)

  let next = ref 0

  let create (type a) () =
    let n = !next in
    next := n + 1;
    let module M = struct
      type t = a
      type 'a Key.t += T : t Key.t
      let id = n
    end in
    (module M : T with type t = a)

  let id (type a) (module M : T with type t = a) = M.id

  let eq (type a) (type b)
        (module A : T with type t = a)
        (module B : T with type t = b) : (a, b) eq =
    match A.T with
    | B.T -> Eq
    | _ -> assert false
end

module Binding = struct
  type t = T : 'a Var0.t * 'a -> t
end

module Int_map = Map.Make(struct
    type t = int
    let compare : int -> int -> int = compare
  end)

module Execution_context = struct
  type t =
    { on_error : exn -> unit (* This callback must never raise *)
    ; fibers   : int ref (* Number of fibers running in this execution
                            context *)
    ; vars     : Binding.t Int_map.t
    ; stack    : string list ref
    }

  let create () =
    { on_error = reraise
    ; fibers   = ref 0
    ; vars     = Int_map.empty
    ; stack    = ref []
    }

  let forward_error t pos exn =
    let bt = Printexc.get_raw_backtrace () in
    t.stack := pos :: !(t.stack);
    try
      t.on_error exn
    with exn2 ->
      (* We can't abort the execution at this point, so we just dump
         the error on stderr *)
      let bt2 = Printexc.get_backtrace () in
      let s =
        (sprintf "%s\n%s\nOriginal exception was: %s\n%s"
           (Printexc.to_string exn2) bt2
           (Printexc.to_string exn) (Printexc.raw_backtrace_to_string bt))
        |> String.split_lines
        |> List.map ~f:(sprintf "| %s")
        |> String.concat ~sep:"\n"
      in
      let line = String.make 71 '-' in
      Format.eprintf
        "/%s\n\
         | @{<error>Internal error@}: \
         Fiber.Execution_context.forward_error: error handler raised.\n\
         %s\n\
         \\%s@."
        line s line
end

open Execution_context

type 'a t = Execution_context.t -> ('a -> unit) -> unit

let return x _ k = k x

let delay f ctx k =
  f () ctx k

module O = struct
  let (>>>) a b ctx k =
    a ctx (fun () -> b ctx k)

  let (>>=) t f ctx k =
    t ctx (fun x -> f x ctx k)

  let (>>|) t f ctx k =
    t ctx (fun x -> k (f x))
end

open O

type ('a, 'b) both_state =
  | Nothing_yet
  | Got_a of 'a
  | Got_b of 'b

let fork fa fb ctx k =
  let state = ref Nothing_yet in
  incr ctx.fibers;
  begin
    try
      fa () ctx (fun a ->
        match !state with
        | Nothing_yet -> decr ctx.fibers; state := Got_a a
        | Got_a _ -> assert false
        | Got_b b -> k (a, b))
    with exn ->
      forward_error ctx "fork" exn
  end;
  fb () ctx (fun b ->
    match !state with
    | Nothing_yet -> decr ctx.fibers; state := Got_b b
    | Got_a a -> k (a, b)
    | Got_b _ -> assert false)

let fork_unit fa fb ctx k =
  let state = ref Nothing_yet in
  incr ctx.fibers;
  begin
    try
      fa () ctx (fun () ->
        match !state with
        | Nothing_yet -> decr ctx.fibers; state := Got_a ()
        | Got_a _ -> assert false
        | Got_b b -> k b)
    with exn ->
      forward_error ctx "fork_unit" exn
  end;
  fb () ctx (fun b ->
    match !state with
    | Nothing_yet -> decr ctx.fibers; state := Got_b b
    | Got_a () -> k b
    | Got_b _ -> assert false)

let list_of_option_array =
  let rec loop arr i acc =
    if i = 0 then
      acc
    else
      let i = i - 1 in
      match arr.(i) with
      | None -> assert false
      | Some x ->
        loop arr i (x :: acc)
  in
  fun a -> loop a (Array.length a) []

let nfork_map l ~f ctx k =
  match l with
  | [] -> k []
  | [x] -> f x ctx (fun x -> k [x])
  | _ ->
    let n = List.length l in
    ctx.fibers += (n - 1);
    let left_over = ref n in
    let results = Array.make n None in
    List.iteri l ~f:(fun i x ->
      try
        f x ctx (fun y ->
          results.(i) <- Some y;
          decr left_over;
          if !left_over = 0 then
            k (list_of_option_array results)
          else
            decr ctx.fibers)
      with exn ->
        forward_error ctx "nfork_map" exn)

let nfork_iter l ~f ctx k =
  match l with
  | [] -> k ()
  | [x] -> f x ctx k
  | _ ->
    let n = List.length l in
    ctx.fibers += (n - 1);
    let left_over = ref n in
    let k () =
      decr left_over;
      if !left_over = 0 then k () else decr ctx.fibers
    in
    List.iter l ~f:(fun x ->
      try
        f x ctx k
      with exn ->
        forward_error ctx "nfork_iter" exn)

module Var = struct
  include Var0

  let cast (type a) (type b) (Eq : (a, b) eq) (x : a) : b = x

  let find ctx var =
    match Int_map.find (id var) ctx.vars with
    | None -> None
    | Some (Binding.T (var', v)) ->
      let eq = eq var' var in
      Some (cast eq v)

  let find_exn ctx var =
    match Int_map.find (id var) ctx.vars with
    | None -> failwith "Fiber.Var.find_exn"
    | Some (Binding.T (var', v)) ->
      let eq = eq var' var in
      cast eq v

  let get     var ctx k = k (find     ctx var)
  let get_exn var ctx k = k (find_exn ctx var)

  let set (type a) (var : a t) x fiber ctx k =
    let (module M) = var in
    let data = Binding.T (var, x) in
    let ctx =
      { ctx with vars = Int_map.add ctx.vars ~key:M.id ~data }
    in
    fiber ctx k
end

let iter_errors_internal f ~on_error ctx k =
  let fibers = ref 1 in
  let finished = ref false in
  let stack = ref [] in
  let check () =
    if !finished then begin
      List.iter (List.rev !stack) ~f:prerr_endline;
      assert false
    end
  in
  let on_error exn =
    check ();
    stack := sprintf "error %d" !fibers :: !stack;
    let n = !fibers - 1 in
    assert (n >= 0);
    fibers := n;
    begin
      match on_error with
      | None ->
        incr ctx.fibers;
        forward_error ctx "iter_errors 1" exn
      | Some f ->
        try
          f exn
        with exn ->
          incr ctx.fibers;
          forward_error ctx "iter_errors 2" exn
    end;
    if n = 0 then (
      check ();
      finished := true;
      stack := "raised" :: !stack;
      try
        k (Error ())
      with exn ->
        forward_error ctx "iter_errors 3" exn
    )
  in
  let sub_ctx = { ctx with on_error; fibers; stack } in
  try
    f () sub_ctx (fun x ->
      check ();
      finished := true;
      stack := "done" :: !stack;
      assert (!fibers = 1);
      fibers := 0;
      try
        k (Ok x)
      with exn ->
        forward_error ctx "iter_errors 4" exn);
    stack := "delayed" :: !stack
  with exn ->
    forward_error sub_ctx "iter_errors 5" exn

let wait_errors f = iter_errors_internal f ~on_error:None
let iter_errors f ~on_error = iter_errors_internal f ~on_error:(Some on_error)

let fold_errors f ~init ~on_error ctx k =
  let acc = ref init in
  let on_error exn =
    acc := on_error exn !acc
  in
  iter_errors f ~on_error ctx (function
    | Ok _ as ok -> k ok
    | Error ()   -> k (Error !acc))

let catch_errors f =
  fold_errors f
    ~init:[]
    ~on_error:(fun e l -> e :: l)

let sink _ _ = ()

let finalize f ~finally =
  wait_errors f >>= fun res ->
  finally () >>= fun () ->
  match res with
  | Ok x -> return x
  | Error () -> sink

module Handler = struct
  type 'a t =
    { run : 'a -> unit
    ; ctx : Execution_context.t
    }

  let run t x =
    try
      t.run x
    with exn ->
      forward_error t.ctx "handler" exn
end

module Ivar = struct
  type 'a state =
    | Full  of 'a
    | Empty of 'a Handler.t Queue.t

  type 'a t = { mutable state : 'a state }

  let create () = { state = Empty (Queue.create ()) }

  let fill t x _ctx k =
    match t.state with
    | Full  _ -> failwith "Fiber.Ivar.fill"
    | Empty q ->
      t.state <- Full x;
      Queue.iter
        (fun handler ->
           Handler.run handler x)
        q;
      k ()

  let read t ctx k =
    match t.state with
    | Full  x -> k x
    | Empty q ->
      ctx.stack := "read" :: !(ctx.stack);
      Queue.push { Handler. run = k; ctx } q
end

exception Already_reported

let n = ref String_set.empty
let p = ref String_set.empty
let () = at_exit (fun () ->
  String_set.iter !n ~f:(Printf.eprintf "empty: %S\n%!"))

let memoize s t =
  if String_set.mem s !p then failwith s;
  p := String_set.add s !p;
  let cell = ref None in
  delay (fun () ->
    match !cell with
    | Some ivar ->
      (Ivar.read ivar >>| function
       | Ok x -> x
       | Error () -> raise Already_reported)
    | None ->
      let ivar = Ivar.create () in
      cell := Some ivar;
      n := String_set.add s !n;
      wait_errors (fun () -> t) >>= fun res ->
      n := String_set.remove s !n;
      Ivar.fill ivar res >>| fun () ->
      match res with
      | Ok x -> x
      | Error () -> raise Already_reported)

module Mutex = struct
  type t =
    { mutable locked  : bool
    ; mutable waiters : unit Handler.t Queue.t
    }

  let lock t ctx k =
    if t.locked then
      let () = ctx.stack := "lock" :: !(ctx.stack) in
      Queue.push { Handler. run = k; ctx } t.waiters
    else begin
      t.locked <- true;
      k ()
    end

  let unlock t _ctx k =
    assert t.locked;
    if Queue.is_empty t.waiters then
      t.locked <- false
    else
      Handler.run (Queue.pop t.waiters) ();
    k ()

  let with_lock t f =
    lock t >>= fun () ->
    finalize f ~finally:(fun () -> unlock t)

  let create () =
    { locked  = false
    ; waiters = Queue.create ()
    }
end

module Scheduler = struct
  type running_job =
    { pid             : int
    ; handler         : Unix.process_status Handler.t
    }

  module Running_jobs : sig
    val add : running_job -> unit
    val wait : unit -> running_job * Unix.process_status
    val count : unit -> int
  end = struct
    let all = Hashtbl.create 128

    let add job = Hashtbl.add all ~key:job.pid ~data:job

    let count () = Hashtbl.length all

    let resolve_and_remove_job pid =
      let job =
        Hashtbl.find_exn all pid ~string_of_key:(sprintf "<pid:%d>")
          ~table_desc:(fun _ -> "<running-jobs>")
      in
      Hashtbl.remove all pid;
      job

    exception Finished of running_job * Unix.process_status

    let wait_nonblocking_win32 () =
      match
        Hashtbl.iter all ~f:(fun ~key:pid ~data:job ->
          let pid, status = Unix.waitpid [WNOHANG] pid in
          if pid <> 0 then
            raise_notrace (Finished (job, status)))
      with
      | () -> None
      | exception (Finished (job, status)) ->
        Hashtbl.remove all job.pid;
        Some (job, status)

    let rec wait_win32 () =
      match wait_nonblocking_win32 () with
      | None ->
        ignore (Unix.select [] [] [] 0.001);
        wait_win32 ()
      | Some x -> x

    let wait_unix () =
      let pid, status = Unix.wait () in
      (resolve_and_remove_job pid, status)

    let wait =
      if Sys.win32 then
        wait_win32
      else
        wait_unix
  end

  type info =
    { log : Log.t
    ; original_cwd : string
    }

  let info_var : info Var.t = Var.create ()

  let waiting_for_available_job = Queue.create ()
  let wait_for_available_job ctx k =
    if Running_jobs.count () < !Clflags.concurrency then
      k (Var.find_exn ctx info_var)
    else
      let () = ctx.stack := "wait job" :: !(ctx.stack) in
      Queue.push { Handler. ctx; run = k } waiting_for_available_job

  let wait_for_process pid ctx k =
    let () = ctx.stack := "wait proc" :: !(ctx.stack) in
    Running_jobs.add
      { pid
      ; handler = { Handler. ctx; run = k }
      }

  let rec go_rec info result =
    match !result with
    | Some x -> x
    | None ->
      if Running_jobs.count () = 0 then
        code_errorf "Fiber.Scheduler.go: no more processes running";
      let job, status = Running_jobs.wait () in
      if not (Queue.is_empty waiting_for_available_job) then
        Handler.run (Queue.pop waiting_for_available_job) info;
      Handler.run job.handler status;
      go_rec info result

  let go ?(log=Log.no_log) fiber =
    Lazy.force Ansi_color.setup_env_for_colors;
    Log.info log ("Workspace root: " ^ !Clflags.workspace_root);
    let cwd = Sys.getcwd () in
    if cwd <> initial_cwd then
      Printf.eprintf "Entering directory '%s'\n%!" cwd;
    let info = { log; original_cwd = cwd } in
    let fiber =
      Var.set info_var info
        (iter_errors (fun () -> fiber) ~on_error:(function
           | Already_reported -> ()
           | exn ->
             Format.eprintf "%a@?" Report_error.report exn))
    in
    let result = ref None in
    fiber (Execution_context.create ()) (fun x -> result := Some x);
    match go_rec info result with
    | Ok x -> x
    | Error _ -> die ""
end
