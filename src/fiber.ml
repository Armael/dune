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
    { on_error : exn -> unit
    ; fibers   : int ref (* number of fibers running in this
                            execution context *)
    ; vars     : Binding.t Int_map.t
    }

  let create () =
    { on_error = reraise
    ; fibers   = ref 0
    ; vars     = Int_map.empty
    }
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
      ctx.on_error exn
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
      ctx.on_error exn
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
        ctx.on_error exn)

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
        ctx.on_error exn)

module Var = struct
  include Var0

  let cast (type a) (type b) (Eq : (a, b) eq) (x : a) : b = x

  let get (type a) (var : a t) ctx k =
    match Int_map.find (id var) ctx.vars with
    | None -> k None
    | Some (Binding.T (var', v)) ->
      let eq = eq var' var in
      k (Some (cast eq v))

  let get_exn var ctx k =
    match Int_map.find (id var) ctx.vars with
    | None -> failwith "Fiber.Var.get_exn"
    | Some (Binding.T (var', v)) ->
      let eq = eq var' var in
      k (cast eq v)

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
  let on_error exn =
    let n = !fibers - 1 in
    fibers := n;
    assert (n > 0);
    begin
      match on_error with
      | None ->
        incr ctx.fibers;
        ctx.on_error exn
      | Some f ->
        try
          f exn
        with exn ->
          incr ctx.fibers;
          ctx.on_error exn
    end;
    if n = 0 then k (Error ())
  in
  let ctx = { ctx with on_error; fibers } in
  try
    f () ctx (fun x ->
      assert (!fibers = 1);
      fibers := 0;
      k (Ok x))
  with exn when !fibers > 0 ->
    on_error exn

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
      t.ctx.on_error exn
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
    | Empty q -> Queue.push { Handler. run = k; ctx } q
end

exception Already_reported

let memoize t =
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
      wait_errors (fun () -> t) >>= fun res ->
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

type accepted_codes =
  | These of int list
  | All

let code_is_ok accepted_codes n =
  match accepted_codes with
  | These set -> List.mem n ~set
  | All -> true

type ('a, 'b) failure_mode =
  | Strict : ('a, 'a) failure_mode
  | Accept : accepted_codes -> ('a, ('a, int) result) failure_mode

let accepted_codes : type a b. (a, b) failure_mode -> accepted_codes = function
  | Strict -> These [0]
  | Accept (These codes) -> These (0 :: codes)
  | Accept All -> All

let map_result
  : type a b. (a, b) failure_mode -> int t -> f:(unit -> a) -> b t
  = fun mode t ~f ->
    match mode with
    | Strict   -> t >>| fun _ -> f ()
    | Accept _ ->
      t >>| function
      | 0 -> Ok (f ())
      | n -> Error n

type std_output_to =
  | Terminal
  | File        of string
  | Opened_file of opened_file

and opened_file =
  { filename : string
  ; desc     : opened_file_desc
  ; tail     : bool
  }

and opened_file_desc =
  | Fd      of Unix.file_descr
  | Channel of out_channel

(** Why a Fiber.t was run *)
type purpose =
  | Internal_job
  | Build_job of Path.t list

type job =
  { prog      : string
  ; args      : string list
  ; dir       : string option
  ; stdout_to : std_output_to
  ; stderr_to : std_output_to
  ; env       : string array option
  ; handler   : int Handler.t
  ; ok_codes  : accepted_codes
  ; purpose   : purpose
  }

let to_run : job Queue.t = Queue.create ()

let run_internal ?dir ?(stdout_to=Terminal) ?(stderr_to=Terminal) ?env ~purpose fail_mode prog args ctx k =
  let dir =
    match dir with
    | Some "." -> None
    | _ -> dir
  in
  Queue.push
    { prog
    ; args
    ; dir
    ; stdout_to
    ; stderr_to
    ; env
    ; handler = { run = k; ctx }
    ; ok_codes = accepted_codes fail_mode
    ; purpose
    }
    to_run

let run ?dir ?stdout_to ?stderr_to ?env ?(purpose=Internal_job) fail_mode prog args =
  map_result fail_mode (run_internal ?dir ?stdout_to ?stderr_to ?env ~purpose fail_mode prog args)
    ~f:ignore

module Temp = struct
  let tmp_files = ref String_set.empty
  let () =
    at_exit (fun () ->
      let fns = !tmp_files in
      tmp_files := String_set.empty;
      String_set.iter fns ~f:(fun fn ->
        try Sys.force_remove fn with _ -> ()))

  let create prefix suffix =
    let fn = Filename.temp_file prefix suffix in
    tmp_files := String_set.add fn !tmp_files;
    fn

  let destroy fn =
    (try Sys.force_remove fn with Sys_error _ -> ());
    tmp_files := String_set.remove fn !tmp_files
end

let run_capture_gen ?dir ?env ?(purpose=Internal_job) fail_mode prog args ~f =
  let fn = Temp.create "jbuild" ".output" in
  map_result fail_mode (run_internal ?dir ~stdout_to:(File fn) ?env ~purpose fail_mode prog args)
    ~f:(fun () ->
      let x = f fn in
      Temp.destroy fn;
      x)

let run_capture       = run_capture_gen ~f:Io.read_file
let run_capture_lines = run_capture_gen ~f:Io.lines_of_file

let run_capture_line ?dir ?env ?(purpose=Internal_job) fail_mode prog args =
  run_capture_gen ?dir ?env ~purpose fail_mode prog args ~f:(fun fn ->
    match Io.lines_of_file fn with
    | [x] -> x
    | l ->
      let cmdline =
        let s = String.concat (prog :: args) ~sep:" " in
        match dir with
        | None -> s
        | Some dir -> sprintf "cd %s && %s" dir s
      in
      match l with
      | [] ->
        die "command returned nothing: %s" cmdline
      | _ ->
        die "command returned too many lines: %s\n%s"
          cmdline (String.concat l ~sep:"\n"))

module Scheduler = struct
  let split_prog s =
    let len = String.length s in
    if len = 0 then
      "", "", ""
    else begin
      let rec find_prog_start i =
        if i < 0 then
          0
        else
          match s.[i] with
          | '\\' | '/' -> (i + 1)
          | _ -> find_prog_start (i - 1)
      in
      let prog_end =
        match s.[len - 1] with
        | '"' -> len - 1
        | _   -> len
      in
      let prog_start = find_prog_start (prog_end - 1) in
      let prog_end =
        match String.index_from s prog_start '.' with
        | exception _ -> prog_end
        | i -> i
      in
      let before = String.sub s ~pos:0 ~len:prog_start in
      let after = String.sub s ~pos:prog_end ~len:(len - prog_end) in
      let prog = String.sub s ~pos:prog_start ~len:(prog_end - prog_start) in
      before, prog, after
    end

  let colorize_prog s =
    let len = String.length s in
    if len = 0 then
      s
    else
      let before, prog, after = split_prog s in
      before ^ Ansi_color.colorize ~key:prog prog ^ after

  let rec colorize_args = function
    | [] -> []
    | "-o" :: fn :: rest ->
      "-o" :: Ansi_color.(apply_string output_filename) fn :: colorize_args rest
    | x :: rest -> x :: colorize_args rest

  let command_line { prog; args; dir; stdout_to; stderr_to; _ } =
    let quote = quote_for_shell in
    let prog = colorize_prog (quote prog) in
    let s = String.concat (prog :: colorize_args (List.map args ~f:quote)) ~sep:" " in
    let s =
      match dir with
      | None -> s
      | Some dir -> sprintf "(cd %s && %s)" dir s
    in
    match stdout_to, stderr_to with
    | (File fn1 | Opened_file { filename = fn1; _ }),
      (File fn2 | Opened_file { filename = fn2; _ }) when fn1 = fn2 ->
      sprintf "%s &> %s" s fn1
    | _ ->
      let s =
        match stdout_to with
        | Terminal -> s
        | File fn | Opened_file { filename = fn; _ } -> sprintf "%s > %s" s fn
      in
      match stderr_to with
      | Terminal -> s
      | File fn | Opened_file { filename = fn; _ } -> sprintf "%s 2> %s" s fn

  let pp_purpose ppf = function
    | Internal_job ->
      Format.fprintf ppf "(internal)"
    | Build_job targets ->
      let rec split_paths targets_acc ctxs_acc = function
        | [] -> List.rev targets_acc, String_set.(elements (of_list ctxs_acc))
        | path :: rest ->
          let add_ctx ctx acc = if ctx = "default" then acc else ctx :: acc in
          match Utils.analyse_target path with
          | Other path ->
            split_paths (Path.to_string path :: targets_acc) ctxs_acc rest
          | Regular (ctx, filename) ->
            split_paths (Path.to_string filename :: targets_acc) (add_ctx ctx ctxs_acc) rest
          | Alias (ctx, name) ->
            split_paths (("alias " ^ Path.to_string name) :: targets_acc) (add_ctx ctx ctxs_acc) rest
      in
      let target_names, contexts = split_paths [] [] targets in
      let target_names_grouped_by_prefix =
        List.map target_names ~f:Filename.split_extension_after_dot
        |> String_map.of_alist_multi
        |> String_map.bindings
      in
      let pp_comma ppf () = Format.fprintf ppf "," in
      let pp_group ppf (prefix, suffixes) =
        match suffixes with
        | [] -> assert false
        | [suffix] -> Format.fprintf ppf "%s%s" prefix suffix
        | _ ->
          Format.fprintf ppf "%s{%a}"
            prefix
            (Format.pp_print_list ~pp_sep:pp_comma Format.pp_print_string)
            suffixes
      in
      let pp_contexts ppf = function
        | [] -> ()
        | ctxs ->
          Format.fprintf ppf " @{<details>[%a]@}"
            (Format.pp_print_list ~pp_sep:pp_comma
               (fun ppf s -> Format.fprintf ppf "%s" s))
            ctxs
      in
      Format.fprintf ppf "%a%a"
        (Format.pp_print_list ~pp_sep:pp_comma pp_group)
        target_names_grouped_by_prefix
        pp_contexts
        contexts;

  type running_job =
    { id              : int
    ; job             : job
    ; pid             : int
    ; output_filename : string option
    ; (* for logs, with ansi colors code always included in the string *)
      command_line    : string
    ; log             : Log.t
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

  let process_done job (status : Unix.process_status) =
    let output =
      match job.output_filename with
      | None -> ""
      | Some fn ->
        let s = Io.read_file fn in
        Temp.destroy fn;
        let len = String.length s in
        if len > 0 && s.[len - 1] <> '\n' then
          s ^ "\n"
        else
          s
    in
    Log.command job.log
      ~command_line:job.command_line
      ~output:output
      ~exit_status:status;
    let _, progname, _ = split_prog job.job.prog in
    match status with
    | WEXITED n when code_is_ok job.job.ok_codes n ->
      if !Clflags.verbose then begin
        if output <> "" then
          Format.eprintf "@{<kwd>Output@}[@{<id>%d@}]:\n%s%!" job.id output;
        if n <> 0 then
          Format.eprintf
            "@{<warning>Warning@}: Command [@{<id>%d@}] exited with code %d, \
             but I'm ignore it, hope that's OK.\n%!" job.id n;
      end else if (output <> "" || job.job.purpose <> Internal_job) then
        begin
          Format.eprintf "@{<ok>%12s@} %a@." progname pp_purpose job.job.purpose;
          Format.eprintf "%s%!" output;
        end;
      Handler.run job.job.handler n
    | WEXITED n ->
      if !Clflags.verbose then begin
        Format.eprintf "\n@{<kwd>Command@} [@{<id>%d@}] exited with code %d:\n\
                        @{<prompt>$@} %s\n%s%!"
          job.id n
          (Ansi_color.strip_colors_for_stderr job.command_line)
          (Ansi_color.strip_colors_for_stderr output)
      end else begin
        Format.eprintf "@{<error>%12s@} %a @{<error>(exit %d)@}@."
          progname pp_purpose job.job.purpose n;
        Format.eprintf "@{<details>%s@}@."
          (Ansi_color.strip job.command_line);
        Format.eprintf "%s%!" output;
      end;
      job.job.handler.ctx.on_error (Fatal_error "")
    | WSIGNALED n ->
      if !Clflags.verbose then begin
        Format.eprintf "\n@{<kwd>Command@} [@{<id>%d@}] got signal %s:\n\
                        @{<prompt>$@} %s\n%s%!"
          job.id (Utils.signal_name n)
          (Ansi_color.strip_colors_for_stderr job.command_line)
          (Ansi_color.strip_colors_for_stderr output);
      end else begin
        Format.eprintf "@{<error>%12s@} %a @{<error>(got signal %s)@}@."
          progname pp_purpose job.job.purpose (Utils.signal_name n);
        Format.eprintf "@{<details>%s@}@."
          (Ansi_color.strip job.command_line);
        Format.eprintf "%s%!" output;
      end;
      job.job.handler.ctx.on_error (Fatal_error "")
    | WSTOPPED _ -> assert false

  let gen_id =
    let next = ref (-1) in
    fun () -> incr next; !next

  let at_exit_after_waiting_for_commands = at_exit

  let get_std_output ~default = function
    | Terminal -> (default, None)
    | File fn ->
      let fd = Unix.openfile fn [O_WRONLY; O_CREAT; O_TRUNC; O_SHARE_DELETE] 0o666 in
      (fd, Some (Fd fd))
    | Opened_file { desc; tail; _ } ->
      let fd =
        match desc with
        | Fd      fd -> fd
        | Channel oc -> flush oc; Unix.descr_of_out_channel oc
      in
      (fd, Option.some_if tail desc)

  let close_std_output = function
    | None -> ()
    | Some (Fd      fd) -> Unix.close fd
    | Some (Channel oc) -> close_out  oc

  let rec go_rec cwd log result =
    match !result with
    | Some x -> x
    | None ->
      while Running_jobs.count () < !Clflags.concurrency &&
            not (Queue.is_empty to_run) do
        let job = Queue.pop to_run in
        let id = gen_id () in
        let command_line = command_line job in
        if !Clflags.verbose then
          Format.eprintf "@{<kwd>Running@}[@{<id>%d@}]: %s@." id
            (Ansi_color.strip_colors_for_stderr command_line);
        let argv = Array.of_list (job.prog :: job.args) in
        let output_filename, stdout_fd, stderr_fd, to_close =
          match job.stdout_to, job.stderr_to with
          | (Terminal, _ | _, Terminal) when !Clflags.capture_outputs ->
            let fn = Temp.create "jbuilder" ".output" in
            let fd = Unix.openfile fn [O_WRONLY; O_SHARE_DELETE] 0 in
            (Some fn, fd, fd, Some fd)
          | _ ->
            (None, Unix.stdout, Unix.stderr, None)
        in
        let stdout, close_stdout = get_std_output job.stdout_to ~default:stdout_fd in
        let stderr, close_stderr = get_std_output job.stderr_to ~default:stderr_fd in
        Option.iter job.dir ~f:(fun dir -> Sys.chdir dir);
        let pid =
          match job.env with
          | None ->
            Unix.create_process job.prog argv
              Unix.stdin stdout stderr
          | Some env ->
            Unix.create_process_env job.prog argv env
              Unix.stdin stdout stderr
        in
        Option.iter job.dir ~f:(fun _ -> Sys.chdir cwd);
        Option.iter to_close ~f:Unix.close;
        close_std_output close_stdout;
        close_std_output close_stderr;
        Running_jobs.add
          { id
          ; job
          ; pid
          ; output_filename
          ; command_line
          ; log
          }
      done;
      let job, status = Running_jobs.wait () in
      process_done job status;
      go_rec cwd log result

  let go ?(log=Log.no_log) t =
    Lazy.force Ansi_color.setup_env_for_colors;
    Log.info log ("Workspace root: " ^ !Clflags.workspace_root);
    let cwd = Sys.getcwd () in
    if cwd <> initial_cwd then
      Printf.eprintf "Entering directory '%s'\n%!" cwd;
    let t =
      iter_errors (fun () -> t) ~on_error:(fun exn ->
        Format.eprintf "%a@?" Report_error.report exn)
    in
    let result = ref None in
    t (Execution_context.create ()) (fun x -> result := Some x);
    match go_rec cwd log result with
    | Ok x -> x
    | Error _ -> die ""
end
