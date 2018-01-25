open Import
open Fiber.O

type setup =
  { build_system : Build_system.t
  ; stanzas      : (Path.t * Jbuild.Scope.t * Jbuild.Stanzas.t) list String_map.t
  ; contexts     : Context.t list
  ; packages     : Package.t String_map.t
  ; file_tree    : File_tree.t
  }

let package_install_file { packages; _ } pkg =
  match String_map.find pkg packages with
  | None -> Error ()
  | Some p ->
    Ok (Path.relative p.path (Utils.install_file ~package:p.name ~findlib_toolchain:None))

let setup ?(log=Log.no_log)
      ?filter_out_optional_stanzas_with_missing_deps
      ?workspace ?(workspace_file="jbuild-workspace")
      ?(use_findlib=true)
      ?only_packages
      ?extra_ignored_subtrees
      ?x
      () =
  let conf = Jbuild_load.load ?extra_ignored_subtrees () in
  Option.iter only_packages ~f:(fun set ->
    String_set.iter set ~f:(fun pkg ->
      if not (String_map.mem pkg conf.packages) then
        die "@{<error>Error@}: I don't know about package %s \
             (passed through --only-packages/--release)%s"
          pkg (hint pkg (String_map.keys conf.packages))));
  let workspace =
    match workspace with
    | Some w -> w
    | None ->
      if Sys.file_exists workspace_file then
        Workspace.load ?x workspace_file
      else
        { merlin_context = Some "default"
        ; contexts = [Default [
            match x with
            | None -> Native
            | Some x -> Named x
          ]]
        }
  in

  Fiber.all (
    List.map workspace.contexts ~f:(fun ctx_def ->
      let name = Workspace.Context.name ctx_def in
      Context.create ctx_def ~merlin:(workspace.merlin_context = Some name) ~use_findlib)
  )
  >>= fun contexts ->
  let contexts = List.concat contexts in
  List.iter contexts ~f:(fun (ctx : Context.t) ->
    Log.infof log "@[<1>Jbuilder context:@,%a@]@." Sexp.pp (Context.sexp_of_t ctx));
  let build_system =
    Build_system.create ~contexts ~file_tree:conf.file_tree
  in
  Gen_rules.gen conf
    ~build_system
    ~contexts
    ?only_packages
    ?filter_out_optional_stanzas_with_missing_deps
  >>= fun stanzas ->
  Fiber.return
    { build_system
    ; stanzas
    ; contexts
    ; packages = conf.packages
    ; file_tree = conf.file_tree
    }

let external_lib_deps ?log ~packages () =
  Fiber.Scheduler.go ?log
    (setup () ~filter_out_optional_stanzas_with_missing_deps:false
     >>| fun setup ->
     let install_files =
       List.map packages ~f:(fun pkg ->
         match package_install_file setup pkg with
         | Ok path -> path
         | Error () -> die "Unknown package %S" pkg)
     in
     match String_map.find "default" setup.stanzas with
     | None -> die "You need to set a default context to use external-lib-deps"
     | Some stanzas ->
       let internals = Jbuild.Stanzas.lib_names stanzas in
       Path.Map.map
         (Build_system.all_lib_deps setup.build_system
            ~request:(Build.paths install_files))
         ~f:(String_map.filter ~f:(fun name _ ->
           not (String_set.mem name internals))))

let ignored_during_bootstrap =
  Path.Set.of_list
    (List.map ~f:Path.of_string
       [ "test"
       ; "example"
       ])

(* Called by the script generated by ../build.ml *)
let bootstrap () =
  Ansi_color.setup_err_formatter_colors ();
  let main () =
    let anon s = raise (Arg.Bad (Printf.sprintf "don't know what to do with %s\n" s)) in
    let subst () =
      Fiber.Scheduler.go (Watermarks.subst () ~name:"jbuilder");
      exit 0
    in
    Arg.parse
      [ "-j"           , Set_int Clflags.concurrency, "JOBS concurrency"
      ; "--dev"        , Set Clflags.dev_mode       , " set development mode"
      ; "--verbose"    , Set Clflags.verbose        , " print detailed information about commands being run"
      ; "--subst"      , Unit subst                 , " substitute watermarks in source files"
      ]
      anon "Usage: boot.exe [-j JOBS] [--dev]\nOptions are:";
    Clflags.debug_dep_path := true;
    let log = Log.create () in
    Fiber.Scheduler.go ~log
      (setup ~log ~workspace:{ merlin_context = Some "default"
                             ; contexts = [Default [Native]] }
         ~use_findlib:false
         ~extra_ignored_subtrees:ignored_during_bootstrap
         ()
       >>= fun { build_system = bs; _ } ->
       Build_system.do_build bs
         ~request:(Build.path (Path.of_string "_build/default/jbuilder.install")))
  in
  try
    main ()
  with
  | Fiber.Already_reported ->
    exit 1
  | exn ->
    Format.eprintf "%a@?" Report_error.report exn;
    exit 1

let setup = setup ~use_findlib:true ~extra_ignored_subtrees:Path.Set.empty

let find_context_exn t ~name =
  match List.find t.contexts ~f:(fun c -> c.name = name) with
  | Some ctx -> ctx
  | None ->
    Format.eprintf "@{<Error>Error@}: Context %S not found!@." name;
    die ""
