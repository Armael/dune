(** Build rules *)

open! Import

type t

(** {1 Setup} *)

(** {2 Creation} *)

(** Create a new build system. [file_tree] represent the source
    tree. *)
val create
  :  contexts:Context.t list
  -> file_tree:File_tree.t
  -> t

type extra_sub_directories_to_keep =
  | All
  | These of String_set.t

(** Set the rule generators callback. There must be one callback per
   build context name.

    Each callback is used to generate the rules for a given directory
    in the corresponding build context. It receive the directory for
    which to generate the rules and the splitted part of the path after
    the build context. It must return an additional list of
    sub-directories to keep. This is in addition to the ones that are
    present in the source tree and the ones that already contain rules.

    It is expected that [f] only generate rules whose targets are
   descendant of [dir]. *)
val set_rule_generators : t -> (dir:Path.t -> string list -> extra_sub_directories_to_keep) String_map.t -> unit

(** All other functions in this section must be called inside the rule generator
    callback. *)

(** {2 Primitive for rule generations} *)

(** Add a rule to the system. This function must be called from the [gen_rules]
    callback. All the target of the rule must be in the same directory.

    Assuming that [gen_rules ~dir:a] calls [add_rule r] where [r.dir] is [Some b], one of
    the following assumption must hold:

    - [a] and [b] are the same
    - [gen_rules ~dir:b] calls [load_dir ~dir:a]

    The call to [load_dir ~dir:a] from [gen_rules ~dir:b] declares a directory dependency
    from [b] to [a]. There must be no cyclic directory dependencies.
*)
val add_rule : t -> Build_interpret.Rule.t -> unit

(** [eval_glob t ~dir re ~f] returns the list of files in [dir] that matches [re] to
    [f]. The list of files includes the list of targets. *)
val eval_glob : t -> dir:Path.t -> Re.re -> string list

(** Returns the set of targets in the given directory. *)
val targets_of : t -> dir:Path.t -> Path.Set.t

(** Load the rules for this directory. *)
val load_dir : t -> dir:Path.t -> unit

(** [on_load_dir ~dir ~f] remembers to run [f] when loading the rules for [dir]. *)
val on_load_dir : t -> dir:Path.t -> f:(unit -> unit) -> unit

(** Stamp file that depends on all files of [dir] with extension [ext]. *)
val stamp_file_for_files_of : t -> dir:Path.t -> ext:string -> Path.t

(** {2 Aliases} *)

module Alias : sig
  type build_system = t
  type t

  val pp : t Fmt.t

  val make : string -> dir:Path.t -> t

  val of_path : Path.t -> t

  (** The following always holds:

      {[
        make (name t) ~dir:(dir t) = t
      ]}
  *)
  val name : t -> string
  val dir  : t -> Path.t

  val fully_qualified_name : t -> Path.t

  val default : dir:Path.t -> t
  val runtest : dir:Path.t -> t
  val install : dir:Path.t -> t
  val doc     : dir:Path.t -> t
  val lint    : dir:Path.t -> t

  (** Return the underlying stamp file *)
  val stamp_file : t -> Path.t

  (** [dep t = Build.path (stamp_file t)] *)
  val dep : t -> ('a, 'a) Build.t

  (** Implements [(alias_rec ...)] in dependency specification *)
  val dep_rec
    :  t
    -> loc:Loc.t
    -> file_tree:File_tree.t
    -> (unit, unit) Build.t

  (** Implements [@alias] on the command line *)
  val dep_rec_multi_contexts
    :  dir:Path.t
    -> name:string
    -> file_tree:File_tree.t
    -> contexts:string list
    -> (unit, unit) Build.t

  (** [add_deps store alias deps] arrange things so that all [deps]
      are built as part of the build of alias [alias]. *)
  val add_deps : build_system -> t -> Path.t list -> unit

  (** [add_action store alias ~stamp action] arrange things so that
      [action] is executed as part of the build of alias
      [alias]. [stamp] is any S-expression that is unique and
      persistent S-expression.
  *)
  val add_action
    :  build_system
    -> t
    -> ?locks:Path.t list
    -> stamp:Sexp.t
    -> (unit, Action.t) Build.t
    -> unit
end with type build_system := t

(** {1 Building} *)

(** ALl the functions in this section must be called outside the rule generator
    callback. *)

(** Do the actual build *)
val do_build
  :  t
  -> request:(unit, unit) Build.t
  -> unit Fiber.t

(** {2 Other queries} *)

val is_target : t -> Path.t -> bool

(** Return all the library dependencies (as written by the user)
   needed to build this request *)
val all_lib_deps
  :  t
  -> request:(unit, unit) Build.t
  -> Build.lib_deps Path.Map.t

(** Return all the library dependencies required to build this
   request, by context name *)
val all_lib_deps_by_context
  :  t
  -> request:(unit, unit) Build.t
  -> Build.lib_deps String_map.t

(** List of all buildable targets *)
val all_targets : t -> Path.t list

(** Return the list of files that were created in the source tree and
    needs to be deleted *)
val files_in_source_tree_to_delete
  :  unit
  -> Path.t list

(** {2 Build rules} *)

(** A fully built rule *)
module Rule : sig
  module Id : sig
    type t
    val to_int : t -> int
    val compare : t -> t -> int
  end

  type t =
    { id      : Id.t
    ; deps    : Path.Set.t
    ; targets : Path.Set.t
    ; context : Context.t option
    ; action  : Action.t
    }
end

(** Return the list of rules used to build the given targets. If
    [recursive] is [true], return all the rules needed to build the
    given targets and their transitive dependencies. *)
val build_rules
  :  ?recursive:bool (* default false *)
  -> t
  -> request:(unit, unit) Build.t
  -> Rule.t list Fiber.t

(** {1 Misc} *)

(** Dump various databases on disk *)
val finalize : t -> unit
