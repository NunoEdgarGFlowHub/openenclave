(* Copyright (c) Open Enclave SDK contributors.
   Licensed under the MIT License. *)

(** This module is Open Enclave's plugin for Intel's Edger8r, allowing
    us to share the same Enclave Definition Language, but emit our
    SDK's bindings. *)

open Intel.Ast
open Printf
open Common

(** Emit [struct], [union], or [enum]. *)
let emit_composite_type =
  let emit_struct (s : struct_def) =
    [
      "typedef struct " ^ s.sname;
      "{";
      String.concat "\n"
        (List.map
           (fun (ptype, decl) ->
             sprintf "    %s %s%s;"
               (get_tystr (get_param_atype ptype))
               decl.identifier
               (get_array_dims decl.array_dims))
           s.smlist);
      "} " ^ s.sname ^ ";";
      "";
    ]
  in
  let emit_union (u : union_def) =
    [
      "typedef union " ^ u.uname;
      "{";
      String.concat "\n"
        (List.map
           (fun (atype, decl) ->
             sprintf "    %s %s%s;" (get_tystr atype) decl.identifier
               (get_array_dims decl.array_dims))
           u.umlist);
      "} " ^ u.uname ^ ";";
      "";
    ]
  in
  let emit_enum (e : enum_def) =
    [
      "typedef enum " ^ e.enname;
      "{";
      String.concat ",\n"
        (List.map
           (fun (name, value) ->
             sprintf "    %s%s" name
               ( match value with
               | EnumVal (AString s) -> " = " ^ s
               | EnumVal (ANumber n) -> " = " ^ string_of_int n
               | EnumValNone -> "" ))
           e.enbody);
      "} " ^ e.enname ^ ";";
      "";
    ]
  in
  function
  | StructDef s -> emit_struct s
  | UnionDef u -> emit_union u
  | EnumDef e -> emit_enum e

let warn_non_portable_types (fd : func_decl) =
  (* Check if any of the parameters or the return type has the given
     root type. *)
  let uses_type (t : atype) =
    t = fd.rtype || List.exists (fun (p, _) -> t = get_param_atype p) fd.plist
  in
  let print_portability_warning ty =
    printf
      "Warning: Function '%s': %s has different sizes on Windows and Linux. \
       This enclave cannot be built in Linux and then safely loaded in Windows.\n"
      fd.fname ty
  in
  let print_portability_warning_with_recommendation ty recommendation =
    printf
      "Warning: Function '%s': %s has different sizes on Windows and Linux. \
       This enclave cannot be built in Linux and then safely loaded in \
       Windows. Consider using %s instead.\n"
      fd.fname ty recommendation
  in
  (* longs are represented as an Int type *)
  let long_t = Int { ia_signedness = Signed; ia_shortness = ILong } in
  let ulong_t = Int { ia_signedness = Unsigned; ia_shortness = ILong } in
  if uses_type WChar then print_portability_warning "wchar_t";
  if uses_type LDouble then print_portability_warning "long double";
  (* Handle long type *)
  if uses_type (Long Signed) || uses_type long_t then
    print_portability_warning_with_recommendation "long" "int64_t or int32_t";
  (* Handle unsigned long type *)
  if uses_type (Long Unsigned) || uses_type ulong_t then
    print_portability_warning_with_recommendation "unsigned long"
      "uint64_t or uint32_t"

let warn_signed_size_or_count_types (fd : func_decl) =
  let print_signedness_warning p =
    printf
      "Warning: Function '%s': Size or count parameter '%s' should not be \
       signed.\n"
      fd.fname p
  in
  (* Get the names of all size and count parameters for the function [fd]. *)
  let size_params =
    filter_map
      (fun (ptype, _) ->
        (* The size may be either a [count] or [size], and then
           either a number or string. We are interested in the
           strings, as they indicate named [size] or [count]
           parameters. *)
        let param_name { ps_size; ps_count } =
          match (ps_size, ps_count) with
          (* [s] is the name of the parameter as a string. *)
          | None, Some (AString s) | Some (AString s), None -> Some s
          (* TODO: Check for [Some (ANumber n)] that [n < 1] *)
          | _ -> None
        in
        (* Only variables that are pointers where [chkptr] is true may
           have size parameters. *)
        match ptype with
        | PTPtr (_, a) when a.pa_chkptr -> param_name a.pa_size
        | _ -> None)
      fd.plist
  in
  (* Print warnings for size parameters that are [Signed]. *)
  List.iter
    (fun (ptype, decl) ->
      let id = decl.identifier in
      if List.mem id size_params then
        match ptype with
        | PTVal (Long s | LLong s) when s = Signed ->
            print_signedness_warning id
        | PTVal (Int i) when i.ia_signedness = Signed ->
            print_signedness_warning id
        | _ -> ())
    fd.plist

let warn_size_and_count_params (fd : func_decl) =
  let print_size_and_count_warning { ps_size; ps_count } =
    match (ps_size, ps_count) with
    | Some (AString p), Some (AString q) ->
        Intel.Util.failwithf
          "Function '%s': simultaneous 'size' and 'count' parameters '%s' and \
           '%s' are not supported by oeedger8r.\n"
          fd.fname p q
    | _ -> ()
  in
  List.iter
    (fun (ptype, _) ->
      match ptype with
      | PTPtr (_, ptr_attr) when ptr_attr.pa_chkptr ->
          print_size_and_count_warning ptr_attr.pa_size
      | _ -> ())
    fd.plist

(** Generate the Enclave code. *)
let gen_enclave_code (ec : enclave_content) (ep : Intel.Util.edger8r_params) =
  (* Short aliases for the trusted and untrusted function
     declarations. *)
  let tfs = ec.tfunc_decls in
  let ufs = ec.ufunc_decls in
  (* Validate Open Enclave supported EDL features. NOTE: This
     validation has the side effects of printed warnings or failure
     with an error message. *)
  if ep.use_prefix then
    Intel.Util.failwithf "--use_prefix option is not supported by oeedger8r.";
  List.iter
    (fun f ->
      if f.tf_is_priv then
        Intel.Util.failwithf
          "Function '%s': 'private' specifier is not supported by oeedger8r"
          f.tf_fdecl.fname;
      if f.tf_is_switchless then
        Intel.Util.failwithf
          "Function '%s': trusted switchless ecalls are not yet supported by \
           Open Enclave SDK."
          f.tf_fdecl.fname)
    tfs;
  List.iter
    (fun f ->
      ( if f.uf_fattr.fa_convention <> CC_NONE then
        let cconv_str = get_call_conv_str f.uf_fattr.fa_convention in
        printf
          "Warning: Function '%s': Calling convention '%s' for ocalls is not \
           supported by oeedger8r.\n"
          f.uf_fdecl.fname cconv_str );
      if f.uf_fattr.fa_dllimport then
        Intel.Util.failwithf
          "Function '%s': dllimport is not supported by oeedger8r."
          f.uf_fdecl.fname;
      if f.uf_allow_list != [] then
        printf
          "Warning: Function '%s': Reentrant ocalls are not supported by Open \
           Enclave. Allow list ignored.\n"
          f.uf_fdecl.fname)
    ufs;
  (* Map warning functions over trusted and untrusted function
     declarations *)
  let ufuncs = List.map (fun f -> f.uf_fdecl) ufs in
  let tfuncs = List.map (fun f -> f.tf_fdecl) tfs in
  let funcs = List.append ufuncs tfuncs in
  List.iter
    (fun f ->
      warn_non_portable_types f;
      warn_signed_size_or_count_types f;
      warn_size_and_count_params f)
    funcs;

  (* End EDL validation. *)
  (* Emit IDs in enum for trusted functions. *)
  let emit_trusted_function_ids =
    [
      "enum";
      "{";
      String.concat "\n"
        (List.mapi
           (fun i f -> sprintf "    %s = %d," (get_function_id ec f.tf_fdecl) i)
           tfs);
      "    " ^ ec.enclave_name ^ "_fcn_id_trusted_call_id_max = OE_ENUM_MAX";
      "};";
    ]
  in
  (* Emit IDs in enum for untrusted functions. *)
  let emit_untrusted_function_ids =
    [
      "enum";
      "{";
      String.concat "\n"
        (List.mapi
           (fun i f -> sprintf "    %s = %d," (get_function_id ec f.uf_fdecl) i)
           ufs);
      "    " ^ ec.enclave_name ^ "_fcn_id_untrusted_call_max = OE_ENUM_MAX";
      "};";
    ]
  in
  (* Generate [args.h] which contains [struct]s for ecalls and ocalls *)
  let oe_gen_args_header =
    let oe_gen_marshal_struct (fd : func_decl) (errno : bool) =
      let gen_member_decl (ptype, decl) =
        let aty = get_param_atype ptype in
        let tystr = get_tystr aty in
        let tystr =
          if is_foreign_array ptype then
            sprintf "/* foreign array of type %s */ void*" tystr
          else tystr
        in
        let need_strlen =
          is_str_or_wstr_ptr (ptype, decl) && is_in_or_inout_ptr (ptype, decl)
        in
        let id = decl.identifier in
        [
          [ tystr ^ " " ^ id ^ ";" ];
          (if need_strlen then [ sprintf "size_t %s_len;" id ] else []);
        ]
        |> List.flatten
      in
      let struct_name = fd.fname ^ "_args_t" in
      let retval_decl = { identifier = "_retval"; array_dims = [] } in
      let members =
        [
          [ "oe_result_t _result;" ];
          ( if fd.rtype = Void then []
          else gen_member_decl (PTVal fd.rtype, retval_decl) );
          (if errno then [ "int _ocall_errno;" ] else []);
          flatten_map gen_member_decl (List.map conv_array_to_ptr fd.plist);
        ]
        |> List.flatten
      in
      [
        "typedef struct _" ^ struct_name;
        "{";
        "    " ^ String.concat "\n    " members;
        "} " ^ struct_name ^ ";";
        "";
      ]
    in
    let oe_gen_user_includes (includes : string list) =
      if includes <> [] then List.map (sprintf "#include \"%s\"") includes
      else [ "/* There were no user includes. */" ]
    in
    let oe_gen_user_types (cts : composite_type list) =
      if cts <> [] then flatten_map emit_composite_type cts
      else [ "/* There were no user defined types. */"; "" ]
    in
    let oe_gen_ecall_marshal_structs =
      if tfs <> [] then
        flatten_map (fun tf -> oe_gen_marshal_struct tf.tf_fdecl false) tfs
      else [ "/* There were no ecalls. */"; "" ]
    in
    let oe_gen_ocall_marshal_structs =
      if ufs <> [] then
        flatten_map
          (fun uf -> oe_gen_marshal_struct uf.uf_fdecl uf.uf_propagate_errno)
          ufs
      else [ "/* There were no ocalls. */"; "" ]
    in
    let with_errno = List.exists (fun uf -> uf.uf_propagate_errno) ufs in
    let guard_macro =
      "EDGER8R_" ^ String.uppercase_ascii ec.enclave_name ^ "_ARGS_H"
    in
    [
      "#ifndef " ^ guard_macro;
      "#define " ^ guard_macro;
      "";
      "#include <stdint.h>";
      "#include <stdlib.h> /* for wchar_t */";
      "";
      (let s = "#include <errno.h>" in
       if with_errno then s
       else
         sprintf "/* %s - Errno propagation not enabled so not included. */" s);
      "";
      "#include <openenclave/bits/result.h>";
      "";
      "/**** User includes. ****/";
      String.concat "\n" (oe_gen_user_includes ec.include_list);
      "";
      "/**** User defined types in EDL. ****/";
      String.concat "\n" (oe_gen_user_types ec.comp_defs);
      "/**** ECALL marshalling structs. ****/";
      String.concat "\n" oe_gen_ecall_marshal_structs;
      "/**** OCALL marshalling structs. ****/";
      String.concat "\n" oe_gen_ocall_marshal_structs;
      "/**** Trusted function IDs ****/";
      String.concat "\n" emit_trusted_function_ids;
      "";
      "/**** Untrusted function IDs. ****/";
      String.concat "\n" emit_untrusted_function_ids;
      "";
      "#endif // " ^ guard_macro;
      "";
    ]
  in
  (* NOTE: The below code encapsulates all our file I/O. *)
  let args_h = ec.file_shortnm ^ "_args.h" in
  if ep.gen_trusted then (
    write_file oe_gen_args_header args_h ep.trusted_dir;
    write_file (Headers.gen_t_h ec ep) (ec.file_shortnm ^ "_t.h") ep.trusted_dir;
    if not ep.header_only then
      write_file (Sources.gen_t_c ec ep) (ec.file_shortnm ^ "_t.c")
        ep.trusted_dir );
  if ep.gen_untrusted then (
    write_file oe_gen_args_header args_h ep.untrusted_dir;
    write_file (Headers.gen_u_h ec ep) (ec.file_shortnm ^ "_u.h")
      ep.untrusted_dir;
    if not ep.header_only then
      write_file (Sources.gen_u_c ec ep) (ec.file_shortnm ^ "_u.c")
        ep.untrusted_dir );
  printf "Success.\n"

(** Install the plugin. *)
let _ =
  Printf.printf "Generating edge routines for the Open Enclave SDK.\n";
  Intel.Plugin.instance.available <- true;
  Intel.Plugin.instance.gen_edge_routines <- gen_enclave_code
