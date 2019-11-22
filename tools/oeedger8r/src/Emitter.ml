(* Copyright (c) Open Enclave SDK contributors.
   Licensed under the MIT License. *)

(** This module is Open Enclave's plugin for Intel's Edger8r, allowing
    us to share the same Enclave Definition Language, but emit our
    SDK's bindings. *)

open Intel.Ast
open Printf
open Common

(** Generate the prototype for a given function. *)
let oe_gen_prototype (fd : func_decl) =
  let plist_str =
    let args = List.map gen_parm_str fd.plist in
    match args with
    | [] -> "void"
    | [ arg ] -> arg
    | _ -> "\n    " ^ String.concat ",\n    " args
  in
  sprintf "%s %s(%s)" (get_tystr fd.rtype) fd.fname plist_str

(** Generate the wrapper prototype for a given function. Optionally
    add an [oe_enclave_t*] first parameter. *)
let oe_gen_wrapper_prototype (fd : func_decl) (is_ecall : bool) =
  let plist_str =
    let args =
      [
        (if is_ecall then [ "oe_enclave_t* enclave" ] else []);
        ( match fd.rtype with
        | Void -> []
        | _ -> [ get_tystr fd.rtype ^ "* _retval" ] );
        List.map gen_parm_str fd.plist;
      ]
      |> List.flatten
    in
    match args with
    | [ arg ] -> arg
    | _ -> "\n    " ^ String.concat ",\n    " args
  in
  sprintf "oe_result_t %s(%s)" fd.fname plist_str

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

(** Generate a cast expression for a pointer argument. Pointer
    arguments need to be cast to their root type, since the marshalling
    struct has the root pointer. For example:
    {[
      int a[10][20]
    ]}
    needs to be cast to [int *].

    NOTE: Foreign arrays are marshalled as [void *], but foreign pointers
    are marshalled as-is. *)
let get_cast_to_mem_expr (ptype, decl) (parens : bool) =
  match ptype with
  | PTVal _ -> ""
  | PTPtr (t, _) ->
      let tystr = get_tystr t in
      if is_array decl then
        let s = tystr ^ "*" in
        if parens then sprintf "(%s)" s else s
      else if is_foreign_array ptype then
        let s = if parens then "(void*)" else "void*" in
        sprintf "/* foreign array of type %s */ %s" tystr s
      else if parens then sprintf "(%s)" tystr
      else tystr

(** Generate a cast expression to a specific pointer type. For example,
    [int*] needs to be cast to
    {[
      *(int ( * )[5][6])
    ]}. *)
let get_cast_from_mem_expr (ptype, decl) =
  match ptype with
  | PTVal _ -> ""
  | PTPtr (t, attr) ->
      if is_array decl then
        sprintf "*(%s(*)%s)" (get_tystr t) (get_array_dims decl.array_dims)
      else if is_foreign_array ptype then
        sprintf "/* foreign array */ *(%s*)" (get_tystr t)
      else if attr.pa_rdonly then
        (* for ptrs, only constness is removed; add it back *)
        sprintf "(const %s)" (get_tystr t)
      else ""

let oe_gen_call_user_function (fd : func_decl) =
  [
    "/* Call user function. */";
    (match fd.rtype with Void -> "" | _ -> "pargs_out->_retval = ")
    ^ fd.fname ^ "(";
    String.concat ",\n    "
      (List.map
         (fun (ptype, decl) ->
           let cast_expr = get_cast_from_mem_expr (ptype, decl) in
           sprintf "    %spargs_in->%s" cast_expr decl.identifier)
         fd.plist)
    ^ ");";
  ]

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
  (* Given [name], return the corresponding [StructDef], or [None]. *)
  let get_struct_by_name name =
    (* [ec.comp_defs] is a list of all composite types, but we're only
       interested in the structs, so we filter out the rest and unwrap
       them from [composite_type]. *)
    let structs =
      filter_map (function StructDef s -> Some s | _ -> None) ec.comp_defs
    in
    List.find_opt (fun s -> s.sname = name) structs
  in
  (* We need to check [Ptr]s for [Foreign] or [Struct] types, then
     check those against the user's [Struct]s, and then check if any
     members should be deep copied. What we return is the list of
     members of the [Struct] which should be deep-copied, otherwise we
     return an empty list. *)
  let get_deepcopy_members (a : atype) =
    let should_deepcopy_a = function
      | Ptr (Struct n) | Ptr (Foreign n) -> get_struct_by_name n
      | _ -> None
    in
    (* Only enabled with --experimental! *)
    if ep.experimental then
      match should_deepcopy_a a with
      | Some s -> List.filter (fun (p, _) -> is_marshalled_ptr p) s.smlist
      | None -> []
    else []
  in
  let get_function_id (f : func_decl) =
    ec.enclave_name ^ "_fcn_id_" ^ f.fname
  in
  (* Emit IDs in enum for trusted functions. *)
  let emit_trusted_function_ids =
    [
      "enum";
      "{";
      String.concat "\n"
        (List.mapi
           (fun i f -> sprintf "    %s = %d," (get_function_id f.tf_fdecl) i)
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
           (fun i f -> sprintf "    %s = %d," (get_function_id f.uf_fdecl) i)
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
  (* Prepare [input_buffer]. *)
  let oe_prepare_input_buffer (fd : func_decl) (alloc_func : string) =
    let oe_compute_buffer_size buffer predicate plist =
      let rec gen_add_size args count (ptype, decl) =
        let argstruct = oe_get_argstruct "_args." args count in
        let size = oe_get_param_size (ptype, decl, argstruct) in
        let arg =
          match args with
          | [] -> decl.identifier
          | hd :: _ ->
              hd ^ gen_c_deref (List.length args) count ^ decl.identifier
        in
        gen_c_for (List.length args) count
          ( [
              [
                sprintf "if (%s)" (String.concat " && " (List.rev (arg :: args)));
              ];
              [ sprintf "    OE_ADD_SIZE(%s, %s);" buffer size ];
              (let param_count = oe_get_param_count (ptype, decl, argstruct) in
               flatten_map
                 (gen_add_size (arg :: args) param_count)
                 (get_deepcopy_members (get_param_atype ptype)));
            ]
          |> List.flatten )
      in
      let params =
        flatten_map (gen_add_size [] "1") (List.filter predicate plist)
      in
      (* Note that the indentation for the first line is applied by the
         parent function. *)
      if params <> [] then String.concat "\n    " params
      else "/* There were no corresponding parameters. */"
    in
    let oe_compute_input_buffer_size =
      oe_compute_buffer_size "_input_buffer_size" is_in_or_inout_ptr
    in
    let oe_compute_output_buffer_size =
      oe_compute_buffer_size "_output_buffer_size" is_out_or_inout_ptr
    in
    let oe_serialize_buffer_inputs (plist : pdecl list) =
      let rec gen_serialize args count (ptype, decl) =
        let argstruct = oe_get_argstruct "_args." args count in
        let size = oe_get_param_size (ptype, decl, argstruct) in
        let arg =
          match args with
          | [] -> decl.identifier
          | hd :: _ ->
              hd ^ gen_c_deref (List.length args) count ^ decl.identifier
        in
        let tystr = get_cast_to_mem_expr (ptype, decl) false in
        (* These need to be in order and so done together. *)
        gen_c_for (List.length args) count
          ( [
              (* NOTE: This makes the embedded check in the `OE_` macro superfluous. *)
              [
                sprintf "if (%s)"
                  (String.concat " && " (List.rev (arg :: args)));
              ];
              [
                (* NOTE: The [WRITE_IN_OUT] macro is defined to be the
                   [WRITE_IN] macro. *)
                sprintf "    OE_WRITE_%s_PARAM(%s, %s, %s);"
                  (if is_in_ptr ptype then "IN" else "IN_OUT")
                  arg size tystr;
              ];
              (let param_count = oe_get_param_count (ptype, decl, argstruct) in
               flatten_map
                 (gen_serialize (arg :: args) param_count)
                 (get_deepcopy_members (get_param_atype ptype)));
            ]
          |> List.flatten )
      in
      let params =
        flatten_map (gen_serialize [] "1")
          (List.filter is_in_or_inout_ptr plist)
      in
      (* Note that the indentation for the first line is applied by the
         parent function. *)
      if params <> [] then String.concat "\n    " params
      else "/* There were no in nor in-out parameters. */"
    in
    [
      "/* Compute input buffer size. Include in and in-out parameters. */";
      sprintf "OE_ADD_SIZE(_input_buffer_size, sizeof(%s_args_t));" fd.fname;
      oe_compute_input_buffer_size fd.plist;
      "";
      "/* Compute output buffer size. Include out and in-out parameters. */";
      sprintf "OE_ADD_SIZE(_output_buffer_size, sizeof(%s_args_t));" fd.fname;
      oe_compute_output_buffer_size fd.plist;
      "";
      "/* Allocate marshalling buffer. */";
      "_total_buffer_size = _input_buffer_size;";
      "OE_ADD_SIZE(_total_buffer_size, _output_buffer_size);";
      sprintf "_buffer = (uint8_t*)%s(_total_buffer_size);" alloc_func;
      "_input_buffer = _buffer;";
      "_output_buffer = _buffer + _input_buffer_size;";
      "if (_buffer == NULL)";
      "{";
      "    _result = OE_OUT_OF_MEMORY;";
      "    goto done;";
      "}";
      "";
      "/* Serialize buffer inputs (in and in-out parameters). */";
      sprintf "_pargs_in = (%s_args_t*)_input_buffer;" fd.fname;
      "OE_ADD_SIZE(_input_buffer_offset, sizeof(*_pargs_in));";
      oe_serialize_buffer_inputs fd.plist;
      "";
      "/* Copy args structure (now filled) to input buffer. */";
      "memcpy(_pargs_in, &_args, sizeof(*_pargs_in));";
    ]
  in
  let gen_times count body =
    (* The first two conditionals check for the multiplicative identity
       and prevent unnecessary expressions from being generated.
       Otherwise we multiply the sum of [body] by [count]. *)
    if count = "1" || body = [] then body
    else if List.length body = 1 && List.hd body = "1" then [ count ]
    else [ count ^ " * (" ^ String.concat " + " body ^ ")" ]
  in
  let rec gen_ptr_count args count (ptype, decl) =
    let id = decl.identifier in
    (* TODO: The use of [gen_c_deref] does not work here as we are not
       within a [gen_c_for] loop when producing the count. Therefore
       arrays of structs which use members for the count of another
       nested parameter are not yet supported. *)
    let argstruct = oe_get_argstruct "" args count in
    let arg =
      match args with
      | [] -> id
      | hd :: _ -> hd ^ gen_c_deref (List.length args) count ^ id
    in
    let param_count = oe_get_param_count (ptype, decl, argstruct) in
    let members = get_deepcopy_members (get_param_atype ptype) in
    if is_marshalled_ptr ptype then
      (* The base case is a marshalled pointer. We count 1 for every
         one of these, except for the top-level pointers as they are
         the original function arguments, and so do not need to be
         saved/restored.

         For a marshalled pointer, we then need to recurse. If there
         are no members to recurse on, then [members] is the empty
         list and the recursion is a no-op, leaving us back at the
         base case of counting 1. If there are members to recurse on,
         then we count 1 plus the current [param_count] times the
         number of members for each nested structure. *)
      (if args <> [] then [ "1" ] else [])
      @ gen_times param_count
          (flatten_map (gen_ptr_count (arg :: args) param_count) members)
    else []
  in
  let gen_ptr_array (plist : pdecl list) =
    let count =
      flatten_map (gen_ptr_count [] "1") (List.filter is_out_or_inout_ptr plist)
    in
    if count <> [] then
      (* TODO: Switch to malloc() to handle variable lengths. *)
      [
        "size_t _ptrs_index = 0;";
        sprintf "void** _ptrs = malloc(sizeof(void*) * (%s));"
          (String.concat " + " count);
        "if (_ptrs == NULL)";
        "{";
        "    _result = OE_OUT_OF_MEMORY;";
        "    goto done;";
        "}";
      ]
    else [ "/* No pointers to save for deep copy. */" ]
  in
  let gen_reset_ptr_index (plist : pdecl list) =
    let count =
      flatten_map (gen_ptr_count [] "1") (List.filter is_out_or_inout_ptr plist)
    in
    if count <> [] then "_ptrs_index = 0; /* For deep copy. */"
    else "/* No pointers to restore for deep copy. */"
  in
  let gen_free_ptrs (plist : pdecl list) =
    let count =
      flatten_map (gen_ptr_count [] "1") (List.filter is_out_or_inout_ptr plist)
    in
    if count <> [] then [ "if (_ptrs)"; "    free(_ptrs);" ]
    else [ "/* No `_ptrs` to free for deep copy. */" ]
  in
  let oe_process_output_buffer (fd : func_decl) =
    let oe_serialize_buffer_outputs (plist : pdecl list) =
      let rec gen_serialize args count (ptype, decl) =
        let argstruct = oe_get_argstruct "_args." args count in
        let size = oe_get_param_size (ptype, decl, argstruct) in
        let arg =
          match args with
          | [] -> decl.identifier
          | hd :: _ ->
              hd ^ gen_c_deref (List.length args) count ^ decl.identifier
        in
        gen_c_for (List.length args) count
          ( [
              ( if is_str_or_wstr_ptr (ptype, decl) then
                [
                  sprintf
                    "OE_CHECK_NULL_TERMINATOR%s(_output_buffer + \
                     _output_buffer_offset, _args.%s_len);"
                    (if is_wstr_ptr ptype then "_WIDE" else "")
                    arg;
                ]
              else [] );
              (let s =
                 sprintf "OE_READ_%s_PARAM(%s, (size_t)(%s));"
                   (if is_out_ptr ptype then "OUT" else "IN_OUT")
                   arg size
               in
               match args with
               | [] -> [ s ]
               | _ ->
                   let tystr = get_cast_to_mem_expr (ptype, decl) true in
                   [
                     sprintf "if (%s)" (String.concat " && " (List.rev args));
                     "{";
                     "    /* Restore original pointer. */";
                     sprintf "    %s = %s_ptrs[_ptrs_index++];" arg tystr;
                     "    " ^ s;
                     "}";
                   ]);
              (let param_count = oe_get_param_count (ptype, decl, argstruct) in
               flatten_map
                 (gen_serialize (arg :: args) param_count)
                 (get_deepcopy_members (get_param_atype ptype)));
            ]
          |> List.flatten )
      in
      let params =
        flatten_map (gen_serialize [] "1")
          (List.filter is_out_or_inout_ptr plist)
      in
      if params <> [] then String.concat "\n    " params
      else "/* There were no out nor in-out parameters. */"
    in
    [
      (* Verify that the ecall succeeded *)
      "/* Setup output arg struct pointer. */";
      sprintf "_pargs_out = (%s_args_t*)_output_buffer;" fd.fname;
      "OE_ADD_SIZE(_output_buffer_offset, sizeof(*_pargs_out));";
      "";
      "/* Check if the call succeeded. */";
      "if ((_result = _pargs_out->_result) != OE_OK)";
      "    goto done;";
      "";
      "/* Currently exactly _output_buffer_size bytes must be written. */";
      "if (_output_bytes_written != _output_buffer_size)";
      "{";
      "    _result = OE_FAILURE;";
      "    goto done;";
      "}";
      "";
      "/* Unmarshal return value and out, in-out parameters. */";
      ( if fd.rtype <> Void then "*_retval = _pargs_out->_retval;"
      else "/* No return value. */" );
      gen_reset_ptr_index fd.plist;
      oe_serialize_buffer_outputs fd.plist;
    ]
  in
  let rec oe_gen_set_pointers args count setter (ptype, decl) =
    let argstruct = oe_get_argstruct "pargs_in->" args count in
    let size = oe_get_param_size (ptype, decl, argstruct) in
    let arg =
      match args with
      | [] -> decl.identifier
      | hd :: _ -> hd ^ gen_c_deref (List.length args) count ^ decl.identifier
    in
    let tystr = get_cast_to_mem_expr (ptype, decl) false in
    gen_c_for (List.length args) count
      ( [
          (* NOTE: This makes the embedded check in the `OE_` macro superfluous. *)
          [
            sprintf "if (pargs_in->%s)"
              (String.concat " && pargs_in->" (List.rev (arg :: args)));
          ];
          [ sprintf "    OE_%s_POINTER(%s, %s, %s);" setter arg size tystr ];
          (let param_count = oe_get_param_count (ptype, decl, argstruct) in
           flatten_map
             (oe_gen_set_pointers (arg :: args) param_count setter)
             (get_deepcopy_members (get_param_atype ptype)));
        ]
      |> List.flatten )
  in
  let oe_gen_in_and_inout_setters (plist : pdecl list) =
    let params =
      let ptrs = List.filter is_in_or_inout_ptr plist in
      let setters =
        List.map
          (fun (p, _) -> if is_in_ptr p then "SET_IN" else "SET_IN_OUT")
          ptrs
      in
      flatten_map2 (oe_gen_set_pointers [] "1") setters ptrs
    in
    "    "
    ^ String.concat "\n    "
        [
          "/* Set in and in-out pointers. */";
          ( if params <> [] then String.concat "\n    " params
          else "/* There were no in nor in-out parameters. */" );
        ]
  in
  let oe_gen_out_and_inout_setters (plist : pdecl list) =
    let params =
      let ptrs = List.filter is_out_or_inout_ptr plist in
      let setters =
        List.map
          (fun (p, _) ->
            if is_out_ptr p then "SET_OUT" else "COPY_AND_SET_IN_OUT")
          ptrs
      in
      flatten_map2 (oe_gen_set_pointers [] "1") setters ptrs
    in
    "    "
    ^ String.concat "\n    "
        [
          "/* Set out and in-out pointers. */";
          "/* In-out parameters are copied to output buffer. */";
          ( if params <> [] then String.concat "\n    " params
          else "/* There were no out nor in-out parameters. */" );
        ]
  in
  (* Generate ecall function. *)
  let oe_gen_ecall_function (tf : trusted_func) =
    let fd = tf.tf_fdecl in
    [
      sprintf "void ecall_%s(" fd.fname;
      "    uint8_t* input_buffer,";
      "    size_t input_buffer_size,";
      "    uint8_t* output_buffer,";
      "    size_t output_buffer_size,";
      "    size_t* output_bytes_written)";
      "{";
      (* Variable declarations *)
      "    oe_result_t _result = OE_FAILURE;";
      "";
      "    /* Prepare parameters. */";
      sprintf "    %s_args_t* pargs_in = (%s_args_t*)input_buffer;" fd.fname
        fd.fname;
      sprintf "    %s_args_t* pargs_out = (%s_args_t*)output_buffer;" fd.fname
        fd.fname;
      "";
      "    size_t input_buffer_offset = 0;";
      "    size_t output_buffer_offset = 0;";
      "    OE_ADD_SIZE(input_buffer_offset, sizeof(*pargs_in));";
      "    OE_ADD_SIZE(output_buffer_offset, sizeof(*pargs_out));";
      "";
      (* Buffer validation *)
      "    /* Make sure input and output buffers lie within the enclave. */";
      "    if (!input_buffer || !oe_is_within_enclave(input_buffer, \
       input_buffer_size))";
      "        goto done;";
      "";
      "    if (!output_buffer || !oe_is_within_enclave(output_buffer, \
       output_buffer_size))";
      "        goto done;";
      "";
      (* Prepare in and in-out parameters *)
      oe_gen_in_and_inout_setters fd.plist;
      "";
      (* Prepare out and in-out parameters. The in-out parameter is
         copied to output buffer. *)
      oe_gen_out_and_inout_setters fd.plist;
      "";
      "    /* Check that in/in-out strings are null terminated. */"
      (* NOTE: We do not support deep copy for strings, so there is not
         (yet) anything to do here. *);
      (let params =
         List.map
           (fun (ptype, decl) ->
             sprintf
               "    OE_CHECK_NULL_TERMINATOR%s(pargs_in->%s, pargs_in->%s_len);"
               (if is_wstr_ptr ptype then "_WIDE" else "")
               decl.identifier decl.identifier)
           (List.filter
              (fun p -> is_str_or_wstr_ptr p && is_in_or_inout_ptr p)
              fd.plist)
       in
       if params <> [] then String.concat "\n" params
       else "    /* There were no in nor in-out string parameters. */");
      "";
      "    /* lfence after checks. */";
      "    oe_lfence();";
      "";
      (* Call the enclave function *)
      "    " ^ String.concat "\n    " (oe_gen_call_user_function fd);
      "";
      (* Mark call as success *)
      "    /* Success. */";
      "    _result = OE_OK;";
      "    *output_bytes_written = output_buffer_offset;";
      "";
      "done:";
      "    if (pargs_out && output_buffer_size >= sizeof(*pargs_out))";
      "        pargs_out->_result = _result;";
      "}";
      "";
    ]
  in
  let gen_fill_marshal_struct (fd : func_decl) =
    (* Generate assignment argument to corresponding field in args. This
       is necessary for all arguments, not just copy-as-value, because
       they are used directly by later marshalling code. *)
    let gen_assignment (ptype, decl) =
      let arg = decl.identifier in
      [
        [
          sprintf "_args.%s = %s%s;" arg
            (get_cast_to_mem_expr (ptype, decl) true)
            arg;
        ];
        (* for string parameter fill the len field *)
        ( if is_str_ptr ptype then
          [ sprintf "_args.%s_len = (%s) ? (strlen(%s) + 1) : 0;" arg arg arg ]
        else if is_wstr_ptr ptype then
          [ sprintf "_args.%s_len = (%s) ? (wcslen(%s) + 1) : 0;" arg arg arg ]
        else [] );
      ]
      |> List.flatten
    in
    flatten_map gen_assignment fd.plist
    @
    let rec gen_save_ptrs args count (ptype, decl) =
      let id = decl.identifier in
      let argstruct = oe_get_argstruct "_args." args count in
      let arg =
        match args with
        | [] -> id
        | hd :: _ -> hd ^ gen_c_deref (List.length args) count ^ id
      in
      gen_c_for (List.length args) count
        ( [
            ( if args <> [] then
              [ sprintf "if (%s)" (String.concat " && " (List.rev args)) ]
            else [] );
            ( if args <> [] && is_marshalled_ptr ptype then
              [ "    _ptrs[_ptrs_index++] = (void*)" ^ arg ^ ";" ]
            else [] );
            (let param_count = oe_get_param_count (ptype, decl, argstruct) in
             flatten_map
               (gen_save_ptrs (arg :: args) param_count)
               (get_deepcopy_members (get_param_atype ptype)));
          ]
        |> List.flatten )
    in
    flatten_map (gen_save_ptrs [] "1")
      (List.filter is_out_or_inout_ptr fd.plist)
  in
  (* Generate host ECALL wrapper function. *)
  let oe_gen_host_ecall_wrapper (tf : trusted_func) =
    let fd = tf.tf_fdecl in
    let oe_ecall_function =
      if tf.tf_is_switchless then "oe_switchless_call_enclave_function"
      else "oe_call_enclave_function"
    in
    [
      oe_gen_wrapper_prototype fd true;
      "{";
      "    oe_result_t _result = OE_FAILURE;";
      "";
      "    /* Marshalling struct. */";
      sprintf "    %s_args_t _args, *_pargs_in = NULL, *_pargs_out = NULL;"
        fd.fname;
      "";
      "    /* Marshalling buffer and sizes. */";
      "    size_t _input_buffer_size = 0;";
      "    size_t _output_buffer_size = 0;";
      "    size_t _total_buffer_size = 0;";
      "    uint8_t* _buffer = NULL;";
      "    uint8_t* _input_buffer = NULL;";
      "    uint8_t* _output_buffer = NULL;";
      "    size_t _input_buffer_offset = 0;";
      "    size_t _output_buffer_offset = 0;";
      "    size_t _output_bytes_written = 0;";
      "";
      "    /* Deep copy buffer. */";
      "    " ^ String.concat "\n    " (gen_ptr_array fd.plist);
      "";
      "    /* Fill marshalling struct. */";
      "    memset(&_args, 0, sizeof(_args));";
      "    " ^ String.concat "\n    " (gen_fill_marshal_struct fd);
      "";
      "    " ^ String.concat "\n    " (oe_prepare_input_buffer fd "malloc");
      "";
      "    /* Call enclave function. */";
      "    if ((_result = " ^ oe_ecall_function ^ "(";
      "             "
      ^ String.concat ",\n             "
          [
            "enclave";
            get_function_id fd;
            "_input_buffer";
            "_input_buffer_size";
            "_output_buffer";
            "_output_buffer_size";
            "&_output_bytes_written)) != OE_OK)";
          ];
      "        goto done;";
      "";
      "    " ^ String.concat "\n    " (oe_process_output_buffer fd);
      "";
      "    _result = OE_OK;";
      "";
      "done:";
      "    if (_buffer)";
      "        free(_buffer);";
      "";
      "    " ^ String.concat "\n    " (gen_free_ptrs fd.plist);
      "";
      "    return _result;";
      "}";
      "";
    ]
  in
  (* Generate enclave OCALL wrapper function. *)
  let oe_gen_enclave_ocall_wrapper (uf : untrusted_func) =
    let fd = uf.uf_fdecl in
    let allocate_buffer, call_function, free_buffer =
      if uf.uf_is_switchless then
        ( "oe_allocate_switchless_ocall_buffer",
          "oe_switchless_call_host_function",
          "oe_free_switchless_ocall_buffer" )
      else
        ( "oe_allocate_ocall_buffer",
          "oe_call_host_function",
          "oe_free_ocall_buffer" )
    in
    [
      oe_gen_wrapper_prototype fd false;
      "{";
      "    oe_result_t _result = OE_FAILURE;";
      "";
      "    /* If the enclave is in crashing/crashed status, new OCALL should \
       fail";
      "       immediately. */";
      "    if (oe_get_enclave_status() != OE_OK)";
      "        return oe_get_enclave_status();";
      "";
      "    /* Marshalling struct. */";
      sprintf "    %s_args_t _args, *_pargs_in = NULL, *_pargs_out = NULL;"
        fd.fname;
      "    " ^ String.concat "\n    " (gen_ptr_array fd.plist);
      "";
      "    /* Marshalling buffer and sizes. */";
      "    size_t _input_buffer_size = 0;";
      "    size_t _output_buffer_size = 0;";
      "    size_t _total_buffer_size = 0;";
      "    uint8_t* _buffer = NULL;";
      "    uint8_t* _input_buffer = NULL;";
      "    uint8_t* _output_buffer = NULL;";
      "    size_t _input_buffer_offset = 0;";
      "    size_t _output_buffer_offset = 0;";
      "    size_t _output_bytes_written = 0;";
      "";
      "    /* Fill marshalling struct. */";
      "    memset(&_args, 0, sizeof(_args));";
      "    " ^ String.concat "\n    " (gen_fill_marshal_struct fd);
      "";
      "    "
      ^ String.concat "\n    " (oe_prepare_input_buffer fd allocate_buffer);
      "";
      "    /* Call host function. */";
      "    if ((_result = " ^ call_function ^ "(";
      "             "
      ^ String.concat ",\n             "
          [
            get_function_id fd;
            "_input_buffer";
            "_input_buffer_size";
            "_output_buffer";
            "_output_buffer_size";
            "&_output_bytes_written)) != OE_OK)";
          ];
      "        goto done;";
      "";
      "    " ^ String.concat "\n    " (oe_process_output_buffer fd);
      "";
      "    /* Retrieve propagated errno from OCALL. */";
      ( if uf.uf_propagate_errno then "    errno = _pargs_out->_ocall_errno;\n"
      else sprintf "    /* Errno propagation not enabled. */" );
      "";
      "    _result = OE_OK;";
      "";
      "done:";
      "    if (_buffer)";
      "        " ^ free_buffer ^ "(_buffer);";
      "    return _result;";
      "}";
      "";
    ]
  in
  (* Generate ocall function. *)
  let oe_gen_ocall_function (uf : untrusted_func) =
    let fd = uf.uf_fdecl in
    [
      sprintf "void ocall_%s(" fd.fname;
      "    uint8_t* input_buffer,";
      "    size_t input_buffer_size,";
      "    uint8_t* output_buffer,";
      "    size_t output_buffer_size,";
      "    size_t* output_bytes_written)";
      "{";
      (* Variable declarations *)
      "    oe_result_t _result = OE_FAILURE;";
      "    OE_UNUSED(input_buffer_size);";
      "";
      "    /* Prepare parameters. */";
      sprintf "    %s_args_t* pargs_in = (%s_args_t*)input_buffer;" fd.fname
        fd.fname;
      sprintf "    %s_args_t* pargs_out = (%s_args_t*)output_buffer;" fd.fname
        fd.fname;
      "";
      "    size_t input_buffer_offset = 0;";
      "    size_t output_buffer_offset = 0;";
      "    OE_ADD_SIZE(input_buffer_offset, sizeof(*pargs_in));";
      "    OE_ADD_SIZE(output_buffer_offset, sizeof(*pargs_out));";
      "";
      (* Buffer validation *)
      "    /* Make sure input and output buffers are valid. */";
      "    if (!input_buffer || !output_buffer) {";
      "        _result = OE_INVALID_PARAMETER;";
      "        goto done;";
      "    }";
      "";
      (* Prepare in and in-out parameters *)
      oe_gen_in_and_inout_setters fd.plist;
      "";
      (* Prepare out and in-out parameters. The in-out parameter is copied to output buffer. *)
      oe_gen_out_and_inout_setters fd.plist;
      "";
      (* Call the host function *)
      "    " ^ String.concat "\n    " (oe_gen_call_user_function fd);
      "";
      "    /* Propagate errno back to enclave. */";
      ( if uf.uf_propagate_errno then "    pargs_out->_ocall_errno = errno;"
      else "    /* Errno propagation not enabled. */" );
      "";
      (* Mark call as success *)
      "    /* Success. */";
      "    _result = OE_OK;";
      "    *output_bytes_written = output_buffer_offset;";
      "";
      "done:";
      "    if (pargs_out && output_buffer_size >= sizeof(*pargs_out))";
      "        pargs_out->_result = _result;";
      "}";
      "";
    ]
  in
  (* Includes are emitted in [args.h]. Imported functions have already
     been brought into function lists. *)
  let gen_t_h =
    let oe_gen_tfunc_prototypes =
      if tfs <> [] then
        List.map (fun f -> sprintf "%s;" (oe_gen_prototype f.tf_fdecl)) tfs
      else [ "/* There were no ecalls. */" ]
    in
    let oe_gen_ufunc_wrapper_prototypes =
      if ufs <> [] then
        List.map
          (fun f -> sprintf "%s;" (oe_gen_wrapper_prototype f.uf_fdecl false))
          ufs
      else [ "/* There were no ocalls. */" ]
    in
    let guard = "EDGER8R_" ^ String.uppercase_ascii ec.file_shortnm ^ "_T_H" in
    [
      "#ifndef " ^ guard;
      "#define " ^ guard;
      "";
      "#include <openenclave/enclave.h>";
      "";
      sprintf "#include \"%s_args.h\"" ec.file_shortnm;
      "";
      "OE_EXTERNC_BEGIN";
      "";
      "/**** ECALL prototypes. ****/";
      String.concat "\n\n" oe_gen_tfunc_prototypes;
      "";
      "/**** OCALL prototypes. ****/";
      String.concat "\n\n" oe_gen_ufunc_wrapper_prototypes;
      "";
      "OE_EXTERNC_END";
      "";
      "#endif // " ^ guard;
      "";
    ]
  in
  let gen_t_c =
    let oe_gen_ecall_functions =
      if tfs <> [] then flatten_map oe_gen_ecall_function tfs
      else [ "/* There were no ecalls. */" ]
    in
    let oe_gen_ecall_table =
      let table = "__oe_ecalls_table" in
      if tfs <> [] then
        [
          sprintf "oe_ecall_func_t %s[] = {" table;
          "    "
          ^ String.concat ",\n    "
              (List.map
                 (fun f -> "(oe_ecall_func_t) ecall_" ^ f.tf_fdecl.fname)
                 tfs);
          "};";
          "";
          sprintf "size_t %s_size = OE_COUNTOF(%s);" table table;
        ]
      else [ "/* There were no ecalls. */" ]
    in
    let oe_gen_enclave_ocall_wrappers =
      if ufs <> [] then flatten_map oe_gen_enclave_ocall_wrapper ufs
      else [ "/* There were no ocalls. */" ]
    in
    [
      sprintf "#include \"%s_t.h\"" ec.file_shortnm;
      "";
      "#include <openenclave/edger8r/enclave.h>";
      "";
      "#include <stdlib.h>";
      "#include <string.h>";
      "#include <wchar.h>";
      "";
      "OE_EXTERNC_BEGIN";
      "";
      "/**** ECALL functions. ****/";
      "";
      String.concat "\n" oe_gen_ecall_functions;
      "/**** ECALL function table. ****/";
      "";
      String.concat "\n" oe_gen_ecall_table;
      "";
      "/**** OCALL function wrappers. ****/";
      "";
      String.concat "\n" oe_gen_enclave_ocall_wrappers;
      "OE_EXTERNC_END";
      "";
    ]
  in
  let gen_u_h =
    let oe_gen_tfunc_wrapper_prototypes =
      if tfs <> [] then
        List.map (fun f -> oe_gen_wrapper_prototype f.tf_fdecl true ^ ";") tfs
      else [ "/* There were no ecalls. */" ]
    in
    let oe_gen_ufunc_prototypes =
      if ufs <> [] then
        List.map (fun f -> oe_gen_prototype f.uf_fdecl ^ ";") ufs
      else [ "/* There were no ocalls. */" ]
    in
    let guard = "EDGER8R_" ^ String.uppercase_ascii ec.file_shortnm ^ "_U_H" in
    [
      "#ifndef " ^ guard;
      "#define " ^ guard;
      "";
      "#include <openenclave/host.h>";
      "";
      sprintf "#include \"%s_args.h\"" ec.file_shortnm;
      "";
      "OE_EXTERNC_BEGIN";
      "";
      sprintf "oe_result_t oe_create_%s_enclave(" ec.enclave_name;
      "    const char* path,";
      "    oe_enclave_type_t type,";
      "    uint32_t flags,";
      "    const oe_enclave_setting_t* settings,";
      "    uint32_t setting_count,";
      "    oe_enclave_t** enclave);";
      "";
      "/**** ECALL prototypes. ****/";
      String.concat "\n\n" oe_gen_tfunc_wrapper_prototypes;
      "";
      "/**** OCALL prototypes. ****/";
      String.concat "\n\n" oe_gen_ufunc_prototypes;
      "";
      "OE_EXTERNC_END";
      "";
      "#endif // " ^ guard;
      "";
    ]
  in
  let gen_u_c =
    let oe_gen_host_ecall_wrappers =
      if tfs <> [] then flatten_map oe_gen_host_ecall_wrapper tfs
      else [ "/* There were no ecalls. */" ]
    in
    let oe_gen_ocall_functions =
      if ufs <> [] then flatten_map oe_gen_ocall_function ufs
      else [ "/* There were no ocalls. */" ]
    in
    let oe_gen_ocall_table =
      [
        sprintf "static oe_ocall_func_t __%s_ocall_function_table[] = {"
          ec.enclave_name;
        "    "
        ^ String.concat "\n    "
            (List.map
               (fun f -> "(oe_ocall_func_t) ocall_" ^ f.uf_fdecl.fname ^ ",")
               ufs);
        "    NULL";
        "};";
      ]
    in
    [
      sprintf "#include \"%s_u.h\"" ec.file_shortnm;
      "";
      "#include <openenclave/edger8r/host.h>";
      "";
      "#include <stdlib.h>";
      "#include <string.h>";
      "#include <wchar.h>";
      "";
      "OE_EXTERNC_BEGIN";
      "";
      "/**** ECALL function wrappers. ****/";
      "";
      String.concat "\n" oe_gen_host_ecall_wrappers;
      "/**** OCALL functions. ****/";
      "";
      String.concat "\n" oe_gen_ocall_functions;
      "/**** OCALL function table. ****/";
      "";
      String.concat "\n" oe_gen_ocall_table;
      "";
      sprintf "oe_result_t oe_create_%s_enclave(" ec.enclave_name;
      "    const char* path,";
      "    oe_enclave_type_t type,";
      "    uint32_t flags,";
      "    const oe_enclave_setting_t* settings,";
      "    uint32_t setting_count,";
      "    oe_enclave_t** enclave)";
      "{";
      "    return oe_create_enclave(";
      "               path,";
      "               type,";
      "               flags,";
      "               settings,";
      "               setting_count,";
      sprintf "               __%s_ocall_function_table," ec.enclave_name;
      sprintf "               %d," (List.length ufs);
      "               enclave);";
      "}";
      "";
      "OE_EXTERNC_END";
      "";
    ]
  in
  (* NOTE: The below code encapsulates all our file I/O. *)
  let args_h = ec.file_shortnm ^ "_args.h" in
  if ep.gen_trusted then (
    write_file oe_gen_args_header args_h ep.trusted_dir;
    write_file gen_t_h (ec.file_shortnm ^ "_t.h") ep.trusted_dir;
    if not ep.header_only then
      write_file gen_t_c (ec.file_shortnm ^ "_t.c") ep.trusted_dir );
  if ep.gen_untrusted then (
    write_file oe_gen_args_header args_h ep.untrusted_dir;
    write_file gen_u_h (ec.file_shortnm ^ "_u.h") ep.untrusted_dir;
    if not ep.header_only then
      write_file gen_u_c (ec.file_shortnm ^ "_u.c") ep.untrusted_dir );
  printf "Success.\n"

(** Install the plugin. *)
let _ =
  Printf.printf "Generating edge routines for the Open Enclave SDK.\n";
  Intel.Plugin.instance.available <- true;
  Intel.Plugin.instance.gen_edge_routines <- gen_enclave_code
