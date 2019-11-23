(* Copyright (c) Open Enclave SDK contributors.
   Licensed under the MIT License. *)

open Intel.Ast
open Common
open Printf

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

(* Includes are emitted in [args.h]. Imported functions have already
     been brought into function lists. *)
let gen_t_h (ec : enclave_content) (ep : Intel.Util.edger8r_params) =
  let tfs = ec.tfunc_decls in
  let ufs = ec.ufunc_decls in
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

let gen_u_h (ec : enclave_content) (ep : Intel.Util.edger8r_params) =
  let tfs = ec.tfunc_decls in
  let ufs = ec.ufunc_decls in
  let oe_gen_tfunc_wrapper_prototypes =
    if tfs <> [] then
      List.map (fun f -> oe_gen_wrapper_prototype f.tf_fdecl true ^ ";") tfs
    else [ "/* There were no ecalls. */" ]
  in
  let oe_gen_ufunc_prototypes =
    if ufs <> [] then List.map (fun f -> oe_gen_prototype f.uf_fdecl ^ ";") ufs
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
