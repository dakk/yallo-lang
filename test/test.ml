open Yallo
open Helpers.Errors

let opt = Compiler.{
  contract = None;
  target = None;
  print_pt = false;
  print_ast = false;
  print_ligo = false;
  verbose = false;
  no_remove_unused = true;
}

let optc = { opt with no_remove_unused = false }

let optl = { opt with target=Some("ligo") }
let optlc = { optc with target=Some("ligo") }

let optt = { opt with target=Some("tz") }
let opttc = { optc with target=Some("tz") }

let compile opt exc path cname _ = 
  let compile_failure = try (Compiler.compile path { opt with contract=cname }; None) with 
  | f -> Some(f) in
  (match compile_failure, exc with 
  | None, None -> ()
  | Some(SyntaxError(_,_)), Some(SyntaxError(_,_)) -> ()
  | Some(APIError(_,_)), Some(APIError(_,_)) -> ()
  | Some(TypeError(_,_)), Some(TypeError(_,_)) -> ()
  | Some(DuplicateSymbolError(_,_)), Some(DuplicateSymbolError(_,_)) -> ()
  | Some(DeclarationError(_,_)), Some(DeclarationError(_,_)) -> ()
  | Some(SymbolNotFound(_,_)), Some(SymbolNotFound(_,_)) -> ()
  | Some(f), _ -> failwith @@ "Invalid" ^ Printexc.to_string f
  | None, Some(e) -> failwith @@ "Expected an exception, none catched: " ^ Printexc.to_string e)




let () =
  Alcotest.run "yallo" [
    "compiler", [
      "pragma", `Quick, compile opt None "test/compiler/pragma.yallo" None;
      "comments", `Quick, compile opt None "test/compiler/comments.yallo" None;
    ];
    "type", [
      "types", `Quick, compile opt None "test/type/types.yallo" None;
      "list_methods", `Quick, compile opttc None "test/type/list_methods.yallo" None;
      "option_methods", `Quick, compile opt None "test/type/option_methods.yallo" None;
    ];
    "expression", [
      "pack_unpack", `Quick, compile opt None "test/expr/pack_unpack.yallo" None;
      "list_bfun", `Quick, compile opt None "test/expr/list_bfun.yallo" None;
      "set_bfun", `Quick, compile opt None "test/expr/set_bfun.yallo" None;
      "timestamp_duration", `Quick, compile opt None "test/expr/timestamp_duration.yallo" None;
      "neg_fail", `Quick, compile opt (Some(APIError(None, ""))) "test/expr/neg_fail.yallo" None;
      "record", `Quick, compile opt None "test/expr/record.yallo" None;
      "literal", `Quick, compile opt None "test/expr/literal.yallo" None;
      "literal_untyped", `Quick, compile opt None "test/expr/literal_untyped.yallo" None;
      "literal_fail_infer", `Quick, compile opt (Some(TypeError(None, ""))) "test/expr/literal_fail_infer.yallo" None;
      "lambda", `Quick, compile opt None "test/expr/lambda.yallo" None;
      "assoc_bool", `Quick, compile opttc None "test/expr/assoc_bool.yallo" None;
      "match_case", `Quick, compile opt None "test/expr/match_case.yallo" None;
      "tuple_destruct_typed", `Quick, compile opt None "test/expr/tuple_destruct_typed.yallo" None;
      "tuple_destruct_untyped", `Quick, compile opt None "test/expr/tuple_destruct_untyped.yallo" None;
    ];
    "constants", [
      "cont", `Quick, compile opt None "test/const/cont.yallo" None;
      "numeric", `Quick, compile opt None "test/const/numeric.yallo" None;
      "string", `Quick, compile opt None "test/const/string.yallo" None;
      "lambda", `Quick, compile opt None "test/const/lambda.yallo" None;
      "lambda_fail", `Quick, compile opt (Some(TypeError(None, ""))) "test/const/lambda_fail.yallo" None;
      "expr", `Quick, compile opt None "test/const/expr.yallo" None;
      "let_expr", `Quick, compile opt None "test/const/let_expr.yallo" None;
      "enum", `Quick, compile opt None "test/const/enum.yallo" None;
      "crypto_f", `Quick, compile opt None "test/const/crypto_f.yallo" None;
      "infer", `Quick, compile opt None "test/const/infer.yallo" None;
      "let_infer", `Quick, compile opt None "test/const/let_infer.yallo" None;
    ];
    "import", [
      "interface", `Quick, compile opt None "test/import/interface.yallo" None;
      "type", `Quick, compile opt None "test/import/type.yallo" None;
    ];
    "interface", [
      "i1", `Quick, compile opt None "test/interface/i1.yallo" None;
      "view", `Quick, compile opt None "test/interface/view.yallo" None;
      "extend", `Quick, compile opt None "test/interface/extend.yallo" None;
      "empty", `Quick, compile opt None "test/interface/empty.yallo" None;
      "duplicate_fail", `Quick, compile opt (Some(DuplicateSymbolError(None, ""))) "test/interface/duplicate_fail.yallo" None;
      "dup_entry_fail", `Quick, compile opt (Some(DeclarationError(None, ""))) "test/interface/dup_entry_fail.yallo" None;
    ];
    "function", [
      "wrong_return_type", `Quick, compile opt (Some(TypeError(None, ""))) "test/function/wrong_return_type.yallo" None;
      "base_fun", `Quick, compile opt None "test/function/base_fun.yallo" None;
    ];
    "contract", [
      "coq_test", `Quick, compile optt None "test/contract/coq_test.yallo" None;
      "assert_test", `Quick, compile optt None "test/contract/assert_test.yallo" None;
      "view", `Quick, compile optt None "test/contract/view.yallo" None;
      "itoken", `Quick, compile opt None "test/contract/itoken.yallo" None;
      "king_of_tezos", `Quick, compile optt None "test/contract/king_of_tezos.yallo" None;
      "king_of_tezos_lambdalized", `Quick, compile optt None "test/contract/king_of_tezos_lambdalized.yallo" None;
      "loan", `Quick, compile optt None "test/contract/loan.yallo" None;
      "token", `Quick, compile optt None "test/contract/token.yallo" None;
      "token_with_view", `Quick, compile optt None "test/contract/token_with_view.yallo" None;
      "token_create", `Quick, compile optlc None "test/contract/token_create.yallo" None;
      "token_using", `Quick, compile optt None "test/contract/token_using.yallo" (Some("usingAToken"));
      "field_record", `Quick, compile optt None "test/contract/field_record.yallo" None;
      "field_list", `Quick, compile optt None "test/contract/field_list.yallo" None;
      "field_enum", `Quick, compile optt None "test/contract/field_enum.yallo" None;
      "field_lambda_map", `Quick, compile optt None "test/contract/field_lambda_map.yallo" None;
      "field_lambda", `Quick, compile optt None "test/contract/field_lambda.yallo" None;
      "field_unbound", `Quick, compile optt (Some(SymbolNotFound(None, ""))) "test/contract/field_unbound.yallo" None;
      "ctor_incomplete_fail", `Quick, compile optt (Some(DeclarationError(None, ""))) "test/contract/ctor_incomplete_fail.yallo" None;
      "ctor_ok", `Quick, compile optt None "test/contract/ctor_ok.yallo" None;
    ]
  ]