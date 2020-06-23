open Yallo
open Helpers.Errors

let opt = Compiler.{
  contract = None;
  target = None;
  print_pt = false;
  print_ast = false;
  verbose = false;
  no_remove_unused = true;
}

let compile exc path cname _ = 
  let compile_failure = try (Compiler.compile path { opt with contract=cname }; None) with 
  | f -> Some(f) in

  (match compile_failure, exc with 
  | None, None -> ()
  | Some(SyntaxError(_,_)), Some(SyntaxError(_,_)) -> ()
  | Some(TypeError(_,_)), Some(TypeError(_,_)) -> ()
  | Some(DuplicateSymbolError(_,_)), Some(DuplicateSymbolError(_,_)) -> ()
  | Some(DeclarationError(_,_)), Some(DeclarationError(_,_)) -> ()
  | Some(SymbolNotFound(_,_)), Some(SymbolNotFound(_,_)) -> ()
  | Some(f), _ -> failwith @@ "Invalid" ^ Printexc.to_string f
  | None, Some(e) -> failwith @@ "Expected an exception, none catched: " ^ Printexc.to_string e)




let () =
  Alcotest.run "yallo" [
    "compiler", [
      "pragma", `Quick, compile None "test/compiler/pragma.yallo" None;
    ];
    "type", [
      "types", `Quick, compile None "test/type/types.yallo" None;
      "list_methods", `Quick, compile None "test/type/list_methods.yallo" None;
      "option_methods", `Quick, compile None "test/type/option_methods.yallo" None;
    ];
    "expression", [
      "record", `Quick, compile None "test/expr/record.yallo" None;
      "literal", `Quick, compile None "test/expr/literal.yallo" None;
      "literal_untyped", `Quick, compile None "test/expr/literal_untyped.yallo" None;
      "literal_fail_infer", `Quick, compile (Some(TypeError(None, ""))) "test/expr/literal_fail_infer.yallo" None;
      "lambda", `Quick, compile None "test/expr/lambda.yallo" None;
      "assoc_bool", `Quick, compile None "test/expr/assoc_bool.yallo" None;
      "match_case", `Quick, compile None "test/expr/match_case.yallo" None;
      "tuple_destruct_typed", `Quick, compile None "test/expr/tuple_destruct_typed.yallo" None;
      "tuple_destruct_untyped", `Quick, compile None "test/expr/tuple_destruct_untyped.yallo" None;
    ];
    "constants", [
      "cont", `Quick, compile None "test/const/cont.yallo" None;
      "numeric", `Quick, compile None "test/const/numeric.yallo" None;
      "string", `Quick, compile None "test/const/string.yallo" None;
      "lambda", `Quick, compile None "test/const/lambda.yallo" None;
      "lambda_fail", `Quick, compile (Some(TypeError(None, ""))) "test/const/lambda_fail.yallo" None;
      "expr", `Quick, compile None "test/const/expr.yallo" None;
      "let_expr", `Quick, compile None "test/const/let_expr.yallo" None;
      "enum", `Quick, compile None "test/const/enum.yallo" None;
      "crypto_f", `Quick, compile None "test/const/crypto_f.yallo" None;
      "infer", `Quick, compile None "test/const/infer.yallo" None;
      "let_infer", `Quick, compile None "test/const/let_infer.yallo" None;
    ];
    "import", [
      "interface", `Quick, compile None "test/import/interface.yallo" None;
      "type", `Quick, compile None "test/import/type.yallo" None;
    ];
    "interface", [
      "i1", `Quick, compile None "test/interface/i1.yallo" None;
      "extend", `Quick, compile None "test/interface/extend.yallo" None;
      "empty", `Quick, compile None "test/interface/empty.yallo" None;
      "duplicate_fail", `Quick, compile (Some(DuplicateSymbolError(None, ""))) "test/interface/duplicate_fail.yallo" None;
      "dup_entry_fail", `Quick, compile (Some(DeclarationError(None, ""))) "test/interface/dup_entry_fail.yallo" None;
    ];
    "function", [
      "wrong_return_type", `Quick, compile (Some(TypeError(None, ""))) "test/function/wrong_return_type.yallo" None;
      "base_fun", `Quick, compile None "test/function/base_fun.yallo" None;
    ];
    "contract", [
      "itoken", `Quick, compile None "test/contract/itoken.yallo" None;
      "loan", `Quick, compile None "test/contract/loan.yallo" None;
      "token", `Quick, compile None "test/contract/token.yallo" None;
      "token_create", `Quick, compile None "test/contract/token_create.yallo" None;
      "token_using", `Quick, compile None "test/contract/token_using.yallo" None;
      "crec", `Quick, compile None "test/contract/crec.yallo" None;
      "cenum", `Quick, compile None "test/contract/cenum.yallo" None;
      "unbound_field", `Quick, compile (Some(SymbolNotFound(None, ""))) "test/contract/unbound_field.yallo" None;
      "ctor_fail", `Quick, compile (Some(DeclarationError(None, ""))) "test/contract/ctor_fail.yallo" None;
    ]
  ]