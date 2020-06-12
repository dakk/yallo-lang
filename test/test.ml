open Yallo

let opt = Compiler.{
  contract = None;
  print_pt = false;
  print_ast = false;
  verbose = false;
}

let compile success path cname octx = 
  let compiled = try (Compiler.compile path { opt with contract=cname }; true) with 
  | _ -> false in

  (match compiled, success with 
  | true, true -> ()
  | false, false -> ()
  | _, _ -> failwith "Invalid")



let test_random = [
  "declarations", `Quick, compile true "test/random/declarations.yallo" None;
  "expr", `Quick, compile true "test/random/expr.yallo" None;
  "literal", `Quick, compile true "test/random/literal.yallo" None;
  "record", `Quick, compile true "test/random/record.yallo" None;
  "statements", `Quick, compile true "test/random/statements.yallo" None;
  "typemod", `Quick, compile true "test/random/typemod.yallo" None;
  "types", `Quick, compile true "test/random/types.yallo" None;
]

let () =
  Alcotest.run "yallo" [
    "random", test_random;
  ]