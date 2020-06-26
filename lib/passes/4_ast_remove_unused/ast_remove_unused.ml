(* remove unused code / symbol / contract / interfaces *)

let remove_unused ctr (ast: Ast.t) = 
  let ast = Unused_contract.remove_unused ctr ast in 
  let ast = Unused_const.remove_unused ctr ast in 
  let ast = Unused_field.remove_unused ctr ast in 
  { ast with ifaces=[] }
