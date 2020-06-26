type l = (Lexing.position * string * int * int )

module type Emp_t = sig type t end

module LocationTable (M : Emp_t) = Ephemeron.K1.Make(struct
  type t = M.t
  let hash = Hashtbl.hash 
  let equal = (=) 
end) 

