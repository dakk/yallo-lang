(* polymorphic ast traversal *)
open Expr

type 'a t_ovverride = texpr -> 'a
type 'a t_join = 'a -> 'a -> 'a

let traverse (te: texpr) (tf: 'a t_ovverride) (jf: 'a t_join) (empty: 'a) = 
  let rec traverse' (t, e) =
    try 
      tf (t, e)
    with | _ -> match e with
  | Map (a)
  | BigMap (a) ->
    let a, b = List.split a in 
    jf
      (List.fold_left (fun acc e -> jf (traverse' e) acc) empty a)
      (List.fold_left (fun acc e -> jf (traverse' e) acc) empty b)

  | Record (a) -> 
    (List.fold_left (fun acc e -> jf (traverse' @@ snd e) acc) empty a)

  | BuildContractCodeAndStorage (_, a)
  | List (a)
  | Set (a) 
  | Tuple (a) -> 
    (List.fold_left (fun acc e -> jf (traverse' e) acc) empty a)

  | MatchWith (a, eel) -> 
    jf (traverse' a)
    (let a,b = List.split eel in 
    jf
      (List.fold_left (fun acc e -> jf (traverse' e) acc) empty a)
      (List.fold_left (fun acc e -> jf (traverse' e) acc) empty b))

  | Enum (_, _)
  | Bool (_)
  | Nat (_)
  | Int (_)
  | Mutez (_)
  | ChainId (_)
  | Address (_)
  | String (_)
  | Bytes (_)
  | KeyHash (_)
  | Key (_)
  | Signature (_)
  | Typed (_)
  | StorageEntry (_)
  | LocalRef (_)
  | StorageRef (_)
  | EnumValue (_)
  | TezosNow
  | TezosAmount
  | TezosBalance
  | TezosChainId
  | TezosSelf
  | TezosSelfAddress
  | TezosSource
  | TezosSender
  | None
  | Unit 
  | MapEmpty
  | BigMapEmpty
  | SetEmpty
  | ListEmpty
  | GlobalRef (_)
  | CaseDefault -> empty

  | Lambda (_, a)
  | ContractInstance (a) 
  | TezosImplicitAccount (a)
  | Entrypoint (a, _)
  | TezosSetDelegate (a)
  | TezosAddressOfContract (a)
  | TezosContractOfAddress (a)
  | CryptoBlake2B (a)
  | CryptoHashKey (a) 
  | CryptoSha256 (a)
  | CryptoSha512 (a)
  | Some (a)
  | RecordAccess (a, _)
  | OptionGetSome (a) 
  | OptionIsNone (a)
  | MapSize (a)
  | OptionIsSome (a)
  | SetSize (a)
  | ListSize (a)
  | ListHead (a)
  | ListTail (a)
  | StringSize (a)
  | BytesSize (a)
  | BytesPack (a)
  | BytesUnpack (a)
  | TupleFst (a)
  | TupleSnd (a)
  | Abs (a)
  | Neg (a)
  | IsNat (a)
  | Fail (a)
  | FailIf (a)
  | Assert (a)
  | Let (_, _, a)
  | LetTuple (_, a)
  | SAssign (_, a)
  | Not (a)
  | ToInt (a)
  | SRecAssign (_, _, a) -> traverse' a

  | MapGetOpt (a, b)
  | MapMem (a, b)
  | MapMapWith (a, b)
  | MapRemove (a, b) 
  | BigMapGetOpt (a, b)
  | BigMapMem (a, b)
  | BigMapRemove (a, b) 
  | SetMem (a, b)
  | ListPrepend (a, b)
  | ListMapWith (a, b)
  | StringConcat (a, b) 
  | BytesConcat (a, b)
  | Add (a, b)
  | Sub (a, b)
  | Mul (a, b)
  | Div (a, b)
  | Mod (a, b)
  | EDiv (a, b)
  | And (a, b)
  | Or (a, b)
  | Lt (a, b)
  | Lte (a, b)
  | Gt (a, b)
  | Gte (a, b)
  | Eq (a, b)
  | Neq (a, b)
  | Apply (a, b)
  | FailIfMessage (a, b)
  | LetIn (_, _, a, b)
  | LetTupleIn(_, a, b)
  | Seq (a, b) -> jf (traverse' a) (traverse' b)

  | TezosTransfer (a, b, c)
  | TezosCreateContract (a, b, c)
  | CryptoCheckSignature (a, b, c)
  | ListFold (a, b, c)
  | StringSlice (a, b, c)
  | BytesSlice (a, b, c)
  | SetUpdate (a, b, c) 
  | BigMapGet (a, b, c)
  | BigMapUpdate (a, b, c) 
  | MapFold (a, b, c)
  | MapUpdate (a, b, c) 
  | MapGet (a, b, c)
  | IfThenElse (a, b, c) -> jf (jf (traverse' a) (traverse' b)) (traverse' c)
  in traverse' te


