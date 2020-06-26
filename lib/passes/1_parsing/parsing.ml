open Lexing
open Lexer
open Parse_tree
open Helpers.Errors

module Pt_loc = Pt_loc
module I = Parser.MenhirInterpreter

exception SyntaxErrorLoced of (int * int) option * string 

let pos lexbuf = let pos = Lexing.lexeme_start_p lexbuf in (pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1)

let get_parse_error env =
    match I.stack env with
    | lazy Nil -> "Invalid syntax"
    | lazy (Cons (I.Element (state, _, _, _), _)) ->
        try (Parser_messages.message (I.number state)) with
        | Not_found -> "invalid syntax (no specific message for this eror)"


let rec parse_inc lexbuf (checkpoint : Parse_tree.t I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _env ->
      let token = Lexer.token lexbuf in
      let startp = lexbuf.lex_start_p
      and endp = lexbuf.lex_curr_p in
      let checkpoint = I.offer checkpoint (token, startp, endp) in
      parse_inc lexbuf checkpoint
  | I.Shifting _
  | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      parse_inc lexbuf checkpoint
  | I.HandlingError _env ->
      let line, pos = pos lexbuf in
      let err = get_parse_error _env in
      raise (SyntaxErrorLoced (Some (line, pos), err))
  | I.Accepted v -> v
  | I.Rejected ->
       raise (SyntaxErrorLoced (None, "invalid syntax (parser rejected the input)"))


let parse filename s = 
  let lexbuf = Lexing.from_string s in 
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  Pt_loc.filename := filename;

  try parse_inc lexbuf (Parser.Incremental.program lexbuf.lex_curr_p)
  with SyntaxErrorLoced (pos, err) ->
    match pos with
    | Some (line, pos) -> raise @@ SyntaxError (Some (lexbuf.lex_curr_p, filename, line, pos), err)
    | None -> raise @@ SyntaxError (Some(lexbuf.lex_curr_p, filename, -1, 0), err)

let parse_file filename = 
  let rec readfile ic = 
    try let line = input_line ic in (line ^ "\n")::(readfile ic) with _ -> close_in_noerr ic; []
  in
  filename 
  |> open_in 
  |> readfile 
  |> List.fold_left (fun acc x -> acc ^ x) "" 
  |> parse filename