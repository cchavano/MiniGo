open Lexing

let from_single_pos pos =
  let file = pos.pos_fname in
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  Printf.sprintf "%s:%d:%d:" file l c

let from_interval pos1 pos2 =
  if pos1.pos_cnum = pos2.pos_cnum - 1 then from_single_pos pos1
  else
    let file = pos1.pos_fname in
    let line = pos1.pos_lnum in
    let char1 = pos1.pos_cnum - pos1.pos_bol + 1 in
    let char2 = pos2.pos_cnum - pos1.pos_bol in
    Printf.sprintf "%s:%d:%d-%d:" file line char1 char2
