(** The type representing a location associated to the content [content]. *)
type 'a t = {
  startpos : Lexing.position;
  endpos : Lexing.position;
  content : 'a;
}

(** [make startpos endpos content] returns a location beginning at [startpos], ending at [endpos] and containing [content]. *)
let make startpos endpos content = { startpos; endpos; content }
