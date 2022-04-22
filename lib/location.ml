type 'a t = {
  startpos : Lexing.position;
  endpos : Lexing.position;
  content : 'a;
}

let make startpos endpos content = { startpos; endpos; content }
