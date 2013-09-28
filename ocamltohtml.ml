open Lexer

let html_escape s =
  let b = Buffer.create 42 in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '&' | '<' | '>' | '\'' | '"' as c ->
      Printf.bprintf b "&#%d;" (int_of_char c);
    | c -> 
      Buffer.add_char b c
  done;
  Buffer.contents b


let to_html t =
  let open Printf in
  let open Buffer in
  let b = Buffer.create 42 in
  begin
    match t.kind with
    | String (_, s) ->
      bprintf b "<span class='string'>%s</span>" (html_escape s)
    | Char (_, s) ->
      bprintf b "<span class='string'>%s</span>" (html_escape s)
    | Keyword (_, s) ->
      bprintf b "<span class='keyword'>%s</span>" (html_escape s)
    | Keyop (_, s) ->
      bprintf b "<span class='keywordsign'>%s</span>" (html_escape s)
    | Number (_, s) ->
      bprintf b "<span class='number'>%s</span>" (html_escape s)
    | Upper s ->
      bprintf b "<span class='constructor module'>%s</span>" (html_escape s)
    | Lower s ->
      bprintf b "<span class='lower'>%s</span>" (html_escape s)
    | Infix s ->
      bprintf b "<span class='keywordsign infix'>%s</span>" (html_escape s)
    | Prefix s ->
      bprintf b "<span class='keywordsign prefix'>%s</span>" (html_escape s)
    | Operator s ->
      bprintf b "<span class='keywordsign operator'>%s</span>" (html_escape s)
    | Comment s ->
      bprintf b "<span class='comment'>%s</span>" (html_escape s)
    | Spaces s -> Buffer.add_string b s
  end;
  Buffer.contents b


let html_of_tl l =
  let b = Buffer.create 42 in
  List.iter (fun t -> Buffer.add_string b (to_html t)) l;
  Buffer.contents b

let _ =
  print_string (html_of_tl (read (stream_of_inchannel stdin)))



let new____to_html tl =
  let open Buffer in
  let open Printf in
  let b = create 42 in
  let rec loop (new_def:bool) (type_def:bool) (let_def:bool) (in_par:bool)
      (params:bool) (tl:'a list) =
    let f ?nd:(new_def=new_def) ?td:(type_def=type_def) ?ld:(let_def=let_def)
        ?ip:(in_par=in_par) ?pa:(params=params) tl =
      loop new_def type_def let_def in_par params tl
    in
    match tl with
    | [] -> Buffer.contents b
    | hd::tl ->
      match hd.kind with
      | Keyop(SemiColonSemiColon, s) ->
        bprintf b "<span class=''>%s</span>" s;
        f ~nd:true tl
      | Keyop(Eq, s) ->
        bprintf b "<span class=''>%s</span>" s;
        f ~nd:true tl
      | Keyword(Let, s) ->
        bprintf b "<span class='let'>%s</span>" s;
        f ~nd:true ~td:false ~ld:true tl
      | Keyword(And, s) ->
        bprintf b "<span class='let'>%s</span>" s;
        f ~nd:true ~td:false ~ld:true tl
      | Keyword(Type, s) ->
        bprintf b "<span class='type'>%s</span>" s;
        f ~nd:true ~td:true ~ld:false tl
      | String (_, s) ->
        bprintf b "<span class='string'>%s</span>" (html_escape s);
        f tl
      | Char (_, s) ->
        bprintf b "<span class='string'>%s</span>" (html_escape s);
        f tl
      | Keyword (_, s) ->
        bprintf b "<span class='keyword'>%s</span>" (html_escape s);
        f tl
      | Keyop (_, s) ->
        bprintf b "<span class='keywordsign'>%s</span>" (html_escape s);
        f tl
      | Number (_, s) ->
        bprintf b "<span class='number'>%s</span>" (html_escape s);
        f tl
      | Upper s ->
        bprintf b "<span class='constructor module'>%s</span>" (html_escape s);
        f tl
      | Lower s ->
        bprintf b "<span class='lower'>%s</span>" (html_escape s);
        f tl
      | Infix s ->
        bprintf b "<span class='keywordsign infix'>%s</span>" (html_escape s);
        f tl
      | Prefix s ->
        bprintf b "<span class='keywordsign prefix'>%s</span>" (html_escape s);
        f tl
      | Operator s ->
        bprintf b "<span class='keywordsign operator'>%s</span>" (html_escape s);
        f tl
      | Comment s ->
        bprintf b "<span class='comment'>%s</span>" (html_escape s);
        f tl
      | Spaces s -> Buffer.add_string b s;
        f tl
  in loop true false false tl
