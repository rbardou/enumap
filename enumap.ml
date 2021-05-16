let error s =
  prerr_endline ("Error: " ^ s);
  exit 1

let error x = Printf.ksprintf error x

let check_name s =
  if String.length s = 0 then error "invalid name: empty name";
  (
    match s.[0] with
      | 'a'..'z' ->
          ()
      | c ->
          error "invalid character %C at the beginning of name: %S" c s
  );
  for i = 1 to String.length s - 1 do
    match s.[i] with
      | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' ->
          ()
      | c ->
          error "invalid character %C in name: %S" c s
  done

let iter l f = List.iter f l
let map l f = List.map f l

let cap = String.capitalize_ascii

let with_open_in filename f =
  let ch = open_in filename in
  match f ch with
    | exception exn ->
        close_in ch;
        raise exn
    | x ->
        close_in ch;
        x

let with_open_out filename f =
  let ch = open_out filename in
  match f ch with
    | exception exn ->
        close_out ch;
        raise exn
    | x ->
        close_out ch;
        x

let () =
  let input_filename, output_filename =
    match Sys.argv with
      | [| _; i; o |] ->
          i, o
      | _ ->
          prerr_endline "Usage: enumap <INPUT_FILENAME> <OUTPUT_FILENAME>";
          exit 0
  in
  let enums =
    with_open_in input_filename @@ fun ch ->
    let rec loop other_enums current_enum_name current_enum =
      match input_line ch with
        | exception End_of_file ->
            if current_enum_name = "" then error "no enum in input";
            if current_enum = [] then error "enum with no value: %S" current_enum_name;
            List.rev ((current_enum_name, List.rev current_enum) :: other_enums)
        | "" ->
            loop other_enums current_enum_name current_enum
        | line ->
            if line.[0] = '#' then
              loop other_enums current_enum_name current_enum
            else if String.length line >= 2 && line.[0] = '*' && line.[1] = ' ' then
              let name = String.sub line 2 (String.length line - 2) in
              check_name name;
              if current_enum_name = "" then
                (
                  assert (other_enums = []);
                  assert (current_enum = []);
                  loop [] name []
                )
              else if current_enum = [] then error "enum with no value: %S" current_enum_name
              else
                loop ((current_enum_name, List.rev current_enum) :: other_enums) name []
            else if current_enum_name = "" then
              error "first line must be an enum name, i.e. start with '*'"
            else
              let names =
                String.split_on_char '/' line
                |> List.map String.trim
                |> List.filter ((<>) "")
              in
              iter names check_name;
              match names with
                | [] ->
                    loop other_enums current_enum_name current_enum
                | main_name :: alternative_names ->
                    loop other_enums current_enum_name
                      ((main_name, alternative_names) :: current_enum)
    in
    loop [] "" []
  in
  with_open_out output_filename @@ fun ch ->
  let l x = Printf.ksprintf (fun s -> output_string ch s; output_char ch '\n') x in
  l "(* This file was automatically generated, do not edit it manually. *)";
  iter enums @@ fun (enum_name, values) ->
  l "";
  l "module %s =" (cap enum_name);
  l "struct";
  l "  type t =";
  iter values (fun (v, _) -> l "    | %s" (cap v));
  l "";
  l "  let compare = (Stdlib.compare: t -> t -> int)";
  l "";
  l "  let list =";
  l "    [";
  iter values (fun (v, _) -> l "      %s;" (cap v));
  l "    ]";
  l "";
  iter values (fun (v, _) -> l "  let %s_id = Protype.Id.make %S" v (cap v));
  l "";
  l "  let id = function";
  iter values (fun (v, _) -> l "    | %s -> %s_id" (cap v) v);
  l "";
  l "  let typ =";
  iter values (fun (v, _) -> l "    let %s = %s_id in" v v);
  l "    Protype.enum";
  (
    let rename = List.flatten @@ map values @@ fun (v, a) -> map a @@ fun a -> a, v in
    match rename with
      | [] ->
          ()
      | _ ->
          l "      ~rename: [";
          iter rename (fun (a, b) -> l "        Protype.Id.make %S, %s;" (cap a) b);
          l "      ]"
  );
  l "      [";
  iter values (fun (v, _) -> l "        %s, %s;" v (cap v));
  l "      ]";
  l "      id";
  l "";
  l "  type 'a map =";
  l "    {";
  iter values (fun (v, _) -> l "      %s: 'a;" v);
  l "    }";
  l "";
  l "  let make f =";
  l "    {";
  iter values (fun (v, _) -> l "      %s = f %s;" v (cap v));
  l "    }";
  l "";
  l "  let get r k =";
  l "    match k with";
  iter values (fun (v, _) -> l "      | %s -> r.%s" (cap v) v);
  l "";
  l "  let set r k v =";
  l "    match k with";
  iter values (fun (v, _) -> l "      | %s -> { r with %s = v }" (cap v) v);
  l "";
  l "  let iter r f =";
  iter values (fun (v, _) -> l "    f %s r.%s;" (cap v) v);
  l "    ()";
  l "";
  l "  let map r f =";
  l "    {";
  iter values (fun (v, _) -> l "      %s = f %s r.%s;" v (cap v) v);
  l "    }";
  l "";
  l "  let fold a r f =";
  iter values (fun (v, _) -> l "    let a = f a %s r.%s in" (cap v) v);
  l "    a";
  l "";
  l "  let for_all r f =";
  iter values (fun (v, _) -> l "    f %s r.%s &&" (cap v) v);
  l "    true";
  l "end";
  ()

(* val iter: 'a r -> (t -> 'a -> unit) *)
(* val map: 'a r -> (t -> 'a -> 'b) -> 'b r *)
(* val fold: 'b -> 'a r -> ('b -> 'a -> 'b) -> 'b *)
