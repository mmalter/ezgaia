(* Files management *)

let readlines (filename : string) : string list =
    let rec readline channel =
        match input_line channel with
        | line -> line :: readline channel
        | exception End_of_file -> []
    in
    readline (open_in filename)

let clean_readlines (filename) =
    let f x = String.sub x 0 ((String.length x)-1) in (* Drop newlines *)
    let l = readlines filename in
    List.map f l

let write (channel, s) =
    Printf.fprintf channel "%s\n" s


(* Parsing *)

let split_line (channel : in_channel) : string list =
    String.split_on_char ',' (input_line channel)

let join_str (s : string list) : string =
    String.concat "," s

let header_index (spec, headers : string * string list) : int =
    let rec aux (spec, headers, i) =
        match headers with
        | h :: hs -> if h = spec then i else aux (spec, hs, i+1)
        | [] -> raise (Failure "Header not found.")
    in
    aux (spec, headers, 0)

let header_indexes (specs, headers : string list * string list) : int list =
    let f x = header_index (x, headers) in
    List.map f specs

let write_headers (channel, headers : out_channel * string list) : unit =
    write (channel, join_str headers)

let rec take_values (line, h_idxs : string list * int list) : string list =
    match h_idxs with
    | id :: ids -> List.nth line id :: take_values (line, ids)
    | [] -> []

let rec write_selected (i, o, h_idxs : in_channel * out_channel * int list) : unit =
    match split_line i with
    | l -> write (o, join_str (take_values (l, h_idxs))) ; write_selected(i, o, h_idxs)
    | exception End_of_file -> ()


let transform (input, output, specs : string * string * string list) : unit =
    let i = open_in input
    and o = open_out output
    in
    let h_idxs = header_indexes (specs, split_line i)
    in
    write_headers (o, specs) ;
    write_selected (i, o, h_idxs) ;
    close_in i ;
    close_out o

let rec last l =
    match l with
    | [x] -> x
    | x :: xs -> last xs
    | [] -> ""

let rec firsts l =
    match l with
    | [x] -> []
    | x :: xs -> x :: firsts xs
    | [] -> []

let output_path input_path =
    let output_name = last (String.split_on_char '_' input_path)
    and path = String.concat "/" (firsts (String.split_on_char '/' input_path))
    in
    String.concat "/" [path ; "light"; output_name]

let main (input, specs) =
    let output = output_path input
    in transform (input, output, specs)
;;

let input = "/mnt/data/gaia/GaiaSource_000000-003111.csv"
and specs = ["solution_id"; "l"; "b"; "parallax"; "pseudocolour"; "phot_g_mean_mag"]
in main(input, specs)
