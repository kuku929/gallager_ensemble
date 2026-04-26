type parity_check = {
  block_len : int; (* n *)
  dimension : int; (* k *)
  r : int; (*degree of variable node*)
}

(* Initializes a parity check code description. *)
let init_code block_length dimension check_node_degree =
  { block_len = block_length; dimension; r = check_node_degree; }

(* Generates a randomized matrix a base matrix by swapping columns *)
let randomize m =
  Random.self_init ();
  (* Produces a shuffled copy of the base matrix. *)
  let copy_m = Array.copy m in
  Array.shuffle ~rand:Random.int copy_m;
  copy_m

(* Builds a Gallager-style parity check matrix for the code. *)
let generate_parity_matrix code =
  (* Determines the base parity matrix entry for a given row/column. *)
  let r = code.r in
  let n = code.block_len in
  let k = code.dimension in
  assert(n mod r = 0 && ((n - k) mod (n / r)) = 0);
  let f x y =
    let offset = r * y mod n in
    if x < offset + r && x >= offset then
      1
    else
      0
  in
  (* NOTE: we compute Q transpose for ease of randomization*)
  let qT =
    Array.init_matrix n (n / r) f
  in
  let rec add h =
    if (Array.length h.(0)) = (n - k) then h
    else begin
      add (Array.map2 (Array.append) h (randomize qT))
    end
  in
  add @@ qT

(* Decodes a received vector using belief propagation on one parity matrix. *)
let decode parity_matrix chan niters input =
  let g = TannerGraph.create_decoder parity_matrix in
  let output = TannerGraph.belief_prop chan input g niters in
  output |> Array.map (fun log_lr -> if log_lr >= 0.0 then 0 else 1)

(* Prints the parity matrix in row-major form. *)
let print_matrix m =
  (* Recursively prints matrix rows. *)
  let rec print_row r =
    if r < (Array.length m.(0)) then begin
      for c = 0 to (Array.length m) - 1 do
        Printf.printf "%d, " m.(c).(r)
      done;
      print_endline "";
      print_row (r + 1)
    end
    else
      ()
  in
  print_row 0
