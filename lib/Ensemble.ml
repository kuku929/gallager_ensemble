type parity_check = {
  block_len : int; (* n *)
  dimension : int; (* k *)
  r : int; (*degree of variable node*)
}

(* Initializes a parity check code description. *)
let init_code block_length dimension var_node_degree =
  { block_len = block_length; dimension; r = var_node_degree }

(* Builds a Gallager-style parity check matrix for the code. *)
let generate_parity_matrix code =
  (* Determines the base parity matrix entry for a given row/column. *)
  let r = code.r in
  let f x y =
    let offset = r * x mod code.block_len in
    if y < offset + code.r && y >= offset then
      1
    else
      0
  in
  let a =
    Array.init_matrix (code.block_len - code.dimension) code.block_len f
  in
  let check_node_degree () =
    (* Computes the sum of a matrix column. *)
    let col_sum j =
      let acc = ref 0 in
      for i = 0 to code.block_len - code.dimension - 1 do
        acc := !acc + a.(i).(j)
      done;
      !acc
    in
    (* Checks that every column has the same degree. *)
    Array.init code.block_len (fun i -> i)
    |> Array.fold_left (fun acc a -> acc && col_sum 0 = col_sum a) true
  in
  assert (check_node_degree ());
  a

(* Generates a set of randomized matrix constructors from a base matrix. *)
let generate_ensemble m count =
  Random.self_init ();
  (* Produces a shuffled copy of the base matrix. *)
  let thunk () =
    let copy_m = Array.copy m in
    Array.shuffle ~rand:Random.int copy_m;
    copy_m
  in
  Array.init count (fun _ -> thunk)

(* Creates and prints decoder graphs for each matrix in the ensemble. *)
let create_graph ensemble =
  Array.iter
    (fun m -> TannerGraph.print_graph (TannerGraph.create_decoder (m ())))
    ensemble

(* Adds two probability distributions elementwise. *)
let add_distributions a b = Array.map2 (fun ai bi -> ai +. bi) a b

(* Decodes a received vector using belief propagation on one parity matrix. *)
let decode_one parity_matrix chan niters input =
  let g = TannerGraph.create_decoder (parity_matrix ()) in
  let output = TannerGraph.belief_prop chan input g niters in
  (* output is log likelihood, convert to probability*)
  (* p / (1 - p) = e ^ output*)
  (* p is probability of 0*)
  Array.map (fun log_lr -> 1. -. (1. /. (1. +. exp log_lr))) output

(* Combines ensemble decoding results into a final hard-decision output. *)
let decode ensemble chan niters input =
  let n = Array.length input in
  let output_probability =
    Array.fold_left
      (fun acc m -> add_distributions acc (decode_one m chan niters input))
      (Array.make (Array.length input) 0.)
      ensemble
    |> Array.map (fun p -> p /. float_of_int n)
  in
  Array.map (fun p -> if p >= 0.5 then 0 else 1) output_probability

(* Prints the parity matrix in row-major form. *)
let print_matrix m =
  (* Recursively prints matrix rows. *)
  let rec print_row r =
    if r < Array.length m then begin
      Array.iter
        (fun ai ->
          print_int ai;
          print_char ' ')
        m.(r);
      print_endline "";
      print_row (r + 1)
    end
    else
      ()
  in
  print_row 0
