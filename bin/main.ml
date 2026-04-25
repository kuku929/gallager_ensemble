let block_length = 16 in
let dimension = 4 in
let var_node_degree = 4 in
let chan_error_probability = 0.5 in
let ensemble_count = 1 in

let code = Ensemble.init_code block_length dimension var_node_degree in
let par = Ensemble.generate_parity_matrix code in
(* Ensemble.print_matrix par; *)
let ensemble = Ensemble.generate_ensemble par ensemble_count in
(* Array.iter (fun m -> Ensemble.print_matrix (m ()); print_endline "")
   ensemble; *)
let channel = BSC.create_chan chan_error_probability in
let output =
  Array.make block_length 0 |> BSC.send channel
  |> Ensemble.decode ensemble channel 100
in
Array.iter (fun a -> Printf.printf "%d " a) output
