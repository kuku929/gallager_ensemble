type 'a node = {
  adj : int array; (*adjacency list*)
  inbox : 'a ref array; (*inbox array*)
  log_lr : float ref;
}

type bipartite_graph = {
  n : int; (* number of variable nodes*)
  n_k : int; (* number of check nodes*)
  variable : float node array;
  check : float node array;
}

let create_decoder parity_matrix =
  (* Creates a Tanner Graph using a parity check matrix *)
  let n_k = Array.length parity_matrix in
  let n = Array.length parity_matrix.(0) in
  let create_check_node j =
    (* find variable nodes adjacent to check node j*)
    let adj =
      Array.of_list
        (Array.init n (fun i -> i)
        |> Array.fold_left
             (fun acc i -> if parity_matrix.(j).(i) = 1 then i :: acc else acc)
             [])
    in
    {
      adj;
      inbox = Array.init (Array.length adj) (fun _ -> ref 0.0);
      log_lr = ref 0.;
    }
  in
  let create_variable_node i =
    (* find check nodes adjacent to variable node i*)
    let adj =
      Array.of_list
        (Array.init n_k (fun i -> i)
        |> Array.fold_left
             (fun acc j -> if parity_matrix.(j).(i) = 1 then j :: acc else acc)
             [])
    in
    {
      adj;
      inbox = Array.init (Array.length adj) (fun _ -> ref 0.0);
      log_lr = ref 0.;
    }
  in
  let variable =
    Array.make n 0 |> Array.mapi (fun i _ -> create_variable_node i)
  in
  let check = Array.make n_k 0 |> Array.mapi (fun i _ -> create_check_node i) in
  { n; n_k; variable; check }

let init input chan g =
  (*Initilize the log likelihood ratio of environment nodes*)
  let p = BSC.get_p chan in
  Array.map
    (fun symbol ->
      let pr0 = if symbol = 1 then p else 1.0 -. p in
      let pr1 = if symbol = 0 then p else 1.0 -. p in
      assert (pr1 != 0.);
      log (pr0 /. pr1))
    input

let update environment g =
  (*One pass of the BP algorithm*)
  ()

let belief_prop chan input g niters =
  let environment = init input chan g in
  for i = 1 to niters do
    update environment g
  done;
  Array.map (fun node -> node.log_lr.contents) g.variable

let print_node node =
  print_string "  (adj : ";
  Array.iter (fun a -> Printf.printf "%d, " a) node.adj;
  print_endline ")";
  print_string "  (inbox : ";
  Array.iter (fun a -> Printf.printf "%f " !a) node.inbox;
  print_endline ")"

let print_graph g =
  Printf.printf "n : %d\n" g.n;
  Printf.printf "n - k : %d\n" g.n_k;
  Array.iteri
    (fun i node ->
      Printf.printf "Variable Node %d\n" i;
      print_node node)
    g.variable;
  Array.iteri
    (fun i node ->
      Printf.printf "Check Node %d\n" i;
      print_node node)
    g.check
