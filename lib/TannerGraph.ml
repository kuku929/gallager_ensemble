type node = {
  adj : int array; (* adjacency list*)
  inbox : float array; (* inbox array*)
  mutable log_lr : float;
}

type bipartite_graph = {
  n : int; (* number of variable nodes*)
  n_k : int; (* number of check nodes*)
  variable : node array;
  check : node array;
}

let create_decoder parity_matrix =
  (* Creates a Tanner Graph using a parity check matrix *)
  (* The columns are accessed using the first index*)
  let n = Array.length parity_matrix in
  let n_k = Array.length parity_matrix.(0) in
  let create_check_node j =
    (* find variable nodes adjacent to check node j*)
    let adj =
      Array.of_list
        (Array.init n (fun i -> i)
        |> Array.fold_left
             (fun acc i -> if parity_matrix.(i).(j) = 1 then i :: acc else acc)
             [])
    in
    { adj; inbox = Array.init n (fun _ -> 0.0); log_lr = 0. }
  in
  let create_variable_node i =
    (* find check nodes adjacent to variable node i*)
    let adj =
      Array.of_list
        (Array.init n_k (fun i -> i)
        |> Array.fold_left
             (fun acc j -> if parity_matrix.(i).(j) = 1 then j :: acc else acc)
             [])
    in
    { adj; inbox = Array.init n_k (fun _ -> 0.0); log_lr = 0. }
  in
  let variable =
    Array.make n 0 |> Array.mapi (fun i _ -> create_variable_node i)
  in
  let check = Array.make n_k 0 |> Array.mapi (fun i _ -> create_check_node i) in
  { n; n_k; variable; check }

let init input chan =
  (* Initilize the log likelihood ratio of environment nodes*)
  let p = BSC.get_p chan in
  Array.map
    (fun symbol ->
      let pr0 = if symbol = 1 then p else 1.0 -. p in
      let pr1 = if symbol = 0 then p else 1.0 -. p in
      assert (pr1 != 0.);
      log (pr0 /. pr1))
    input

let clamp x =
  let eps = 1e-12 in
  if x >= 1.0 then 1.0 -. eps
  else if x <= -1.0 then -1.0 +. eps
  else x

let phi x =
  if abs_float x < 1e-8 then -1e12
  else begin
    let x = abs_float x in
    log (tanh x)
  end

let phi_inv x =
  if abs_float x < 1e-12 then
    1. -. 1e-12
  else exp x

let print_node node =
  print_string "  (adj : ";
  Array.iter (fun a -> Printf.printf "%d, " a) node.adj;
  print_endline ")";
  print_string "  (inbox : ";
  Array.iter (fun a -> Printf.printf "%f " node.inbox.(a)) node.adj;
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

let update environment g =
  (* One pass of the BP algorithm*)
  (* i -> a*)
  let message_from_i i var_node =
    let sum =
      environment.(i)
      +. Array.fold_left
           (fun acc a -> acc +. var_node.inbox.(a))
           0. var_node.adj
    in
    let message_to_a a =
      (*remove a's message from the total and send to a*)
      g.check.(a).inbox.(i) <- sum -. var_node.inbox.(a)
    in
    Array.iter (fun a -> message_to_a a) var_node.adj;
  in
  (* Can be parallel*)
  Array.iteri (fun i node -> message_from_i i node) g.variable;

  (* a -> i*)
  let message_from_a a check_node =
    (* For message a to i we want to find product for all j != i and we cannot
       divide by total product because of division by zero. Therefore we maintain
       a prefix product and calculate the suffix product while iterating. *)
    let signs =
      Array.init (Array.length check_node.adj) (fun ind ->
        let i = check_node.adj.(ind) in
        if check_node.inbox.(i) < 0. then -1.0 else 1.0)
    in
    let total_sign = Array.fold_left ( *. ) 1.0 signs in
    let prefix_array =
      Array.fold_left_map
        (fun prefix i ->
          let next_pref = (prefix +. phi (check_node.inbox.(i) /. 2.0)) in
          (next_pref, prefix))
        0.0 check_node.adj
      |> snd
    in
    (* Updated during reverse iter*)
    let suffix = ref 0.0 in
    let message_to_i i ind prefix =
      let value = (prefix +. suffix.contents) in
      let value = clamp (total_sign *. signs.(ind) *. phi_inv value) in
      g.variable.(i).inbox.(a) <- 2.0 *. atanh value
    in
    let rec reverse_iter ind =
      if ind = -1 then
        ()
      else begin
        let i = check_node.adj.(ind) in
        message_to_i i ind prefix_array.(ind);
        suffix := !suffix +. phi (check_node.inbox.(i) /. 2.0);
        reverse_iter (ind - 1)
      end
    in
        reverse_iter (Array.length check_node.adj - 1)
  in
  Array.iteri (fun a node -> message_from_a a node) g.check

let belief_prop chan input g niters =
  let environment = init input chan in
  for _ = 1 to niters do
    update environment g
  done;
  let post_convergence () =
    Array.iteri
      (fun i var_node ->
        var_node.log_lr <-
          Array.fold_left
            (fun acc a -> acc +. var_node.inbox.(a))
            0. var_node.adj
          +. environment.(i))
      g.variable
  in
  post_convergence ();
  Array.map (fun node -> node.log_lr) g.variable
