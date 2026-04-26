type chan = { p : float }

let create_chan error_probability = 
    Random.self_init ();
    { p = error_probability }

let get_p chan = chan.p

let bernoulli p =
  (* returns 1 with probability p*)
  if p >= Random.float 1. then
    1
  else
    0

let xor a b =
  if a <> b then
    1
  else
    0

let dbg_array name a =
  Printf.printf "%s:" name;
  Array.iter
    (fun a ->
      print_int a;
      print_string ", ")
    a;
  print_endline ""

let send chan input =
  input |> Array.map (fun a -> xor (bernoulli chan.p) a)
