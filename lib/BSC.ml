type chan = { p : float }

let create_chan error_probability = { p = error_probability }
let get_p chan = chan.p

let bernoulli p =
  (*returns 1 with probability p*)
  if p >= Random.float 1. then
    1
  else
    0

let xor a b =
  if a <> b then
    1
  else
    0

let send chan input =
  Random.self_init ();
  input |> Array.map (fun a -> xor (bernoulli chan.p) a)
