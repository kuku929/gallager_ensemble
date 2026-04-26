module T = Domainslib.Task

let block_length = 2048
let dimension = 1024
let check_node_degree = 32
let nsamples = 100
let code = Ensemble.init_code block_length dimension check_node_degree
let pool = T.setup_pool ~num_domains:(Domain.recommended_domain_count () - 1) ()
let _ = Printf.printf "%d %d %d\n" block_length dimension check_node_degree; flush stdout

let rec test step p final_p =
  if p > final_p then
    ()
  else begin
    let chan_error_probability = p in
    let fails = Atomic.make 0 in
    let work _ =
      let channel = BSC.create_chan chan_error_probability in
      let input = Array.make block_length 0 in
      let par = Ensemble.generate_parity_matrix code in
      let output =
        input |> BSC.send channel |> Ensemble.decode par channel 100
      in
      if false = Array.for_all2 (fun a b -> a = b) output input then
        Atomic.incr fails;
      ()
    in
    T.parallel_for ~start:1 ~finish:nsamples ~body:work pool;
    Printf.printf "%f %d %d\n" p (Atomic.get fails) nsamples;
    flush stdout;
    test step (p +. step) final_p
  end

let _ = T.run pool (fun () -> test 0.01 0.01 0.2)
let _ = T.teardown_pool pool
