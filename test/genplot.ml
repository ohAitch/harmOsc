(* #uni2ascii -qaF| ocamlc -; gnuplot -persist -e'plot for [c=1:2] "{}" u c w l t col' *)

open Printf;;
let repeat n f x =
  let rec make_list n f l = if n == 0 then l else make_list (n-1) f ((f (List.hd l))::l) in
    List.rev (make_list n f x);; (*Still looking for a primitve*) 
let model ?(delta = 1e-4) ?(z0 = 0.) ?(k = 1.) ?(mu = -. 1e-3) ?(points = 50000) init_val = 
  let t_prime (z, v, a) = 
    (z +. delta *. v , v +. delta *. a, mu *. ((z -. z0) *. -. k) )
  in repeat points t_prime [init_val, delta, 0.];;
let e_and_p ?(k = 1.) vals =
let reversed, _, _ = List.fold_left (
  fun (acc, sign, i) (x,v, _) ->
    let e = k*.x**2. +. v**2. in
      if sign *. x < 0. || x = 0. then ((e, i)::acc, x, 1.)
        else ((e, nan)::acc, x, i +. 1.)) ([], 0., 1.) vals
in List.rev reversed;;
Random.init 53;;
let r n = 10.**(Random.float n) in
  let delta = r (-4.) and mu = 1. -. r (-2.) and pts = int_of_float (1000. *. r 2.) and amp = r 2. in
for iter = 1 to 50 do
  let k = exp((float iter) /. 7.) in
    let out = open_out(sprintf "Sim%02d" iter) in
      fprintf out "#k = %g, Δ = %.1g, μ = %.1g\n" k delta mu;
      fprintf out "Energy Frequency\n";
        List.iter (fun (e, p) -> fprintf out "%f %f\n" e (float pts /. p))
          (e_and_p ~k:k (model ~k:k ~delta:delta ~mu:mu ~points:pts amp)); close_out out
done;;
