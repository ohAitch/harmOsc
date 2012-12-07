(* #uni2ascii -qaF| ocamlc -; gnuplot -persist -e'plot for [c=1:2] "{}" u c w l t col' *)

open Printf;;
let repeat n f x =
  let rec make_list n f l = if n == 0 then l else make_list (n-1) f ((f (List.hd l))::l) in
    List.rev (make_list n f x);; (*Still looking for a primitve*) 
let model ?(delta = 1e-4) ?(z0 = 0.) ?(k = 1.) ?(mu = -. 1e-3) ?(points = 50000) ?(m = 1.) init_val = 
  let t_prime (z, v, a) = 
    (z +. delta *. v , v +. delta *. a, mu *. v -. ((z -. z0) *. k /. m) )
  in repeat points t_prime [init_val, delta, 0.];;
let period vals =
let intervals, _, _ = List.fold_left (
  fun (acc, sign, i) (_, v, _) ->
      if sign *. v < 0. then (i::acc, v, 1.) else (acc, v, i +. 1.))
  ([], nan, 0.) vals
and avg l = (List.fold_left ( +. ) 0. l) /. float (List.length l)
in avg intervals ;;
let energy ?(k = 1.) ?(m = 1.) (x, v, _) = 0.5 *. (k*.x**2. +. m*.v**2.);; 
let out = open_out("Surface") in
for iter = 1 to 25 do
    let k = 1. and mu = (float iter) *. -0.01 and delta, pts = 1e-4, 100000 in
      let l = (model ~k:k ~mu:mu ~delta:delta ~points:pts 10.) in
         let init, fin = energy ~k:k (List.hd l), energy ~k:k (List.hd (List.rev l)) in (*Extract initial/final energy*)
            let growth = fin /. init and
                frequency = (period l)** -1. /. delta in 
               fprintf out "%f %f %f %f\n" k mu growth (frequency /. (k ** 0.5))
done ;;
