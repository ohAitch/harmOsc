(* #uni2ascii -qaF| ocamlc -; gnuplot -persist -e'plot for [c=1:2] "{}" u c w l t col' *)

open Printf;;
let repeat n f x =
  let rec make_list n f l = if n == 0 then l else make_list (n-1) f ((f (List.hd l))::l) in
    List.rev (make_list n f x);; (*Still looking for a primitve*) 
let model ?(delta = 1e-4) ?(z0 = 0.) ?(k = 1.) ?(mu = -. 1e-3) ?(points = 50000) init_val = 
  let t_prime (z, v, a) = 
    (z +. delta *. v , v +. delta *. a, mu *. v -. ((z -. z0) *. k) )
  in repeat points t_prime [init_val, delta, 0.];;
let period vals =
let reversed, _, _ = List.fold_left (
  fun (acc, sign, i) (_, v, _) ->
      if sign *. v < 0. then (i::acc, v, 1.)
        else (acc, v, i +. 1.))
  ([], 0., 1.) vals
in List.rev reversed;;
let energy ?(k = 1.) (x, v, _) = k*.x**2. +. v**2.;; 
let out = open_out("Surface") in
for iter = 1 to 50 do
  for jter = 1 to 50 do
    let k = 1000. *. (float iter) and mu =  -24. *. (float jter) and delta, pts = 1e-3, 10000 in
      let l = (model ~k:k ~mu:mu ~delta:delta ~points:pts 10.) in
         let init, fin = energy ~k:k (List.hd l), energy ~k:k (List.hd (List.rev l)) in (*Extract initial/final energy*)
            let growth = init /. fin and frequency = float (List.length (period l)) /. (float pts *. delta) in 
               fprintf out "%f %f %f %f\n" k mu ((log growth) /. k) frequency
done done;;
