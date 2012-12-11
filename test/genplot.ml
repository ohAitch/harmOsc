(* #uni2ascii -qaF| ocamlc -; gnuplot -persist -e'plot for [c=1:2] "{}" u c w l t col' *)

open Printf;;
let repeat n f x =
  let rec make_list n f l = if n == 0 then l else make_list (n-1) f ((f (List.hd l))::l) in
    (make_list n f x);; (*Still looking for a primitve*) 
let model ?(delta = 1e-4) ?(z0 = 0.) ?(k = 1.) ?(mu = -. 1e-3) ?(points = 50000) ?(m = 1.) init_val = 
  let t_prime (t, (z, v, a)) = 
    t +. delta, (z +. delta *. v , v +. delta *. a, (mu *. v -. ((z -. z0) *. k)) /. m)
  in repeat points t_prime [0., (init_val, delta, 0.)];;
let period vals =
let intervals, _, _ = List.fold_left (
  fun (acc, prev, i) (_, (_, v, _)) ->
      if prev <= 0. && v > 0. then (i::acc, v, 1.) else (acc, v, i +. 1.))
  ([], nan, 1.) vals
and avg l = (List.fold_left ( +. ) 0. l) /. float (List.length l)
in avg intervals;;
let energy ?(k = 1.) ?(m = 1.) (x, v, _) = 0.5 *. (k*.x**2. +. m*.v**2.);; 
let out = open_out("Model") in
 for iter = 1 to 25 do
  let k = 3. *. (float iter) and mu = -0.01 and delta, pts, m = 1e-5, 100000, 1e-2 in
    let l = (model ~k:k ~mu:mu ~delta:delta ~points:pts ~m:m 10.) in
       let data = List.rev_map (fun (t, datum) -> t, datum, energy ~k:k ~m:m datum) l in
          let (_, _, init), (_, _, fin) = (List.hd data), (List.hd (List.rev data)) in (*Extract initial/final energy*)
          let growth = fin /. init and
              frequency = (period l)** -1. /. delta and
              time = (float pts) *. delta in 
            fprintf out "k m mu Half-Life Freq\n";
            fprintf out "%f %f %f %f %f\n" k m (-. mu) (-. time /. (log growth /. log 2.)) frequency
            (*fprintf out "Time Position Velocity Energy\n";
            List.iter (fun (t, (z,v,_), e) -> fprintf out "%f %f %f %f\n" t z v e) data;;*) done;;
