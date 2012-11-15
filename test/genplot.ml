(* #uni2ascii -qaF| ocamlc -; gnuplot -persist -e'plot for [c=1:2] "{}" u c w l t col' *)

open Printf;;
let repeat n f x =
  let rec make_list n f l = if n == 0 then l else make_list (n-1) f ((f (List.hd l))::l) in
    List.rev (make_list n f x);; (*Still looking for a primitve*) 
let model ?(delta = 1e-3) ?(z0 = 0.) ?(k = 1.) ?(mu = -. 1e-3) ?(points = 50000) init_val = 
  let t_prime (z, v, a) = 
    (z +. delta *. v , v +. delta *. a, mu *. v -. ((z -. z0) *. k) )
  in repeat points t_prime [init_val, delta, 0.];;
let e_and_p ?(k = 1.) vals =
let reversed, _, _ = List.fold_left (
  fun (acc, sign, i) (x,v, _) ->
    let e = k*.x**2. +. v**2. in
      if sign *. x < 0. || x = 0. then ((e, i)::acc, x, 1.)
        else ((e, nan)::acc, x, i +. 1.)) ([], 0., 1.) vals
in List.rev reversed;;
let out = open_out("Surface") in
fprintf out "Springiness Friction log(scale)";
for iter = 1 to 50 do
  for jter = 1 to 50 do
    let k = 10.**((float iter) /. 30. +. 3.) and mu =  -1200. /. (float jter) and pts = 10000 in
      (*let out = open_out(sprintf "Sim%d-%d" iter jter) in
        fprintf out "#k = %g, μ = %g\n" k mu;
        fprintf out "Energy Frequency\n";
          List.iter (fun (e, p) -> fprintf out "%f %f\n" e (float pts /. p))*)
           let l = (e_and_p ~k:k (model ~k:k ~mu:mu ~points:pts 10.)) in
             let (init, _), (fin, _) = List.hd l, List.hd (List.rev l) in
               fprintf out "%f %f %f\n" k mu (log (init /. fin))
done done;;
