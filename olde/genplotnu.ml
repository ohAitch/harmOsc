


(* #uni2ascii -qaF| ocaml; gnuplot -persist -e'plot for [c=1:2] "{}" u c w l t col' *)

open Printf;;
let repeat n f x =
  let rec make_list n f l = if n == 0 then l else make_list (n-1) f ((f (List.hd l))::l) in
    List.rev (make_list n f x);; (*Still looking for a primitve*) 
let model ?(u0394 = 1e-4) ?(zu2080 = 0.) ?(k = 1.) ?(u03BC = -. 1e-3) ?(points = 50000) init_val = 
  let tu2032 (z, zu0307, zu0308) = let zu0307u2032 = zu0307 +. u0394 *. zu0308 in (z +. u0394 *. (zu0307 +. zu0307u2032) /. 2., zu0307u2032, (zu0307u2032 *. u03BC) -. ((z -. zu2080) *. k) ) in
  repeat points tu2032 [init_val, 0., 0.];;
let e_and_p ?(k = 1.) vals =
let reversed, _, _ = List.fold_left (
fun (acc, sign, i) (x,v, _) -> 
  let e = k*.x*.x +. v*.v in
    if sign *. x < 0. || x = 0. then ((e, i)::acc, x, 1.)
      else ((e, nan)::acc, x, i +. 1.)) ([], 0., 1.) vals
in List.rev reversed;;
Random.init 53;
let r n = 10.**(Random.float n) in
  let u0394 = r (-4.) and u03BC = -. r (-2.) and pts = int_of_float (1000. *. r 2.) and amp = r 2. in
    for iter = 1 to 50 do
      let k = exp((float iter) /. 5. -. 3.) in
        let out = open_out(sprintf "Sim%02d:k=%g__delta=%.1g__mu=%.1g__%d_points__starting_at_%.1g.data" iter k u0394 u03BC pts amp) in
          fprintf out "Energy Frequency\n";
          List.iter (fun (e, p) -> fprintf out "%f %f\n" e (float pts /. p))
                    (e_and_p ~k:k (model ~k:k ~u0394:u0394 ~u03BC:u03BC ~points:pts amp)); close_out out done ;;

