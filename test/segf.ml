open Printf;;
let repeat n f x =
  let rec make_list n f l = if n == 0 then l else make_list (n-1) f ((f (List.hd l))::l) in
    List.rev (make_list n f x);; (*Still looking for a primitve*)
let model ?(delta = 1e-4) ?(z0 = 0.) ?(k = 1.) ?(mu = -. 1e-3) ?(points = 50000) ?(m = 1.) init_val =
  let t_prime (t, (z, v, a)) =
    t +. delta, (z +. delta *. v , v +. delta *. a, mu *. v -. ((z -. z0) *. k /. m) )
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
  let k = 2. and mu = -0.02 and delta, pts = 1e-4, 500000 in
    let l = (model ~k:k ~mu:mu ~delta:delta ~points:pts 10.) in
       let data = List.map (fun (t, datum) -> t, datum, energy ~k:k datum) l in
          let (_, _, init), (_, _, fin) = (List.hd data), (List.hd (List.rev data)) in (*Extract initial/final energy*)
          let growth = fin /. init and
              frequency = (period l)** -1. /. delta in
            fprintf out "#K:%f mu:%f growth:%f Freq:%f\n" k (-. mu) growth frequency;
            List.iter (fun (t, (z,v,_), e) -> fprintf out "%f %f %f %f\n" t z v e) data;;
