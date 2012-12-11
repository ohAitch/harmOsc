open Printf
;;
let energy k m x v = 0.5 *. (k*.x**2. +. m*.v**2.)
let model delta k mu points m z0 init=
  let rec timestep i acc t z v a=
    if i > 0 then
      let t = t +. delta
      and z = z +. delta *. v 
      and v = v +. delta *. a
      and a = (mu *. v -. ((z -. z0) *. k)) /. m
      in
      timestep (i-1) ((t,z,v,a,energy k m z v)::acc) t z v a
    else
      acc
  in
  timestep points [] 0. init delta 0.
;;
let cycles l =
  let rec up_crossings prev count= function
  (_, v, _, _, _)::tl when prev <= 0. && v > 0. -> up_crossings v (count+1) tl
  | (_, v, _, _, _)::tl -> up_crossings v count tl
  | [] -> count
  in
  float (up_crossings 1. 0 l)
and begin_energy l = 
  let e (_,_,_,_,n) = n
  in
  match l with
  a::b::c::d::f::tl -> (e a +. e b +. e c +. e d +. e f) /. 5.
  | a -> 0.
;;
let out = open_out "harmosc.dat"
in
let delta = 1e-4
and k     = 30.
and mu    = -0.01
and pts   = truncate 1e+5
and m     = 0.01
in
let l     = model delta k mu pts m 0. 10.
in
let growth    = (begin_energy l) /. (begin_energy (List.rev l))
and time      = delta *. (float pts)  in
let frequency = cycles l /. time
and half_life = -. time /. (log growth /. log 2.)
in
fprintf out "#k:%f m:%f mu:%f Half-Life:%f Freq:%f\n" k m mu half_life frequency;
fprintf out "Time Position Velocity Energy\n";
List.iter (fun (t,z,v,_,e) -> fprintf out "%f %f %f %f\n"
             t    z        v        e        ) l
;;
