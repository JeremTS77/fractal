(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   julia.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jelefebv <jelefebv@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2016/04/07 00:47:38 by jelefebv          #+#    #+#             *)
(*   Updated: 2016/04/07 10:20:21 by jelefebv         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

open Graphics;;
open_graph " 800x800";;
clear_graph ();;
 
let diverge_julia a b cr ci n =
let x = ref a in
let y = ref b in
let xtemp=ref 0. in
let ytemp=ref 0. in
let k = ref 0 in
while ((!x *. !x+. !y *. !y) < 4.)&&(!k<n) do
xtemp:=(!x)*. (!x)-. (!y)*. (!y)+. cr;
ytemp:=2. *. (!x) *.(!y)+. ci;
x:= !xtemp;
y:= !ytemp;
k:= !k+1;
done;
(!k);;
 
let julia cr ci n d =
clear_graph ();
let k = ref 0 in
for x=(-511) to (511) do
for y=(-383) to (383) do
k:=((diverge_julia ((float_of_int x) /. d) ((float_of_int y) /. d) cr ci n)*10);
set_color (rgb (255-(!k)/2) (255-(!k)/2) (255-(!k)));
plot (512+x) (384+y);
done;
done;;
 
julia (-0.181) (-0.667) 100 200.;;
ignore (read_key () );;
