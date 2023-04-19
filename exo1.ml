(* --------------------------------------------------------------------------Question 1.1 *)
 let f a b = a+b;;


 (* 1er maniere  *)
 (* 3+(g-1)=g+2 *)
 let ajout_deux g = if g = 0 then f 2 0 else f (2 + 1) (g - 1)

 (* 2ieme maniere *)
 (* ((2+1)+g)-1 = g+2 *)
 let ajout_deux2 g = if g = 0 then f 2 0 else f (2 + 1) g - 1
 (* test  *)
 let g = ajout_deux 0;;
 let g = ajout_deux 2;;
 let g = ajout_deux2 0;;
 let g = ajout_deux2 2;;

 (* ------------------------------------------------------------------------Question 1.2 *)
 (* erreur1 et 2*)
(* pour distinguer (match l1 with) et (match l2 with),il faut ajouter begin,end ou () à les 2 "match l2 with" *)
(* sinon tous les codes seront traités comme s'ils appartenaient à la première "match l1 with" comme
let somme_derniers l1 l2 = match l1 with
| [] -> match l2 with
| [] -> 0
| x2 :: [] -> x2
| hd2 :: tl2 -> somme_derniers [] tl2
| x1 :: [] -> match l2 with
| [] -> x1
| x2 :: [] -> x1 + x2
| hd2 @ tl2 -> somme_derniers [ x1 ] tl2
| hd1 :: tl1 -> somme_derniers tl1 l2 *)

(* erreur3 *)
(* il faut ajouter rec pour les fonctions recursives *)

(* erreur4 *)
(* pour "hd2 @ tl2 -> somme_derniers [ x1 ] tl2" , @ est le symbol de concatenation, ca sert a rien ici,pour
representer une liste dont le premier élément est hd2 correctement,il faut utilise le symbol  "::" *)



(* correction *)
let rec somme_derniers l1 l2 = match l1 with
| [] -> 
begin
match l2 with
| [] -> 0
| x2 :: [] -> x2
| hd2 :: tl2 -> somme_derniers [] tl2
end
| x1 :: [] -> 
begin
match l2 with
       | [] -> x1
       | x2 :: [] -> x1 + x2
       | hd2 :: tl2 -> somme_derniers [ x1 ] tl2
end
| hd1 :: tl1 -> somme_derniers tl1 l2

(* test *)
let l1 = [1;2];;
let l2 = [3;4];;
let res = somme_derniers l1 l2;;
let l3 = [];;
let l4 = [3;4];;
let res = somme_derniers l3 l4;;
let l5 = [1];;
let l6 = [3;4];;
let res = somme_derniers l5 l6;;

(* ------------------------------------------------------------------------Question 1.3 *)
(* erreur1 *)
let x a b = a+.b 
(* a type float->float->float mais dans 'else x v 0' ,0 est le type int ,Incompatibilité de type *)
(* “%d” print type int, 0 est le type int, donc ici let x a b doit avoir le type int->int->int
le code correcte est let x a b = a + b *)

(* erreur2 *)
(* il manque 'in'  entre (let v = y + 5 ) et (if)  *)

(* erreur3 *)
(* syntax erreur pour la ligne if z > y then Printf.printf "%d" z ; z
else x v 0 retourne un entier 0,donc la code apres 'then' doit egalement retourner un entier
la solution est de ajouter parenthese a cette ligne afin que l'expression (Printf.printf "%d" z ; z) peut retourner l'entier z *)

(* correction *)
let x a b = a + b;;
let f y z = let v = y + 5 in
if z > y then (Printf.printf "%d" z ; z)
else x v 0;;

(* test *)
let res =  f 5 6 ;;
let res =  f 5 4 ;;

(* ------------------------------------------------------------------------Question 1.4 *)
(* (let y =
      let y = 
        (
          y +. 2.5
         in 
         (let z = u y)
         )
              in y *. z
                       in 
                       (let u = 2.0) 
                       )
                       in y +. y +. u ;; *)


(* 1 *)let y = 5.3 ;; (* y:ligne1 *)
(* 2 *)let u z = (* u,z:ligne 2 *)
(* 3 *) z -. y ;; (* z:ligne2 y:ligne1 *)
(* 4 *)let y = (* y:ligne4 *)
(* 5 *) let y = (* y:ligne5 *)
(* 6 *) y +. 2.5 (* y:ligne1 *)
(* 7 *) in let z = u y (* y:ligne5,z:ligne7,u:ligne2 *)
(* 8 *) in y *. z  (* z:ligne7,y:ligne5 *)
(* 9 *) in let u = 2.0 in (* u:ligne9 *)
(*10 *) y +. y +. u ;; (* y:ligne4,u:ligne9 *)
