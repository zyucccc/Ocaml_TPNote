type command = Up | Down | Left | Right | Seq of (command list);;

 (* --------------------------------------------------------------------------Question 5.1 *)
 
 let f3 x y z = for i = 0 to x do y.(i) <- z done;;
 let evalpos (c:command) =
   match c with
   | Up -> (0,1)
   | Down -> (0,-1)
   | Left -> (-1,0)
   | Right ->  (1,0)
   | Seq [] -> (0,0)
   | Seq  (o::k)->
  let rec evalpos2 co1 l abc coor =   (* evalpos2 est utilise pour calculer la liste principale *)
    begin
    match co1 with
    | Up -> if coor+1 >100  then failwith"la tortue sort du terrain" 
    else if l!=[] then evalpos2 (List.hd l) (List.tl l) abc (coor+1)
    else (abc,coor+1)
    | Down -> if coor - 1 < -100 then failwith"la tortue sort du terrain" 
    else if l!=[] then evalpos2 (List.hd l) (List.tl l) abc (coor-1)
    else (abc,coor-1)
    | Left -> if abc -1 < -100  then failwith"la tortue sort du terrain" 
    else if l!=[] then evalpos2 (List.hd l) (List.tl l) (abc-1) coor
    else (abc-1,coor)
    | Right -> if abc + 1 > 100  then failwith"la tortue sort du terrain" 
    else if l!=[] then evalpos2 (List.hd l) (List.tl l) (abc+1) coor
    else (abc+1,coor)
    | Seq [] -> if l!=[] then evalpos2 (List.hd l) (List.tl l) abc coor
    else (abc,coor)
    | Seq (t::s) -> let rec seqp co lst abc coor =    (* seqp est utilise pour calculer tous les decalage de coordonnee de tous les sous-liste(Seq) de la liste principale *)
                   begin                                 (* l'idee de seqp est que il recoit abcisse et coordonne origine pour commencer,et il va rendre les coordonnes finale (int*int) *)
                  match co with                         (* la fonction evalpos2 peut obtenir cette coordonnée décalée via 'fst', 'snd'，cest a dire la nouvelle coordonnee est ((fst seqp ),(snd seqp)) *)
                  | Up -> if coor+1 >100  then failwith"la tortue sort du terrain" else if lst = [] then (abc,coor+1) else seqp (List.hd lst) (List.tl lst) abc (coor+1)
                  | Down -> if coor - 1 < -100 then failwith"la tortue sort du terrain" else if lst = [] then (abc,coor-1) else seqp (List.hd lst) (List.tl lst) abc (coor-1)
                  | Left -> if abc - 1 < -100  then failwith"la tortue sort du terrain" else if lst = [] then (abc-1,coor) else seqp (List.hd lst) (List.tl lst) (abc-1) coor
                  | Right -> if abc + 1 > 100  then failwith"la tortue sort du terrain" else if lst = [] then (abc+1,coor) else seqp (List.hd lst) (List.tl lst) (abc+1) coor
                  | Seq [] -> if lst = [] then (abc,coor) else seqp (List.hd lst) (List.tl lst) abc coor
                  | Seq (h::p) -> if lst = [] then ((fst (seqp h p abc coor)),(snd (seqp h p abc coor))) else seqp (List.hd lst) (List.tl lst) (fst (seqp h p abc coor)) (snd (seqp h p abc coor))
                   end
                   in if l!=[] then evalpos2 (List.hd l) (List.tl l) (fst (seqp t s abc coor)) (snd (seqp t s abc coor))  else ((fst (seqp t s abc coor)),(snd (seqp t s abc coor)))
                   (* seqp t s abc coor; *)
                  end
    in  evalpos2 o k 0 0; 
                     ;;

        let cl = Seq [Up;Seq[Up;Up;Seq[Up;Up;Up;Seq[Up;Up;Seq[];Up;Up;];Up];Up];Up;Up;Up;Up;Seq[Up;Up;Up]];;
        let cl2 = Seq [Up;Seq[Up;Left;Seq[Up;Up;Up;Seq[Up;Down;Seq[];Up;Up;];Right];Up];Up;Up;Up;Up;Seq[Up;Left;Up]];;
        let res = evalpos cl;;
        let res = evalpos cl2;;


(* --------------------------------------------------------------------------Question 5.2 *)


type command = Up | Down | Left | Right | Seq of (command list) | Repeter of command*int;;

let rec evalpos (c:command) =
  match c with
  | Up -> (0,1)
  | Down -> (0,-1)
  | Left -> (-1,0)
  | Right ->  (1,0)
  | Seq [] -> (0,0)
  | Repeter (c,fois) -> evalpos (Seq (List.init fois (fun _ -> c))) (* l'idee de repeter est juste faire une fois appel recusive pour redonne une command Seq[] avec le nombre 'fois' de commandes dedans *)
  | Seq  (o::k)->
 let rec evalpos2 co1 l abc coor =   (* evalpos2 est utilise pour calculer la liste principale *)
   begin
   match co1 with
   | Up -> if coor+1 >100  then failwith"la tortue sort du terrain" 
   else if l!=[] then evalpos2 (List.hd l) (List.tl l) abc (coor+1)
   else (abc,coor+1)
   | Down -> if coor - 1 < -100 then failwith"la tortue sort du terrain" 
   else if l!=[] then evalpos2 (List.hd l) (List.tl l) abc (coor-1)
   else (abc,coor-1)
   | Left -> if abc -1 < -100  then failwith"la tortue sort du terrain" 
   else if l!=[] then evalpos2 (List.hd l) (List.tl l) (abc-1) coor
   else (abc-1,coor)
   | Right -> if abc + 1 > 100  then failwith"la tortue sort du terrain" 
   else if l!=[] then evalpos2 (List.hd l) (List.tl l) (abc+1) coor
   else (abc+1,coor)
   | Seq [] -> if l!=[] then evalpos2 (List.hd l) (List.tl l) abc coor
   else (abc,coor)
   | Repeter (c,fois) ->  evalpos2 (Seq (List.init fois (fun _ -> c))) l abc coor 
   | Seq (t::s) -> let rec seqp co lst abc coor =    (* seqp est utilise pour calculer tous les decalage de coordonnee de tous les sous-liste(Seq) de la liste principale *)
                  begin                                 (* l'idee de seqp est que il recoit abcisse et coordonne origine pour commencer,et il va rendre les coordonnes finale (int*int) *)
                 match co with                         (* la fonction evalpos2 peut obtenir cette coordonnée décalée via 'fst', 'snd'，cest a dire la nouvelle coordonnee est ((fst seqp ),(snd seqp)) *)
                 | Up -> if coor+1 >100  then failwith"la tortue sort du terrain" else if lst = [] then (abc,coor+1) else seqp (List.hd lst) (List.tl lst) abc (coor+1)
                 | Down -> if coor - 1 < -100 then failwith"la tortue sort du terrain" else if lst = [] then (abc,coor-1) else seqp (List.hd lst) (List.tl lst) abc (coor-1)
                 | Left -> if abc - 1 < -100  then failwith"la tortue sort du terrain" else if lst = [] then (abc-1,coor) else seqp (List.hd lst) (List.tl lst) (abc-1) coor
                 | Right -> if abc + 1 > 100  then failwith"la tortue sort du terrain" else if lst = [] then (abc+1,coor) else seqp (List.hd lst) (List.tl lst) (abc+1) coor
                 | Seq [] -> if lst = [] then (abc,coor) else seqp (List.hd lst) (List.tl lst) abc coor
                 | Repeter (c,fois) ->  seqp (Seq (List.init fois (fun _ -> c))) lst abc coor
                 | Seq (h::p) -> if lst = [] then ((fst (seqp h p abc coor)),(snd (seqp h p abc coor))) else seqp (List.hd lst) (List.tl lst) (fst (seqp h p abc coor)) (snd (seqp h p abc coor))
                  end
                  in if l!=[] then evalpos2 (List.hd l) (List.tl l) (fst (seqp t s abc coor)) (snd (seqp t s abc coor))  else ((fst (seqp t s abc coor)),(snd (seqp t s abc coor)))
                  (* seqp t s abc coor; *)
                end
   in  evalpos2 o k 0 0; 
                    ;;
(* test *)
                    let cl = Seq [Up;Seq[Up;Up;Seq[Up;Up;Up;Seq[Up;Up;Seq[];Up;Up;];Up];Up];Up;Up;Up;Repeter (Up,5);Seq[Up;Up;Up]];;
                    let cl2 = Repeter (Seq[Left;Right],5);;
                    let cl3 = Repeter (Seq[Up;Down],5);;
                    let res = evalpos cl;;
                    let res = evalpos cl2;;
                    let res = evalpos cl3;;

(* --------------------------------------------------------------------------Question 5.3 *)


type command = Up | Down | Left | Right | Seq of (command list) | Repeter of command*int | Infini of command;;

let safety (c:command) =
let rec evalpos (c:command) =
  match c with
  | Up -> (0,1)
  | Down -> (0,-1)
  | Left -> (-1,0)
  | Right ->  (1,0)
  | Seq [] -> (0,0)
  | Repeter (c,fois) -> evalpos (Seq (List.init fois (fun _ -> c))) (* l'idee de repeter est juste faire une fois appel recusive pour redonne une command Seq[] avec le nombre 'fois' de commandes dedans *)
  | Infini c ->  begin
                      match c with
                      | Up -> failwith"la tortue sort du terrain" 
                      | Down -> failwith"la tortue sort du terrain" 
                      | Left -> failwith"la tortue sort du terrain" 
                      | Right ->  failwith"la tortue sort du terrain" 
                      | Seq [] -> (0,0)
                      | Infini c ->failwith"erreur" 
                      | Seq (o::k) -> let res = evalpos (Seq (o::k))  in  if res = (0,0) then (0,0) else failwith"la tortue sort du terrain"  (* if res <> (0,0) then failwith"la tortue sort du terrain" else (0,0) *)
                      | Repeter (c,fois) -> let res = evalpos c in if res != (0,0) then failwith"la tortue sort du terrain"  else (0,0)
                    end
  | Seq  (o::k)->
 let rec evalpos2 co1 l abc coor =   (* evalpos2 est utilise pour calculer la liste principale *)
   begin
   match co1 with
   | Up -> if coor+1 >100  then failwith"la tortue sort du terrain" 
   else if l!=[] then evalpos2 (List.hd l) (List.tl l) abc (coor+1)
   else (abc,coor+1)
   | Down -> if coor - 1 < -100 then failwith"la tortue sort du terrain" 
   else if l!=[] then evalpos2 (List.hd l) (List.tl l) abc (coor-1)
   else (abc,coor-1)
   | Left -> if abc -1 < -100  then failwith"la tortue sort du terrain" 
   else if l!=[] then evalpos2 (List.hd l) (List.tl l) (abc-1) coor
   else (abc-1,coor)
   | Right -> if abc + 1 > 100  then failwith"la tortue sort du terrain" 
   else if l!=[] then evalpos2 (List.hd l) (List.tl l) (abc+1) coor
   else (abc+1,coor)
   | Seq [] -> if l!=[] then evalpos2 (List.hd l) (List.tl l) abc coor
   else (abc,coor)
   | Repeter (c,fois) -> if l!=[] then evalpos2 (Seq (List.init fois (fun _ -> c))) l abc coor else evalpos2 (Seq (List.init fois (fun _ -> c))) [] abc coor
   | Infini c ->   begin
                    match c with
                    | Up -> failwith"la tortue sort du terrain" 
                    | Down -> failwith"la tortue sort du terrain" 
                    | Left -> failwith"la tortue sort du terrain" 
                    | Right ->  failwith"la tortue sort du terrain" 
                    | Seq [] -> (abc,coor)
                    | Infini c ->failwith"erreur" 
                    | Seq (o::k) -> let res = evalpos2 (Seq (o::k)) l abc coor in if res = (abc,coor) then (abc,coor)  else failwith"la tortue sort du terrain" 
                    | Repeter (c,fois) -> let res = evalpos2 c l abc coor in if res != (0,0) then failwith"la tortue sort du terrain"  else (0,0)
                  end
   | Seq (t::s) -> let rec seqp co lst abc coor =    (* seqp est utilise pour calculer tous les decalage de coordonnee de tous les sous-liste(Seq) de la liste principale *)
                  begin                                 (* l'idee de seqp est que il recoit abcisse et coordonne origine pour commencer,et il va rendre les coordonnes finale (int*int) *)
                 match co with                         (* la fonction evalpos2 peut obtenir cette coordonnée décalée via 'fst', 'snd'，cest a dire la nouvelle coordonnee est ((fst seqp ),(snd seqp)) *)
                 | Up -> if coor+1 >100  then failwith"la tortue sort du terrain" else if lst = [] then (abc,coor+1) else seqp (List.hd lst) (List.tl lst) abc (coor+1)
                 | Down -> if coor - 1 < -100 then failwith"la tortue sort du terrain" else if lst = [] then (abc,coor-1) else seqp (List.hd lst) (List.tl lst) abc (coor-1)
                 | Left -> if abc - 1 < -100  then failwith"la tortue sort du terrain" else if lst = [] then (abc-1,coor) else seqp (List.hd lst) (List.tl lst) (abc-1) coor
                 | Right -> if abc + 1 > 100  then failwith"la tortue sort du terrain" else if lst = [] then (abc+1,coor) else seqp (List.hd lst) (List.tl lst) (abc+1) coor
                 | Seq [] -> if lst = [] then (abc,coor) else seqp (List.hd lst) (List.tl lst) abc coor
                 | Repeter (c,fois) -> if lst = [] then seqp (Seq (List.init fois (fun _ -> c))) [] abc coor else seqp (Seq (List.init fois (fun _ -> c))) lst abc coor
                 | Infini c ->   begin
                                    match c with
                                    | Up -> failwith"la tortue sort du terrain" 
                                    | Down -> failwith"la tortue sort du terrain" 
                                    | Left -> failwith"la tortue sort du terrain" 
                                    | Right ->  failwith"la tortue sort du terrain" 
                                    | Seq [] -> (abc,coor)
                                    | Infini c ->failwith"erreur" 
                                    | Seq (o::k) -> let res = seqp (Seq (o::k)) lst abc coor in if res = (abc,coor) then (abc,coor)  else failwith"la tortue sort du terrain" 
                                    | Repeter (c,fois) -> let res = seqp c lst abc coor in if res != (0,0) then failwith"la tortue sort du terrain"  else (0,0)
                                  end
                 | Seq (h::p) -> if lst = [] then ((fst (seqp h p abc coor)),(snd (seqp h p abc coor))) else seqp (List.hd lst) (List.tl lst) (fst (seqp h p abc coor)) (snd (seqp h p abc coor))
                  end
                  in if l!=[] then evalpos2 (List.hd l) (List.tl l) (fst (seqp t s abc coor)) (snd (seqp t s abc coor))  else ((fst (seqp t s abc coor)),(snd (seqp t s abc coor)))
                  (* seqp t s abc coor; *)
                end
   in  evalpos2 o k 0 0; 
             (* in     if evalpos c = failwith"la tortue sort du terrain" || evalpos c = failwith"erreur" then  false else true  *)
                
                 (* in if fst(evalpos c) < 101 || fst(evalpos c) > -1 || snd(evalpos c) < 101 || snd(evalpos c) > -1 then true else false *)

            in
            try
             let (_,_)= evalpos c in true
            with failwith -> false;; 
    
    (* in evalpos c *)
  ;;




  let cl =  Up;;
  let cl6 = Infini (Seq[Left;Right;Left]);;
  let cl2 = Infini (Seq[Up;Down]);;
  let cl11 = Infini (Seq[Up;Down;Infini Up]);;
  let cl7 = Infini (Seq[Up;Seq[Down]]);;
  let cl3 = Infini  Up;;
  let cl4 = Infini (Seq[]);;
  let cl5 = Infini (Infini Up);;
  let cl12 = Seq[Up;Down;Down;Seq[Infini (Seq[Up;Down])]];;
 
   (* let res = safety cl2;;  *)
   let res = safety cl2;;
   let res = safety cl7;;
   let res = safety cl6;;
   let res = safety cl5;;
   let res = safety cl11;;
   let res = safety cl12;;
  (* try safety cl5 with
  |Failure "la tortue sort du terrain" -> (000,000)
  |Failure "erreur" -> (00000,00000);; *)










