(* --------------------------------------------------------------------------Question 3.1 *)
type grid = int list list;;

let g_example = [[-2;0;1;4]; [7;2;-3;-4];[6;-1;3;5]];;

(* --------------------------------------------------------------------------Question 3.2 *)
let rec height (g:grid) =
  match g with
| [] -> failwith "grid vide!"
| t::s -> 
begin
  let rec long lst =
  match lst with
  | [] -> 0
  | h::p -> 1 + long p
  in long t
end
;;
let res = height g_example;;

(* --------------------------------------------------------------------------Question 3.3 *)
 let  wf_grid_exn (g:grid) =
  match g with
| [] -> failwith "grid vide!"
| t::s -> 
let acc = height g in 
let rec wf_grid_exn2 (g:grid)  =   (* En général, cette fonction ne renvoie pas nécessairement un résultat, donc *)                               
  match g with
  | [] -> ()
  | t::s ->                       (*  son type de retour est unit,mais si nous utilisons la fonction originale  *)          
                              (* (wf_grid_exn)pour la récursion,Lorsque nous récurons jusqu'au dernier élément, *) 
  let equal acc lst =        (*  cest sur que il va retourner une exception, car le dernier élément est toujours un [] *)
    begin
    match lst with                (* pour eviter le cas comme ca,Ici, j'ai créé une fonction wf_grid_exn2 qui ne cible que  *)
    | []-> failwith "grille non formee"    (* les listes non vide pour la récursion *)
    | h::p -> let longeur = List.length lst in
            if longeur = acc then wf_grid_exn2 s 
                              else failwith "grille non formee"
    end
    in equal acc t
    in wf_grid_exn2 g
;;
(* test *)
let g_example2 = [[-2;0;1;4]; [7;2;-3;-4];[6;-1;3]];;
let g_example3 = [[-2;0;1;4]; [6;-1;3]];;
let res = wf_grid_exn g_example;;  
 (* res = wf_grid_exn g_example2;;  *)


(* --------------------------------------------------------------------------Question 3.4 *)
(* max：4+7+6=17 *)
(* on choisit la case 4 dans le 1er colone,d'apres l'enonce de chemin,on peut continuer a la colone 2 que par les case  7,-3,-4
ensuite on prend la case qui contient la valeur le plus grand,cest la 7,on continue a  la colone 3 par les cases des 6 5 -1.On choisit le plus grand
valeur dans ces 3 cases,cest 6
Donc la somme au final est 4+7+6=17 *)

(* --------------------------------------------------------------------------Question 3.5 *)
let rotate_up lst =
  match lst with
  | [] -> failwith "liste vide!"
  | t::s ->
    let acc = t in
    let rec donne lst =
      begin
      match lst with
      |[]-> acc::[]
      |h::p ->h::(donne p)
    end
    in donne s
;;
    (* test *)
    let lst = [1;2;3];;
    let res = rotate_up lst;;
  
(* --------------------------------------------------------------------------Question 3.6 *)
let rotate_down lst =
  match lst with
  | [] -> failwith "liste vide!"
  | t::s ->
  let lst' = List.rev lst in
  let acc = List.hd lst' in
  let lst''=List.tl lst' in
  let lst''' = List.rev lst'' in
  match lst''' with
  |[] -> acc::[]
  |t::s -> acc::lst'''
  ;;
  (* test *)
  let lst = [1;2;3;6;5;2];;
  let res = rotate_down lst;;

(* --------------------------------------------------------------------------Question 3.7 *)
let best_option (lst:int list) =
  match lst with
  |[] -> []
  |t::[s] -> List.init 2 (fun _ -> max t s)
  |t::s ->
   let max (a:int) (b:int) (c :int)=
    if a >= b then if a >= c then a
                  else c
             else if b >= c then b
             else c
      in  
      let lst2 = rotate_down lst in          
      let lst3 = List.rev (rotate_up lst) in
      let debut = (max (List.hd lst2) (List.hd (List.tl lst2)) (List.hd (List.tl(List.tl lst2)))) in
      let fin = (max (List.hd lst3) (List.hd (List.tl lst3)) (List.hd (List.tl(List.tl lst3)))) in  (* Nous isolons deux cas particuliers, l'élément de début et l'élément de fin, car ce sont les deux seuls cas où le nombre de lignes n'est pas consécutif. *)
         let rec res lst acc = 
           if acc = 1 then [fin]      (* acc est utilise pour distinguer les 2 cas particuliers,cest a dire si on vient de commencer,et si on va terminer *)
           else if acc = (List.length lst2) then debut::(res lst (acc-1))
           else 
           (max (List.hd lst) (List.hd (List.tl lst)) (List.hd (List.tl (List.tl lst))))::(res (List.tl lst) (acc-1))
          
           in res (lst) (List.length lst2)
           ;;

 let lst = [1;2;3;4];;
 let res = best_option lst;;
 let lst = [1;2;3;6;5;2];;
 let res = best_option lst;;
(* --------------------------------------------------------------------------Question 3.8 *)

let sums (g:grid) =
  let rec som lst1 lst2 =
    if lst1 = [] || lst2=[] then [] else
    ((List.hd lst1) + (List.hd lst2))::som (List.tl lst1) (List.tl lst2) in
    let rec comparer (lst1:int list) (lst2:int list) =
      if lst1 = [] || lst2=[] then [] else
      (max (List.hd lst1) (List.hd lst2))::comparer (List.tl lst1) (List.tl lst2)
      in
  let rec res g vale acc=     (* acc enregistre les grands valeur des chemins partiel dans notre processus de calcul *)
    match g with
    | [] ->  vale 
    | t::[s] ->   comparer vale (som (best_option acc) s)  
    | t::s ->   res s (comparer (vale) (som (best_option acc) (List.hd s))) (som (best_option acc) (List.hd s))
    in res g (List.hd g) (List.hd g)
  ;;
let res = sums g_example;;
(* val res : int list = [17; 10; 6; 16] *)
(* --------------------------------------------------------------------------Question 3.9 *)
let max_list (lst:int list) =
  match lst with
  | []-> failwith "liste vide!"
  | [x] -> x
  | t::s ->
  let rec maxl lst =
    begin
    match lst with
      | []->0
      | h::p -> max h (maxl p)
    end
    in maxl lst
    ;;

(* test *)
let lst = [1;2;3;6;5;2];;
let res = max_list lst;;
    
(* --------------------------------------------------------------------------Question 3.10 *)
let solve (g:grid) =
  max_list (sums g);;

  let res = solve g_example;;
  (* val res : int = 17 *)