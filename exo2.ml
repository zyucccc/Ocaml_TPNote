(* --------------------------------------------------------------------------Question 2.1 *)
let f1 (x , y) z =
  let g a b = a < b in
  if z then g x else g y ;;

  (* g a b = a < b donc le resultat renvoye par  'g a b ' est boolean,le type de g: 'a ->'a -> bool
  'if z' donc z est le type boolean,g x et g y boolean comme l‘interpretation precedente
  et z ,g x,g y ont les memes types -> (boolean)
  donc le resultat renvoye(z ou gx ou gy) par f1 est boolean
(x,y) est une struct binaire,le type de x et y est indifferent,x et y peut etre n'importe quel type
donc le type de x et y est type 'a (x,y doivent etre le meme type)
le type de f1:'a * 'a -> bool -> 'a -> bool
(x,y) est le type 'a*'a
z:boolean
(x,y):'a * 'a
resultat de f1:boolean *)


(* --------------------------------------------------------------------------Question 2.2 *)
let list_sum p = List.fold_left ( fun x y -> if p y then x + 1 else x ) 0;;

(* list_sum peut compter le nombre d'éléments  qui satisfont la fonction p dans une liste
p y renvoye un resultat boolean
au debut
acc est 0,x prend la valeur 0,et y est le premier element de la liste
si p y est vraie,donc acc + 1,cest a dire x+1 = 0+1=1
etape suivante 
on continue avec (x = 1),mantenant,y prendre le deuxieme element de la liste,
si p y est vraie,x+1=2,sinon,on va regarder le 3ieme element..........etc *)
(* types:
('a -> bool) -> 'a list -> int
le premier argument de list_sum est un fonction p,
pour p,il prend n'importe quel type de argument -> 'a,et renvoye le type bool ->bool
p->('a -> bool)
List.fold_left est utilise pour les listes,le type de la liste n'est pas spécifié
donc le 2ieme argument de list_sum est type 'a list
list_sum renvoye un entier qui reprensente le nombre de elements correspondantes -> int
donc le type de list_sums doit etre ('a -> bool) -> 'a list -> int *)

let list_or = List.fold_left ( fun x y -> x || y ) false ;;

(* list_or peut verifier si il y des elements vrais dans une liste,si il y a au moins un element qui veaut vrai,il renvoye vrai
sinon,il renvoye faux
acc est false,au debut
x prend la valeur de acc -> false,et y prend le premier element de la liste,si y est false,acc reserve la valeur false,
et il continue a regarder si le 2ieme,3ieme,4ieme .....element de la liste est vraie
Une fois que cette fonction trouve un élément vrai, le résultat est vrai *)
(* types:
bool list -> bool
list_or prend un seul argument,cest une liste,parce que List.fold_left est pour les listes
comme l'intrepretation precedente,acc est type bool, via ' x || y ' on peut savoir les elements dans cette liste
doivent etre type bool,cest a dire cest une bool liste
en fin cet fonction renvoye la valeur de acc(x),le type est bool
donc le type de list_or est bool list -> bool *)


(* ligne 4-6:  Bien type *)
(* let a = [];;  *)   (* ici a est type 'a list,car cest une liste vide *) (* bien typé *)
(* list_sum a ;; *) (* ici a est bien typé,parce que list_sum attends une liste de  type 'a list,mais cet fonction manque 
                    son premier argument->fonction:(('a -> bool)) *)(*  bien typé *)
(* list_or a ;; *)  (* ici list_or attends une liste de type bool list,a a type 'a,mais il peut etre considere comme bool list,a est bien typé ici *)
                      (* bien typé *)


(* ligne 8-10: Bien type*)
(* let a = ref [];; *) (* ici a est type '_a list ,il refere une memoire qui contient une valeur de type 'a list *)
                      (* bien typé *)
(* list_sum !a ;; *) (* ici list_sum attends une liste 'a list,parce que nous déreference le variables "a" et la mémoire qui contient 
                      la valeur de type 'a list,a est attribue le type 'a list;mais comme l'explication dans ligne 4-6,il aussi manque un argument*) 
                       (*  bien typé *)
(* list_or !a ;; *) (* ici list_or attends une liste de type bool liste, dans la ligne 9,a est attribue la type 'a list,'a liste peut etre considere comme bool liste,donc bien type ici *)
                       (* bien typé *)


(* --------------------------------------------------------------------------Question 2.3 *)
type 'a arbre_binaire =
  Feuille
  | Noeud of 'a * 'a arbre_binaire * 'a arbre_binaire ;;


  let rec map_arbre f = function 
     | Feuille -> Feuille
     | Noeud (v,l,r)-> Noeud (f v,map_arbre f l,map_arbre f r);;

     (* types:
     'a arbre_binaire -> ('a -> 'b) -> 'b arbre_binaire
     le premier argument est une arbre binaire-> 'a arbre_binaire
     le 2ieme argument est un fonction qui peut transformer une n'importe quel valeur à d'autre vAssert_failure
     donc le type de cet fonction est 'a->'b
     au final,map_arbre renvoye une arbre binaire qui est transforme-> 'b arbre_binaire *)

  let rec forall_arbre f = function
     | Feuille -> true
     | Noeud (v,l,r)-> if f v then true else false && forall_arbre f l && forall_arbre f r ;;
       (* types:
     'a arbre_binaire -> ('a -> bool) -> bool
     le premier argument est une arbre binaire-> 'a arbre_binaire
     le 2ieme argument est un fonction de type 'a->bool
     au final,si toutes les etiquettes verifient le predicat,forall_arbre envoye vrai,sinon false
     le resultat est type bool
     *)