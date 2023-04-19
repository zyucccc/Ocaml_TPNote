(* --------------------------------------------------------------------------Question 4.1 *)
let rec fibo (val1:int) =
  if val1 < 0 then failwith "argument negatif"
  else 
  match val1 with
  | 0 -> 0
  | 1 -> 1
  | x -> fibo (val1-1) + fibo(val1-2)
  ;;

  (* test *)
  (* f2=1 f3=2 *)
  let res = fibo 3;;

  (* --------------------------------------------------------------------------Question 4.2 *)
  let count = ref 0;;
  let rec fibocount (val1:int) =
    count := !count +1;
    if val1 < 0 then failwith "argument negatif"
    else 
    match val1 with
    | 0 -> 0
    | 1 -> 1
    | x -> fibocount (val1-1) + fibocount(val1-2)
    ;;
  for i=0 to 20 do
    count := 0;
    let res = fibocount i in Printf.printf "fibocount %d ,res= %d,count = %d\n" i res !count

  done;;
(* 
fibocount 0 ,res= 0,count = 1
fibocount 1 ,res= 1,count = 1
fibocount 2 ,res= 1,count = 3
fibocount 3 ,res= 2,count = 5
fibocount 4 ,res= 3,count = 9
fibocount 5 ,res= 5,count = 15
fibocount 6 ,res= 8,count = 25
fibocount 7 ,res= 13,count = 41
fibocount 8 ,res= 21,count = 67
fibocount 9 ,res= 34,count = 109
fibocount 10 ,res= 55,count = 177
fibocount 11 ,res= 89,count = 287
fibocount 12 ,res= 144,count = 465
fibocount 13 ,res= 233,count = 753
fibocount 14 ,res= 377,count = 1219
fibocount 15 ,res= 610,count = 1973
fibocount 16 ,res= 987,count = 3193
fibocount 17 ,res= 1597,count = 5167
fibocount 18 ,res= 2584,count = 8361
fibocount 19 ,res= 4181,count = 13529
fibocount 20 ,res= 6765,count = 21891
*)
(* d'aprs ces resultats
on peut deduit que *)
(* 
indice count
0      1
1      1
2      3=1+1     +1
3      5=1+3     +1
4      9=3+5     +1
5      15=5+9    +1
.......
n  count n=count(n-1)+count(n-2) +1
*)
(* Donc pour Fn,le nombre d'appel à fibocount(count n) est la somme de count(n-1)+count(n-2) +1 
   



*)

 (* --------------------------------------------------------------------------Question 4.3 *)

 let f = Array.make (1024) (-1);;
 let rec memofibocount (val1:int) =
  if val1 < 0 then failwith "argument negatif"
  else 
  match val1 with
  | 0 -> 0
  | 1 -> 1
  | x -> fibocheck (val1-1) + fibocheck (val1-2)
  and
  fibocheck (val2:int) =
  if f.(val2) = -1 then f.(val2)
  else
  let res = memofibocount val2 in
  Array.set f val2 res;
  res;
  ;;
(* test *)
let res = memofibocount 3;;

 (* --------------------------------------------------------------------------Question 4.4 *)
 let f = Array.make (1024) (-1);;
 let rec memofibocount (val1:int) =
  count := !count +1;
  if val1 < 0 then failwith "argument negatif"
  else 
  match val1 with
  | 0 -> 0
  | 1 -> 1
  | x -> fibocheck (val1-1) + fibocheck (val1-2)
  and
  fibocheck (val2:int) =
  if f.(val2) != -1 then f.(val2)
  else
  let res = memofibocount val2 in
  Array.set f val2 res;
  res;
;;

count := 0;let res = memofibocount 5 in Printf.printf "memofibocount %d ,resultat = %d count = %d\n" 5 res !count;;
   (* count: 6         *)
 count := 0; for i = 0 to 1023 do Array.set f (i) (-1) done;;
count := 0;let res = memofibocount 167 in Printf.printf "memofibocount %d ,resultat = %d count = %d\n" 167 res !count;;
(* count:168        *)
count := 0; for i = 0 to 1023 do Array.set f (i) (-1) done;;
count := 0;let res = memofibocount 333 in Printf.printf "memofibocount %d ,resultat = %d count = %d\n" 333 res !count;;
(* count:334            *)
count := 0; for i = 0 to 1023 do Array.set f (i) (-1) done;;
 count := 0;let res = memofibocount 888 in Printf.printf "memofibocount %d ,resultat = %d count = %d\n" 888 res !count;;
(* count: 889          *)
(* d'apres les resultat de cette partie,on peut savoir Si nous exécutons cette fonction UNE SEULE FOIS(memofibocount n) pour le tableau alloue initialise(tous les valeur valent -1)，
   le nombre de count = n+1 *)


(*********** Mais le résultat de nombre de count changera si nous exécutons cette fonction plusieurs fois sans reinitialiser le tableau alloue f*************************)


  count := 0;for i = 0 to 1023 do Array.set f (i) (-1) done;;
  let res = memofibocount 200 in Printf.printf "memofibocount %d ,resultat = %d count = %d\n" 200 res !count;;
  (* count=201 *)
  count := 0;let res = memofibocount 100 in Printf.printf "memofibocount %d ,resultat = %d count = %d\n" 100 res !count;;
  (* count=1 *)
  count := 0;let res = memofibocount 500 in Printf.printf "memofibocount %d ,resultat = %d count = %d\n" 500 res !count;;
  (* count=301 *)
  (* on regarde cette example,si on exectute cette fonction 2 fois,le premier fois avec n=200,le nombre de count est de 201,
  ce résultat est cohérent avec notre conclusion------count = n+1
  mais le 2ieme fois,avec n =100,le nombre de count = 1,cest parce que Lorsque nous exécutons la fonction pour la première fois,
   les 200 premiers positions du tableau f est déjà rempli des données, le 2ieme fois avec n =100<200,donc cet fois on n'a pas besoin
   de appel memofibocount pour reobtenir les valeurs, donc on a besoin de appel 1 seul fois memofibocount parce que fibocheck va
   rendre les valeurs dans le tableau directement


    Cest donc la différence de comportement par rapport à 4.1, à 4.1,on n'a pas alloue un tableau,le nombre de fois que nous exécutons la fonction
   n'a aucun effet sur le résultat.
   mais ici il y a un tableau,Si la valeur à cette position dans ce tableau est déjà remplie, nous n'avons pas besoin d'appeler memofibocount,fibocheck
   donner directement la valeur


   conclusion:
   le nombre de count n est toujours égal au nombre de fois que nous comptons la valeur non comptée plus 1

   par example
   memofibocount 200,count = 201
   memofibocount 100  count =1
   memofibocount 500 count =301  car le nombre de fois que on compte la valeur non comptée = 500-200=300,alors count n = 300+1 =301
     
  
  
   *)
 






  




