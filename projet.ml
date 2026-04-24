type eb = X of int | Vrai | Faux | Et of eb * eb | Ou of eb * eb | Non of eb;;
type valeur = Vrai | Faux;;
type env = (int * valeur) list;;
type equation = eb * eb;;
type systeme= equation list;;

let rec append list1 list2 = match list1 with
  | [] -> list2
  | x::ll1 -> x :: append ll1 list2;;

let rec appartient x list1 = match list1 with
  | [] -> false
  | y::ll1 -> if x = y then true else appartient x ll1;;

let rec union_2 list1 list2 = match list1 with
  | [] -> list2
  | x::ll1 -> if appartient x list2
      then union_2 ll1 list2
      else x :: union_2 ll1 list2;;

let rec det_var eb = match eb with
  | X b -> b::[]
  | Vrai->[]
  | Faux->[]
  | Non x -> det_var x
  | Ou(x1, x2) -> union_2 (det_var x1) (det_var x2)
  | Et(x1, x2) -> union_2 (det_var x1) (det_var x2);;

let det_var_eq (e1,e2) =
  union_2 (det_var e1) (det_var e2);;

let rec det_list_var systeme = match systeme with
  | [] -> []
  | eq::ll -> union_2 (det_var_eq eq) (det_list_var ll);;

let rec ajouter_devant elem liste_de_listes =
  match liste_de_listes with
  | [] -> []
  | l::ll -> (elem::l)::(ajouter_devant elem ll);;

let temp list a b =
  let avec_a = ajouter_devant a list in
  let avec_b = ajouter_devant b list in
  append avec_a avec_b;;

let rec cons_list_env_cpl list = match list with
  | [] -> [[]]
  | x::ll ->
      let reste = cons_list_env_cpl ll in
      let a = (x, Vrai) in
      let b = (x, Faux) in
      temp reste a b;;

let var_to_bool var =match var with
  |Vrai->true
  |Faux->false;;

let bool_to_var bool=match bool with
  |false->Faux
  |true->Vrai;;

let rec eval eb env = match eb with
  | X b -> List.assoc b env
  | Vrai -> Vrai
  | Faux -> Faux
  | Et(x1, x2) -> bool_to_var (var_to_bool(eval x1 env) && var_to_bool(eval x2 env))
  | Ou(x1, x2) -> bool_to_var (var_to_bool(eval x1 env) || var_to_bool(eval x2 env))
  | Non x1 -> bool_to_var(not (var_to_bool(eval x1 env)));;

let satisfies_equation env (x1,x2) =
  eval x1 env = eval x2 env;;

let satisfies_system env system =
  List.for_all (satisfies_equation env) system;;

let rec satisfies_system_allenv list_env system= match list_env with
  |[]->[]
  |[[]]->[]
  |env::ll_env->if (satisfies_system env system=true)
      then env::(satisfies_system_allenv ll_env system)
      else (satisfies_system_allenv ll_env system);;

      let main system =
        satisfies_system_allenv (cons_list_env_cpl (det_list_var( system ) ) ) system 