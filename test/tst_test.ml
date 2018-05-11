open Core
open QCheck

module Tst = Ternary_search_tree


let add_all ?(override=false) tst ls ~f = List.foldi ls ~init:tst ~f:(fun i tst l -> Tst.add tst l (f i l))

let add_all_unit ?(override=false) tst ls = add_all ~override tst ls ~f:(fun _ _ -> ())

let remove_all tst ls = List.fold ls ~init:tst ~f:(fun tst l -> Tst.remove tst l)

let unique l = List.(permute @@ dedup @@ sort ~cmp:Pervasives.compare l)

let rec replicate n x = if n <= 0 then [] else x :: replicate (n-1) x

let add_idempotency =
  Test.make ~count:100 ~name:"add idempotency"
    (list (list char))
    (fun ls ->
       let tst = add_all_unit Tst.create ls in
       add_all_unit tst ls = tst)

let add_override_false =
  Test.make ~count:100 ~name:"add override false"
    (triple (list char) int int)
    (fun (l,v1,v2) ->
       let tst = Tst.(add (add create l v1) l v2) in
       Option.value_exn (Tst.search tst l) = v1)

let add_override_true =
  Test.make ~count:100 ~name:"add override true"
    (triple (list char) int int)
    (fun (l,v1,v2) ->
       let tst = Tst.(add ~override:true (add create l v1) l ~value:v2) in
       Option.value_exn (Tst.search tst l) = v2)

let find_added =
  Test.make ~count:100 ~name:"search added members"
    (list (list char))
    (fun ls -> let tst = add_all_unit Tst.create ls in
      List.for_alli ls ~f:(fun _ l -> Tst.mem tst l))

let remove_non_member =
  Test.make ~count:100 ~name:"remove non-member"
    (list (list char))
    (fun ls ->
       let unique_list = unique ls in
       assume (unique_list <> []);
       let tst = add_all_unit Tst.create (List.tl_exn unique_list) in
       Tst.remove tst (List.hd_exn unique_list) = tst)

let remove_all =
  Test.make ~count:100 ~name:"remove all"
    (list (list char))
    (fun ls -> remove_all (add_all_unit Tst.create ls) ls = Tst.create)

let count =
  Test.make ~count:100 ~name:"count"
    (list (list char))
    (fun ls -> Tst.count (add_all_unit Tst.create ls) = Set.Poly.(length @@ of_list ls))

let to_list_sorted =
  Test.make ~count:100 ~name:"to_list order"
    (list (list char))
    (fun ls -> List.is_sorted ~compare @@ Tst.to_list @@ add_all_unit Tst.create ls)

let fold_visit =
  Test.make ~count:100 ~name:"fold visit order"
    (list (list char))
    (fun ls ->
       let l = Tst.fold (add_all_unit Tst.create ls) ~init:[] ~f:(fun l p _ -> p :: l) in
       List.is_sorted ~compare (List.rev l))

let rev_fold_visit =
  Test.make ~count:100 ~name:"rev_fold visit order"
    (list (list char))
    (fun ls ->
       let l = Tst.rev_fold (add_all_unit Tst.create ls) ~init:[] ~f:(fun l p _ -> p :: l) in
       List.is_sorted ~compare l)

let modify =
  Test.make ~count:100 ~name:"modify"
    (list (list char))
    (fun ls ->
       let f = fun v -> 10 * v in
       let unique_list = unique ls in
       let tst = add_all Tst.create unique_list ~f:(fun i _ -> i) in
       let tst' = List.fold unique_list ~init:tst ~f:(fun tst l -> Tst.modify tst l ~f) in
       List.for_alli (Tst.to_list tst') ~f:(fun _ (p,v) -> f (Option.value_exn (Tst.search tst p)) = v))

let set_equal =
  Test.make ~count:100 ~name:"set_equal"
    (list (list char))
    (fun ls -> Tst.set_equal (add_all_unit Tst.create ls) (add_all_unit Tst.create (List.rev ls)))

let pm_search_literal =
  Test.make ~count:100 ~name:"pm_search literal"
    (list (list char))
    (fun ls ->
       let patterns = List.(map ls ~f:(fun l -> map l ~f:(fun c -> Tst.Pattern_match.Literal c))) in
       let tst = add_all_unit Tst.create ls in
       List.for_alli patterns ~f:(fun i pat -> List.length (Tst.pm_search tst pat) = 1))

let pm_search_wildcard =
  Test.make ~count:100 ~name:"pm_search wildcard"
    (pair (list (list char)) small_int)
    (fun (ls,n) ->
       let unique_list = unique ls in
       let tst = add_all_unit Tst.create unique_list in
       let expected_count = List.(length @@ filter ~f:(fun x -> length x = max 0 n) unique_list) in
       List.length (Tst.pm_search tst (replicate n Tst.Pattern_match.Wildcard)) = expected_count)

let near_search_negative_distance =
  Test.make ~count:100 ~name:"near_search negative distance"
    (triple (list (list char)) (list char) int)
    (fun (ls,l,num) ->
       let tst = add_all_unit Tst.create ls in
       num < 0 ==> (Tst.near_search tst l num = []))

let near_search_distance_0 =
  Test.make ~count:100 ~name:"near_search distance 0"
    (list (list char))
    (fun ls ->
       let tst = add_all_unit Tst.create ls in
       List.for_alli ls ~f:(fun i l -> List.length (Tst.near_search tst l 0) = 1))

let near_search_change_all =
  Test.make ~count:100 ~name:"near_search change all elements"
    (pair (list (list char)) small_int)
    (fun (ls,n) ->
       let unique_list = unique ls in
       let tst = add_all_unit Tst.create unique_list in
       let count = List.(length @@ filter ~f:(fun x -> length x = max 0 n) unique_list) in
       List.length (Tst.near_search tst (replicate n 'x') n) = count)

let once ~name body = Test.make ~count:1 ~name unit body

let words = ["ban";"as";"like";"is";"bike";"it";"panama";"in";"rice";"banana";"dead";"beef";"deadly";"likely"]

let tst = words |> List.map ~f:String.to_list |> add_all_unit Tst.create

let to_pattern s =
  let open Tst.Pattern_match in
  let rec aux = function
    | [] -> []
    | '.' :: xs -> Wildcard :: aux xs
    | x :: xs -> Literal x :: aux xs
  in aux (String.to_list s)

let result_to_string_list = List.map ~f:(fun (l,_) -> String.of_char_list l)

let pm_search_1 =
  once
    ~name:"pm_search 1"
    (fun () -> (Tst.pm_search tst (to_pattern ".a.a.a") |> result_to_string_list)
               = ["banana";"panama"])

let pm_search_2 =
  once ~name:"pm_search 2"
    (fun () -> (Tst.pm_search tst (to_pattern ".ike") |> result_to_string_list)
               = ["bike";"like"])

let pm_search_3 =
  once ~name:"pm_search 3"
    (fun () -> (Tst.pm_search tst (to_pattern "i.") |> result_to_string_list)
               = ["in";"is";"it"])

let near_search_1 =
  once ~name:"near_search 1"
    (fun () -> (Tst.near_search tst (String.to_list "like") 1 |> result_to_string_list)
               = ["bike";"like"])

let near_search_2 =
  once ~name:"near_search 2"
    (fun () -> (Tst.near_search tst (String.to_list "like") 2 |> result_to_string_list)
               = ["bike";"like";"rice"])

let subtree_1 =
  once ~name:"subtree 1"
    (fun () ->
       Tst.(to_list @@ subtree tst (String.to_list "b") |> result_to_string_list)
       = ["an";"anana";"eef";"ike"])

let subtree_2 =
  once ~name:"subtree 2"
    (fun () ->
       Tst.(to_list @@ subtree tst (String.to_list "ba") |> result_to_string_list)
       = ["n";"nana"])


let () =
  QCheck_runner.run_tests_main [
    add_idempotency;
    add_override_false;
    add_override_true;
    find_added;
    remove_all;
    remove_non_member;
    count;
    to_list_sorted;
    fold_visit;
    rev_fold_visit;
    modify;
    set_equal;
    pm_search_literal;
    pm_search_wildcard;
    near_search_negative_distance;
    near_search_distance_0;
    near_search_change_all;
    pm_search_1;
    pm_search_2;
    pm_search_3;
    near_search_1;
    near_search_2;
    subtree_1;
    subtree_2
  ]
