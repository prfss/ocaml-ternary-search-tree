module type Comparable = sig
  type t
  val compare : t -> t -> int
end

module Pattern_match = struct
  type 'char t =
    | Literal of 'char
    | Wildcard
end

module Make(Char : Comparable) = struct
  type 'v node =
    | Branch of Char.t * 'v node * 'v node * 'v node
    | End of 'v * 'v node
    | Null

  type 'v t = 'v node


  let create = Null

  let add ?(override=false) tst path ~value =
    let rec add' tst path =
      match path, tst with
      | [], Null -> End (value, Null)
      | [], End (_, m) -> if override then End (value, m) else tst
      | [], Branch _ -> End (value, tst)
      | c :: path', Null -> Branch (c, Null, add' Null path', Null)
      | _, End (v, m) -> End (v, add' m path)
      | c :: path', Branch (c', l, m, r) ->
        match Char.compare c c' with
        | x when x < 0 -> Branch (c', add' l path, m, r)
        | 0            -> Branch (c, l, add' m path', r)
        | _            -> Branch (c', l, m, add' r path)
    in add' tst path

  let rec remove tst path =
    match path, tst with
    | [], Null | [], Branch _ -> tst
    | [], End (_, m) -> m
    | _, Null -> tst
    | _, End (v, m) -> End (v, remove m path)
    | c :: path', Branch (c', l, m, r) ->
      let new_nodes =
        match Char.compare c c' with
        | x when x < 0 -> remove l path, m, r
        | 0            -> l, remove m path', r
        | _            -> l, m, remove r path
      in match new_nodes with
      | Null, Null, Null -> Null
      | new_l, new_m, new_r -> Branch (c', new_l, new_m, new_r)

  let fold tst ~init ~f =
    let rec aux accum rev_path = function
      | Null -> accum
      | End (v, m) -> aux (f accum (List.rev rev_path) v) rev_path m
      | Branch (c, l, m, r) ->
        let accum_l = aux accum rev_path l in
        let accum_m = aux accum_l (c :: rev_path) m in
        aux accum_m rev_path r
    in aux init [] tst

  let rev_fold tst ~init ~f =
    let rec aux accum rev_path = function
      | Null -> accum
      | End (v, m) -> f (aux accum rev_path m) (List.rev rev_path) v
      | Branch (c, l, m, r) ->
        let accum_r = aux accum rev_path r in
        let accum_m = aux accum_r (c :: rev_path) m in
        aux accum_m rev_path l
    in aux init [] tst

  let count tst = fold tst ~init:0 ~f:(fun c _ _ -> c + 1)

  let to_list tst = rev_fold tst ~init:[] ~f:(fun accum p v -> (p,v) :: accum)

  let rec modify tst path ~f =
    match path, tst with
    | _, Null | [], Branch _ -> tst
    | [], End (v, m) -> End (f v, m)
    | _, End (v, m) -> End (v, modify m path ~f)
    | c :: path', Branch (c', l, m, r) ->
      match Char.compare c c' with
      | x when x < 0 -> Branch (c', modify l path ~f, m, r)
      | 0            -> Branch (c, l, modify m path' ~f, r)
      | _            -> Branch (c', l, m, modify r path ~f)

  let rec search tst path =
    match path, tst with
    | _, Null | [], Branch _ -> None
    | [], End (v, _) -> Some v
    | _, End (_, m) -> search m path
    | c :: path', Branch (c', l, m, r) ->
      match Char.compare c c' with
      | x when x < 0 -> search l path
      | 0            -> search m path'
      | _            -> search r path

  let mem tst path =
    match search tst path with
    | Some _ -> true
    | _ -> false

  let pm_search tst pattern =
    let open Pattern_match in
    let rec pm_search' tst pattern rev_path =
      match pattern, tst with
      | _, Null | [], Branch _ -> []
      | [], End (v, _) -> [List.rev rev_path, v]
      | _, End (_, m) -> pm_search' m pattern rev_path
      | p :: pattern', Branch (c, l, m, r) ->
        match p with
        | Wildcard ->
          let result_l = pm_search' l pattern rev_path in
          let result_m = pm_search' m pattern' (c :: rev_path) in
          result_l @ result_m @ pm_search' r pattern rev_path
        | Literal c' ->
          match Char.compare c' c with
          | x when x < 0 -> pm_search' l pattern rev_path
          | 0            -> pm_search' m pattern' (c :: rev_path)
          | _            -> pm_search' r pattern rev_path
    in pm_search' tst pattern []

  let near_search tst path distance =
    let rec near_search' tst path distance rev_path =
      if distance < 0 then
        []
      else match path, tst with
        | _, Null | [], Branch _ -> []
        | [], End (v, _) -> [List.rev rev_path, v]
        | _, End (_, m) -> near_search' m path distance rev_path
        | c :: path', Branch (c', l, m, r) ->
          let result_l = near_search' l path distance rev_path in
          let result_m = near_search' m path' (if Char.compare c c' = 0 then distance else distance - 1) (c' :: rev_path) in
          result_l @ result_m @ near_search' r path distance rev_path
    in near_search' tst path distance []

  let rec subtree tst path =
    match path, tst with
    | _, Null | [], Branch _ -> tst
    | [], End (_, m) -> m
    | _, End (_, m) -> subtree m path
    | c :: path', Branch (c', l, m, r) ->
      match Char.compare c c' with
      | x when x < 0 -> subtree l path
      | 0            -> subtree m path'
      | _            -> subtree r path

  let set_equal tst other = to_list tst = to_list other

end
