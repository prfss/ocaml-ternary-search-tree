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

  let change tst path ~f =
    let new_end tst = function
      | None -> tst
      | Some v -> End (v, tst) in
    let new_branch c l m r =
      match l, m, r with
      | Null, Null, Null -> Null
      | _ -> Branch (c, l, m, r) in
    let rec change' tst path =
      match path, tst with
      | [], Null | [], Branch _ -> new_end tst (f None)
      | [], End (v, m) -> new_end m (f (Some v))
      | c :: path', Null -> new_branch c Null (change' Null path') Null
      | _, End (v, m) -> End (v, change' m path)
      | c :: path', Branch (c', l, m, r) ->
        match Char.compare c c' with
        | x when x < 0 -> new_branch c' (change' l path) m r
        | 0            -> new_branch c l (change' m path') r
        | _            -> new_branch c' l m (change' r path)
    in change' tst path

  let add ?(override=false) tst path ~value =
    change tst path (function | None -> Some value
                              | Some _ when override -> Some value
                              | otherwise -> otherwise)

  let rec remove tst path = change tst path (fun _ -> None)

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

  let map tst ~f =
    let rec map' rev_path = function
      | Null -> Null
      | End (v, m) -> End (f (List.rev rev_path) v, map' rev_path m)
      | Branch (c, l, m, r) ->
        Branch (c, map' rev_path l, map' (c :: rev_path) m, map' rev_path r)
    in map' [] tst

  let iter tst ~f =
    let rec iter' rev_path = function
      | Null -> ()
      | End (v, m) -> f (List.rev rev_path) v; iter' rev_path m
      | Branch (c, l, m, r) ->
        iter' rev_path l; iter' (c :: rev_path) m; iter' rev_path r
    in iter' [] tst

  let count tst = fold tst ~init:0 ~f:(fun c _ _ -> c + 1)

  let to_list tst = rev_fold tst ~init:[] ~f:(fun accum p v -> (p,v) :: accum)

  let to_iterator tst =
    let open OSeq.Generator in
    let rec aux rev_path = function
      | Null -> empty
      | End (v, m) -> yield (List.rev rev_path, v) >>= fun () -> aux rev_path m
      | Branch (c, l, m, r) ->
        aux rev_path l >>= fun () -> aux (c :: rev_path) m >>= fun () -> aux rev_path r
    in run (aux [] tst)

  let update ?default tst path ~f =
    change tst path ~f:(function | None -> default
                                 | Some v -> Some (f v))

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
