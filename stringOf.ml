(**
    The [StringOf] module is used to convert various data structures to [string]s that do not have a string_of function
    associated with it
    
    {{:https://stackoverflow.com/a/65052754/14135254} Credit to Shawn} for advice on generalization with fold for the
    {{:https://tinyurl.com/ocaml-hashtbl} Hashtbl.t} and {{:https://tinyurl.com/ocaml-map-s} Map.S.t} structures
*)

(**
    [string_of_char] converts a given [char] to a [string]
    @param c The [char] to convert
    @return The [string] form of [c]
*)
let string_of_char (c: char) = String.make 1 c

(**
    [string_of_pair] returns the given pair as a [string]
    @param string_of_fst The function to convert the first element of the pair to a [string]
    @param string_of_snd The function to convert the second element of the pair to a [string]
    @param fst           The first element of the pair
    @param snd           The second element of the pair
    @return The [string] form of the given pair
*)
let string_of_pair (string_of_fst: 'a -> string) (string_of_snd: 'b -> string) ((fst, snd): 'a * 'b): string =
    Printf.sprintf "(%s, %s)" (string_of_fst fst) (string_of_snd snd)

(**
    [string_of_triple] returns the given [* 'b * 'c] triple as a string
    @param string_of_fst The function to convert the first element of the triple to a [string]
    @param string_of_snd The function to convert the second element of the triple to a [string]
    @param string_of_trd The function to convert the third element of the triple to a [string]
    @param fst           The first element of the triple
    @param snd           The second element of the triple
    @param trd           The third element of the triple
    @return The [string] form of the given triple
*)
let string_of_triple (string_of_fst: 'a -> string) (string_of_snd: 'b -> string) (string_of_trd: 'c -> string)
((fst, snd, trd): 'a * 'b * 'c): string =
    Printf.sprintf "(%s, %s, %s)" (string_of_fst fst) (string_of_snd snd) (string_of_trd trd)

(** The type corresponding to the function passed to the fold function for {!string_of_foldable} *)
type 'a fold_string_fn_t = 'a -> string list -> string list

(** The type corresponding to the fold function for {!string_of_foldable} *)
type ('a, 'b) fold_string_t = 'a fold_string_fn_t -> 'b -> string list -> string list

(**
    [string_of_foldable] converts a data structure that has a fold function in the form
    [('a -> 'c -> 'c) -> 'b -> 'c -> 'c] to a [string] composed of element [string]s, concatenated with "; "
    @param string_of_el The function used to turn each element of the data structure to a [string]
    @param fold         The fold function in the aforementioned format to use with the given data structure
    @param ds           The data structure to convert to a [string]
    @return The [string] form of [ds]
*)
let string_of_foldable (string_of_el: 'a -> string) (fold: ('a, 'b) fold_string_t) (ds: 'b): string =
    let fold_string_fn (el: 'a) (cur_lst: string list): string list =
        (string_of_el el)::cur_lst
    in fold fold_string_fn ds [] |> String.concat "; "

(**
    [string_of_foldable_exp] converts a data structure that has a fold function in the form
    [('a -> 'c -> 'c) -> 'b -> 'c -> 'c] to a [string] composed of element [string]s, concatenated with ";\n"
    @param string_of_el The function used to turn each element of the data structure to a [string]
    @param fold         The fold function in the aforementioned format to use with the given data structure
    @param ds           The data structure to convert to a [string]
    @return The [string] form of [ds]
*)
let string_of_foldable_exp (string_of_el: 'a -> string) (fold: ('a, 'b) fold_string_t) (ds: 'b): string =
    let fold_string_fn (el: 'a) (cur_lst: string list): string list =
        ("    " ^ string_of_el el)::cur_lst
    in fold fold_string_fn ds [] |> String.concat ";\n"

(**
    [string_of_list] returns the given [list] as a [string]
    @param string_of_el The function used to turn each element of the [list] to a [string]
    @param lst          The [list] to convert to a [string]
    @return The given [list] as a string
*)
let string_of_list (string_of_el: 'a -> string) (lst: 'a list): string =
    string_of_foldable string_of_el List.fold_right lst |> Printf.sprintf "[%s]"

(**
    [string_of_list_exp] returns the given [list] as a [string] in expanded form
    @param string_of_el The function used to turn each element of the [list] to a [string]
    @param lst          The [list] to convert to a [string]
    @return The given [list] as a string in expanded form
*)
let string_of_list_exp (string_of_el: 'a -> string) (lst: 'a list): string =
    string_of_foldable_exp string_of_el List.fold_right lst |> Printf.sprintf "[\n%s\n]"

(**
    [string_of_array] returns given [array] as a [string]
    @param string_of_el The function used to turn each element of the [array] to a [string]
    @param arr          The [array] to convert to a [string]
    @return The given [array] as a [string]
*)
let string_of_array (string_of_el: 'a -> string) (arr: 'a array): string =
    string_of_foldable string_of_el Array.fold_right arr |> Printf.sprintf "[|%s|]"

(**
    [string_of_array_exp] returns given [array] as a [string] in expanded form
    @param string_of_el The function used to turn each element of the [array] to a [string]
    @param arr          The [array] to convert to a [string]
    @return The given [array] as a [string] in expanded form
*)
let string_of_array_exp (string_of_el: 'a -> string) (arr: 'a array): string =
    string_of_foldable_exp string_of_el Array.fold_right arr |> Printf.sprintf "[|\n%s\n|]"

(**
    The type corresponding to the function passed to the fold function for
    {{:https://tinyurl.com/ocaml-hashtbl} Hashtbl.t} and {{:https://tinyurl.com/ocaml-map-s} Map.S.t} for
    {!string_of_foldable_map_ds}
*)
type ('a, 'b, 'd) map_fold_fn_t = 'a -> 'b -> 'd -> 'd

(**
    The type corresponding to the fold function for {{:https://tinyurl.com/ocaml-hashtbl} Hashtbl.t} and
    {{:https://tinyurl.com/ocaml-map-s} Map.S.t} for {!string_of_foldable_map_ds}
*)
type ('a, 'b, 'c, 'd) map_fold_t = ('a, 'b, 'd) map_fold_fn_t -> 'c -> 'd -> 'd

(**
    [string_of_foldable_map_ds] returns the given map-like data structure (such as
    {{:https://tinyurl.com/ocaml-hashtbl} Hashtbl.t} and {{:https://tinyurl.com/ocaml-map-s} Map.S.t}) as a [string]
    @param string_of_key The function used to turn each key of the data structure to a [string]
    @param string_of_val The function used to turn each value of the data structure to a [string]
    @param map_fold      The function that can be used to fold [ds]
    @param ds            The map-like data structure to convert to a [string]
    @return The [string] form of [ds]
*)
let string_of_foldable_map_ds (string_of_key: 'a -> string) (string_of_val: 'b -> string)
(map_fold: ('a, 'b, 'c, 'd) map_fold_t) (ds: 'c): string =
    let map_fold_fn (key: 'a) (value: 'b) (cur_lst: ('a * 'b) list): ('a * 'b) list =
        (key, value)::cur_lst
    in map_fold map_fold_fn ds [] |>
        string_of_list (string_of_pair string_of_key string_of_val)

(**
    [string_of_foldable_map_ds_exp] returns the given map-like data structure (such as
    {{:https://tinyurl.com/ocaml-hashtbl} Hashtbl.t} and {{:https://tinyurl.com/ocaml-map-s} Map.S.t}) as a [string] in
    expanded form
    @param string_of_key The function used to turn each key of the data structure to a [string]
    @param string_of_val The function used to turn each value of the data structure to a [string]
    @param map_fold      The function that can be used to fold [ds]
    @param ds            The map-like data structure to convert to a [string]
    @return The [string] form of [ds] in expanded form
*)
let string_of_foldable_map_ds_exp (string_of_key: 'a -> string) (string_of_val: 'b -> string)
(map_fold: ('a, 'b, 'c, 'd) map_fold_t) (ds: 'c): string =
    let map_fold_fn (key: 'a) (value: 'b) (cur_lst: ('a * 'b) list): ('a * 'b) list =
        (key, value)::cur_lst
    in map_fold map_fold_fn ds [] |>
        string_of_list_exp (string_of_pair string_of_key string_of_val)

(**
    [string_of_hashtbl] returns given {!('a, 'b) Hashtbl.t} as a [string] in expanded form
    @param string_of_key The function used to turn each key of the {{:https://tinyurl.com/ocaml-hashtbl} Hashtbl.t} to a
                         [string]
    @param string_of_val The function used to turn each value of the {{:https://tinyurl.com/ocaml-hashtbl} Hashtbl.t} to
                         a [string]
    @param tbl           The {{:https://tinyurl.com/ocaml-hashtbl} Hashtbl.t} to convert to a [string]
    @return The given {!('a, 'b) Hashtbl.t} as a [string] in expanded form
*)
let string_of_hashtbl (string_of_key: 'a -> string) (string_of_val: 'b -> string) (tbl: ('a, 'b) Hashtbl.t): string =
    string_of_foldable_map_ds string_of_key string_of_val Hashtbl.fold tbl

(**
    [string_of_hashtbl_exp] returns given {!('a, 'b) Hashtbl.t} as a [string] in expanded form
    @param string_of_key The function used to turn each key of the {{:https://tinyurl.com/ocaml-hashtbl} Hashtbl.t} to a
                         [string]
    @param string_of_val The function used to turn each value of the {{:https://tinyurl.com/ocaml-hashtbl} Hashtbl.t} to
                         a [string]
    @param tbl           The {{:https://tinyurl.com/ocaml-hashtbl} Hashtbl.t} to convert to a [string]
    @return The given {!('a, 'b) Hashtbl.t} as a [string] in expanded form
*)
let string_of_hashtbl_exp (string_of_key: 'a -> string) (string_of_val: 'b -> string) (tbl: ('a, 'b) Hashtbl.t):
string =
    string_of_foldable_map_ds_exp string_of_key string_of_val Hashtbl.fold tbl

(**
    [StringOfSet] is a submodule functor that, given a module [S] that is implemented using
    {{:https://tinyurl.com/ocaml-set-make} Set.Make}, will create a module capable of converting instances of [S] to
    [string] form
    @param S Test
*)
module StringOfSet (S: Set.S) = struct
    (** The type of [S]'s set elements *)
    type elt = S.elt
    
    (** The type of [S] instances *)
    type t = S.t
    
    (**
        [string_of_set] returns the given [S.t] as a [string]
        @param string_of_el The function used to turn each element of the [S.t] to a [string]
        @param set          The [S.t] to convert to a [string]
        @return The given [list] as a string
    *)
    let string_of_set (string_of_el: elt -> string) (set: t): string =
        string_of_foldable string_of_el S.fold set |> Printf.sprintf "[%s]"
    
    (**
        [string_of_set_exp] returns the given [S.t] as a [string] in expanded form
        @param string_of_el The function used to turn each element of the [S.t] to a [string]
        @param set          The [S.t] to convert to a [string]
        @return The given [list] as a string in expanded form
    *)
    let string_of_set_exp (string_of_el: elt -> string) (set: t): string =
        string_of_foldable_exp string_of_el S.fold set |> Printf.sprintf "[\n%s\n]"
end

(**
    [StringOfMap] is a submodule functor that, given a module [M] that is implemented using
    {{:https://tinyurl.com/ocaml-map-make} Map.Make}, will create a module capable of converting instances of [M] to
    [string] form
*)
module StringOfMap (M: Map.S) = struct
    (** The type of [M]'s keys *)
    type key = M.key
    
    (** The type of [M]'s values *)
    type 'a t = 'a M.t
    
    (**
        [string_of_map] returns the given [M.t] as a [string]
        @param string_of_key The function used to turn each key of the [M.t] to a [string]
        @param string_of_val The function used to turn each value of the [M.t] to a [string]
        @param tbl           The map to convert to a [string]
        @return The given [M.t] as a [string]
    *)
    let string_of_map (string_of_key: key -> string) (string_of_val: 'a -> string) (map: 'a t) =
        string_of_foldable_map_ds string_of_key string_of_val M.fold map
    
    (**
        [string_of_map_exp] returns the given [M.t] as a [string] in expanded form
        @param string_of_key The function used to turn each key of the [M.t] to a [string]
        @param string_of_val The function used to turn each value of the [M.t] to a [string]
        @param tbl           The map to convert to a [string]
        @return The given [M.t] as a [string] in expanded form
    *)
    let string_of_map_exp (string_of_key: key -> string) (string_of_val: 'a -> string) (map: 'a t) =
        string_of_foldable_map_ds_exp string_of_key string_of_val M.fold map
end
