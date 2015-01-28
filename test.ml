#!/usr/bin/env ocaml

#use "topfind";;

#directory "_build";;
#load "indexed_set.cma";;

#require "ppx_deriving.std";;
#require "sequence";;

type person = { id: int; age: int; name: string } [@@deriving ord];;

module S = IxSet.Make(struct
  type t = person
  let compare = compare_person
end)

let idx_id = S.def_idx_int ~project:(fun {id}->id) ;;
let idx_age = S.def_idx_int ~project:(fun {age}->age) ;;
let idx_name = S.def_idx_string ~project:(fun {name}->name) ;;

let empty = S.make S.(Cons (idx_id, Cons (idx_age, Cons (idx_name, Nil)))) ;;

let p1 = {id=1; name="Jean-Charles Édouard" ; age=18} ;;
let p2 = {id=2; name="Chocolatine qui parle"; age=3};;

let set = S.of_list empty [p1; p2] ;;

assert (S.find ~idx:S.k0 1 set |> Sequence.to_list = [p1]);;
assert (S.find ~idx:S.k0 2 set |> Sequence.to_list = [p2]);;
assert (S.find ~idx:S.k1 3 set |> Sequence.to_list = [p2]);;

