#!/usr/bin/env ocaml

#use "topfind";;

#directory "_build";;
#load "indexed_set.cma";;

#require "ppx_deriving.std";;
#require "sequence";;

type t = { id: int; name: string } [@@deriving ord];;
let set_t = IxSet.def_set compare;;
let idx1 = IxSet.def_idx_int ~project:(fun {id}->id) set_t;;
let idx2 = IxSet.def_idx_string ~project:(fun {name}->name) set_t;;
let def = IxSet.def set_t IxSet.(Cons (idx1, Cons (idx2, Nil))) ;;

let p1 = {id=1; name="1"} ;;
let p2 = {id=2; name="2"};;

let set = IxSet.make def |> IxSet.add p1 |> IxSet.add p2;;

assert (IxSet.find ~idx:IxSet.KThere 1 set |> Sequence.to_list = [p1]);;
assert (IxSet.find ~idx:IxSet.KThere 2 set |> Sequence.to_list = [p2]);;
assert (IxSet.find ~idx:IxSet.KThere 3 set |> Sequence.to_list = []);;
assert (IxSet.find ~idx:IxSet.k1 "1" set |> Sequence.to_list = [p1]);;

