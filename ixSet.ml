(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Indexed Set} *)

type 'a sequence = ('a -> unit) -> unit
type 'a ord = 'a -> 'a -> int

module type SET = IxSet_intf.SET
module type S = IxSet_intf.S
module type INDEX = IxSet_intf.INDEX

(* typeclass for sets *)
type _ def_set =
  | DefSet : (module SET with type elt = 'a and type t = 't) -> 'a def_set

(* definition of a set *)
let def_set (type a) compare =
  let module S = Set.Make(struct type t = a let compare = compare end) in
  DefSet (module S)

type _ set =
  | Set : (module SET with type elt = 'a and type t = 't) * 't -> 'a set

let mk_set (type a) (def : a def_set) : a set =
  let DefSet (module S) = def in
  Set ((module S), S.empty)

(** Typeclass for indices *)
type ('elt, 'key) def_index =
  | DefIndex :
    (module INDEX with type elt = 'elt and type key = 'key and type t = 't)
    -> ('elt, 'key) def_index

type ('elt, 'key) index =
  | Index :
      (module INDEX with type elt = 'elt and type key = 'key and type t = 't)
      * 't
      -> ('elt, 'key) index

let def_idx (type k) (type e) ~project ~compare (set:e def_set) : (e, k) def_index =
  let module M = Map.Make(struct
    type t = k
    let compare = compare
  end) in
  let DefSet (module S) = set in
  let module I = struct
    type t = S.t M.t
    type key = k
    type elt = e

    let empty = M.empty

    let add m x =
      let key = project x in
      let set = try M.find key m with Not_found -> S.empty in
      M.add key (S.add x set) m

    let remove m x =
      let key = project x in
      try
        let set = M.find key m in
        let set = S.remove x set in
        if S.is_empty set
          then M.remove key m
          else M.add key set m
      with Not_found -> m

    let find m k yield =
      try S.iter yield (M.find k m)
      with Not_found -> ()
  end in
  DefIndex (module I)

let def_idx_int ~project set =
  let compare (i:int) j = Pervasives.compare i j in
  def_idx ~project ~compare set

let def_idx_string ~project set =
  def_idx ~project ~compare:String.compare set

(* indice definitions *)
type (_,_) hlist =
  | Nil : ('elt, unit) hlist
  | Cons :
    ('elt, 'key) def_index
    * ('elt, 'tail) hlist
    -> ('elt, 'key * 'tail) hlist

(* indices (with their definitions) *)
type (_,_) indices_hlist =
  | INil : ('elt, unit) indices_hlist
  | ICons :
      ('elt, 'key) index
      * ('elt, 'tail) indices_hlist
      -> ('elt, 'key * 'tail) indices_hlist

(* access some index by its (Peano) number *)
type (_,_) idx_key =
  | KThere : (('a * 'b), 'a) idx_key
  | KNext : ('b, 'c) idx_key -> (('a * 'b), 'c) idx_key

let there = KThere
let next k = KNext k

let k0 = KThere
let k1 = KNext KThere
let k2 = KNext k1
let k3 = KNext k2

(** More accurate definition *)
module type IX_SET = sig
  include S

  val find : idx:(indices, 'k) idx_key -> t -> 'k -> elt sequence
end

type ('elt, 'indices) def =
  | DefIxSet :
    (module IX_SET with type elt = 'elt and type indices = 'indices and type t = 't)
    -> ('elt, 'indices) def

type ('elt, 'indices) t =
  | IxSet :
    (module IX_SET with type elt = 'elt and type indices = 'indices and type t = 't)
    * 't
    -> ('elt, 'indices) t

let def (type e) (type i) (dset:e def_set) (indices:(e,i) hlist) : (e,i) def =
  let module Res = struct
    type elt = e
    type indices = i

    type t = {
      indices : (e, i) indices_hlist;
      set : e set;
    }

    (* create empty indices from their definitions *)
    let rec indices_empty
      : type e i. (e,i) hlist -> (e,i) indices_hlist
      = function
      | Nil -> INil
      | Cons (DefIndex (module Def), tail) ->
          ICons(Index ((module Def), Def.empty), indices_empty tail)

    let empty = {
      indices = indices_empty indices;
      set = mk_set dset;
    }

    let rec indices_add
      : type e i. (e,i) indices_hlist -> e -> (e,i) indices_hlist
      = fun l x -> match l with
      | INil -> INil
      | ICons (Index ((module Def) as def, idx), tail) ->
          let idx = Def.add idx x in
          ICons (Index (def, idx), indices_add tail x)

    let add t x =
      let Set ((module S), s) = t.set in
      let set = Set ((module S), S.add x s) in
      let indices = indices_add t.indices x in
      { set; indices; }

    let rec indices_remove
      : type e i. (e,i) indices_hlist -> e -> (e,i) indices_hlist
      = fun l x -> match l with
      | INil -> INil
      | ICons (Index ((module Def) as def, idx), tail) ->
          let idx = Def.remove idx x in
          ICons (Index (def, idx), indices_remove tail x)

    let remove t x =
      let Set ((module S), s) = t.set in
      let set = Set ((module S), S.remove x s) in
      let indices = indices_remove t.indices x in
      { set; indices; }

    let rec indices_find
      : type e i k. (i,k) idx_key -> (e,i) indices_hlist -> k -> e sequence
      = fun idx l k -> match idx, l with
      | KThere, ICons (Index ((module Def), idx), _) -> Def.find idx k
      | KNext idx', ICons (_, tail) -> indices_find idx' tail k

    let find ~idx t k = indices_find idx t.indices k

    let iter set k = assert false
    let inter s1 s2 = assert false
    let union s1 s2 = assert false
    let size t =
      let Set ((module S), set) = t.set in
      S.cardinal set
    let to_list s = assert false
    let of_list s = assert false
    let to_seq s = assert false
    let of_seq s = assert false
  end in
  DefIxSet (module Res)

let make (type e) (type i) (def:(e,i) def) : (e,i) t =
  let DefIxSet (module I) = def in
  IxSet ((module I), I.empty)

let add (type e)(type i) (x:e) (s:(e,i) t) =
  let IxSet ((module I), set) = s in
  IxSet ((module I), I.add set x)

let remove (type e)(type i) (x:e) (s:(e,i) t) =
  let IxSet ((module I), set) = s in
  IxSet ((module I), I.remove set x)

let find (type e)(type k)(type i) ~(idx:(i,k) idx_key) (k:k) (s:(e,i)t) =
  let IxSet ((module I), set) = s in
  I.find ~idx set k

