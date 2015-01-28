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

module type ORDERED = sig
  type t
  val compare : t ord
end

module type INDEX = sig
  type t

  type elt

  type key

  val empty : t
  (** Empty index *)

  val add : t -> elt -> t
  (** Add a binding *)

  val remove : t -> elt -> t
  (** Remove a binding *)

  val find : t -> key -> elt sequence
  (** Retrieve by key *)
end

module Make(X : ORDERED) = struct
  type elt = X.t

  module EltSet = Set.Make(X)

  (** Typeclass for indices *)
  type 'key def_index =
    | DefIndex :
      (module INDEX with type elt = elt and type key = 'key and type t = 't)
      -> 'key def_index

  type 'key index =
    | Index :
        (module INDEX with type elt = elt and type key = 'key and type t = 't)
        * 't
        -> 'key index

  let def_idx (type k) ~project ~compare : k def_index =
    let module M = Map.Make(struct
      type t = k
      let compare = compare
    end) in
    let module I = struct
      type t = EltSet.t M.t
      type key = k
      type elt = X.t

      let empty = M.empty

      let add m x =
        let key = project x in
        let set = try M.find key m with Not_found -> EltSet.empty in
        M.add key (EltSet.add x set) m

      let remove m x =
        let key = project x in
        try
          let set = M.find key m in
          let set = EltSet.remove x set in
          if EltSet.is_empty set
            then M.remove key m
            else M.add key set m
        with Not_found -> m

      let find m k yield =
        try EltSet.iter yield (M.find k m)
        with Not_found -> ()
    end in
    DefIndex (module I)

  let def_idx_int ~project =
    let compare (i:int) j = Pervasives.compare i j in
    def_idx ~project ~compare

  let def_idx_string ~project =
    def_idx ~project ~compare:String.compare

  (* indice definitions *)
  type _ hlist =
    | Nil : unit hlist
    | Cons : 'key def_index * 'tail hlist -> ('key * 'tail) hlist

  (* indices (with their definitions) *)
  type _ indices_hlist =
    | INil : unit indices_hlist
    | ICons : 'key index * 'tail indices_hlist -> ('key * 'tail) indices_hlist

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

  (** Main type *)
  type 'i t = {
    indices : 'i indices_hlist;
    set : EltSet.t;
  }

  (* create empty indices from their definitions *)
  let rec indices_empty
    : type i. i hlist -> i indices_hlist
    = function
    | Nil -> INil
    | Cons (DefIndex (module Def), tail) ->
        ICons(Index ((module Def), Def.empty), indices_empty tail)

  let make i = {
    indices = indices_empty i;
    set = EltSet.empty;
  }

  let rec indices_add
    : type i. i indices_hlist -> elt -> i indices_hlist
    = fun l x -> match l with
    | INil -> INil
    | ICons (Index ((module Def) as def, idx), tail) ->
        let idx = Def.add idx x in
        ICons (Index (def, idx), indices_add tail x)

  let add x t =
    let set = EltSet.add x t.set in
    let indices = indices_add t.indices x in
    { set; indices; }

  let rec indices_remove
    : type i. i indices_hlist -> elt -> i indices_hlist
    = fun l x -> match l with
    | INil -> INil
    | ICons (Index ((module Def) as def, idx), tail) ->
        let idx = Def.remove idx x in
        ICons (Index (def, idx), indices_remove tail x)

  let remove x t =
    let set = EltSet.remove x t.set in
    let indices = indices_remove t.indices x in
    { set; indices; }

  let rec indices_find
    : type i j k. (i,k) idx_key -> i indices_hlist -> k -> elt sequence
    = fun idx l k -> match idx, l with
    | KThere, ICons (Index ((module Def), idx), _) -> Def.find idx k
    | KNext idx', ICons (_, tail) -> indices_find idx' tail k

  let find ~idx k t = indices_find idx t.indices k

  let iter k t = EltSet.iter k t.set

  let size t = EltSet.cardinal t.set

  let to_list t = EltSet.elements t.set

  let of_list init l = List.fold_left (fun set x -> add x set) init l

  let to_seq t yield = iter yield t

  let of_seq init seq =
    let set = ref init in
    seq (fun x -> set := add x !set);
    !set
end
