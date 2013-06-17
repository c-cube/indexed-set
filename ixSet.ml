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

(** {2 Universal type} *)
module Univ = struct
  (** This is largely inspired by https://ocaml.janestreet.com/?q=node/18 . *)

  type t = unit -> unit
   (** The universal type *)

  type 'a embedding = {
    pack : 'a -> t;           (** Pack a 'a into a univ value *)
    unpack : t -> 'a option;  (** Try to unpack the univ value into an 'a *)
  } (** Conversion between the universal type and 'a *)

  (** Create a new embedding. Values packed by a given embedding can
      only be unpacked by the same embedding. *)
  let embed () = 
    let id = ref () in  (* unique ID of the embedding *)
    let r = ref None in (* place to store values *)
    let pack a =        (* pack the 'a value into a new univ cell *)
      let o = Some a in
    (fun () -> r := o)
    in
    let unpack t =      (* try to extract the content of a univ cell *)
      r := None;
      t ();
      let a = !r in
      a
    in
    { pack; unpack; }

  let pack emb x = emb.pack x

  let unpack emb t = emb.unpack t
end

(** Index for a set of values of type 'a by keys of type 'b *)
class type ['a, 'b] index =
  object
    method embed : ('a, 'b) index Univ.embedding
    method empty : ('a, 'b) index
    method add : 'a -> ('a, 'b) index
    method remove : 'a -> ('a, 'b) index
    method select : 'b -> ('a -> unit) -> unit
    method keys : ('b -> unit) -> unit
  end

(** Index each element by its image by this function *)
let idx_fun (type k) (type e) ~(cmp : k ->  k -> int) (f : (e -> k list)) =
  let embed = Univ.embed () in
  let module M = Map.Make(struct
    type t = k
    let compare = cmp
  end) in
  (* make an index given the current map index -> element list *)
  let rec make map =
    (object
      method embed = embed
      method empty = make M.empty
      method add e =
        let keys = f e in
        let map' =
          List.fold_left
            (fun map key ->
              let es = try M.find key map with Not_found -> [] in
              M.add key (e::es) map)
            map keys
        in
        (make map' :> (e, k) index)
      method remove e =
        let keys = f e in
        let map' =
          List.fold_left
            (fun map key ->
              let es = try M.find key map with Not_found -> [] in
              let es' = List.filter
                (fun e' -> e != e')
                es in
              if es' = []
                then M.remove key map
                else M.add key es' map)
            map keys
        in
        (make map' :> (e, k) index)
      method select key f' =
        let es = try M.find key map with Not_found -> [] in
        List.iter f' es
      method keys f' =
        M.iter (fun k _ -> f' k) map
    end : (e, k) index) in
  make M.empty

class type ['a] set =
  object
    method add : 'a -> 'a set
    method remove : 'a -> 'a set
    method iter : ('a -> unit) -> unit
    method mem : 'a -> bool
    method size : int
  end

(** Polymorphic set *)
let mk_set (type elt) ~cmp =
  let module S = Set.Make(struct
    type t = elt
    let compare = cmp
  end) in
  (* the object encapsulating [set] *)
  let rec build set = object
    method add elt = build (S.add elt set)
    method remove elt = build (S.remove elt set)
    method iter f = S.iter f set
    method mem elt = S.mem elt set
    method size = S.cardinal set
  end in
  (build S.empty :> elt set)

module IndexList = struct
  type 'a t = 'a cell list
  and _ cell =
    | Cell : (('a, 'b) index Univ.embedding * Univ.t) -> 'a cell

  let nil = []
  let cons idx l = (Cell (idx#embed, Univ.pack idx#embed idx)) :: l
end

(** An indexed set is a Set, plus a list of indexes. *)
type 'a t = {
  cmp : 'a -> 'a -> int;
  set : 'a set;
  indexes : 'a IndexList.t;
}

(** Build an indexed set, using the given list of indexes *)
let mk_set ?(cmp=Pervasives.compare) il =
  (* regular set *)
  let set = {
    cmp;
    set = mk_set ~cmp;
    indexes = il;
  } in
  set

let get_eq set idx key =
  let rec lookup l = match l with
  | [] -> failwith "bad index for this set"
  | (IndexList.Cell (_, univ)) :: l' ->
    begin match Univ.unpack idx#embed univ with
    | None -> lookup l' (* not the index you are looking for *)
    | Some idx' ->
      let l = ref [] in
      idx'#select key (fun e -> l := e :: !l);
      !l
    end
  in lookup set.indexes

let get_filter set idx p =
  let rec lookup l = match l with
  | [] -> failwith "bad index for this set"
  | (IndexList.Cell (_, univ)) :: l' ->
    begin match Univ.unpack idx#embed univ with
    | None -> lookup l' (* not the index you are looking for *)
    | Some idx' ->
      let l = ref [] in
      idx'#keys
        (fun k ->
          if p k then idx'#select k (fun e -> l := e :: !l));
      !l
    end
  in lookup set.indexes

let mem set e =
  set.set#mem e

let add set e =
  let indexes = List.map
    (fun (IndexList.Cell (embed, univ)) ->
      match Univ.unpack embed univ with
      | None -> assert false
      | Some idx ->
        let idx' = idx#add e in
        IndexList.Cell (embed, Univ.pack embed idx'))
    set.indexes
  in
  { set with set=set.set#add e; indexes; }

let remove set e =
  let indexes = List.map
    (fun (IndexList.Cell (embed, univ)) ->
      match Univ.unpack embed univ with
      | None -> assert false
      | Some idx ->
        let idx' = idx#remove e in
        IndexList.Cell (embed, Univ.pack embed idx'))
    set.indexes
  in
  { set with set=set.set#remove e; indexes; }

let iter set f =
  set.set#iter f

let to_list set =
  let l = ref [] in
  set.set#iter (fun x -> l := x :: !l);
  !l

let inter s1 s2 =
  let indexes = List.map
    (fun (IndexList.Cell (embed, univ)) ->
      match Univ.unpack embed univ with
      | None -> assert false
      | Some idx -> IndexList.Cell (embed, Univ.pack embed idx#empty))
    s1.indexes
  in
  let new_set = ref (mk_set ~cmp:s1.cmp indexes) in
  iter s1
    (fun e -> if mem s2 e then new_set := add !new_set e);
  !new_set

let union s1 s2 =
  let new_set = ref s1 in
  iter s2
    (fun e -> if (not (mem !new_set e)) then new_set := add !new_set e);
  !new_set

let group_by set idx =
  failwith "not implemented"

let add_idx set idx =
  let idx = ref idx in
  set.set#iter (fun e -> idx := (!idx)#add e);
  { set with indexes = IndexList.cons !idx set.indexes; }

let size set =
  set.set#size
