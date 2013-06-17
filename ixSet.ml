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

module type S = sig
  type elt
    (** The type of the elements of the set *)

  type 'a index
    (** Indexes elements by a key of type 'a *)

  val idx_fun : cmp:('a -> 'a -> int) ->
                (elt -> 'a list) -> 'a index
    (** Index the elements by their image by the given function *)

  module IndexList : sig
    type t

    val nil : t
    val cons : 'a index -> t -> t
    val of_list : 'a index list -> t
  end

  type t
    (** The type for an indexed set *)

  val empty : IndexList.t -> t
    (** Empty set *)

  val add : t -> elt -> t
    (** Add an element to the set *)

  val remove : t -> elt -> t
    (** Remove an element to the set *)

  val by : t -> 'a index -> 'a -> elt list
    (** Select by key *)

  val filter : t -> 'a index -> ('a -> bool) -> elt list
    (** Only select elements whose given index satisfies the predicate *)

  val iter : t -> (elt -> unit) -> unit
    (** Iterate on all elements *)

  val to_list : t -> elt list

  val of_list : t -> elt list -> t

  val inter : t -> t -> t
    (** Set intersection. It will have the same indexes as the
        first argument. *)

  val union : t -> t -> t
    (** Set union. It will have the same indexes as the first argument. *)

  val group_by : t -> 'a index -> ('a * elt list) list
    (** Group by the given index *)

  val add_idx : t -> 'a index -> t
    (** Add an index on the fly *)

  val size : t -> int
    (** Number of elements *)
end

module Make(X : Set.OrderedType) = struct
  type elt = X.t

  (** Index for a set of values of type 'a by keys of type 'b *)
  class type ['a] index =
    object
      method embed : 'a index Univ.embedding
      method empty : 'a index
      method add : elt -> 'a index
      method remove : elt -> 'a index
      method select : 'a -> (elt -> unit) -> unit
      method keys : ('a -> unit) -> unit
    end

  (** Index each element by its image by this function *)
  let idx_fun (type k) ~(cmp : k ->  k -> int) (f : (elt -> k list)) =
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
          (make map' :> k index)
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
          (make map' :> k index)
        method select key f' =
          let es = try M.find key map with Not_found -> [] in
          List.iter f' es
        method keys f' =
          M.iter (fun k _ -> f' k) map
      end : k index) in
    make M.empty

  module Set = Set.Make(struct
    type t = X.t
    let compare = X.compare
  end)

  module IndexList = struct
    type t = cell list
    and cell =
      | Cell : ('a index Univ.embedding * Univ.t) -> cell

    let nil = []
    let cons idx l = (Cell (idx#embed, Univ.pack idx#embed idx)) :: l
    let of_list l = List.map (fun idx -> Cell (idx#embed, Univ.pack idx#embed idx)) l

    let empty l =
      List.map
        (fun (Cell (embed, univ)) -> match Univ.unpack embed univ with
          | None -> assert false
          | Some idx -> Cell (embed, Univ.pack embed idx#empty))
        l

    let add l e =
      List.map
        (fun (Cell (embed, univ)) -> match Univ.unpack embed univ with
          | None -> assert false
          | Some idx -> Cell (embed, Univ.pack embed (idx#add e)))
        l

    let remove l e =
      List.map
        (fun (Cell (embed, univ)) -> match Univ.unpack embed univ with
          | None -> assert false
          | Some idx -> Cell (embed, Univ.pack embed (idx#remove e)))
        l
  end

  (** An indexed set is a Set, plus a list of indexes. *)
  type t = {
    set : Set.t;
    indexes : IndexList.t;
  }

  (** Build an indexed set, using the given list of indexes *)
  let empty il =
    (* regular set *)
    let set = {
      set = Set.empty;
      indexes = il;
    } in
    set

  let by set idx key =
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

  let filter set idx p =
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
    Set.mem e set.set

  let add set e =
    let indexes = IndexList.add set.indexes e in
    { set=Set.add e set.set; indexes; }

  let remove set e =
    let indexes = IndexList.remove set.indexes e in
    { set=Set.remove e set.set; indexes; }

  let iter set f =
    Set.iter f set.set

  let to_list set =
    Set.elements set.set

  let of_list set l =
    List.fold_left add set l

  let inter s1 s2 =
    let indexes = IndexList.empty s1.indexes in
    (* compute intersection *)
    let set = Set.inter s1.set s2.set in
    (* update indexes *)
    let indexes = Set.fold
      (fun e indexes -> IndexList.add indexes e)
      set indexes
    in
    { set; indexes; }

let union s1 s2 =
    let indexes = IndexList.empty s1.indexes in
    (* compute union *)
    let set = Set.union s1.set s2.set in
    (* update indexes *)
    let indexes = Set.fold
      (fun e indexes -> IndexList.add indexes e)
      set indexes
    in
    { set; indexes; }

  let group_by set idx =
    failwith "not implemented"

  let add_idx set idx =
    let idx = Set.fold
      (fun e idx -> idx#add e)
      set.set idx
    in
    let indexes = IndexList.cons idx set.indexes in
    { set with indexes; }

  let size set =
    Set.cardinal set.set
end
