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

type ('a, 'b) index
  (** Indexes elements of type 'a by keys of type 'b *)

val idx_fun : cmp:('b -> 'b -> int) ->
              ('a -> 'b list) -> ('a, 'b) index
  (** Index the elements by their image by the given function *)

module IndexList : sig
  type 'a t

  val nil : 'a t
  val cons : ('a, 'b) index -> 'a t -> 'a t
end

type 'a t
  (** The type for an indexed set *)

val mk_set : ?cmp:('a -> 'a -> int) -> 'a IndexList.t -> 'a t
  (** Create a set indexed by the given list of indexes,
      and also by the comparison function (like a regular set) *)

val add : 'a t -> 'a -> 'a t
  (** Add an element to the set *)

val remove : 'a t -> 'a -> 'a t
  (** Remove an element to the set *)

val get_eq : 'a t -> ('a, 'b) index -> 'b -> 'a list
  (** Select by predicate *)

val get_filter : 'a t -> ('a, 'b) index -> ('b -> bool) -> 'a list
  (** Only select elements whose given index satisfies the predicate *)

val iter : 'a t -> ('a -> unit) -> unit
  (** Iterate on all elements *)

val to_list : 'a t -> 'a list

val inter : 'a t -> 'a t -> 'a t
  (** Set intersection. It will have the same indexes as the
      first argument. *)

val union : 'a t -> 'a t -> 'a t
  (** Set union. It will have the same indexes as the first argument. *)

val group_by : 'a t -> ('a, 'b) index -> ('b * 'a list) list
  (** Group by the given index *)

val add_idx : 'a t -> ('a, 'b) index -> 'a t
  (** Add an index on the fly *)

val size : 'a t -> int
  (** Number of elements *)
