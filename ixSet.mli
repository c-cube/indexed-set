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
  end

  type t
    (** The type for an indexed set *)

  val empty : IndexList.t -> t
    (** Empty set *)

  val add : t -> elt -> t
    (** Add an element to the set *)

  val remove : t -> elt -> t
    (** Remove an element to the set *)

  val get_eq : t -> 'a index -> 'a -> elt list
    (** Select by key *)

  val get_filter : t -> 'a index -> ('a -> bool) -> elt list
    (** Only select elements whose given index satisfies the predicate *)

  val iter : t -> (elt -> unit) -> unit
    (** Iterate on all elements *)

  val to_list : t -> elt list

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

module Make(X : Set.OrderedType) : S with type elt = X.t
