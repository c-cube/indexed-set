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
(** Comparison function *)

module type ORDERED = sig
  type t
  val compare : t ord
end

module Make(X : ORDERED) : sig
  type elt = X.t

  type 'key def_index
  (** The definition of a secondary index *)

  val def_idx : project:(elt -> 'key) ->
                compare:'key ord ->
                'key def_index
  (** Definition of an index. It allows to access values of type elt
      by their projection into a type 'key *)

  val def_idx_int : project:(elt -> int) -> int def_index
  (** Index by integer *)

  val def_idx_string : project:(elt -> string) -> string def_index
  (** Index by string *)

  type _ hlist =
    | Nil : unit hlist
    | Cons : 'key def_index * 'tail hlist -> ('key * 'tail) hlist

  (** {2 Access Within Indices List} *)

  (** The type [('i, 'k) idx_key] describes how to access the index for
      the type ['k] within a list of indices ['i] *)
  type (_,_) idx_key =
    | KThere : (('a * 'b), 'a) idx_key
    | KNext : ('b, 'c) idx_key -> (('a * 'b), 'c) idx_key

  val there : ('a * _, 'a) idx_key
  val next : ('a, 'b) idx_key -> ('c * 'a, 'b) idx_key

  val k0 : ('a * _, 'a) idx_key
  val k1 : (_ * ('a * _), 'a) idx_key
  val k2 : (_ * (_ * ('a * _)), 'a) idx_key
  val k3 : (_ * (_ * (_ * ('a * _))), 'a) idx_key

  type 'indices t
  (** A set of elements of type [elt], with indices described
      by the type ['indices] *)

  val make : 'indices hlist -> 'indices t
  (** Create a new empty set  *)

  val add : elt -> 'i t -> 'i t
  (** Add an element *)

  val remove : elt -> 'i t -> 'i t
  (** Remove an element *)

  val find : idx:('i, 'k) idx_key -> 'k -> 'i t -> elt sequence
  (** [find ~idx set k] finds all elements of [set] that are indexed
      with the key [k] in the index number [idx] *)

  val iter : (elt -> unit) -> _ t -> unit

  val size : _ t -> int
  (** Number of elements *)

  val to_list : _ t -> elt list
  val of_list : 'i t -> elt list -> 'i t
  val to_seq : _ t -> elt sequence
  val of_seq : 'i t -> elt sequence -> 'i t
end
