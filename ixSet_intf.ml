(*
Copyright (c) 2015, Simon Cruanes
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

type 'a sequence = ('a -> unit) -> unit

(** {2 A Simple Set} *)
module type SET = sig
  type t
  type elt

  val empty : t
  val add : elt -> t -> t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val remove : elt -> t -> t
  val cardinal : t -> int
  val iter : (elt -> unit) -> t -> unit
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

module type S = sig
  type elt
  (** The type of the elements of the set *)

  type t
  (** The type for an indexed set *)

  type indices
  (** A list of indices *)

  val empty : t
  (** Empty set *)

  val add : t -> elt -> t
  (** Add an element to the set *)

  val remove : t -> elt -> t
  (** Remove an element to the set *)

  val iter : t -> (elt -> unit) -> unit
    (** Iterate on all elements *)

  val inter : t -> t -> t
  (** Set intersection. It will have the same indexes as the
      first argument. *)

  val union : t -> t -> t
  (** Set union. It will have the same indexes as the first argument. *)

  val size : t -> int
  (** Number of elements *)

  val to_list : t -> elt list
  val of_list : t -> elt list -> t
  val to_seq : t -> elt sequence
  val of_seq : t -> elt sequence -> t
end
