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

type 'a def_set
(** Typeclass of sets of type 'a *)

val def_set : 'a ord -> 'a def_set
(** Create a definition of sets *)

type ('elt, 'key) def_index

val def_idx : project:('elt -> 'key) ->
              compare:'key ord ->
             'elt def_set ->
             ('elt, 'key) def_index
(** Definition of an index. It allows to access values of type 'elt
    by their projection into a type 'key *)

val def_idx_int : project:('elt -> int) ->
                'elt def_set ->
                 ('elt, int) def_index
(** Index by integer *)

val def_idx_string : project:('elt -> string) ->
                    'elt def_set ->
                     ('elt, string) def_index
(** Index by string *)

type (_,_) hlist =
  | Nil : ('elt, unit) hlist
  | Cons :
    ('elt, 'key) def_index
    * ('elt, 'tail) hlist
    -> ('elt, 'key * 'tail) hlist

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

type ('elt, 'indices) def
(** Definition of the type of sets of element 'elt indexed by 'indices *)

type ('elt, 'indices) t
(** An instance of ('elt, 'indices) def *)

val def : 'elt def_set -> ('elt, 'indices) hlist -> ('elt, 'indices) def
(** Create a new definition *)

val make : ('elt, 'indices) def -> ('elt, 'indices) t
(** Instantiate the given definition *)

val add : 'e -> ('e, 'i) t -> ('e, 'i) t
(** Add an element *)

val remove : 'e -> ('e, 'i) t -> ('e, 'i) t
(** Remove an element *)

val find : idx:('i, 'k) idx_key -> 'k -> ('e, 'i) t -> 'e sequence
(** [find ~idx set k] finds all elements of [set] that are indexed
    with the key [k] in the index number [idx] *)

