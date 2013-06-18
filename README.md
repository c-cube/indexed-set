indexed-set
===========

OCaml implementation of indexed sets (inspired from Haskell's ixSet library).

## What?

`IxSet` is an attempt to adapt some of Haskell's
[ixSet](http://hackage.haskell.org/packages/archive/ixset/1.0.2/doc/html/Data-IxSet.html)
ideas to OCaml.

It provides a functor on some type that we will designate as `elt`. The result
is a module providing a set type `S.t`, similar to `Set.S`. Additional **indexes**
can be added, to access elements by some of their features.

For instance, if `elt` is a record containing data about people (an
extended phone book), let say,

```ocaml
type person = {
    phone : string;
    name : string; 
    age : int;
}
```

Then we can define some indexed set as follows:

```ocaml
module P = struct
    type t = person
    let compare p1 p2 = String.compare p1.name p2.name
end

module PersonSet = IxSet.Make(P)

let ixPhone = PersonSet.idx_fun ~cmp:String.compare (fun p -> [p.phone])
let ixName = PersonSet.idx_fun ~cmp:String.compare (fun p -> [p.name])
let ixAge = PersonSet.idx_fun ~cmp:Pervasives.compare (fun p -> [p.age])

let set =
    let set = PersonSet.empty in
    let set = PersonSet.add_idx set ixPhone in
    let set = PersonSet.add_idx set ixName in
    let set = PersonSet.add_idx set ixAge in
    set

let set3 = PersonSet.of_list set
    [ { phone = "01.23.45.67.89";
        name = "Georges Abitbol";
        age = 42;
      };
      { phone = "01.23.45.67.89";
        name = "Captain Falcon";
        age = 0x18;
      };
      { phone = "404.404.404";
        name = "Anne O'Nymous";
        age = 0;
      }
    ]
```

We have defined a module `PersonSet`, that allows us to manipulate indexed
sets of `person`s.

Indexes can be defined with `PersonSet.idx_fun`, which
indexes by another type (`string` or `int` here), and requires this type
to be comparable. It also requires a function that maps `person`s to
a list of such keys. Here, we took a straightforward mapping of a person
to its age, name and phone number.

Then, we define an empty set `set`, and add our indexes to it
(`PersonSet.empty_with` is an alternative, using heterogenous lists to specify
which indexes we want). `set3` is defined by adding three persons to `set`.

Let us see in a OCaml prompt:

```ocaml
$ make
$ rlwrap ocaml
> #load "_build/ixSet.cma";;
> PersonSet.by set3 ixAge 42;;
- : PersonSet.elt list = [{phone = "01.23.45.67.89"; name = "Georges Abitbol"; age = 42}]
> PersonSet.by set3 ixPhone "01.23.45.67.89"
- : PersonSet.elt list =
[{phone = "01.23.45.67.89"; name = "Georges Abitbol"; age = 42};
{phone = "01.23.45.67.89"; name = "Captain Falcon"; age = 24}]
```

So, using `PersonSet.by`, we can select on a given index. Notice that
it returns a list of values, since keys do not have to be injective.

But we can also
`filter` using a predicate (it's linear, but using the ordering on key types
it should be easy to have some efficient ordering filters for this example):

```ocaml
> PersonSet.filter set3 ixAge (fun x -> x < 40);;
- : PersonSet.elt list =
[{phone = "01.23.45.67.89"; name = "Captain Falcon"; age = 24};
{phone = "404.404.404"; name = "Anne O'Nymous"; age = 0}] 
```


## License

The code is free, released under the BSD-2 license. Contributions and
improvements to the API are very welcome!
