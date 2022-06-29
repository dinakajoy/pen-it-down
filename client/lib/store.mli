val repo: string

val info: 'a -> unit -> 'b

val local_commit: 'a -> 'b -> 'c -> unit Lwt.t

val local_get: 'a -> 'b -> 'c

val list: 'a -> 'b list Lwt.t

val init: 'a -> 'b Lwt.t