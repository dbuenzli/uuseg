(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Sentence segmenter. *)

(** {1 Segmenter} *)

type t
val create : unit -> t
val copy : t -> t
val add : t ->  [ `Await | `End | `Uchar of Uchar.t ] -> Uuseg_base.ret
val equal : t -> t -> bool
