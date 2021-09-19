(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2020 Roddy <github@roddymacsween.co.uk>                 *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(**************************************************************************)

let () =
  Memtrace.trace_if_requested ();
  Redis_clone_lib.Resp_async.main ()
