open Lwt.Infix

let _ = Lwt_main.run (Counter.test () >>= Lww_register.test)
