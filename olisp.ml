let usage_msg = "usage: ./olisp.native [-a|-s|-l|-c] [file.ol]" in
let channel = ref stdin in
  Arg.parse [] ((fun filename -> channel := open_in filename) usage_msg)