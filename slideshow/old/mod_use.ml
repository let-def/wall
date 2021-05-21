let mod_use fname =
  let buf = Buffer.create 256 in
  let fmt = Format.formatter_of_buffer buf in
  if not (Toploop.mod_use_file fmt fname) then (
    Format.pp_print_flush fmt ();
    prerr_endline (Buffer.contents buf)
  )
