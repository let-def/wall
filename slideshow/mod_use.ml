let mod_use fname =
  ignore (Toploop.mod_use_file Format.std_formatter fname : bool);
