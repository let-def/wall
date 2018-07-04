let other_args = List.tl (Array.to_list Sys.argv)
let () = Toploop.initialize_toplevel_env ()
let () = Slideshow.auto_reload other_args
