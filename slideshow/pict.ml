open Wall

type t =
  | Text of string
  | Text_font of Wall_text.Font.t * t
  | Text_color of Color.t * t
  | Image of Wall.Image.t * Gg.size2
  |

