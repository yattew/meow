let string_of_char_list xs = xs |> List.map Char.escaped |> String.concat ""
let zero_token = Token.{ token_type = Eof; literal = "" }
