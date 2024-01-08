let string_of_char_list xs =
    xs |> (List.map Char.escaped) |> (String.concat "")
