module Question exposing (words, validate)

import String


words : String -> List (List String)
words question =
    List.map separateSlash (String.split "," question)


validate : List (List String) -> List String -> Bool
validate correctWords words =
    let
        isOk (components, word) =
            List.member word components
    in
        if List.length correctWords == List.length words then
            List.all isOk (List.map2 (,) correctWords words)
        else
            False


separateSlash : String -> List String
separateSlash word =
    List.map String.trim (String.split "/" word)
