module Question exposing (validate, words)

import String


words : String -> List (List String)
words question =
    List.map separateSlash (String.split "," question)


validate : List (List String) -> List String -> Bool
validate correctWords ws =
    let
        isOk ( components, word ) =
            List.member word components
    in
    if List.length correctWords == List.length ws then
        List.all isOk (List.map2 (\a b -> ( a, b )) correctWords ws)

    else
        False


separateSlash : String -> List String
separateSlash word =
    List.map String.trim (String.split "/" word)
