module Scale.Band.Extra exposing (invert)

import List.Extra as List
import Scale exposing (BandConfig)


invert : List a -> ( Float, Float ) -> Float -> Maybe a
invert domain ( d0, d1 ) d =
    let
        ( start, stop ) =
            if d0 < d1 then
                ( d0, d1 )

            else
                ( d1, d0 )

        n =
            toFloat <| List.length domain

        i =
            max 0 (((d / (stop - start)) * n |> floor) - 1)
    in
    List.getAt i domain
