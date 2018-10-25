module Types.Octave exposing
    ( Octave
    , add
    , all
    , down
    , eight
    , five
    , four
    , number
    , octave
    , one
    , seven
    , six
    , three
    , two
    , up
    , zero
    )


type Octave
    = Octave Int


octave : Int -> Maybe Octave
octave n =
    if n >= 0 && n <= 8 then
        Octave n |> Just

    else
        Nothing


up : Octave -> Maybe Octave
up (Octave n) =
    octave (n + 1)


down : Octave -> Maybe Octave
down (Octave n) =
    octave (n - 1)


number : Octave -> Int
number (Octave n) =
    n


add : Int -> Octave -> Maybe Octave
add n (Octave num) =
    octave (n + num)


all : List Octave
all =
    List.range 0 8 |> List.map Octave


zero : Octave
zero =
    Octave 0


one : Octave
one =
    Octave 1


two : Octave
two =
    Octave 2


three : Octave
three =
    Octave 3


four : Octave
four =
    Octave 4


five : Octave
five =
    Octave 5


six : Octave
six =
    Octave 6


seven : Octave
seven =
    Octave 7


eight : Octave
eight =
    Octave 8
