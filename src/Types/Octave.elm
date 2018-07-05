module Types.Octave exposing (Octave, number, add, all, zero, one, two, three, four, five, six, seven, eight)


type Octave
    = Octave Int


octave : Int -> Maybe Octave
octave n =
    if n >= 0 && n <= 8 then
        Octave n |> Just
    else
        Nothing


number : Octave -> Int
number (Octave n) =
    n


add : Int -> Octave -> Maybe Octave
add n (Octave number) =
    octave (n + number)


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
