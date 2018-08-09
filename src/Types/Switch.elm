module Types.Switch exposing (Switch, fold, toggle, on, off)


type Switch
    = On
    | Off


on : Switch
on =
    On


off : Switch
off =
    Off


fold : a -> a -> Switch -> a
fold on off switch =
    case switch of
        On ->
            on

        Off ->
            off


toggle : Switch -> Switch
toggle switch =
    case switch of
        On ->
            Off

        Off ->
            On
