module Types.Switch exposing (Switch, fold, off, on, toggle)


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
fold switchOn switchOff switch =
    case switch of
        On ->
            switchOn

        Off ->
            switchOff


toggle : Switch -> Switch
toggle switch =
    case switch of
        On ->
            Off

        Off ->
            On
