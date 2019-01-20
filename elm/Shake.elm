port module Shake exposing (shake)


port shake : (() -> msg) -> Sub msg
