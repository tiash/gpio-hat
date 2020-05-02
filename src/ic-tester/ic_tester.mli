open Core

module Monad = Ic_monad
module Model = Model


val command : Model.t list -> Command.t
