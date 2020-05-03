open! Core

module type S = sig
  type 'a t

  type pin

  module Let_syntax : sig
    module Let_syntax : sig
      val map : 'a t -> f:('a -> 'b) -> 'b t

      val both : 'a t -> 'b t -> ('a * 'b) t

      val return : 'a -> 'a t

      module Open_on_rhs : sig
        val vcc : pin -> unit t

        val gnd : pin -> unit t

        val not_connected : pin -> unit t

        val constant : string -> pin -> bool -> unit t

        val input : string -> pin -> bool Logic.t t

        val input' : string -> pin -> (bool -> unit Logic.t) t

        val output : string -> pin -> (bool -> unit Logic.t) t

        val input_output :
          string -> pin -> (bool Logic.t * (bool -> unit Logic.t)) t

        val input_output' :
          string -> pin -> ((bool -> unit Logic.t) * (bool -> unit Logic.t)) t

        val all : 'a t list -> 'a list t

        val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

        val ( >>=* ) : 'a Logic.t t -> ('a -> 'b Logic.t) -> 'b Logic.t t

        val ( >>|* ) : 'a Logic.t t -> ('a -> 'b) -> 'b Logic.t t
      end
    end
  end
end
