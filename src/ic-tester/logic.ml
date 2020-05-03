open Core

module Action = struct
  type t = Input of bool | Output of bool | Constant of bool | Not_connected
  [@@deriving sexp_of, compare, equal, enumerate]

  let compatible a b =
    match (a, b) with
    | Input _, Input _ -> true
    | Input _, (Output _ | Constant _ | Not_connected) -> false
    | Output _, Output _ -> true
    | Output _, (Input _ | Constant _ | Not_connected) -> false
    | Constant a, Constant b -> Bool.equal a b
    | Constant _, (Input _ | Output _ | Not_connected) -> false
    | Not_connected, Not_connected -> true
    | Not_connected, (Input _ | Output _ | Constant _) -> false

  let%test_unit "compatible.symetric" =
    List.iter all ~f:(fun a ->
        List.iter all ~f:(fun b ->
            [%test_eq: bool] (compatible a b) (compatible a b)))

  let%test_unit "compatible.transitive" =
    List.iter all ~f:(fun a ->
        List.iter all ~f:(fun b ->
            List.iter all ~f:(fun c ->
                [%test_eq: bool]
                  (compatible a b && compatible b c)
                  (compatible a c))))
end

module T : sig
  type 'a t = private
    | Return : 'a -> 'a t
    | Map : 'a t * ('a -> 'b) -> 'b t
    | Bind : 'a t * ('a -> 'b t) -> 'b t
    | Both : 'a t * 'b t -> ('a * 'b) t
    | Choice : 'a t list -> 'a t
    | Input : Gpio_hat.Pin.t * bool -> unit t
    | Output : Gpio_hat.Pin.t * bool -> unit t
    | Constant : Gpio_hat.Pin.t * bool -> unit t
    | Not_connected : Gpio_hat.Pin.t -> unit t
    | Sync : unit t

  val return : 'a -> 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t

  val both : 'a t -> 'b t -> ('a * 'b) t

  val ignore_m : _ t -> unit t

  val input : Gpio_hat.Pin.t -> bool t

  val input' : Gpio_hat.Pin.t -> bool -> unit t

  val output : Gpio_hat.Pin.t -> bool -> unit t

  val constant : Gpio_hat.Pin.t -> bool -> unit t

  val not_connected : Gpio_hat.Pin.t -> unit t

  val sync : unit t

  val choice : 'a t list -> 'a t
end = struct
  type 'a t =
    | Return : 'a -> 'a t
    | Map : 'a t * ('a -> 'b) -> 'b t
    | Bind : 'a t * ('a -> 'b t) -> 'b t
    | Both : 'a t * 'b t -> ('a * 'b) t
    | Choice : 'a t list -> 'a t
    | Input : Gpio_hat.Pin.t * bool -> unit t
    | Output : Gpio_hat.Pin.t * bool -> unit t
    | Constant : Gpio_hat.Pin.t * bool -> unit t
    | Not_connected : Gpio_hat.Pin.t -> unit t
    | Sync : unit t

  let try_equal a b =
    if phys_equal a b then true else try Poly.equal a b with _ -> false

  let rec equal : type a. a t -> a t -> bool =
    fun (type a) (a : a t) (b : a t) ->
     ( if phys_equal a b then true
       else
         match (a, b) with
         | Return a, Return b -> try_equal a b
         | Map (a, af), Map (b, bf) ->
             if phys_same af bf then equal (Obj.magic a) (Obj.magic b)
             else false
         | Bind (a, af), Bind (b, bf) ->
             if phys_same af bf then equal (Obj.magic a) (Obj.magic b)
             else false
         | Both (a, b), Both (c, d) -> if equal a c then equal b d else false
         | Choice a, Choice b ->
             if List.length a = List.length b then
               List.fold_until a ~init:b
                 ~f:(fun b a ->
                   let rec find_and_remove rev_hd = function
                     | [] -> Continue_or_stop.Stop false
                     | b :: tl ->
                         if equal a b then Continue (List.rev_append rev_hd tl)
                         else find_and_remove (b :: rev_hd) tl
                   in
                   find_and_remove [] b)
                 ~finish:(function [] -> true | _ :: _ -> false)
             else false
         | Input (a, b), Input (c, d) ->
             if Bool.equal b d then Gpio_hat.Pin.equal a c else false
         | Output (a, b), Output (c, d) ->
             if Bool.equal b d then Gpio_hat.Pin.equal a c else false
         | Constant (a, b), Constant (c, d) ->
             if Bool.equal b d then Gpio_hat.Pin.equal a c else false
         | Not_connected a, Not_connected b -> Gpio_hat.Pin.equal a b
         | Sync, Sync -> true
         | ( ( Return _ | Map _ | Bind _ | Both _ | Choice _ | Input _
             | Output _ | Constant _ | Not_connected _ | Sync ),
             ( Return _ | Map _ | Bind _ | Both _ | Choice _ | Input _
             | Output _ | Constant _ | Not_connected _ | Sync ) ) ->
             false
       : bool )

  let rec validate : type a. a t -> unit = function
    | Return _ | Input _ | Output _ | Constant _ | Not_connected _ | Sync -> ()
    | Map (t, _) -> validate_bound t
    | Bind (t, _) -> validate_bound t
    | Both (a, b) ->
        validate_both a;
        validate_both b
    | Choice [] -> failwith "Empty choice is not allowed"
    | Choice [ _ ] -> failwith "Sinble choice is not allowed"
    | Choice ts ->
        let rec no_dups = function
          | [] | [ _ ] -> ()
          | hd :: tl ->
              if List.exists tl ~f:(equal hd) then
                failwith "Choice has duplicates"
              else no_dups tl
        in
        no_dups ts;
        List.iter ts ~f:validate_choice

  and validate_bound : type a. a t -> unit = function
    | Both _ | Input _ | Output _ | Constant _ | Not_connected _ | Sync -> ()
    | Return _ -> failwith "Constant map/bind not allowed"
    | Bind _ | Map _ -> failwith "Nested map/bind not allowed"
    | Choice _ -> failwith "Choice in bind is not allowed"

  and validate_both : type a. a t -> unit = function
    | Input _ | Output _ | Constant _ | Not_connected _ | Sync -> ()
    | Return _ -> failwith "Constant pair not allowed"
    | Bind _ | Map _ -> failwith "Pair of map/bind not allowed"
    | Both (a, b) ->
        validate_both a;
        validate_both b
    | Choice _ -> failwith "Choice in pair is not allowed"

  and validate_choice : type a. a t -> unit = function
    | Return _ | Input _ | Output _ | Constant _ | Not_connected _ | Sync -> ()
    | Map (t, _) -> validate_bound t
    | Bind (t, _) -> validate_bound t
    | Both (a, b) ->
        validate_both a;
        validate_both b
    | Choice _ -> failwith "Nested choice is not allowed"

  let input' p v = Input (p, v)

  let output p v = Output (p, v)

  let constant p v = Constant (p, v)

  let not_connected p = Not_connected p

  let sync = Sync

  let return t = Return t

  let rec ignore_m : type a. a t -> unit t = function
    | Return _ -> return ()
    | Map (t, _) -> ignore_m t
    | Bind (t, f) -> bind t ~f:(fun t -> ignore_m (f t))
    | Both (a, b) -> map (both (ignore_m a) (ignore_m b)) ~f:Fn.ignore
    | Choice ts -> choice (List.map ts ~f:ignore_m)
    | (Input _ | Output _ | Constant _ | Not_connected _ | Sync) as t -> t

  and map : type a b. a t -> f:(a -> b) -> b t =
    fun (type a b) (t : a t) ~(f : a -> b) ->
     ( match t with
       | Return t -> Return (f t)
       | Map (t, f') -> map t ~f:(fun t -> f (f' t))
       | Bind (t, f') -> bind t ~f:(fun t -> map (f' t) ~f)
       | Choice ts -> choice (List.map ts ~f:(map ~f))
       | Both _ | Input _ | Output _ | Constant _ | Not_connected _ | Sync ->
           Map (t, f)
       : b t )

  and bind : type a b. a t -> f:(a -> b t) -> b t =
    fun (type a b) (t : a t) ~(f : a -> b t) ->
     ( match t with
       | Return t -> f t
       | Map (t, f') -> bind t ~f:(fun t -> f (f' t))
       | Bind (t, f') -> bind t ~f:(fun t -> bind (f' t) ~f)
       | Choice ts -> choice (List.map ts ~f:(bind ~f))
       | Both _ | Input _ | Output _ | Constant _ | Not_connected _ | Sync ->
           Bind (t, f)
       : b t )

  and both : type a b. a t -> b t -> (a * b) t =
    fun (type a b) (a : a t) (b : b t) ->
     ( match (a, b) with
       | Return a, Return b -> Return (a, b)
       | Return a, b -> map b ~f:(fun b -> (a, b))
       | a, Return b -> map a ~f:(fun a -> (a, b))
       | Map (a, af), Map (b, bf) ->
           map (both a b) ~f:(fun (a, b) -> (af a, bf b))
       | Bind (a, af), Bind (b, bf) ->
           bind (both a b) ~f:(fun (a, b) -> both (af a) (bf b))
       | Bind (a, af), Map (b, bf) ->
           bind (both a b) ~f:(fun (a, b) -> map (af a) ~f:(fun a -> (a, bf b)))
       | Map (a, af), Bind (b, bf) ->
           bind (both a b) ~f:(fun (a, b) -> map (bf b) ~f:(fun b -> (af a, b)))
       | Map (a, af), b -> map (both a b) ~f:(fun (a, b) -> (af a, b))
       | a, Map (b, bf) -> map (both a b) ~f:(fun (a, b) -> (a, bf b))
       | Bind (a, af), b ->
           bind (both a b) ~f:(fun (a, b) -> map (af a) ~f:(fun a -> (a, b)))
       | a, Bind (b, bf) ->
           bind (both a b) ~f:(fun (a, b) -> map (bf b) ~f:(fun b -> (a, b)))
       | Choice ts_a, Choice ts_b ->
           choice
             (List.map (List.cartesian_product ts_a ts_b) ~f:(fun (a, b) ->
                  both a b))
       | Choice ts_a, b -> choice (List.map ts_a ~f:(fun a -> both a b))
       | a, Choice ts_b -> choice (List.map ts_b ~f:(fun b -> both a b))
       | ( (Both _ | Input _ | Output _ | Constant _ | Not_connected _ | Sync),
           (Both _ | Input _ | Output _ | Constant _ | Not_connected _ | Sync) )
         ->
           Both (a, b)
       : (a * b) t )

  and choice : type a. a t list -> a t =
    let rec dedup : type a. a t list -> a t list -> a t list =
      fun (type a) (acc : a t list) (tl : a t list) ->
       ( match tl with
         | [] -> acc
         | hd :: tl -> (
             match hd with
             | Choice ts -> dedup (dedup acc ts) tl
             | Return _ | Map _ | Bind _ | Both _ | Input _ | Output _
             | Constant _ | Not_connected _ | Sync ->
                 if List.exists acc ~f:(equal hd) then dedup acc tl
                 else dedup (hd :: acc) tl )
         : a t list )
    in
    fun ts ->
      match dedup [] ts with
      | [] -> failwith "Empty choice is not allowed"
      | [ t ] -> t
      | ts -> Choice ts

  let input t =
    choice
      [
        map (input' t true) ~f:(const true);
        map (input' t false) ~f:(const false);
      ]

  let input' a b =
    let t = input' a b in
    validate t;
    t

  let input t =
    let t = input t in
    validate t;
    t

  let output a b =
    let t = output a b in
    validate t;
    t

  let constant a b =
    let t = constant a b in
    validate t;
    t

  let return t =
    let t = return t in
    validate t;
    t

  let ignore_m t =
    let t = ignore_m t in
    validate t;
    t

  let map t ~f =
    let t = map t ~f in
    validate t;
    t

  let bind t ~f =
    let t = bind t ~f in
    validate t;
    t

  let both a b =
    let t = both a b in
    validate t;
    t

  let choice ts =
    let t = choice ts in
    validate t;
    t
end

include T

module Common = struct
  let sync = sync

  let choice = choice
end

module Expert = struct
  let input = input

  let input' = input'

  let output = output

  let constant = constant

  let not_connected = not_connected
end

include Common

let rec all = function
  | [] -> return []
  | [ t ] -> map t ~f:(fun t -> [ t ])
  | hd :: tl -> map (both hd (all tl)) ~f:(fun (hd, tl) -> hd :: tl)

let rec all_unit = function
  | [] -> return ()
  | [ t ] -> ignore_m t
  | hd :: tl -> ignore_m (both hd (all_unit tl))

let join t = bind t ~f:Fn.id

module Monad_infix = struct
  let ( >>| ) t f = map t ~f

  let ( >>= ) t f = bind t ~f
end

include Monad_infix

module Let_syntax = struct
  include Monad_infix

  let return = return

  include Common

  module Let_syntax = struct
    module Open_on_rhs = struct end

    let both = both

    let map = map

    let bind = bind

    let return = return
  end
end

let no_changes ~prev_pins ~pins =
  Map.for_alli pins ~f:(fun ~key ~data ->
      match Map.find prev_pins key with
      | None -> false
      | Some data' -> Action.equal data data')

let eval_sync ~pins t = Sequence.of_list [ (pins, Second t) ]

let eval_return ~pins t = Sequence.of_list [ (pins, First t) ]

let eval_return_set_pin ~prev_pins ~pins ~pin value =
  Option.iter (Map.find prev_pins pin) ~f:(fun prev_value ->
      if not (Action.compatible prev_value value) then
        raise_s
          [%message
            "New usage disagrees with previous use"
              (pin : Gpio_hat.Pin.t)
              (value : Action.t)
              (prev_value : Action.t)]);
  let pins =
    Map.update pins pin ~f:(function
      | None -> value
      | Some other_value ->
          if not (Action.equal value other_value) then
            raise_s
              [%message
                "Conflicting values for"
                  (pin : Gpio_hat.Pin.t)
                  (value : Action.t)
                  (other_value : Action.t)];
          value)
  in
  eval_return ~pins ()

let rec eval' :
    type a.
    prev_pins:Action.t Gpio_hat.Pin.Map.t ->
    pins:Action.t Gpio_hat.Pin.Map.t ->
    a t ->
    (Action.t Gpio_hat.Pin.Map.t * (a, a t) Either.t) Sequence.t =
 fun ~prev_pins ~pins -> function
  | Sync ->
      if no_changes ~prev_pins ~pins then
        eval_return ~pins:Gpio_hat.Pin.Map.empty ()
      else eval_sync ~pins (return ())
  | Return t -> eval_return ~pins t
  | Map (t, f) ->
      Sequence.concat_map (eval' ~prev_pins ~pins t) ~f:(function
        | pins, First t -> eval_return ~pins (f t)
        | pins, Second t ->
            if no_changes ~prev_pins ~pins then
              eval' ~prev_pins:Gpio_hat.Pin.Map.empty ~pins (map t ~f)
            else eval_sync ~pins (map t ~f))
  | Bind (t, f) ->
      Sequence.concat_map (eval' ~prev_pins ~pins t) ~f:(function
        | pins, First t -> eval' ~prev_pins ~pins (f t)
        | pins, Second t ->
            if no_changes ~prev_pins ~pins then
              eval' ~prev_pins:Gpio_hat.Pin.Map.empty ~pins (bind t ~f)
            else eval_sync ~pins (bind t ~f))
  | Both (a, b) ->
      let a = eval' ~prev_pins ~pins a and b = eval' ~prev_pins ~pins b in
      Sequence.cartesian_product a b
      |> Sequence.concat_map ~f:(fun ((pins_a, a), (pins_b, b)) ->
             let pins =
               Map.merge_skewed pins_a pins_b ~combine:(fun ~key:pin a b ->
                   if not (Action.equal a b) then
                     raise_s
                       [%message
                         "Inconsistent actions for"
                           (pin : Gpio_hat.Pin.t)
                           (a : Action.t)
                           (b : Action.t)];
                   a)
             in
             match (a, b) with
             | First a, First b -> eval_return ~pins (a, b)
             | Second _, _ | _, Second _ ->
                 let a = match a with First a -> return a | Second a -> a in
                 let b = match b with First b -> return b | Second b -> b in
                 if no_changes ~prev_pins ~pins then
                   eval' ~prev_pins:Gpio_hat.Pin.Map.empty ~pins (both a b)
                 else eval_sync ~pins (both a b))
  | Choice ts ->
      Sequence.of_list (List.permute ts)
      |> Sequence.concat_map ~f:(eval' ~prev_pins ~pins)
  | Input (pin, value) ->
      eval_return_set_pin ~prev_pins ~pins ~pin (Input value)
  | Output (pin, value) ->
      eval_return_set_pin ~prev_pins ~pins ~pin (Output value)
  | Constant (pin, value) ->
      eval_return_set_pin ~prev_pins ~pins ~pin (Constant value)
  | Not_connected pin -> eval_return_set_pin ~prev_pins ~pins ~pin Not_connected

let eval ?(prev_pins = Gpio_hat.Pin.Map.empty) t =
  eval' ~prev_pins ~pins:Gpio_hat.Pin.Map.empty t
  |> Sequence.map ~f:(fun (pins, t) ->
         (*let pins = Map.merge_skewed prev_pins pins ~combine:(fun ~key:pin prev_usage usage ->
             if (not (Action.compatible prev_usage usage)) then
               raise_s [%message "Pin usage changed" (pin:Gpio_hat.Pin.t) (usage:Action.t) (prev_usage:Action.t)];
             usage)
           in *)
         (pins, t))
