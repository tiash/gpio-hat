(*



let () = Random.self_init ()

let rand ~max ~not_ =
  let n' = Random.int max % max in
  if n' = not_ then (n' + 1) % max else n'

let () =
  dip16 ~name:"74173" ~description:"4-Bit D-Type Register with 3-State Outputs"
    (fun m n q1 q2 q3 q4 clk not_g1 not_g2 d4 d3 d2 d1 clr ->
      (* CR mhorn: can't test for the behaviour of [n] and [m].
         Pinning low for now. *)
      let%bind () = constant n false and () = constant m false in
      let%bind () = input' clr false and () = input' clk false in
      let check state = output4 q1 q2 q3 q4 state in
      let check' state =
        let%bind () = input4' d1 d2 d3 d4 (rand ~max:16 ~not_:state) in
        check state
      in
      let clear =
        let%bind () = sync in
        let%bind () = input' clr true in
        let state = 0 in
        let%bind () = check' state in
        let%bind () = sync in
        let%bind () = input' clr false in
        let%bind () = check' state in
        return state
      in
      let set =
        let%bind () = sync in
        let%bind () = input' not_g1 false and () = input' not_g2 false in
        let%bind state = input4 d1 d2 d3 d4 in
        let%bind () = sync in
        let%bind () = input' clk true in
        let%bind () = check state in
        let%bind () = sync in
        let%bind () = check' state in
        let%bind () = sync in
        let%bind () = input' clk false in
        let%bind () = check' state in
        return state
      in
      let mset state =
        let%bind g1 = input not_g1 >>| not and g2 = input not_g2 >>| not in
        if g1 && g2 then set
        else
          let%bind () = sync in
          let%bind () = check' state in
          let%bind () = sync in
          let%bind () = input' clk true in
          let%bind () = check state in
          let%bind () = sync in
          let%bind () = input' clk false in
          let%bind () = check state in
          return state
      in
      choice [ clear; set >>= mset ] |> Ic_tester.Monad.ignore_m)

let () =
  dip16 ~name:"74595"
    ~description:"8-bit shift register with 3-state output registers"
    (fun q2 q3 q4 q5 q6 q7 q8 sr_out not_sr_clr sr_clk q_clk not_oe sr_in q1 ->
      let check ~sr ~q =
        Ic_tester.Monad.all_unit
          [
            output8 q1 q2 q3 q4 q5 q6 q7 q8 q; output sr_out (sr land 0x80 <> 0);
          ]
      in
      (* CR mhorn: Can't test for NC pins *)
      let%bind () = constant not_oe false in
      let%bind () = input' not_sr_clr false in
      let%bind () = input' sr_clk false in
      let%bind () = input' q_clk false in
      let%bind () = input' sr_in false in
      let%bind () = sync in
      let clear ~q =
        let check =
          match q with None -> output sr_out false | Some q -> check ~sr:0 ~q
        in
        let%bind () = input' not_sr_clr false in
        let%bind () = check in
        let%bind () = sync in
        let%bind () = input' not_sr_clr true in
        return ()
      in
      let push_out ~sr =
        let%bind () = input' q_clk true in
        let%bind () = check ~sr ~q:sr in
        let%bind () = sync in
        let%bind () = input' q_clk false in
        return ()
      in
      let%bind () = clear ~q:None in
      let sr = 0 in
      let%bind () = push_out ~sr in
      let q = sr in
      let%bind () = check ~sr ~q in
      let clear ~q = clear ~q:(Some q) in
      let _ = clear in
      let shift ~q ~sr =
        let%bind sr = input1 sr_in >>| ( lor ) (sr lsl 1) >>| ( land ) 0xff in
        let%bind () = sync in
        let%bind () = input' sr_clk true in
        let%bind () = check ~sr ~q in
        let%bind () = sync in
        let%bind () = input' sr_clk false in
        return sr
      in
      let step ~sr ~q =
        let%bind sr = shift ~q ~sr in
        let%bind q =
          if rand ~max:2 ~not_:2 = 0 then
            let%map () = push_out ~sr in
            sr
          else return q
        in
        let%map () = check ~sr ~q in
        (sr, q)
      in
      let%bind sr, q = step ~sr ~q in
      let%bind sr, q = step ~sr ~q in
      let%bind sr, q = step ~sr ~q in
      let%bind sr, q = step ~sr ~q in
      let%bind sr, q = step ~sr ~q in
      let%bind sr, q = step ~sr ~q in
      let%bind sr, q = step ~sr ~q in
      let%bind sr, q = step ~sr ~q in
      let%bind _sr, _q = step ~sr ~q in
      return ())

let () =
  dip20 ~name:"74245" ~description:"Octal bus Traceiver with 3-State Outputs"
    (fun dir a1 a2 a3 a4 a5 a6 a7 a8 b8 b7 b6 b5 b4 b3 b2 b1 not_oe ->
      let%bind () = constant not_oe false in
      let%bind dir = input dir in
      if dir then
        input8 a1 a2 a3 a4 a5 a6 a7 a8 >>= output8 b1 b2 b3 b4 b5 b6 b7 b8
      else input8 b1 b2 b3 b4 b5 b6 b7 b8 >>= output8 a1 a2 a3 a4 a5 a6 a7 a8)

let () =
  dip20 ~name:"74273" ~description:"Octal D Flip-Flip with Clear"
    (fun not_reset q1 d1 d2 q2 q3 d3 d4 q4 clock q5 d5 d6 q6 q7 d7 d8 q8 ->
      let check n = output8 q1 q2 q3 q4 q5 q6 q7 q8 n in
      let step n0 =
        let n1 = rand ~max:256 ~not_:n0 in
        let%bind reset = input not_reset >>| not
        and () = input8' d1 d2 d3 d4 d5 d6 d7 d8 n1 in
        let%bind () = check (if reset then 0 else n0) in
        let%bind () = sync in
        let%bind () = input' clock true in
        let n = if reset then 0 else n1 in
        let%bind () = check n in
        let%bind () = sync in
        let%bind () = input' clock false in
        let%bind () = check n in
        let%bind () = sync in
        let%bind () = check n in
        let%bind () = sync in
        return n
      in
      let%bind () = input' not_reset false
      and () = input' clock false
      and () = input8' d1 d2 d3 d4 d5 d6 d7 d8 (rand ~max:256 ~not_:256) in
      let%bind () = check 0 in
      let%bind () = sync in
      Ic_tester.Monad.ignore_m (step 0 >>= step >>= step >>= step >>= step >>= step))

let () =
  dip16 ~name:"74189"
    ~description:"64-bit Random Access Memory with 3-State Outputs"
    (fun a1 not_cs not_we d1 not_o1 d2 not_o2 not_o3 d3 not_o4 d4 a4 a3 a2 ->
      let%bind () = constant not_cs false in
      let%bind () = input' not_we true in
      let rec walk ~to_write ~to_read ~data =
        if Map.is_empty to_write && Set.is_empty to_read then return ()
        else
          let addr = rand ~max:16 ~not_:(-1) in
          let%bind () = input4' a1 a2 a3 a4 addr in
          let%bind () =
            match Map.find data addr with
            | None -> return ()
            | Some value -> output4 not_o1 not_o2 not_o3 not_o4 (value lxor 0xf)
          in
          let to_read = Set.remove to_read addr in
          let to_write, value =
            let no_write = Map.mem data addr && rand ~max:3 ~not_:(-1) <> 0 in
            if no_write then (to_write, None)
            else
              match Map.find to_write addr with
              | None -> (to_write, None)
              | Some ts -> (
                  match List.permute ts with
                  | [] -> assert false
                  | [ value ] -> (Map.remove to_write addr, Some value)
                  | value :: (_ :: _ as rest) ->
                      (Map.set to_write ~key:addr ~data:rest, Some value) )
          in
          let%bind data, to_read =
            match value with
            | None -> return (data, to_read)
            | Some value ->
                let%bind () = input4' d1 d2 d3 d4 value in
                let%bind () = sync in
                let%bind () = input' not_we false in
                let%bind () = sync in
                let%map () = input' not_we true in
                (Map.set data ~key:addr ~data:value, Set.add to_read addr)
          in
          let%bind () = sync in
          walk ~to_write ~to_read ~data
      in
      walk ~data:Int.Map.empty ~to_read:Int.Set.empty
        ~to_write:
          ( List.init 16 ~f:(fun addr -> (addr, List.init 16 ~f:Fn.id))
          |> Int.Map.of_alist_exn ))

let all = !all

let of_string s = Map.find_exn !all_ics s

let arg_type = Command.Arg_type.create of_string
*)
