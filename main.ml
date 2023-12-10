(*
 * Copyright © 2023 Sam Henke
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the “Software”), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 *)

open Base
open Stdio

let parse_ints str =
    String.split ~on:' ' str
    |> List.filter_map ~f:Int.of_string_opt
    |> Set.of_list (module Int)

let parse_game line =
    let game, numbers =
        match String.split ~on:':' line with
        | [game; numbers] -> game, numbers
        | _ -> assert false
    in
    let game_number = Stdlib.Scanf.sscanf game "Card %u" (fun x -> x) in
    match String.split ~on:'|' numbers with
    | [winning_numbers; card_numbers] ->
            game_number, parse_ints winning_numbers, parse_ints card_numbers
    | _ -> assert false

let points (_, winning_numbers, card_numbers) =
    Set.inter winning_numbers card_numbers |> Set.length

let sum = List.fold_left ~init:0 ~f:(+)

let rev_map_lines ~f =
    In_channel.fold_lines ~init:[] ~f:(fun acc line -> (f line) :: acc)

let () =
    rev_map_lines In_channel.stdin ~f:(fun line -> parse_game line |> points)
    |> List.fold_left ~init:[] ~f:(fun acc count ->
        List.take acc count
        |> sum
        |> fun x -> (x + 1) :: acc)
    |> sum
    |> printf "Cards:\t%u\n"
