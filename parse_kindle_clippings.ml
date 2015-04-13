#! /usr/bin/env ocaml -w -8
#use "topfind"
#thread
#require "str"
#require "core"
open Core.Std

type note = {
    title : string;
    location : string;
    added : string;
    text : string;
  }

let location_regex = Str.regexp "Loc\\. \\([0-9]+\\)"

let parse_location_n location_s =
  let _ = Str.search_forward location_regex location_s 0 in
  Str.matched_group 1 location_s

(* TODO(hammer): handle bad notes *)
(* TODO(hammer): get location regex working with Core *)
let process_note note =
  let title :: rest = note in
  let location_and_added :: text_parts = rest in
  let location_s :: added :: _ = String.split ~on:'|' location_and_added in
  let location_n = parse_location_n location_s in
  let text = String.concat text_parts in
  { title; location = location_n; added; text }

let rec process_lines lines =
  let note, lines = List.split_while ~f:((<>) "==========") lines in
  match note with
  | [] -> []
  | _ -> (process_note note) :: (process_lines (List.drop lines 1))

let () =
  (* TODO(hammer): use array manipulation utilities from Core *)
  for i = 1 to Array.length Sys.argv - 1 do
    let clippings_file = Sys.argv.(i) in
    let notes = process_lines (In_channel.read_lines clippings_file) in
    List.iter ~f:(fun note -> Format.printf "%s|%s|%s\n" note.title note.location note.text) notes
  done
