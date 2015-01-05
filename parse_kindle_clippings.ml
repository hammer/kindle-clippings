#! /usr/bin/env ocaml -w -8
#use "topfind"
#thread
#require "core"
open Core.Std

(* TODO(hammer): parse from the command line *)
let clippings_file = ""

type note = {
    title : string;
    location : string;
    added : string;
    text : string;
  }

(* TODO(hammer): handle bad notes *)
let process_note note =
  let title :: rest = note in
  let location_and_added :: text_parts = rest in
  let location :: added :: _ = String.split ~on:'|' location_and_added in
  let text = String.concat text_parts in
  { title; location; added; text }

let rec process_lines lines =
  let note, lines = List.split_while ~f:((<>) "==========") lines in
  match note with
  | [] -> []
  | _ -> (process_note note) :: (process_lines (List.drop lines 1))

let () =
  let notes = process_lines (In_channel.read_lines clippings_file) in
  List.iter ~f:(fun note -> Format.printf "%s\n" note.text) notes
