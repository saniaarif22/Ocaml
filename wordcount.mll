{ type token = EOF | Word of string }
rule token = parse
	| eof { EOF }
	| ['a'-'z' 'A'-'Z']+ as word { Word(word) }
	| _ { token lexbuf }
{
	let lexbuf = Lexing.from_channel stdin in
	let wordlist =
		let rec next l = 
			match token lexbuf with
			| EOF -> l
			| Word(s) -> next (s :: l)
	in next []
	in
	let module StringMap = Map.Make(String)
	in
	let wordmap =
		let rec generatewordmap wl wordmap = match wl with
			| [] -> wordmap
			| h::t -> generatewordmap t (StringMap.add h (if StringMap.mem h wordmap then
			(StringMap.find h wordmap) + 1 else 1) wordmap)

	in 
	generatewordmap wordlist StringMap.empty
	in
	let wordcounts =
	StringMap.fold (fun word count a -> (count, word) :: a) wordmap []
	in
	let wordcounts =
	List.sort (fun (c1, _) (c2, _) -> Pervasives.compare c2 c1) wordcounts
	in
	List.iter (fun word -> print_endline(string_of_int (fst word) ^ " " ^ snd word))
	wordcounts;;

	}