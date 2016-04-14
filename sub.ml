(* 

Write a function that subtracts positive integers represented
as lists of decimal digits. 

For example,
subl [2;5;3] [5;7] = [1;9;6]
subl [1;0;0;0;0;0;0;0;0;0;0;0]
[4;2;0;0;0;0;0;0;0;0;0] =
[0;5;8;0;0;0;0;0;0;0;0;0]

Your algorithm may assume the first number is larger
than the second. Arbitrary-precision arithmetic packages
use a similar technique but with a much larger radix. 

*)

let subl list1 list2 =
	let rec len_diff li1 li2 = if List.length li1 > List.length li2 
							   then len_diff li1 (0::li2) 
							   else li2 in
							   
		let rec subtract l1 l2 carry = match l1, l2 with
		| [], [] -> []
		| hd1::tl1,  hd2::tl2 -> if hd1-hd2 < carry 
								then (hd1+10-carry-hd2 :: (subtract tl1 tl2 1)) 
								else (hd1-carry-hd2:: (subtract tl1 tl2 0)) 
		| _, _ -> print_endline "XXX"; [0] (*Extra case*)

	in
			List.rev(subtract (List.rev list1) (List.rev (len_diff list1 list2)) 0)
		;;