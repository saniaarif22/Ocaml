let rec fib i =
if i < 1 then 1 else
fib (i-1) + fib (i-2)
in
print_string( string_of_int (fib 5));;
