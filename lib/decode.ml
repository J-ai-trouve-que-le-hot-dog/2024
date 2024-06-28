let input = {|? B= B$ B$ B$ B$ L$ L$ L$ L# v$ I" I# I$ I% I$ ? B= B$ L$ v$ I+ I+ ? B= BD I$ S4%34 S4 ? B= BT I$ S4%34 S4%3 ? B= B. S4% S34 S4%34 ? U! B& T F ? B& T T ? U! B| F F ? B| F T ? B< U- I$ U- I# ? B> I$ I# ? B= U- I" B% U- I$ I# ? B= I" B% I( I$ ? B= U- I" B/ U- I$ I# ? B= I# B/ I( I$ ? B= I' B* I# I$ ? B= I$ B+ I" I# ? B= U$ I4%34 S4%34 ? B= U# S4%34 I4%34 ? U! F ? B= U- I$ B- I# I& ? B= I$ B- I& I# ? B= S4%34 S4%34 ? B= F F ? B= I$ I$ ? T B. B. SM%,&k#(%#+}IEj}3%.$}z3/,6%},!.'5!'%y4%34} U$ B+ I# B* I$> I1~s:U@ Sz}4/}#,!)-}0/).43}&/2})4 S)&})3}./4}#/22%#4 S").!29}q})3}./4}#/22%#4 S").!29}q})3}./4}#/22%#4 S").!29}q})3}./4}#/22%#4 S").!29}k})3}./4}#/22%#4 S5.!29}k})3}./4}#/22%#4 S5.!29}_})3}./4}#/22%#4 S5.!29}a})3}./4}#/22%#4 S5.!29}b})3}./4}#/22%#4 S").!29}i})3}./4}#/22%#4 S").!29}h})3}./4}#/22%#4 S").!29}m})3}./4}#/22%#4 S").!29}m})3}./4}#/22%#4 S").!29}c})3}./4}#/22%#4 S").!29}c})3}./4}#/22%#4 S").!29}r})3}./4}#/22%#4 S").!29}p})3}./4}#/22%#4 S").!29}{})3}./4}#/22%#4 S").!29}{})3}./4}#/22%#4 S").!29}d})3}./4}#/22%#4 S").!29}d})3}./4}#/22%#4 S").!29}l})3}./4}#/22%#4 S").!29}N})3}./4}#/22%#4 S").!29}>})3}./4}#/22%#4 S!00,)#!4)/.})3}./4}#/22%#4 S!00,)#!4)/.})3}./4}#/22%#4|}

let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&\'()*+,-./:;<=>?@[\\]^_`|~ \n"

let decode_char c =
  chars.[(Char.code c - 33)]

let decode_string s =
  String.map decode_char s

let decode s =
  if s.[0] = 'S' then
    Some (decode_string (String.sub s 1 (String.length s - 1)))
  else
    None

let encode_char c =
  Char.chr (String.index_from chars 0 c + 33)

let encode_string s =
  String.map encode_char s


let parts = String.split_on_char ' ' input

let rem s = String.sub s 1 (String.length s - 1)

let decode_int s =
  let r = ref 0 in
  for i = 0 to String.length s - 1 do
    r := 94 * !r + Char.code s.[i] - 33
  done;
  !r

let cases s : Ast.expr =
  match s.[0] with
  | 'T' -> Bool true
  | 'F' -> Bool false
  | 'I' -> Int (decode_int (rem s))
  | 'S' -> String (decode_string s)
  | _ -> Bool false


let v = List.map cases parts
