let complete_match str regex =
  let result = Str.string_match regex str 0 in
  let eq = String.equal (Str.matched_string str) str in
  result && eq

let money_regex = Str.regexp {|\$?[0-9]*\(\.[0-9][0-9]\)?|}
let username_regex = Str.regexp {|[a-zA-Z]*|}
