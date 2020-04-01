open Camelus_lib

let pr = {
  number = 42;
  base = { repo = { user = "pat"; name = "johnson"; auth = None; };
           ref = "PatJ";
           sha = "cha cha"; };
  head = { repo = { user = "tom"; name = "thomas"; auth = Some ("T","B"); };
           ref = "Tom";
           sha = "chi chi"; };
  pr_user = "blanc";
  message = "prout","caca";
}

let a = Fork_handler.linearize_pr pr
let a' = Array.append (Array.make 42 "") a
let () = Array.iter print_endline a

let () = assert Fork_handler.(delinearize_pr a' 42 = pr)
