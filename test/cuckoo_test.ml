

let gen_str length =
    let gen() = match Random.int(26+26+10) with
        n when n < 26 -> int_of_char 'a' + n
      | n when n < 26 + 26 -> int_of_char 'A' + n - 26
      | n -> int_of_char '0' + n - 26 - 26 in
    let gen _ = String.make 1 (char_of_int(gen())) in
    String.concat "" (Array.to_list (Array.init length gen) )

let words = ["hello"; "all alone"; "comfortabley"; "numb"; "heroin"; "pokemon"]
              

let inserts () =
  let filter = CuckooFilter.create 1000 in
  List.iter (fun x -> CuckooFilter.insert filter x; () ) words;

  let ct_got = List.length words in
  let ct_exp = CuckooFilter.count filter in 
  Alcotest.(check int) "all were inserted" ct_got ct_exp



          




let lookups () =
  let filter = CuckooFilter.create 1000 in
  List.iter (fun x -> CuckooFilter.insert filter x; () ) words;
  let not_got = CuckooFilter.lookup filter "halo odst" in
  let not_e = false in

  Alcotest.(check bool) "Didn't return false positive" not_got not_e;

  let got = List.map (fun x -> CuckooFilter.lookup filter x) words in
  let expected = List.map (fun _ -> true) words in

  Alcotest.(check (list bool) ) "All hits" got expected 



let deletes () =
  let filter = CuckooFilter.create 1000 in
  List.iter (fun x -> CuckooFilter.insert filter x; () ) words;
  CuckooFilter.delete filter "heroin";

  let got_ct = CuckooFilter.count filter in
  let exp_ct = (List.length words) - 1 in
  Alcotest.(check int) "was deleted" got_ct exp_ct;
  
  let not_got = CuckooFilter.lookup filter "heroin" in
  let not_e = false in
  Alcotest.(check bool) "Didn't return false positive" not_got not_e
  

let test_set = [
    "Inserts", `Quick, inserts;
    "Lookups", `Quick, lookups;
    "Deletes", `Quick, deletes; 
]

                 

let () =
  Alcotest.run "Cuckoo Filter Test" [
    "Operations", test_set;
  ]

