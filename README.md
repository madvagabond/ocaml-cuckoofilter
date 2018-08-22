# ocaml-cuckoofilter
A cuckoo filter implementation for Ocaml

A cuckoo filter is similar to a bloom filter as it is a probalistic data structure for finding set membership without storing the actual data.

However, it is superior to bloom filters in regards to lookup time, space efficiency, and depending on it's current capacity lookup time (it starts out faster, but once you get to the point where you have to swap entries, the performance degrades), and greater accuracy.

For more info see the original paper, https://www.cs.cmu.edu/~dga/papers/cuckoo-conext2014.pdf . 


```ocaml

  let words = ["hello"; "all alone"; "comfortabley"; "numb"; "pokemon"; "heroin"] in
  let filter = CuckooFilter.create 1000 in


  List.iter (fun x -> CuckooFilter.insert filter x; () ) words;
  Printf.printf "%d\n" (CuckooFilter.count filter);
    
  List.iter (fun x -> Printf.printf "Look up for %s returned %b\n" x (CuckooFilter.lookup filter x) ) words;


  CuckooFilter.remove filter "heroin";
  CuckooFilter.lookup filter "heroin" |> Printf.printf "\nDeleted Element Found, %b\n ";  
  

  
```