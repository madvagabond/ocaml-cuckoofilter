
type t = {
    buckets: Bucket.t array;
    mutable count: int;
    n_buckets: int
  }

let max_kicks = 500
                 


let num_buckets t = t.n_buckets


let count t =
  t.count


let to_uint i =
  abs (Int64.to_int i)

let create size =
  let i = Int64.of_int size |> Bucket.next_pow2 |> Int64.to_int in

  let cap =
    let c = i / 4 in
    if c = 0 then 1
    else c
  in

  
  let buckets = Array.init cap (fun _ -> Bucket.make () ) in
  {buckets; count=0; n_buckets=cap}


    
let last_bit i =
  let n = 8 in
  let bits = (1 lsl n) - 1 |> Int64.of_int  in
  Int64.logand i bits |> Int64.to_int


let alt_index fp buckets i =
  let hash = Farmhash.hash64 (Printf.sprintf "%d" i) |> to_uint in 
  (hash lxor i) mod buckets


let fingerprint hash =
  let i = Int64.rem hash 255L |> Int64.succ in
  last_bit i 





let search data buckets =
  let hash = Farmhash.hash64 data in 
  let fp = fingerprint hash in
  let i = (to_uint hash) mod buckets in
  let i1 = alt_index fp buckets i in
  (fp, i, i1)



let lookup t data =
  let buckets = Array.length t.buckets in 
  let (fp, i, i1) = search data buckets in
  let (b, b1) = (Array.get t.buckets i), (Array.get t.buckets i1) in
  (Bucket.find_index b fp) > -1 || (Bucket.find_index b1 fp) > -1 



let get_bucket t i =
  Array.get t.buckets i



let pick l =
  let i = Random.int (List.length l) in
  List.nth l i
           
let insert t data =
  let n = num_buckets t in
  print_endline "what's good"; 
  

  let rec aux i k fp  =
    let old = fp in 
    let fp1 = Bucket.swap (get_bucket t i) old in
    let i1 = alt_index fp1 n i  in

    let b = get_bucket t i1 in 
    let p = Bucket.insert b fp1 in

    if p then
      true
    else if p <> true && (k >= max_kicks) then
      false
    else
      aux i1 (k + 1) fp1

  in

  let complete () =
    t.count <- t.count + 1;
    true
  in 

  

  let (fp, i, i1) = search data n in
  print_endline "hello";
  let (b1, b2) = (get_bucket t i), (get_bucket t i1) in




  if (Bucket.insert b1 fp) then
    complete ()
             
  else if (Bucket.insert b2 fp) then
    complete ()
             
  else

    let ind = pick [i; i1] in
    let pred = aux ind 0 fp in

    begin 
      if pred then complete ()
      else false

    end


let delete t data =
  let n = num_buckets t in 
  let (fp, i, i1) = search data n in
  let (b1, b2) = get_bucket t i, get_bucket t i1 in

  let complete () =
    t.count <- t.count - 1;
    true
  in
  
  let pred = Bucket.remove b1 fp || Bucket.remove b2 fp in

  if pred then complete ()
  else false
         
      

      
      
