type t = Cstruct.t
           
let bucket_size = 4
                    
let make () = Cstruct.create 4
                             
let null = 0 

let insert t byte =
  
  let rec aux i =
    let b = Cstruct.get_uint8 t i in

    if b = null then

      begin 
        Cstruct.set_uint8 t i byte; 
        true
      end 

    else if (i >= 3 && b <> null) then
      false

    else
      aux (i + 1)


  in

  aux 0


      
      
      
let remove t byte =

  let rec aux i =
    let b = Cstruct.get_uint8 t i in

    if b = byte then
      begin 
        Cstruct.set_uint8 t i null; 
        true
      end
        
    else if (i >= 3) && (b <> byte) then
      false

    else
      aux (i + 1)


  in

  aux 0



let next_pow2 n =
  let rec aux i s =
    match s with
    | hd :: tl ->
       let i1 = Int64.logor i (Int64.shift_right i hd) in
       aux i1 tl

    | [] ->
       i
  in

  let i = Int64.pred n in
  let s = [1; 2; 4; 8; 16; 32] in

  Int64.succ (aux i s) 
  
       




let find_index t fp =
  let rec aux i = 
   let b = Cstruct.get_uint8 t i in

    if b = fp then
      begin 
        Cstruct.set_uint8 t i null; 
        i
      end
        
    else if (i >= 3) && (b <> fp) then
      (-1)

    else
      aux (i + 1)


  in

  
  aux 0


let swap t fp =
  let i = Random.int bucket_size in
  let fp1 = Cstruct.get_uint8 t i in
  Cstruct.set_uint8 t i fp1;
  fp1 
  
