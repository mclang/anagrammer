///
/// Makes anagrams from the words given as command line arguments. Concatenates
/// words into one and removes spaces before finding all the possible permutations.
///
/// Functional permutation code taken from:
/// https://stackoverflow.com/a/4497738
///
open System

// All ordered picks {x_i1, x_i2, .. , x_ik} of k out of n elements {x_1,..,x_n}
// where i1 < i2 < .. < ik
let picks n L =
    let rec aux nleft acc L = seq {
        match nleft,L with
        | 0,_ -> yield acc
        | _,[] -> ()
        | nleft,h::t -> yield! aux (nleft-1) (h::acc) t
                        yield! aux nleft acc t }
    aux n [] L

// Distribute an element y over a list:
// {x1,..,xn} --> {y,x1,..,xn}, {x1,y,x2,..,xn}, .. , {x1,..,xn,y}
let distrib y L =
    let rec aux pre post = seq {
        match post with
        | [] -> yield (L @ [y])
        | h::t -> yield (pre @ y::post)
                  yield! aux (pre @ [h]) t }
    aux [] L

// All permutations of a single list = the head of a list distributed
// over all permutations of its tail
let rec getAllPerms = function
    | [] -> Seq.singleton []
    | h::t -> getAllPerms t |> Seq.collect (distrib h)

// All k-element permutations out of n elements =
// all permutations of all ordered picks of length k combined
let getPerms2 n lst = picks n lst |> Seq.collect getAllPerms


// Check and parse command line arguments
let words : string[] =
    fsi.CommandLineArgs
    |> Array.tail

if words.Length = 0 then failwith "Please give some words to mangle!"

// Concatenate given words into one, remove spaces and make all characters lower case
let chlst =
    words
    |> String.concat ""
    |> Seq.choose (function
        | ' ' -> None
        | ch  -> Some (Char.ToLower(ch))
    )
    |> List.ofSeq


#time "on"
let permutations =
    chlst
    |> getPerms2 (chlst.Length)
    |> Seq.map (fun lst ->
        lst |> Array.ofList |> System.String
    )

#time "off"

permutations
|> Seq.take 20
|> Seq.iteri (fun idx agram ->
    printfn "%5d: %s" (idx+1) agram
)
