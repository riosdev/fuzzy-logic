open Format
open List
(*wrapper para printf*)
let printf = Format.printf
let makeList f i n = 
    let rec innerMakeList l x =
        let v = f x in
        if x <= n then innerMakeList (v::l) (x + 1)
        else l
    in innerMakeList [] i
    |> List.rev
let plot s l =
    let rec innerplot h = 
        match h with
        | h when h > 0. ->
            let rec row l = 
                match l with
                | n::tail ->
                    printf (if n >= h /. s && n < (h +. 1.) /. s then "*" else "_");
                    row tail
                | [] -> printf "\n"
            in row l;
            innerplot (h -. 1.)
        | _ -> printf "\n"
    in innerplot s
let plot10 = plot 10.

let not u = 1. -. u
let (&&&) u v = min u v
let (|||) u v = max u v
let asc a b u = 
    if u < a then 0.
    else if a <= u && u <= b then (u -. a)/.(b -. a)
    else 1.
let desc a b u = 
    if (u < a) then 1.
    else if (a <= u && u <= b) then (b -. u)/.(b -. a)
    else 0.
let triang a b y u = 
    if (u < a) then  0.
    else if (a <= u && u <= b) then  (u -. a)/.(b -. a)
    else if (b < u && u <= y) then  (y -. u)/.(y -. b)
    else 0.
let trapez a b y d u = 
    if (u < a) then 0.
    else if (a <= u && u <= b) then (u -. a)/.(b -. a)
    else if (b < u && u <= y) then 1.
    else if (y < u && u <= d) then (d -. u)/.(d -. y)
    else 0.
let defuzz inf l = 
    let v = l |> List.map inf in
    let nd = List.fold_left2 (fun s u v -> s +. u *. v) 0. v l in
    let dd = v |> List.fold_left (fun s u -> s +. u) 0. in
    nd /. dd
(****Descricao do problema****)
(*Velocidade*)
let sZero = triang (-25.) 0. 25.
let sNegLow u = u +. 25. |> sZero (*sZero deslocado 25 para a esquerda*)
let sPosLow u = u -. 25. |> sZero (*sZero deslocado 25 para a direita*)
let sNegHigh = desc (-50.) (-25.)
let sPosHigh = asc 25. 50.
(*Angulo*)
let aZero = sZero
let aNegLow = sNegLow
let aPosLow = sPosLow
let aNegHigh = sNegHigh
let aPosHigh = sPosHigh
(*Velocidade Angular*)
let vaZero = sZero
let vaNegLow = sNegLow
let vaPosLow = sPosLow
let vaNegHigh = sNegHigh
let vaPosHigh = sPosHigh
(*Regras*)
let rsZero a v s = 
    (aZero a &&& vaZero v &&& sZero s) ||| 
    (aNegLow a &&& vaPosLow v &&& sZero s) |||
    (aPosLow a &&& vaNegLow v &&& sZero s)
let rsNegLow a v s = 
    (aNegLow a &&& vaZero v &&& sNegLow s) ||| 
    (aZero a &&& vaNegLow v &&& sNegLow s) ||| 
    (aZero a &&& vaPosLow v &&& sNegLow s)
let rsPosLow a v s = 
    (aZero a &&& vaPosLow v &&& sPosLow s) ||| 
    (aPosLow a &&& vaZero v &&& sPosLow s)
let rsNegHigh a v s = 
    (aNegHigh a &&& vaZero v &&& sNegHigh s) ||| 
    (aNegLow a &&& vaPosHigh v &&& sNegHigh s) ||| 
    (aZero a &&& vaNegHigh v &&& sNegHigh s)
let rsPosHigh a v s = 
    (aNegLow a &&& vaPosHigh v &&& sPosHigh s) ||| 
    (aPosHigh a &&& vaZero v &&& sPosHigh s)
(****Fim da Descricao****)
let () = 
    let _ = [   sZero; sNegLow; sPosLow; sNegHigh; sPosHigh;
                aZero; aNegLow; aPosLow; aNegHigh; aPosHigh;
                vaZero; vaNegLow; vaPosLow; vaNegHigh; vaPosHigh] in
    let rlLabel = [ "Speed is Zero"; "Speed is Negative Low"; 
                    "Speed is Positive Low"; "Speed is Negative High"; 
                    "Speed is Positive High"] in
    let rl = [rsZero; rsNegLow; rsPosLow; rsNegHigh; rsPosHigh] in
    let angle = 5. in
    let angularVelocity = (-15.) in
    printf "Testing with angle: %.2f and angular velocity: %.2f\n" angle angularVelocity;
    printf "Printing rules from -50 to 50:\n\n";
    List.iter2 (fun f l ->
        printf "%s:\n" l;
        makeList (fun u -> float u) (-50) (50) 
        |> List.map (fun u -> f angle angularVelocity u)
        |> plot10
    ) rl rlLabel;
    
    printf "That makes:\n";
    makeList (fun u -> float u) (-50) (50)
    |> List.map (fun u ->
        List.fold_left (fun s f -> f angle angularVelocity u |> max s) 0. rl)
    |> plot10;

    let speed =
    makeList (fun u -> float u) (-50) (50)
    |> defuzz (fun u -> 
        rsZero angle angularVelocity u |||
        rsNegLow angle angularVelocity u |||
        rsPosLow angle angularVelocity u |||
        rsNegHigh angle angularVelocity u |||
        rsPosHigh angle angularVelocity u
    ) in
    printf "After defuzzification, speed is: %.2f\n" speed