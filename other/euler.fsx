#r "nuget: Plotly.NET, 2.0.0-preview.18"
#r "nuget: FSharp.Stats, 0.4.5"

open Plotly.NET
open FSharp.Stats

let vMenten (vMax:float) kM s = vMax * s / (s + kM)

// vMentenEulerRule wenn vMenten = k * [Substrat] = v 
let vMentenEuler (vMax:float) (kM:float) (s:float) (deltaT:float) = vMenten vMax kM s * deltaT

// Hypothetical values for Enzyme
let kCat = 100.
let kM = 100.*(10.**(-6.))
let E0 = 2.*(10.**(-6.))
let S0 = 1000.*(10.**(-6.))
let vMax = kCat * E0

//let startingRate = vMenten vMax kM S0

// Time interval = 0.002 

let intervals = [0. .. 10000.]
let integralRes = 
    intervals
    |> List.fold (fun (acc:(float*float) list) t -> 
        let (amountSubstrate,amountProduct) = acc.Head 
        let amountNewProduct = vMentenEuler vMax kM amountSubstrate 0.001
        let (amountSubstrate',amountProduct') = amountSubstrate-amountNewProduct,amountProduct+amountNewProduct
        (amountSubstrate',amountProduct')::acc
        ) [(S0,0.)]
    |> List.rev


[
Chart.Point(intervals |> List.map ((*) 0.001),integralRes |> List.map fst)
Chart.Point(intervals |> List.map ((*) 0.001),integralRes |> List.map snd)
]
|> Chart.combine
|> Chart.show


let createPred maxTime deltaT = 
    let maxI = 
        maxTime / deltaT
        |> int
    let intervals = [0. .. float maxI]
    let integralRes = 
        intervals
        |> List.fold (fun (acc:(float*float) list) t -> 
            let (amountSubstrate,amountProduct) = acc.Head 
            let amountNewProduct = vMentenEuler vMax kM amountSubstrate deltaT
            let (amountSubstrate',amountProduct') = amountSubstrate-amountNewProduct,amountProduct+amountNewProduct
            (amountSubstrate',amountProduct')::acc
            ) [(S0,0.)]
        |> List.rev
    

    [
    Chart.Point(intervals |> List.map ((*) deltaT),integralRes |> List.map fst)
    Chart.Point(intervals |> List.map ((*) deltaT),integralRes |> List.map snd)
    ]
    |> Chart.combine

[
createPred 10. 0.5
createPred 10. 0.001
]
|> Chart.combine
|> Chart.show