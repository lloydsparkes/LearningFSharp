open System

//Install From: http://symbolics.mathdotnet.com/
open MathNet.Symbolics

let random = Random()

let validLargeNumbers = [|25; 50; 75; 100|]
let validSmallNumbers = [|1; 1; 2; 2; 3; 3; 4; 4; 5; 5; 6; 6; 7; 7; 8; 8; 9; 9; 10; 10|]
let validSize = [|3; 5; 7; 9; 11|]


let randomExpression(number : seq<int>, operators: seq<string>) = seq { for n in 0..10 do if n % 2 = 0 then yield (Seq.nth (n/2) number).ToString() else yield Seq.nth (random.Next(0, 3)) operators}

let swap (a: _[]) x y = 
    let tmp = a.[x]
    a.[x] <- a.[y]
    a.[y] <- tmp

let shuffle a = 
    let temp = Seq.toArray a
    Array.iteri (fun i _ -> swap temp i (random.Next(i, Array.length temp))) temp       
    temp
    
type Solution(expression) = 
    member this.expression = expression
    member this.getExpressionString = Seq.fold (fun str x -> str + x) "" this.expression
    member this.getParsed = Infix.parseOrThrow this.getExpressionString
    member this.getIntegerResult = Int32.Parse(Infix.printStrict this.getParsed)
    member this.getRating target = 
        match this.getIntegerResult with
           | x when x = target -> 10
           | x when x > (target - 5) && x < (target + 5) -> 7
           | x when x > (target - 10) && x < (target + 10) -> 5
           | _ -> 0

let innerGenerator(numbers: seq<int>) = seq { for n in 0..10 do if n % 2 = 0 then yield (Seq.nth (n/2) numbers).ToString() else yield Seq.nth (random.Next(0, 3)) [|"+"; "-"; "*"; "/"|]}
let outerGenerator(numbers: seq<int>) = (Seq.take (validSize.[random.Next(0,4)]) (innerGenerator numbers))

type RandomSolution(numbers: seq<int>) = inherit Solution(Seq.toArray (outerGenerator numbers))

let randomGeneration(numbers: seq<int>) = seq { for n in 0..500000 do yield RandomSolution((shuffle numbers)) }

[<EntryPoint>]
let main argv = 
    let largeToPick = random.Next(0, 4)
    printfn "Picking %i large numbers" largeToPick
    let smallToPick = 6 - largeToPick
    let numbers = Seq.toArray (Seq.concat [|(Seq.take largeToPick (shuffle validLargeNumbers)); (Seq.take smallToPick (shuffle validSmallNumbers))|])
    printfn "Large Numbers: %A" validLargeNumbers
    printfn "Small Numbers: %A" validSmallNumbers
    printfn "Numbers Picked: %A" (Array.sort numbers)
    let target = random.Next(100, 999)
    printfn "Target is: %i" target

    for s in Seq.filter (fun (x: RandomSolution) -> (x.getRating target) > 0) (randomGeneration numbers) do
        printfn "%s has Result %i Scores %i" s.getExpressionString s.getIntegerResult (s.getRating target)

    printfn "Done"

    let x = System.Console.ReadLine()
    0
            