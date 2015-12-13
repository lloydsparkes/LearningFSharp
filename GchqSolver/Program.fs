open System
open System.IO

type CellType = Black | Blank | BlackFixed

type Problem(size: int) = 
    member this.size = size
    member val grid = Array2D.create<CellType> size size CellType.Blank
    member val rows = [] with get, set
    member val columns = [] with get, set

let loadFile(name: string) = 
    let mutable p = Problem(0)
    for row in File.ReadAllLines(name) do
        let bits = row.Split ' '
        if bits.[0] = "S" then p <- Problem(Int32.Parse(bits.[1]))
        if bits.[0] = "Y" then p.rows <- p.rows @ [bits]
        if bits.[0] = "X" then p.columns <- p.columns @ [bits]
        if bits.[0] = "F" then p.grid.[Int32.Parse(bits.[1])-1,Int32.Parse(bits.[2])-1] <- CellType.BlackFixed
    p

let printGrid grid =
    let maxY = (Array2D.length2 grid) - 1
    let maxX = (Array2D.length1 grid) - 1
    
    for row in 0 .. maxY do
        for col in 0 .. maxX do
            if grid.[col, row] = CellType.BlackFixed then Console.Write("|F")
            else if grid.[col, row] = CellType.Black then Console.Write("|B")
            else Console.Write("| ")
        Console.WriteLine("|")
    Console.WriteLine()

let shadeRowFromPosition(p:Problem, startPosition:int, numberToFill:int, rowIndex: int) = 
    let mutable startPos = startPosition
    while p.grid.[Math.Min(startPos+numberToFill+1, 24), rowIndex] = CellType.BlackFixed do
        startPos <- startPos+1
    for i in startPos..((startPos+numberToFill)-1) do
        if p.grid.[i, rowIndex] <> CellType.BlackFixed then
            p.grid.[i, rowIndex] <- CellType.Black
    startPos + numberToFill + 1

let shiftRowFromPosition(p:Problem, row:int, startPos:int) = 
    for x in 24..(startPos+1) do
        p.grid.[x, row] <- p.grid.[x-1, row]
    p.grid.[startPos, row] <- CellType.Blank

[<EntryPoint>]
let main argv = 
    let problem = loadFile("Puzzle.txt")
    printGrid problem.grid

    for rowIndex in 0 .. (problem.size-1) do
        let mutable currIndex = 0
        let items = problem.rows.Item rowIndex
        for rowBlock in items do
            if rowBlock <> "Y" then
                currIndex <- shadeRowFromPosition(problem, currIndex, Int32.Parse rowBlock, rowIndex)

    printGrid problem.grid

    for colIndex in 0 .. (problem.size-1) do
        let mutable currRow = 0
        let items = problem.columns.Item colIndex 
        for colBlock in items do
            if colBlock <> "X" then
                let blockSize = Int32.Parse colBlock
                while currRow < 24 && problem.grid.[colIndex, currRow] = CellType.Blank do
                    currRow <- currRow + 1
                currRow <- currRow + blockSize
                if currRow < 25 && problem.grid.[colIndex, currRow] <> CellType.Blank then
                    shiftRowFromPosition(problem, currRow, colIndex)
        for rowToShift in currRow..24 do
            shiftRowFromPosition(problem, rowToShift, colIndex)

    printGrid problem.grid

    Console.ReadLine()
    0 // return an integer exit code
