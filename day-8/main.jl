struct RawNode
    name::String
    left::String
    right::String
end

struct PuzzleInput
    directions::Array{Int}
    graph::Dict{String, Tuple{String, String}}
end

function countStepsUntilEnd(puzzleInput::PuzzleInput, start::String)::Int
    nodeKey = start
    steps = 0
    while !isEndNode(nodeKey)
        direction = puzzleInput.directions[(steps % length(puzzleInput.directions)) + 1]
        nodeKey = puzzleInput.graph[nodeKey][direction]
        steps += 1
    end
    steps
end

function countStepsUntilEndForAllPaths(puzzleInput::PuzzleInput)::Array{Int}
    nodeKeys = findStartNodeKeys(puzzleInput)
    map(nodeKey -> countStepsUntilEnd(puzzleInput, nodeKey), nodeKeys)
end

function findLcm(numbers)
    if length(numbers) < 2
        throw(ArgumentError("Array should contain at least two numbers"))
    end
    lcd_result = numbers[1]
    for num in numbers[2:end]
        lcd_result = lcm(lcd_result, num)
    end
    lcd_result
end

function findPathLength(puzzleInput::PuzzleInput)::Int
    pathLength = 0
    nodeKey = "AAA"
    directions = copy(puzzleInput.directions)
    while nodeKey != "ZZZ"
        direction = popfirst!(directions)
        push!(directions, direction)
        node = puzzleInput.graph[nodeKey]
        nodeKey = node[direction]
        pathLength += 1
    end
    pathLength
end

findStartNodeKeys(puzzleInput::PuzzleInput)::Array{String} = puzzleInput.graph |>
    keys |>
    collect |>
    keys -> filter(k -> endswith(k, "A"), keys)

isEndNode(nodeKey::String)::Bool = endswith(nodeKey, "Z")

function parsePuzzleInput(lines::Array{String})::PuzzleInput
    directions = parseDirections(lines[1])
    graph = parseGraph(lines[2:end])
    PuzzleInput(directions, graph)
end

parseDirections(directions::String)::Array{Int} = map(c -> c == 'L' ? 1 : 2, collect(directions))

function parseGraph(lines::Array{String})::Dict{String, Tuple{String, String}}
    nodes = parseRawNodes(lines)
    reduce(
        (graph, node) -> begin
            graph[node.name] = (node.left, node.right)
            graph
        end,
        nodes,
        init=Dict{String, Tuple{String, String}}()
    )
end

function parseRawNode(line::SubString{String})::RawNode
    matches = match(r"(\w+)\s*=\s*\((\w+),\s*(\w+)\)", line)
    RawNode(matches[1], matches[2], matches[3])
end

parseRawNodes(lines::Array{String})::Array{RawNode} = lines |>
    x -> map(line -> strip(line), x) |>
    x -> filter(line -> length(line) > 0, x) |>
    x -> map(parseRawNode, x)

function main()
    lines = readlines()
    puzzleInput = parsePuzzleInput(lines)
    println("Part 1: ", findPathLength(puzzleInput))
    println("Part 2: ", findLcm(countStepsUntilEndForAllPaths(puzzleInput)))
end

main()
