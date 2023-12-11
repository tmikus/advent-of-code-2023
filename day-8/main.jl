struct RawNode
    name::String
    left::String
    right::String
end

struct PuzzleInput
    directions::Array{Int}
    graph::Dict{String, Tuple{String, String}}
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
    println("Path length: ", findPathLength(puzzleInput))
end

main()
