class Position(val x: Int, val y: Int) {
    override fun equals(other: Any?): Boolean {
        if (other !is Position) {
            return false
        }
        return x == other.x && y == other.y
    }

    override fun toString(): String = "($x, $y)"

    override fun hashCode(): Int {
        var result = x
        result = 31 * result + y
        return result
    }
}

fun computeLongestDistance(lines: List<List<Char>>, start: Position): Int {
    val visited = mutableSetOf<Position>()
    val queue = mutableListOf<Pair<Position, Int>>()
    queue.add(Pair(start, 0))
    var maxDistance = 0
    while (queue.isNotEmpty()) {
        val (position, distance) = queue.removeAt(0)
        if (visited.contains(position)) {
            continue
        }
        visited.add(position)
        if (distance > maxDistance) {
            maxDistance = distance
        }
        val neighbours = getNeighbours(lines, position)
        for (neighbour in neighbours) {
            queue.add(Pair(neighbour, distance + 1))
        }
    }
    return maxDistance
}

fun getNeighbours(lines: List<List<Char>>, position: Position): List<Position> {
    if (isOutOfBounds(lines, position)) {
        return listOf()
    }
    val node = lines[position.y][position.x]
    var neighbours = listOf<Position>()
    when (node) {
        'S' -> {
            neighbours = getStartPointNeighbours(lines, position)
        }
        '|' -> neighbours = listOf(
            getUp(position),
            getDown(position),
        )
        '-' -> neighbours = listOf(
            getLeft(position),
            getRight(position),
        )
        'L' -> neighbours = listOf(
            getUp(position),
            getRight(position),
        )
        'J' -> neighbours = listOf(
            getUp(position),
            getLeft(position),
        )
        '7' -> neighbours = listOf(
            getDown(position),
            getLeft(position),
        )
        'F' -> neighbours = listOf(
            getDown(position),
            getRight(position),
        )
    }
    return neighbours.filter { !isOutOfBounds(lines, it) }
}

fun getStartPointNeighbours(lines: List<List<Char>>, position: Position): List<Position> {
    val neighbours = listOf(
        getUp(position),
        getDown(position),
        getLeft(position),
        getRight(position),
    )
    val result = mutableListOf<Position>()
    for (neighbour in neighbours) {
        if (getNeighbours(lines, neighbour).contains(position)) {
            result.add(neighbour)
        }
    }
    return result
}

fun getLeft(position: Position): Position = Position(position.x - 1, position.y)
fun getRight(position: Position): Position = Position(position.x + 1, position.y)
fun getUp(position: Position): Position = Position(position.x, position.y - 1)
fun getDown(position: Position): Position = Position(position.x, position.y + 1)

fun findStartPosition(lines: List<List<Char>>): Position {
    for (y in lines.indices) {
        for (x in lines[y].indices) {
            if (lines[y][x] == 'S') {
                return Position(x, y)
            }
        }
    }
    throw IllegalArgumentException("No start position found")
}

fun isOutOfBounds(lines: List<List<Char>>, position: Position): Boolean {
    return position.x < 0 || position.y < 0 || position.y >= lines.size || position.x >= lines[position.y].size
}

fun readAllLines(): List<List<Char>> {
    val lines = mutableListOf<List<Char>>()
    while (true) {
        val line = readlnOrNull() ?: break
        lines.add(line.chars().mapToObj { it.toChar() }.toList())
    }
    return lines
}

fun main() {
    val lines = readAllLines()
    val startPosition = findStartPosition(lines)
    println("Start position: $startPosition")
    val distance = computeLongestDistance(lines, startPosition)
    println("Longest distance: $distance")
}