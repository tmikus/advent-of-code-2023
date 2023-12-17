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

fun computeLongestDistance(lines: List<List<Char>>, start: Position): Pair<Int, Set<Position>> {
    val visited = mutableSetOf<Position>()
    val queue = mutableListOf<Pair<Position, Int>>()
    queue.add(Pair(start, 0))
    var maxDistance = 0
    var endPoint = start
    while (queue.isNotEmpty()) {
        val (position, distance) = queue.removeAt(0)
        if (visited.contains(position)) {
            continue
        }
        visited.add(position)
        if (distance > maxDistance) {
            maxDistance = distance
            endPoint = position
        }
        val neighbours = getNeighbours(lines, position)
        for (neighbour in neighbours) {
            queue.add(Pair(neighbour, distance + 1))
        }
    }
    return Pair(maxDistance, visited)
}

fun countEmptyBlocksInsidePath(lines: List<List<Char>>, path: Set<Position>): Int {
    var count = 0
    for (y in lines.indices) {
        for (x in lines[y].indices) {
            if (isPointInsidePath(lines, path, Position(x, y))) {
                count++
            }
        }
    }
    return count
}

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

fun getNeighbours(lines: List<List<Char>>, position: Position): List<Position> {
    val node = lines[position.y][position.x]
    var neighbours = listOf<Position>()
    when (node) {
        'S' -> neighbours = getStartPointNeighbours(lines, position)
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
        if (!isOutOfBounds(lines, neighbour) && getNeighbours(lines, neighbour).contains(position)) {
            result.add(neighbour)
        }
    }
    return result
}

fun getStartingPointSymbol(lines: List<List<Char>>, position: Position): Char {
    val neighbours = getStartPointNeighbours(lines, position)
    if (neighbours.size != 2) {
        throw IllegalArgumentException("Unexpected scenario: Starting point has ${neighbours.size} neighbours. Expected 2")
    }
    val (first, second) = neighbours
    if (first.x == second.x) {
        return '|'
    }
    if (first.y == second.y) {
        return '-'
    }
    if (first.x < second.x) {
        if (first.y < second.y) {
            return 'L'
        }
        return '7'
    }
    if (first.y < second.y) {
        return 'J'
    }
    return 'F'
}

fun getLeft(position: Position): Position = Position(position.x - 1, position.y)
fun getRight(position: Position): Position = Position(position.x + 1, position.y)
fun getUp(position: Position): Position = Position(position.x, position.y - 1)
fun getDown(position: Position): Position = Position(position.x, position.y + 1)

fun isOutOfBounds(lines: List<List<Char>>, position: Position): Boolean {
    return position.x < 0 || position.y < 0 || position.y >= lines.size || position.x >= lines[position.y].size
}

fun isPointInsidePath(lines: List<List<Char>>, path: Set<Position>, position: Position): Boolean {
    if (path.contains(position)) {
        return false
    }
    var count = 0
    var expectedLineEnd: Char? = null
    for (x in 0..position.x) {
        val point = Position(x, position.y)
        if (!path.contains(point)) {
            continue
        }
        var node = lines[point.y][point.x]
        if (node == 'S') {
            node = getStartingPointSymbol(lines, point)
        }
        if (node == '|') {
            count++
            expectedLineEnd = null
        } else if (expectedLineEnd != null) {
            if (node == '-') {
                continue
            }
            if (node == expectedLineEnd) {
                count++
            }
            expectedLineEnd = null
        } else if (node == 'L') {
            expectedLineEnd = '7'
        } else if (node == '7') {
            expectedLineEnd = 'L'
        } else if (node == 'J') {
            expectedLineEnd = 'F'
        } else if (node == 'F') {
            expectedLineEnd = 'J'
        }
    }
    return count % 2 == 1
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
    val start = findStartPosition(lines)
    println("Start position: $start")
    val (distance, path) = computeLongestDistance(lines, start)
    println("Part 1: Longest distance: $distance")
    val emptyBlocks = countEmptyBlocksInsidePath(lines, path)
    println("Part 2: Blocks inside path: $emptyBlocks")
}