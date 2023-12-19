function computeLoad(grid) {
    let answer = 0;
    const gridHeight = grid.length;
    for (let rowIndex = 0; rowIndex < gridHeight; rowIndex++) {
        for (let columnIndex = 0; columnIndex < grid[0].length; columnIndex++) {
            if (grid[rowIndex][columnIndex] === "O") {
                answer += (gridHeight - rowIndex);
            }
        }
    }
    return answer;
}

function shiftRocksNorth(grid) {
    for (let columnIndex = 0; columnIndex < grid[0].length; columnIndex++) {
        shiftRocksNorthInColumn(grid, columnIndex);
    }
}

function shiftRocksNorthInColumn(grid, columnIndex) {
    let rockRowInsertIndex = 0;
    for (let rowIndex = 0; rowIndex < grid.length; rowIndex++) {
        if (grid[rowIndex][columnIndex] === "O") {
            if (rockRowInsertIndex === rowIndex) {
                rockRowInsertIndex++;
            } else {
                grid[rowIndex][columnIndex] = ".";
                grid[rockRowInsertIndex][columnIndex] = "O";
                rockRowInsertIndex++;
            }
        } else if (grid[rowIndex][columnIndex] === "#") {
            // it's an immovable rock, so set the insert index below it
            rockRowInsertIndex = rowIndex + 1;
        }
    }
}

function readInputGrid() {
    // Work on POSIX and Windows
    const fs = require("fs");
    const stdinBuffer = fs.readFileSync(0); // STDIN_FILENO = 0
    return stdinBuffer.toString().trim().split("\n").map(row => row.split(""));
}

function rotateGridClockwise(grid) {
    const newGrid = [];
    for (let columnIndex = 0; columnIndex < grid[0].length; columnIndex++) {
        const row = []
        for (let rowIndex = grid.length - 1; rowIndex >= 0; rowIndex--) {
            row.push(grid[rowIndex][columnIndex]);
        }
        newGrid.push(row);
    }
    return newGrid;
}

function handlePartOfCycle(grid) {
    shiftRocksNorth(grid);
    return rotateGridClockwise(grid);
}

function computeLoads(grid) {
    const loads = [];
    for (let i = 0; i < 1000; i++) {
        grid = handlePartOfCycle(grid);
        grid = handlePartOfCycle(grid);
        grid = handlePartOfCycle(grid);
        grid = handlePartOfCycle(grid);
        loads.push(computeLoad(grid));
    }
    return loads;
}

function repeatsAfter(loads, cycle, startIndex) {
    for (let offset = 0; offset < cycle.length; offset++) {
        if (loads[startIndex + offset] !== cycle[offset % cycle.length]) {
            return false;
        }
    }
    return true;
}

function findShortestRepeatingCycle(loads) {
    for (let cycleStartIndex = 0; cycleStartIndex < loads.length; cycleStartIndex++) {
        for (let cycleLength = 2; cycleLength < loads.length - cycleStartIndex; cycleLength++) {
            const cycle = loads.slice(cycleStartIndex, cycleStartIndex + cycleLength);
            if (repeatsAfter(loads, cycle, cycleStartIndex + cycleLength)) {
                return [cycleStartIndex, cycleLength];
            }
        }
    }
    return [0, 0];
}

function main() {
    let grid = readInputGrid();
    shiftRocksNorth(grid);
    console.log('Part 1: ', computeLoad(grid));
    const loads = computeLoads(grid);
    const [cycleStartIndex, cycleLength] = findShortestRepeatingCycle(loads);
    const numberOfCycles = 1_000_000_000;
    const index = (numberOfCycles - cycleStartIndex - 1) % cycleLength;
    const longestCycle = loads.slice(cycleStartIndex, cycleStartIndex + cycleLength);
    console.log('Part 2: ', longestCycle[index]);
}

main();
