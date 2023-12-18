function getPart1Answer(grid) {
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

function printGrid(grid) {
    for (let rowIndex = 0; rowIndex < grid.length; rowIndex++) {
        console.log(grid[rowIndex].join(""));
    }
}

function readInputGrid() {
    // Work on POSIX and Windows
    const fs = require("fs");
    const stdinBuffer = fs.readFileSync(0); // STDIN_FILENO = 0
    return stdinBuffer.toString().trim().split("\n").map(row => row.split(""));
}

function main() {
    const grid = readInputGrid();
    console.log('Before shifting rocks north:');
    printGrid(grid);
    shiftRocksNorth(grid);
    console.log('\nAfter shifting rocks north:');
    printGrid(grid);
    console.log('Part 1: ', getPart1Answer(grid));
}

main();
