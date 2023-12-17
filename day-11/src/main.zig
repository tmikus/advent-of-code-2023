const std = @import("std");

const ArrayList = std.ArrayList;
const Point = struct { x: usize, y: usize };

const ALLOCATOR = std.heap.page_allocator;
const MAX_INPUT_LENGTH = 1024 * 1024; // 1MB

fn alloc(comptime T: type, n: anytype) []T {
    return ALLOCATOR.alloc(T, n) catch unreachable;
}

fn computeDistance(
    from: Point,
    to: Point,
) usize {
    const distance_x = max(from.x, to.x) - min(from.x, to.x);
    const distance_y = max(from.y, to.y) - min(from.y, to.y);
    return distance_x + distance_y;
}

fn computeDistanceWithExpansion(
    emptyColumns: ArrayList(usize),
    emptyRows: ArrayList(usize),
    from: Point,
    to: Point,
    expansionRate: usize,
) usize {
    const distance = computeDistance(from, to);
    const expansion_x = computeHorizontalExpansion(emptyColumns, from, to, expansionRate);
    const expansion_y = computeVerticalExpansion(emptyRows, from, to, expansionRate);
    return distance + expansion_x + expansion_y;
}

fn computeHorizontalExpansion(
    emptyColumns: ArrayList(usize),
    from: Point,
    to: Point,
    expansionRate: usize,
) usize {
    if (from.x == to.x) {
        return 0;
    }
    var expansion: usize = 0;
    const left_x = min(from.x, to.x);
    const right_x = max(from.x, to.x);
    for (emptyColumns.items) |column_index| {
        if (column_index > left_x and column_index < right_x) {
            expansion += (expansionRate - 1); // because we already counted one space in the original calculation
        }
    }
    return expansion;
}

fn computeVerticalExpansion(
    emptyRows: ArrayList(usize),
    from: Point,
    to: Point,
    expansionRate: usize,
) usize {
    if (from.y == to.y) {
        return 0;
    }
    var expansion: usize = 0;
    const left_y = min(from.y, to.y);
    const right_y = max(from.y, to.y);
    for (emptyRows.items) |row_index| {
        if (row_index > left_y and row_index < right_y) {
            expansion += (expansionRate - 1); // because we already counted one space in the original calculation
        }
    }
    return expansion;
}

fn count(comptime T: type, in: []const T, e: T) usize {
    var c: usize = 0;
    for (in) |x| {
        if (x == e) {
            c += 1;
        }
    }
    return c;
}

fn findEmptyColumns(lines: [][]const u8) ArrayList(usize) {
    var emptyColumns = ArrayList(usize).init(ALLOCATOR);
    for (lines[0], 0..) |_, x| {
        if (!isColumnEmpty(lines, x)) {
            continue;
        }
        emptyColumns.append(x) catch unreachable;
    }
    return emptyColumns;
}

fn findEmptyRows(lines: [][]const u8) ArrayList(usize) {
    var emptyRows = ArrayList(usize).init(ALLOCATOR);
    for (lines, 0..) |_, y| {
        if (!isRowEmpty(lines, y)) {
            continue;
        }
        emptyRows.append(y) catch unreachable;
    }
    return emptyRows;
}

fn findStarCoords(lines: [][]const u8) ArrayList(Point) {
    var result = ArrayList(Point).init(ALLOCATOR);
    for (lines, 0..) |row, y| {
        for (row, 0..) |pixel, x| {
            if (pixel == '#') {
                result.append(Point{ .x = x, .y = y }) catch unreachable;
            }
        }
    }
    return result;
}

fn isColumnEmpty(lines: [][]const u8, x: usize) bool {
    for (lines, 0..) |_, y| {
        if (!isEmpty(lines, x, y)) {
            return false;
        }
    }
    return true;
}

fn isEmpty(lines: [][]const u8, x: usize, y: usize) bool {
    return lines[y][x] == '.';
}

fn isRowEmpty(lines: [][]const u8, y: usize) bool {
    for (lines[y], 0..) |_, x| {
        if (!isEmpty(lines, x, y)) {
            return false;
        }
    }
    return true;
}

fn max(a: usize, b: usize) usize {
    if (a > b) return a;
    return b;
}

fn min(a: usize, b: usize) usize {
    if (a < b) return a;
    return b;
}

fn readFile(path: []const u8) []const u8 {
    const data = std.fs.cwd().readFileAlloc(ALLOCATOR, path, MAX_INPUT_LENGTH) catch unreachable;
    return std.mem.trim(u8, data, "\n");
}

fn splitByte(data: []const u8, b: u8) [][]const u8 {
    var sep = [_]u8{b};
    var groups = alloc([]const u8, count(u8, data, b) + 1);
    var it = std.mem.split(u8, data, sep[0..]);
    for (groups, 0..) |_, i| {
        groups[i] = it.next() orelse unreachable;
    }
    return groups;
}

fn readFileLines(path: []const u8) [][]const u8 {
    return splitByte(readFile(path), '\n');
}

pub fn main() !void {
    var lines = readFileLines("input.txt");
    std.debug.print("Input:\n", .{});
    for (lines) |line| {
        std.debug.print("{s}\n", .{line});
    }
    const emptyColumns = findEmptyColumns(lines);
    std.debug.print("Empty columns count: {d}\n", .{emptyColumns.items.len});
    const emptyRows = findEmptyRows(lines);
    std.debug.print("Empty rows count: {d}\n", .{emptyRows.items.len});
    const stars = findStarCoords(lines);
    std.debug.print("Star count: {d}\n", .{stars.items.len});
    var sumPart1: usize = 0;
    var sumPart2: usize = 0;
    for (stars.items[0 .. stars.items.len - 1], 0..) |from_star, from_index| {
        for (stars.items[(from_index + 1)..stars.items.len]) |to_star| {
            sumPart1 += computeDistanceWithExpansion(
                emptyColumns,
                emptyRows,
                from_star,
                to_star,
                2,
            );
            sumPart2 += computeDistanceWithExpansion(
                emptyColumns,
                emptyRows,
                from_star,
                to_star,
                1_000_000,
            );
        }
    }
    std.debug.print("Part 1: {d}\n", .{sumPart1});
    std.debug.print("Part 2: {d}\n", .{sumPart2});
}
