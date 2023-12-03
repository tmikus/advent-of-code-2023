def find_symbols(characters_grid: list[list[str]]) -> list[tuple[int, int]]:
    symbols = []
    for y, row in enumerate(characters_grid):
        for x, char in enumerate(row):
            if not char.isdigit() and char != '.':
                symbols.append((x, y))
    return symbols

def parse_input_into_character_list(input_lines: list[str]) -> list[list[str]]:
    return [[*line.strip()] for line in input_lines]

def read_input() -> list[str]:
    with open('input.txt', 'r') as f:
        lines = f.readlines()
    return lines

def extract_number_at_position(
    characters_grid: list[list[str]],
    start_x: int,
    start_y: int,
) -> (list[list[str]], list[int]):
    number_start_index = start_x
    number_end_index = start_x
    row = characters_grid[start_y]
    for x in range(start_x, len(row)):
        if not row[x].isdigit():
            break
        number_end_index = x
    for x in range(start_x, -1, -1):
        if not row[x].isdigit():
            break
        number_start_index = x
    if number_start_index == number_end_index and not row[number_start_index].isdigit():
        return characters_grid, []
    number = int(''.join(row[number_start_index:number_end_index + 1]))
    for x in range(number_start_index, number_end_index + 1):
        row[x] = '.'
    return characters_grid, [number]

def extract_all_numbers_near_symbol(
    symbol: tuple[int, int],
    characters_grid: list[list[str]],
) -> (list[list[str]], list[int]):
    all_numbers = []
    symbol_x, symbol_y = symbol
    for x in range(-1, 2):
        for y in range(-1, 2):
            (characters_grid, numbers) = extract_number_at_position(characters_grid, symbol_x + x, symbol_y + y)
            all_numbers = [*all_numbers, *numbers]
    return characters_grid, all_numbers


def extract_all_numbers_near_symbols(
    symbols: list[tuple[int, int]], 
    characters_grid: list[list[str]],
) -> (list[list[str]], list[int]):
    all_numbers = []
    for symbol in symbols:
        (characters_grid, numbers) = extract_all_numbers_near_symbol(symbol, characters_grid)
        all_numbers = [*all_numbers, *numbers]
    return characters_grid, all_numbers
    

def main():
    input_lines = read_input()
    characters = parse_input_into_character_list(input_lines)
    symbol_locations = find_symbols(characters)
    (characters, numbers) = extract_all_numbers_near_symbols(symbol_locations, characters)
    for row in characters:
        print(''.join(row))
    print(sum(numbers))

if __name__ == "__main__":
    main()
