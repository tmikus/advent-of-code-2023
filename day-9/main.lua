local inspect = require("lib.inspect")

local function all(array, predicate)
    local result = true
    for _, item in pairs(array) do
    	result = result and predicate(item)
    end
    return result
end

local function isZero(value)
    return value == 0
end

local function computeDifferencesBetweenNeighbours(numbers)
    local differences = {}
    for i=2,#numbers do
    	local left = numbers[i-1]
    	local right = numbers[i]
    	table.insert(differences, right - left)
    end
    return differences
end

local function computeAllLevelsOfDifferencesBetweenNeighbours(numbers)
    local differences = {numbers}
    while not all(differences[#differences], isZero) do
    	table.insert(differences, computeDifferencesBetweenNeighbours(differences[#differences]))
    end
    return differences
end

local function predictNextNumbers(differences)
    local lastDifferences = differences[#differences]
    local numbers = {lastDifferences[#lastDifferences]}
    for i=#differences-1,1,-1 do
        local left = differences[i]
        local right = numbers[#numbers]
    	local predictedNumber = left[#left] + right
    	table.insert(numbers, predictedNumber)
    end
    return numbers
end

local function predictPreviousNumbers(differences)
    local lastDifferences = differences[#differences]
    local numbers = {lastDifferences[1]}
    for i=#differences-1,1,-1 do
        local left = differences[i]
        local right = numbers[#numbers]
        local predictedNumber = left[1] - right
        table.insert(numbers, predictedNumber)
    end
    return numbers
end

local function split(inputstr, sep)
      if sep == nil then
          sep = "%s"
      end
      local t={}
      for str in string.gmatch(inputstr, "([^"..sep.."]+)") do
          table.insert(t, str)
      end
      return t
end

local function parseNumbers(line)
    local parsedNumbers = {}
    for _, number in pairs(split(line, " ")) do
    	table.insert(parsedNumbers, tonumber(number))
    end
    return parsedNumbers
end

local function main()
    local nextNumbers = 0
    local previousNumbers = 0
    while true do
        local line = io.read()
        if line == nil then
        	break
        end
        local numbers = parseNumbers(line)
        local differences = computeAllLevelsOfDifferencesBetweenNeighbours(numbers)
        local predictedNextNumbers = predictNextNumbers(differences)
        local predictedPreviousNumbers = predictPreviousNumbers(differences)
        nextNumbers = nextNumbers + predictedNextNumbers[#predictedNextNumbers]
        previousNumbers = previousNumbers + predictedPreviousNumbers[#predictedPreviousNumbers]
    end
    print("Part 1:", nextNumbers)
    print("Part 2:", previousNumbers)
end

main()
