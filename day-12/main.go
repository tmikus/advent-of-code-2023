package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type ConditionLine struct {
	condition     string
	damagedGroups []int
}

var cache = make(map[string]int)

func groupsToString(groups []int) string {
	var result string
	for _, group := range groups {
		result += strconv.Itoa(group) + ","
	}
	return result
}

func cachedCountPossibleArrangements(condition string, groups []int) int {
	key := condition + " " + groupsToString(groups)
	if value, ok := cache[key]; ok {
		return value
	}
	value := countPossibleArrangements(condition, groups)
	cache[key] = value
	return value
}

func countPossibleArrangements(condition string, groups []int) int {
	if len(groups) == 0 {
		if strings.Contains(condition, "#") {
			return 0
		}
		return 1
	}
	if len(condition) == 0 {
		if len(groups) > 0 {
			return 0
		}
		return 1
	}
	result := 0
	if condition[0] == '.' || condition[0] == '?' {
		result += cachedCountPossibleArrangements(condition[1:], groups)
	}
	if condition[0] == '#' || condition[0] == '?' {
		if groups[0] <= len(condition) &&
			!strings.Contains(condition[:groups[0]], ".") &&
			(groups[0] == len(condition) || condition[groups[0]] != '#') {
			startIndex := groups[0] + 1
			if startIndex >= len(condition) {
				startIndex = len(condition)
			}
			result += cachedCountPossibleArrangements(condition[startIndex:], groups[1:])
		}
	}
	return result
}

func parseConditionLine(line string) ConditionLine {
	groups := strings.Split(line, " ")
	return ConditionLine{
		condition:     groups[0],
		damagedGroups: parseDamagedGroups(groups[1]),
	}
}

func parseDamagedGroups(value string) []int {
	damagedGroups := strings.Split(value, ",")
	var damagedGroupsInt []int
	for _, group := range damagedGroups {
		groupInt, _ := strconv.ParseInt(group, 10, 64)
		damagedGroupsInt = append(damagedGroupsInt, int(groupInt))
	}
	return damagedGroupsInt
}

func parseConditionLines(lines []string) []ConditionLine {
	var conditionLines []ConditionLine
	for _, line := range lines {
		conditionLines = append(conditionLines, parseConditionLine(line))
	}
	return conditionLines
}

func readInputLines() []string {
	reader := bufio.NewReader(os.Stdin)
	var lines []string
	for {
		line, err := reader.ReadString('\n')
		if err != nil {
			break
		}
		lines = append(lines, strings.TrimSpace(line))
	}
	return lines
}

func main() {
	lines := readInputLines()
	conditionLines := parseConditionLines(lines)
	sum := 0
	for _, conditionLine := range conditionLines {
		sum += cachedCountPossibleArrangements(conditionLine.condition, conditionLine.damagedGroups)
	}
	fmt.Printf("Part 1: %d\n", sum)
	sum = 0
	for _, conditionLine := range conditionLines {
		condition := conditionLine.condition
		for i := 0; i < 4; i++ {
			conditionLine.condition += "?" + condition
		}
		groups := make([]int, len(conditionLine.damagedGroups))
		copy(groups, conditionLine.damagedGroups)
		for i := 0; i < 4; i++ {
			conditionLine.damagedGroups = append(conditionLine.damagedGroups, groups...)
		}
		sum += cachedCountPossibleArrangements(conditionLine.condition, conditionLine.damagedGroups)
	}
	fmt.Printf("Part 2: %d\n", sum)
}
