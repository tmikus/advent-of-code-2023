import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

class PuzzleRange {
  long dest;
  long src;
  long len;

  Optional<Long> getDestIndex(long srcIndex) {
    if (srcIndex < src || srcIndex >= src + len) {
      return Optional.empty();
    }
    return Optional.of((srcIndex - src) + dest);
  }

  static PuzzleRange readPuzzleRange(String line) {
    List<Long> numbers = PuzzleInput.readNumbers(line);
    PuzzleRange puzzleRange = new PuzzleRange();
    puzzleRange.dest = numbers.get(0);
    puzzleRange.src = numbers.get(1);
    puzzleRange.len = numbers.get(2);
    return puzzleRange;
  }
}

class PuzzleInput {
  List<Long> seeds = new ArrayList<>();
  List<PuzzleRange> seedToSoil = new ArrayList<>();
  List<PuzzleRange> soilToFertilizer = new ArrayList<>();
  List<PuzzleRange> fertilizerToWater = new ArrayList<>();
  List<PuzzleRange> waterToLight = new ArrayList<>();
  List<PuzzleRange> lightToTemperature = new ArrayList<>();
  List<PuzzleRange> temperatureToHumidity = new ArrayList<>();
  List<PuzzleRange> humidityToLocation = new ArrayList<>();

  long getDestIndex(List<PuzzleRange> ranges, long srcIndex) {
    for (PuzzleRange range : ranges) {
      Optional<Long> destIndex = range.getDestIndex(srcIndex);
      if (destIndex.isEmpty()) {
        continue;
      }
      return destIndex.get();
    }
    return srcIndex;
  }

  long getLocation(long seed) {
    long value = getDestIndex(seedToSoil, seed);
    value = getDestIndex(soilToFertilizer, value);
    value = getDestIndex(fertilizerToWater, value);
    value = getDestIndex(waterToLight, value);
    value = getDestIndex(lightToTemperature, value);
    value = getDestIndex(temperatureToHumidity, value);
    value = getDestIndex(humidityToLocation, value);
    return value;
  }

  List<PuzzleRange> getListForState(State state) {
    switch (state) {
      case ReadFertilizerToWaterMap:
        return this.fertilizerToWater;
      case ReadHumidityToLocationMap:
        return this.humidityToLocation;
      case ReadLightToTemperatureMap:
        return this.lightToTemperature;
      case ReadSeedToSoilMap:
        return this.seedToSoil;
      case ReadSoilToFertilizerMap:
        return this.soilToFertilizer;
      case ReadTemperatureToHumidityMap:
        return this.temperatureToHumidity;
      case ReadWaterToLightMap:
        return this.waterToLight;
      default:
        return null;
    }
  }

  static List<Long> readNumbers(String line) {
    Pattern p = Pattern.compile("(\\d+)+", Pattern.CASE_INSENSITIVE);
    Matcher m = p.matcher(line);
    List<Long> matches = new ArrayList<>();
    while (m.find()) {
      for (int group = 1; group <= m.groupCount(); group++) {
        String match = m.group(group);
        matches.add(Long.parseLong(match));
      }
    }
    return matches;
  }

  public boolean readSeeds(String line) {
    if (!line.startsWith("seeds:")) {
      return false;
    }
    this.seeds = readNumbers(line);
    return true;
  }
}

enum State {
  ReadSeeds,
  ReadNextGroupName,
  ReadSeedToSoilMap,
  ReadSoilToFertilizerMap,
  ReadFertilizerToWaterMap,
  ReadWaterToLightMap,
  ReadLightToTemperatureMap,
  ReadTemperatureToHumidityMap,
  ReadHumidityToLocationMap,
}

public class Main {
  static State GetNextStateForLine(String line) {
    if (line.startsWith("seed-to-soil")) {
      return State.ReadSeedToSoilMap;
    }
    if (line.startsWith("soil-to-fertilizer")) {
      return State.ReadSoilToFertilizerMap;
    }
    if (line.startsWith("fertilizer-to-water")) {
      return State.ReadFertilizerToWaterMap;
    }
    if (line.startsWith("water-to-light")) {
      return State.ReadWaterToLightMap;
    }
    if (line.startsWith("light-to-temperature")) {
      return State.ReadLightToTemperatureMap;
    }
    if (line.startsWith("temperature-to-humidity")) {
      return State.ReadTemperatureToHumidityMap;
    }
    if (line.startsWith("humidity-to-location")) {
      return State.ReadHumidityToLocationMap;
    }
    return State.ReadNextGroupName;
  }

  static void PrintLowestForSeeds(PuzzleInput puzzleInput, List<Long> seeds) {
    long lowest = Long.MAX_VALUE;
    for (Long seed : seeds) {
      long location = puzzleInput.getLocation(seed);
      if (location < lowest) {
        lowest = location;
      }
    }
    System.out.println("Lowest: " + lowest);
  }

  public static void main(String[] args) throws FileNotFoundException {
    File file = new File("input.txt");
    Scanner scanner = new Scanner(file);
    State state = State.ReadSeeds;
    PuzzleInput puzzleInput = new PuzzleInput();
    while (scanner.hasNext()) {
      String line = scanner.nextLine();
      switch (state) {
        case ReadSeeds:
          if (puzzleInput.readSeeds(line)) {
            state = State.ReadNextGroupName;
          }
          break;
        case ReadNextGroupName:
          state = GetNextStateForLine(line);
          break;
        default:
          if (line.trim().isEmpty()) {
            state = State.ReadNextGroupName;
            break;
          }
          List<PuzzleRange> list = puzzleInput.getListForState(state);
          list.add(PuzzleRange.readPuzzleRange(line));
          break;
      }
    }
    PrintLowestForSeeds(puzzleInput, puzzleInput.seeds);
  }
}
