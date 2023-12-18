extension String {

    var length: Int {
        return count
    }

    subscript (i: Int) -> String {
        return self[i ..< i + 1]
    }

    func substring(fromIndex: Int) -> String {
        return self[min(fromIndex, length) ..< length]
    }

    func substring(toIndex: Int) -> String {
        return self[0 ..< max(0, toIndex)]
    }

    subscript (r: Range<Int>) -> String {
        let range = Range(uncheckedBounds: (lower: max(0, min(length, r.lowerBound)),
                                            upper: min(length, max(0, r.upperBound))))
        let start = index(startIndex, offsetBy: range.lowerBound)
        let end = index(start, offsetBy: range.upperBound - range.lowerBound)
        return String(self[start ..< end])
    }
}

func countHorizontalDifferences(left: String, right: String) -> Int {
    var count = 0
    for column in 0..<left.count {
        if left[column] != right[column] {
            count += 1
        }
    }
    return count
}

func countVerticalDifferences(image: [String], leftColumnIndex: Int, rightColumnIndex: Int) -> Int {
    var count = 0
    for row in 0..<image.count {
        if image[row][leftColumnIndex] != image[row][rightColumnIndex] {
            count += 1
        }
    }
    return count
}

func findHorizontalReflection(image: [String]) -> Int? {
    let height = image.count
    for firstUpperFoldRowIndex in 0..<height-1 {
        let firstLowerFoldRowIndex = firstUpperFoldRowIndex + 1
        for offset in 0..<height {
            let upperRowIndex = firstUpperFoldRowIndex - offset
            let lowerRowIndex = firstLowerFoldRowIndex + offset
            // Have we reached the end?
            if upperRowIndex < 0 || lowerRowIndex >= height {
                return firstUpperFoldRowIndex + 1
            }
            let differences = countHorizontalDifferences(left: image[upperRowIndex], right: image[lowerRowIndex])
            if differences > 0 {
                break
            }
        }
    }
    return nil
}

func findVerticalReflection(image: [String]) -> Int? {
    let width = image[0].count
    let height = image.count
    for firstLeftFoldColumnIndex in 0..<width-1 {
        let firstRightFoldColumnIndex = firstLeftFoldColumnIndex + 1
        for offset in 0..<width {
            let leftColumnIndex = firstLeftFoldColumnIndex - offset
            let rightColumnIndex = firstRightFoldColumnIndex + offset
            // Have we reached the end?
            if leftColumnIndex < 0 || rightColumnIndex >= width {
                return firstLeftFoldColumnIndex + 1
            }
            let differences = countVerticalDifferences(image: image, leftColumnIndex: leftColumnIndex, rightColumnIndex: rightColumnIndex)
            if differences > 0 {
                break
            }
        }
    }
    return nil
}

func findColumnWithOneDifference(image: [String]) -> Int? {
    let width = image[0].count
    let height = image.count
    for firstLeftFoldColumnIndex in 0..<width-1 {
        let firstRightFoldColumnIndex = firstLeftFoldColumnIndex + 1
        var foundPotentialChange = false
        for offset in 0..<width {
            let leftColumnIndex = firstLeftFoldColumnIndex - offset
            let rightColumnIndex = firstRightFoldColumnIndex + offset
            // Have we reached the end?
            if leftColumnIndex < 0 || rightColumnIndex >= width {
                break
            }
            let differences = countVerticalDifferences(image: image, leftColumnIndex: leftColumnIndex, rightColumnIndex: rightColumnIndex)
            if !foundPotentialChange && differences == 1 {
                foundPotentialChange = true
            } else if differences > 0 {
                foundPotentialChange = false;
                break;
            }
        }
        if foundPotentialChange {
            return firstLeftFoldColumnIndex + 1
        }
    }
    return nil
}

func findRowWithOneDifference(image: [String]) -> Int? {
    let height = image.count
    for firstUpperFoldRowIndex in 0..<height-1 {
        let firstLowerFoldRowIndex = firstUpperFoldRowIndex + 1
        var foundPotentialChange = false
        for offset in 0..<height {
            let upperRowIndex = firstUpperFoldRowIndex - offset
            let lowerRowIndex = firstLowerFoldRowIndex + offset
            // Have we reached the end?
            if upperRowIndex < 0 || lowerRowIndex >= height {
                break
            }
            let differences = countHorizontalDifferences(left: image[upperRowIndex], right: image[lowerRowIndex])
            if !foundPotentialChange && differences == 1 {
                foundPotentialChange = true
            } else if differences > 0 {
                foundPotentialChange = false
                break;
            }
        }
        if foundPotentialChange {
            return firstUpperFoldRowIndex + 1
        }
    }
    return nil
}

func readImages() -> [[String]] {
    var images: [[String]] = []
    var currentImage: [String] = []
    while let line = readLine() {
        if line.isEmpty {
            images.append(currentImage)
            currentImage = []
        } else {
            currentImage.append(line)
        }
    }
    return images
}

func printPart1Result(images: [[String]]) {
    var horizontalLines = 0
    var verticalLines = 0
    for image in images {
        if let value = findHorizontalReflection(image: image) {
            horizontalLines += value
        } else if let value = findVerticalReflection(image: image) {
            verticalLines += value
        }
    }
    print("Part 1: ", (verticalLines + (100 * horizontalLines)))
}

func printPart2Result(images: [[String]]) {
    var horizontalLines = 0
    var verticalLines = 0
    for image in images {
        if let value = findRowWithOneDifference(image: image) {
            horizontalLines += value
        } else if let value = findColumnWithOneDifference(image: image) {
            verticalLines += value
        }
    }
    print("Part 2: ", (verticalLines + (100 * horizontalLines)))
}

func main() {
    let images = readImages()
    printPart1Result(images: images)
    printPart2Result(images: images)
}

main()
