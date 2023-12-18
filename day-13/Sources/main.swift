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
            if image[upperRowIndex] != image[lowerRowIndex] {
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
    loop: for offset in 0..<width {
            let leftColumnIndex = firstLeftFoldColumnIndex - offset
            let rightColumnIndex = firstRightFoldColumnIndex + offset
            // Have we reached the end?
            if leftColumnIndex < 0 || rightColumnIndex >= width {
                return firstLeftFoldColumnIndex + 1
            }
            for row in 0..<height {
                let imageRow = image[row]
                if imageRow[leftColumnIndex] != imageRow[rightColumnIndex] {
                    break loop
                }
            }
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

func main() {
    let images = readImages()
    var horizontalLines = 0
    var verticalLines = 0
    for image in images {
        if let value = findHorizontalReflection(image: image) {
            print("Horizontal reflection:", value)
            horizontalLines += value
        } else if let value = findVerticalReflection(image: image) {
            verticalLines += value
        }
    }
    print("Part 1: ", (verticalLines + (100 * horizontalLines)))
}

main()
