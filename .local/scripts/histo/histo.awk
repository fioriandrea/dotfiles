#! /bin/awk -f

function max(n, k) {
    return n > k ? n : k
}

function min(n, k) {
    return n > k ? k : n
}

function rep(ch, n,    res) {
    while (n-- > 0)
        res = res ch
    return res
}

function processWord(count, word) {
    if (length(word) > maxWordLength) {
        word = substr(word, 1, maxWordLength) "..."
    } 
    return sprintf("%s (%d)", word, count)
}

function makeBar(count) {
    count = int((count / maxCount) * maxBar)
    return rep(symbol, count)
}

function printHistoLine(word, count,    ws, bar) {
    ws = rep(" ", padding - length(word))
    bar = makeBar(count)
    printf("%s%s %s\n", ws, word, bar)
}

function printData() {
    for (i = 1; i <= wordCount; i++) {
        word = words[i, "word"]
        count = words[i, "count"]
        printHistoLine(word, count)
    }
}

function printAxisLine(    stepb, ssep, sep) {
    stepb = int(maxBar / axisIntervals)
    printf("%s", rep("-", padding))
    ssep = rep("-", stepb - 1)
    for (i = 0; i <= maxBar; i += stepb) {
        printf("%s+", sep)
        sep = ssep
    }
    printf("\n")
}

function printAxisNumbers(    stepn, stepb, ii) {
    stepb = int(maxBar / axisIntervals)
    stepn = maxCount / axisIntervals
    printf("%s", rep(" ", padding))
    k = 1
    for (i = 0; k <= axisIntervals; i += stepn) {
        ii = int(i)
        printf("%d", ii)
        for (j = 0; j < stepb - length(ii); j++)
            printf(" ")
        k++
    }
    printf(maxCount)
    printf("\n")
}

function limitAxisIntervals() {
   axisIntervals = min(axisIntervals, maxCount)
}

function printBottomAxis() {
    printAxisLine()
    printAxisNumbers()
}

function printTopAxis() {
    printAxisNumbers()
    printAxisLine()
}

function readStandardData() {
    while (getline > 0) {
        wordCount++
        maxCount = max(maxCount, $1)
        word = processWord($1, $2)
        padding = max(padding + 0, length(word))
        words[wordCount, "word"] = word
        words[wordCount, "count"] = $1
    }
}

function readNumericData() {
    while (getline > 0) {
        numberCount++
        total += $1
        numbers[numberCount, "number"] = $2
        numbers[numberCount, "count"] = $1
    }
}

function processNumericData(    minNum, maxNum, step, numi, closed, count) {
    minNum = numbers[1, "number"]
    maxNum = numbers[numberCount, "number"]
    step = (maxNum - minNum) / numericIntervals
    numi = 1
    wordCount = 1
    for (i = minNum; i < maxNum; i += step) {
        closed = (i == (maxNum - step))
        count = 0
        while (numi < numberCount) {
            if (numbers[numi, "number"] >= i + step)
                break
            count += numbers[numi, "count"]
            numi++
        }
        if (closed) {
            count += numbers[numi, "count"]
            numi++
        }
        word = sprintf("%.2f - %.2f" (closed ? "]" : ")") "(%d)", i, i + step, count)
        words[wordCount, "word"] = word
        words[wordCount, "count"] = count
        padding = max(padding + 0, length(word))
        maxCount = max(maxCount, count)
        wordCount++
    }
}

BEGIN {
    # accepts input in the form FREQUENCY VALUE, sorted

    maxBar = 30
    maxWordLength = 10
    symbol = "*"
    numeric = 0
    numericIntervals = 10
    axisIntervals = 2
    hideAxis = 0
    topBottomAxes = 0
    topAxis = 0
    
    for (i = 1; i < ARGC; i++) {
        if (ARGV[i] == "n") {
            numeric = ARGV[++i]
        } else if (ARGV[i] == "ni") {
            numericIntervals = ARGV[++i]
        } else if (ARGV[i] == "wl") {
            maxWordLength = ARGV[++i]
        } else if (ARGV[i] == "mb") {
            maxBar = ARGV[++i]
        } else if (ARGV[i] == "s") {
            symbol = ARGV[++i]
        } else if (ARGV[i] == "ai") {
            axisIntervals = ARGV[++i]
        } else if (ARGV[i] == "ha") {
            hideAxis = ARGV[++i]
        } else if (ARGV[i] == "tb") {
            topBottomAxes = ARGV[++i]
        } else if (ARGV[i] == "t") {
            topAxis = ARGV[++i]
        } else {
            printf("unknown option '%s'\n", ARGV[i]) | "cat 1>&2"
            exit 1
        }
    }
    ARGC = 1

    if (numeric) {
        readNumericData()
        processNumericData()
    } else {
        readStandardData()
    }

    limitAxisIntervals()

    if ((topAxis || topBottomAxes) && !hideAxis)
        printTopAxis()
    printData()
    if (!topAxis && !hideAxis)
        printBottomAxis()
}
