package diffscuss

import (
	"bufio"
	"strings"
	"testing"
)

func TestScanLinePairsNoLines(t *testing.T) {
	scanner := bufio.NewScanner(strings.NewReader(""))
	scannedLinePairs := make(chan *scannedLinePair)
	go scanLinePairs(scanner, scannedLinePairs)

	results := make([]*scannedLinePair, 0)
	for res := range scannedLinePairs {
		results = append(results, res)
	}
	if len(results) != 0 {
		t.Fatalf("Expected no line pairs, got %d", len(results))
	}
}

func checkLinePair(t *testing.T, linePair *scannedLinePair, expectedFirst string, expectedSecond string, expectedHasSecond bool, expectedErr bool) {
	if linePair.first != expectedFirst {
		t.Fatalf("Expected first line %s got %s", expectedFirst, linePair.first)
	}

	if expectedHasSecond != linePair.hasSecond {
		t.Fatalf("Expected hasSecond %s got %s", expectedHasSecond, linePair.hasSecond)
	}

	if expectedSecond != linePair.second {
		t.Fatalf("Expected second %s got %s", expectedSecond, linePair.second)
	}

	if (linePair.err != nil) != expectedErr {
		t.Fatalf("Expected err line %s got %s", expectedErr, linePair.err != nil)
	}
}

func TestScanLinePairsOneLine(t *testing.T) {
	scanner := bufio.NewScanner(strings.NewReader("one line"))
	scannedLinePairs := make(chan *scannedLinePair)
	go scanLinePairs(scanner, scannedLinePairs)

	results := make([]*scannedLinePair, 0)
	for res := range scannedLinePairs {
		results = append(results, res)
	}

	if len(results) != 1 {
		t.Fatalf("Expected 1 line pair, got %d", len(results))
	}

	checkLinePair(t, results[0], "one line", "", false, false)
}

func TestScanLinePairsTwoLines(t *testing.T) {
	scanner := bufio.NewScanner(strings.NewReader("one line\ntwo lines"))
	scannedLinePairs := make(chan *scannedLinePair)
	go scanLinePairs(scanner, scannedLinePairs)

	results := make([]*scannedLinePair, 0)
	for res := range scannedLinePairs {
		results = append(results, res)
	}

	if len(results) != 2 {
		t.Fatalf("Expected 2 line pairs, got %d", len(results))
	}

	checkLinePair(t, results[0], "one line", "two lines", true, false)
	checkLinePair(t, results[1], "two lines", "", false, false)
}


func TestScanLinePairsThreeLines(t *testing.T) {
	scanner := bufio.NewScanner(strings.NewReader("one line\ntwo lines\nthree lines"))
	scannedLinePairs := make(chan *scannedLinePair)
	go scanLinePairs(scanner, scannedLinePairs)

	results := make([]*scannedLinePair, 0)
	for res := range scannedLinePairs {
		results = append(results, res)
	}

	if len(results) != 3 {
		t.Fatalf("Expected 3 line pairs, got %d", len(results))
	}

	checkLinePair(t, results[0], "one line", "two lines", true, false)
	checkLinePair(t, results[1], "two lines", "three lines", true, false)
	checkLinePair(t, results[2], "three lines", "", false, false)
}

func badSplit(data []byte, atEOF bool) (int, []byte, error) {
	return -1, nil, nil
}

func TestScanLinePairsErrorFirstLine(t *testing.T) {
	scanner := bufio.NewScanner(strings.NewReader("this is the first line"))
	scanner.Split(badSplit)
	scannedLinePairs := make(chan *scannedLinePair)
	go scanLinePairs(scanner, scannedLinePairs)

	results := make([]*scannedLinePair, 0)
	for res := range scannedLinePairs {
		results = append(results, res)
	}

	// fmt.Println(results[0].err)

	if len(results) != 1 {
		t.Fatalf("Expected 1 line pairs, got %d", len(results))
	}

	checkLinePair(t, results[0], "", "", false, true)
}

func badSplitAlwaysHello(data []byte, atEOF bool) (int, []byte, error) {
	return 6, []byte("hello"), nil
}

func TestScanLinePairsErrorSecondLine(t *testing.T) {
	scanner := bufio.NewScanner(strings.NewReader("hello\nhi"))
	scanner.Split(badSplitAlwaysHello)
	scannedLinePairs := make(chan *scannedLinePair)
	go scanLinePairs(scanner, scannedLinePairs)

	results := make([]*scannedLinePair, 0)
	for res := range scannedLinePairs {
		results = append(results, res)
	}

	if len(results) != 1 {
		t.Fatalf("Expected 1 line pair, got %d", len(results))
	}

	checkLinePair(t, results[0], "", "", false, true)
}

func TestScanLinePairsErrorThirdLine(t *testing.T) {
	scanner := bufio.NewScanner(strings.NewReader("hello\nhello\nhi"))
	scanner.Split(badSplitAlwaysHello)
	scannedLinePairs := make(chan *scannedLinePair)
	go scanLinePairs(scanner, scannedLinePairs)

	results := make([]*scannedLinePair, 0)
	for res := range scannedLinePairs {
		results = append(results, res)
	}

	if len(results) != 2 {
		t.Fatalf("Expected 2 line pairs, got %d", len(results))
	}

	checkLinePair(t, results[0], "hello", "hello", true, false)
	checkLinePair(t, results[1], "", "", false, true)
}
