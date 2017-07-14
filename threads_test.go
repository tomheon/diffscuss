package diffscuss

import (
	"fmt"
	"math"
	"testing"
	"time"
)

const (
	defaultNumThreads = 2
	defaultNumFiles = 2
	defaultNumHunks = 2
	defaultNumLines = 2
)

func makeComment(curDepth int, maxDepth int, grist int) Comment {
	return Comment{Author: "author", MadeAt: time.Now(), Headers: make(map[string]string), Body: fmt.Sprintf("comment %d %d %d", curDepth, maxDepth, grist)}
}

func makeThreads(curDepth int, maxDepth int, numThreads int) []Thread {
	if curDepth < maxDepth {
		results := make([]Thread, numThreads)
		for i := range results {
			results[i] = Thread{Top: makeComment(curDepth, maxDepth, i), Replies: makeThreads(curDepth + 1, maxDepth, numThreads)}
		}
		return results
	} else {
		return make([]Thread, 0)
	}
}

func makeFile(fileNum int, depth int, numThreads int) FileSection {
	return FileSection{Header: []string{fmt.Sprintf("file %d", fileNum)}, Hunks: make([]HunkSection, 0), Threads: makeThreads(0, depth, numThreads)}
}

func makeFiles(numFiles int, depth int, numThreads int) []FileSection {
	results := make([]FileSection, numFiles)
	for i := range results {
		results[i] = makeFile(i, depth, numThreads)
	}
	return results
}

func makeHunk(hunkNum int, depth int, numThreads int) HunkSection {
	return HunkSection{Header: []string{fmt.Sprintf("hunk %d", hunkNum)}, Lines: make([]Line, 0), Threads: makeThreads(0, depth, numThreads)}
}

func makeHunks(numHunks int, depth int, numThreads int) []HunkSection {
	results := make([]HunkSection, numHunks)
	for i := range results {
		results[i] = makeHunk(i, depth, numThreads)
	}
	return results
}

func makeLine(lineNum int, depth int, numThreads int) Line {
	return Line{Text: fmt.Sprintf("line %d", lineNum), Threads: makeThreads(0, depth, numThreads)}
}

func makeLines(numLines int, depth int, numThreads int) []Line {
	results := make([]Line, numLines)
	for i := range results {
		results[i] = makeLine(i, depth, numThreads)
	}
	return results
}

func createTestDiffscussion(depth int, numThreads int) *Diffscussion {
	diffscussion := NewDiffscussion()
	diffscussion.Threads = makeThreads(0, depth, numThreads)
	diffscussion.Files = makeFiles(defaultNumFiles, depth, numThreads)
	for i := range diffscussion.Files {
		diffscussion.Files[i].Hunks = makeHunks(defaultNumHunks, depth, numThreads)
		for j := range diffscussion.Files[i].Hunks {
			diffscussion.Files[i].Hunks[j].Lines = makeLines(defaultNumLines, depth, numThreads)
		}
	}
	return diffscussion
}


func checkThreads(t *testing.T, threads []Thread, expectedDepth int, originalDepth int, expectedNumThreads int) {
	checkThreadsRecursive(t, threads, expectedDepth, originalDepth, expectedDepth, expectedNumThreads)
}

func sumExponents(base int, from int, to int) int {
	result := 0
	for ; from <= to; from++ {
		result += int(math.Pow(float64(base), float64(from)))
	}
	return result
}

func calculateTerminalNumThreads(originalNumThreads int, originalDepth int, newDepth int) int {
	return sumExponents(originalNumThreads, newDepth, originalDepth) / int(math.Pow(float64(originalNumThreads), float64(newDepth - 1)))
}

func checkThreadsRecursive(t *testing.T, threads []Thread, expectedDepth int, originalDepth int, depthLeft int, originalNumThreads int) {
	if depthLeft == 1 {
		expectedNumThreads := calculateTerminalNumThreads(originalNumThreads, originalDepth, expectedDepth)
		if len(threads) != expectedNumThreads {
			t.Fatalf("Expected %d threads, found %d", expectedNumThreads, len(threads))
		}
	} else if depthLeft > 0 {
		if len(threads) != originalNumThreads {
			t.Fatalf("Expected %d threads, found %d", originalNumThreads, len(threads))
		}

		for i := range threads {
			checkThreadsRecursive(t, threads[i].Replies, expectedDepth, originalDepth, depthLeft -1, originalNumThreads)
		}
	} else {
		for i := range threads {
			if len(threads[i].Replies) != 0 {
				t.Fatalf("Expected no replies, found %d", len(threads[i].Replies))
			}
		}
	}
}

func checkLines(t *testing.T, lines []Line, expectedDepth int, originalDepth int, originalNumThreads int) {
	if len(lines) != defaultNumLines {
		t.Fatalf("Expected %d lines, found %d", defaultNumLines, len(lines))
	}

	for k := range lines {
		l := lines[k]
		checkThreads(t, l.Threads, expectedDepth, originalDepth, originalNumThreads)
	}
}

func checkHunks(t *testing.T, hunks []HunkSection, expectedDepth int, originalDepth int, originalNumThreads int) {
	if len(hunks) != defaultNumHunks {
		t.Fatalf("Expected %d hunks, found %d", defaultNumHunks, len(hunks))
	}
	for j := range hunks {
		h := hunks[j]

		checkThreads(t, h.Threads, expectedDepth, originalDepth, originalNumThreads)
		checkLines(t, h.Lines, expectedDepth, originalDepth, originalNumThreads)
	}
}

func checkFiles(t *testing.T, files []FileSection, expectedDepth int, originalDepth int, originalNumThreads int) {
	if len(files) != defaultNumFiles {
		t.Fatalf("Expected %d files, found %d", defaultNumFiles, len(files))
	}
	for i := range files {
		f := files[i]
		checkThreads(t, f.Threads, expectedDepth, originalDepth, originalNumThreads)
		checkHunks(t, f.Hunks, expectedDepth, originalDepth, originalNumThreads)
	}
}

func checkAllDepths(t *testing.T, diffscussion *Diffscussion, expectedDepth int, originalDepth int, originalNumThreads int) {
	checkThreads(t, diffscussion.Threads, expectedDepth, originalDepth, originalNumThreads)
	checkFiles(t, diffscussion.Files, expectedDepth, originalDepth, originalNumThreads)
}

func TestRethreadingNoOpsOnShallowThreads(t *testing.T) {
	diffscussion1 := createTestDiffscussion(1, defaultNumThreads)
	diffscussion2 := createTestDiffscussion(2, defaultNumThreads)
	checkAllDepths(t, diffscussion1, 1, 1, defaultNumThreads)
	checkAllDepths(t, diffscussion2, 2, 2, defaultNumThreads)
	diffscussion1.Rethread(2)
	diffscussion2.Rethread(2)
	checkAllDepths(t, diffscussion1, 1, 1, defaultNumThreads)
	checkAllDepths(t, diffscussion2, 2, 2, defaultNumThreads)
	diffscussion1.Rethread(1)
	checkAllDepths(t, diffscussion1, 1, 1, defaultNumThreads)
}

func TestRethreadingOneLevelWorks(t *testing.T) {
	diffscussion := createTestDiffscussion(2, defaultNumThreads)
	checkAllDepths(t, diffscussion, 2, 2, defaultNumThreads)
	diffscussion.Rethread(1)
	checkAllDepths(t, diffscussion, 1, 2, defaultNumThreads)
}

func TestRethreadingTwoLevelsWorks(t *testing.T) {
	diffscussion := createTestDiffscussion(3, defaultNumThreads)
	checkAllDepths(t, diffscussion, 3, 3, defaultNumThreads)
	diffscussion.Rethread(1)
	checkAllDepths(t, diffscussion, 1, 3, defaultNumThreads)
}

func TestRepeatedRethreadingWorks(t	*testing.T) {
	diffscussion := createTestDiffscussion(3, defaultNumThreads)
	checkAllDepths(t, diffscussion, 3, 3, defaultNumThreads)
	diffscussion.Rethread(2)
	checkAllDepths(t, diffscussion, 2, 3, defaultNumThreads)
	diffscussion.Rethread(1)
	checkAllDepths(t, diffscussion, 1, 3, defaultNumThreads)
}
