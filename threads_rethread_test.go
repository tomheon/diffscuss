package diffscuss

import (
	"math"
	"testing"
	"time"
)

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
	return sumExponents(originalNumThreads, newDepth, originalDepth) / int(math.Pow(float64(originalNumThreads), float64(newDepth-1)))
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
			checkThreadsRecursive(t, threads[i].Replies, expectedDepth, originalDepth, depthLeft-1, originalNumThreads)
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
	params := newTestDiffscussionParams()
	params.depth = 1
	diffscussion1 := createTestDiffscussion(params)
	params.depth = 2
	diffscussion2 := createTestDiffscussion(params)

	checkAllDepths(t, diffscussion1, 1, 1, defaultNumThreads)
	checkAllDepths(t, diffscussion2, 2, 2, defaultNumThreads)

	if err := diffscussion1.Rethread(2); err != nil {
		t.Fatalf("Expected nil error, got %s", err)
	}

	if err := diffscussion2.Rethread(2); err != nil {
		t.Fatalf("Expected nil error, got %s", err)
	}

	checkAllDepths(t, diffscussion1, 1, 1, defaultNumThreads)
	checkAllDepths(t, diffscussion2, 2, 2, defaultNumThreads)

	if err := diffscussion1.Rethread(1); err != nil {
		t.Fatalf("Expected nil error, got %s", err)
	}
	checkAllDepths(t, diffscussion1, 1, 1, defaultNumThreads)
}

func TestRethreadingOneLevelWorks(t *testing.T) {
	params := newTestDiffscussionParams()
	params.depth = 2
	diffscussion := createTestDiffscussion(params)

	checkAllDepths(t, diffscussion, 2, 2, defaultNumThreads)
	if err := diffscussion.Rethread(1); err != nil {
		t.Fatalf("Expected nil error, got %s", err)
	}
	checkAllDepths(t, diffscussion, 1, 2, defaultNumThreads)
}

func TestRethreadingTwoLevelsWorks(t *testing.T) {
	params := newTestDiffscussionParams()
	params.depth = 3
	diffscussion := createTestDiffscussion(params)
	checkAllDepths(t, diffscussion, 3, 3, defaultNumThreads)
	if err := diffscussion.Rethread(1); err != nil {
		t.Fatalf("Expected nil error, got %s", err)
	}
	checkAllDepths(t, diffscussion, 1, 3, defaultNumThreads)
}

func TestRepeatedRethreadingWorks(t *testing.T) {
	params := newTestDiffscussionParams()
	params.depth = 3
	diffscussion := createTestDiffscussion(params)
	checkAllDepths(t, diffscussion, 3, 3, defaultNumThreads)

	if err := diffscussion.Rethread(2); err != nil {
		t.Fatalf("Expected nil error, got %s", err)
	}
	checkAllDepths(t, diffscussion, 2, 3, defaultNumThreads)
	if err := diffscussion.Rethread(1); err != nil {
		t.Fatalf("Expected nil error, got %s", err)
	}
	checkAllDepths(t, diffscussion, 1, 3, defaultNumThreads)
}

func TestBadDepthsErr(t *testing.T) {
	params := newTestDiffscussionParams()
	params.depth = 2
	diffscussion := createTestDiffscussion(params)
	err := diffscussion.Rethread(0)
	if err == nil {
		t.Fatalf("Expected error on 0 rethread, got nil")
	}

	err = diffscussion.Rethread(-1)
	if err == nil {
		t.Fatalf("Expected error on -1 rethread, got nil")
	}
}

func TestRethreadingResorts(t *testing.T) {
	params := newTestDiffscussionParams()
	params.numThreads = 3
	params.depth = 2
	diffscussion := createTestDiffscussion(params)

	setAllThreadTimes(diffscussion, []time.Time{defaultMadeAtTime, latestMadeAtTime, laterMadeAtTime})
	checkSorted(t, diffscussion, false)
	diffscussion.Rethread(1)
	checkSorted(t, diffscussion, true)
}
