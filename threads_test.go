package diffscuss

import (
	"fmt"
	"testing"
	"time"
)

const (
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

func makeHunk(fileNum int, depth int, numThreads int) HunkSection {
	return HunkSection{Header: []string{fmt.Sprintf("file %d", fileNum)}, Lines: make([]Line, 0), Threads: makeThreads(0, depth, numThreads)}
}

func makeHunks(numHunks int, depth int, numThreads int) []HunkSection {
	results := make([]HunkSection, numHunks)
	for i := range results {
		results[i] = makeHunk(i, depth, numThreads)
	}
	return results
}

func createTestDiffscussion(depth int, numThreads int) *Diffscussion {
	diffscussion := NewDiffscussion()
	diffscussion.Threads = makeThreads(0, depth, numThreads)
	diffscussion.Files = makeFiles(defaultNumFiles, depth, numThreads)
	for i := range diffscussion.Files {
		diffscussion.Files[i].Hunks = makeHunks(defaultNumHunks, depth, numThreads)
	}
	return diffscussion
}

func checkThreads(t *testing.T, threads []Thread, expectedDepth int, expectedNumThreads int) {
	if expectedDepth > 0 {
		if len(threads) != expectedNumThreads {
			t.Fatalf("Expected %d threads, found %d", expectedNumThreads, len(threads))
		}

		for i := range threads {
			checkThreads(t, threads[i].Replies, expectedDepth - 1, expectedNumThreads)
		}
	} else {
		for i := range threads {
			if len(threads[i].Replies) != 0 {
				t.Fatalf("Expected no replies, found %d", len(threads[i].Replies))
			}
		}
	}
}

func checkAllDepths(t *testing.T, diffscussion *Diffscussion, expectedDepth int, expectedNumThreads int) {
	checkThreads(t, diffscussion.Threads, expectedDepth, expectedNumThreads)
	if len(diffscussion.Files) != defaultNumFiles {
		t.Fatalf("Expected %d files, found %d", defaultNumFiles, len(diffscussion.Files))
	}
	for i := range diffscussion.Files {
		f := diffscussion.Files[i]
		checkThreads(t, f.Threads, expectedDepth, expectedNumThreads)

		if len(f.Hunks) != defaultNumHunks {
			t.Fatalf("Expected %d hunks, found %d", defaultNumHunks, len(f.Hunks))
		}
		for j := range f.Hunks {
			h := f.Hunks[j]
			checkThreads(t, h.Threads, expectedDepth, expectedNumThreads)
		}
	}
}

func TestRethreadingNoOpsOnShallowThreads(t *testing.T) {
	const numThreads = 2
	diffscussion1 := createTestDiffscussion(1, numThreads)
	diffscussion2 := createTestDiffscussion(2, numThreads)
	checkAllDepths(t, diffscussion1, 1, numThreads)
	checkAllDepths(t, diffscussion2, 2, numThreads)
}
