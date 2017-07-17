package diffscuss

import (
	"fmt"
	"time"
)

const (
	defaultDepth      = 2
	defaultNumThreads = 2
	defaultNumFiles   = 2
	defaultNumHunks   = 2
	defaultNumLines   = 2
)

var defaultMadeAtTime = time.Unix(1376611200, 0)
var laterMadeAtTime = time.Unix(1376611800, 0)
var latestMadeAtTime = time.Unix(1376612800, 0)

func makeComment(curDepth int, maxDepth int, grist int) Comment {
	return Comment{Author: "author", MadeAt: defaultMadeAtTime, Headers: make(map[string]string), Body: fmt.Sprintf("comment %d %d %d", curDepth, maxDepth, grist)}
}

func makeThreads(curDepth int, maxDepth int, numThreads int) []Thread {
	if curDepth < maxDepth {
		results := make([]Thread, numThreads)
		for i := range results {
			results[i] = Thread{Top: makeComment(curDepth, maxDepth, i), Replies: makeThreads(curDepth+1, maxDepth, numThreads)}
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

type testDiffscussionParams struct {
	depth      int
	numThreads int
	numHunks   int
	numFiles   int
	numLines   int
}

func newTestDiffscussionParams() *testDiffscussionParams {
	return &testDiffscussionParams{depth: defaultDepth, numThreads: defaultNumThreads, numHunks: defaultNumHunks, numFiles: defaultNumFiles, numLines: defaultNumLines}
}

func createTestDiffscussion(params *testDiffscussionParams) *Diffscussion {
	diffscussion := NewDiffscussion()
	diffscussion.Threads = makeThreads(0, params.depth, params.numThreads)
	diffscussion.Files = makeFiles(params.numFiles, params.depth, params.numThreads)
	for i := range diffscussion.Files {
		diffscussion.Files[i].Hunks = makeHunks(params.numHunks, params.depth, params.numThreads)
		for j := range diffscussion.Files[i].Hunks {
			diffscussion.Files[i].Hunks[j].Lines = makeLines(params.numLines, params.depth, params.numThreads)
		}
	}
	return diffscussion
}
