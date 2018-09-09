package diffscuss_test

import (
	"diffscuss.com/diffscuss"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path"
	"reflect"
	"testing"
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

func makeComment(curDepth int, maxDepth int, grist int) diffscuss.Comment {
	return diffscuss.Comment{Author: "author", MadeAt: defaultMadeAtTime, Headers: make([]diffscuss.KeyValuePair, 0),
		Body: []string{fmt.Sprintf("comment %d %d %d", curDepth, maxDepth, grist)}}
}

func makeThreads(curDepth int, maxDepth int, numThreads int) []diffscuss.Thread {
	if curDepth < maxDepth {
		results := make([]diffscuss.Thread, numThreads)
		for i := range results {
			results[i] = diffscuss.Thread{Top: makeComment(curDepth, maxDepth, i), Replies: makeThreads(curDepth+1, maxDepth, numThreads)}
		}
		return results
	} else {
		return make([]diffscuss.Thread, 0)
	}
}

func makeFile(fileNum int, depth int, numThreads int) diffscuss.FileSection {
	return diffscuss.FileSection{Header: []string{fmt.Sprintf("file %d", fileNum)}, Hunks: make([]diffscuss.HunkSection, 0), Threads: makeThreads(0, depth, numThreads)}
}

func makeFiles(numFiles int, depth int, numThreads int) []diffscuss.FileSection {
	results := make([]diffscuss.FileSection, numFiles)
	for i := range results {
		results[i] = makeFile(i, depth, numThreads)
	}
	return results
}

func makeHunk(hunkNum int, depth int, numThreads int) diffscuss.HunkSection {
	return diffscuss.HunkSection{Header: []string{fmt.Sprintf("hunk %d", hunkNum)}, Lines: make([]diffscuss.Line, 0), Threads: makeThreads(0, depth, numThreads)}
}

func makeHunks(numHunks int, depth int, numThreads int) []diffscuss.HunkSection {
	results := make([]diffscuss.HunkSection, numHunks)
	for i := range results {
		results[i] = makeHunk(i, depth, numThreads)
	}
	return results
}

func makeLine(lineNum int, depth int, numThreads int) diffscuss.Line {
	return diffscuss.Line{Text: fmt.Sprintf("line %d", lineNum), Threads: makeThreads(0, depth, numThreads)}
}

func makeLines(numLines int, depth int, numThreads int) []diffscuss.Line {
	results := make([]diffscuss.Line, numLines)
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

func createTestDiffscussion(params *testDiffscussionParams) *diffscuss.Diffscussion {
	diffscussion := diffscuss.NewDiffscussion()
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

func getTestFileReader(diffName string) (io.Reader, error) {
	filePath := path.Join("testfiles", "parser", diffName)
	return os.Open(filePath)
}

func getTestFileBytes(diffName string) ([]byte, error) {
	filePath := path.Join("testfiles", "parser", diffName)
	return ioutil.ReadFile(filePath)
}

func checkHeader(t *testing.T, header []string, lines ...string) {
	if !reflect.DeepEqual(lines, header) {
		t.Fatalf("Expected header lines %s, got %s", lines, header)
	}
}

func checkThreadlessLines(t *testing.T, lines []diffscuss.Line, expected ...string) {
	justText := make([]string, len(lines))
	for i := range lines {
		justText[i] = lines[i].Text
		if len(lines[i].Threads) != 0 {
			t.Fatalf("Expected no threads, found %d", len(lines[i].Threads))
		}
	}

	if !reflect.DeepEqual(justText, expected) {
		t.Fatalf("Expected lines %s, got %s", expected, justText)
	}
}

func checkNoFileOrLowerThreads(t *testing.T, diffscussion *diffscuss.Diffscussion) {
	for i := range diffscussion.Files {
		f := diffscussion.Files[i]
		if len(f.Threads) != 0 {
			t.Fatalf("In file %s expected 0 threads, found %d", f.Header, len(f.Threads))
		}
		checkNoHunkOrLowerThreads(t, f)
	}
}

func checkNoHunkOrLowerThreads(t *testing.T, f diffscuss.FileSection) {
	for i := range f.Hunks {
		h := f.Hunks[i]
		if len(h.Threads) != 0 {
			t.Fatalf("In hunk %s expected 0 threads, found %d", h.Header, len(h.Threads))
		}
		checkNoLineThreads(t, h)
	}
}

func checkNoLineThreads(t *testing.T, h diffscuss.HunkSection) {
	for i := range h.Lines {
		l := h.Lines[i]
		if len(l.Threads) != 0 {
			t.Fatalf("In line %s expected 0 threads, found %d", l.Text, len(l.Threads))
		}
	}
}

func checkComment(t *testing.T, comment diffscuss.Comment, expectedAuthor string, expectedDate string, expectedHeaders []diffscuss.KeyValuePair, expectedBody []string) {
	if comment.Author != expectedAuthor {
		t.Fatalf("Expected author %s, got %s", expectedAuthor, comment.Author)
	}

	date, _ := diffscuss.ParseDiffscussDate(expectedDate)

	if !comment.MadeAt.Equal(date) {
		t.Fatalf("Expected made at %s, got %s", date, comment.MadeAt)
	}

	if !reflect.DeepEqual(comment.Headers, expectedHeaders) {
		t.Fatalf("Expected header %s, got %s", expectedHeaders, comment.Headers)
	}

	if !reflect.DeepEqual(expectedBody, comment.Body) {
		t.Fatalf("Expected body %s, got %s", expectedBody, comment.Body)
	}
}

func checkReplylessThread(t *testing.T, thread diffscuss.Thread, expectedAuthor string, expectedDate string, expectedHeaders []diffscuss.KeyValuePair, expectedBody []string) {
	if len(thread.Replies) != 0 {
		t.Fatalf("Expected 0 replies, got %d", len(thread.Replies))
	}
	checkComment(t, thread.Top, expectedAuthor, expectedDate, expectedHeaders, expectedBody)
}
