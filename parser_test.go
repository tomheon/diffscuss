package diffscuss

import (
	"io"
	"os"
	"path"
	"reflect"
	"testing"
)

func getTestFileReader(diffName string) (io.Reader, error) {
	filePath := path.Join("testfiles", "parser", diffName)
	return os.Open(filePath)
}

func checkHeader(t *testing.T, header []string, lines ...string) {
	if !reflect.DeepEqual(lines, header) {
		t.Fatalf("Expected header lines %s, got %s", lines, header)
	}
}

func checkThreadlessLines(t *testing.T, lines []Line, expected ...string) {
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

func TestParseTinyDiff(t *testing.T) {
	diffFile, err := getTestFileReader("tiny.diff")
	if err != nil {
		t.Fatal(err)
	}

	diffscussion, err := Parse(diffFile)
	if err != nil {
		t.Fatal(err)
	}

	if len(diffscussion.Files) != 3 {
		t.Fatalf("Expected 3 files, got %d", len(diffscussion.Files))
	}

	checkHeader(t, diffscussion.Files[0].Header,
		"diff --git a/file3.txt b/file3.txt",
		"index 5db4c05..81212b6 100644",
		"--- a/file3.txt",
		"+++ b/file3.txt")

	if len(diffscussion.Files[0].Hunks) != 1 {
		t.Fatalf("Expected 1 hunk, found %d", len(diffscussion.Files[0].Hunks))
	}

	checkHeader(t, diffscussion.Files[0].Hunks[0].Header, "@@ -4,6 +4,9 @@ fun")
	checkThreadlessLines(t, diffscussion.Files[0].Hunks[0].Lines,
		" ",
		" exciting!",
		" ",
		"+",
		"+fiejwofwej",
		"+",
		" fun",
		" ",
		" somethign at the end")

	checkHeader(t, diffscussion.Files[1].Header,
		"diff --git a/hi.txt b/hi.txt",
		"index b39fd4b..f17f59b 100644",
		"--- a/hi.txt",
		"+++ b/hi.txt")

	if len(diffscussion.Files[1].Hunks) != 2 {
		t.Fatalf("Expected 2 hunks, found %d", len(diffscussion.Files[1].Hunks))
	}

	checkHeader(t, diffscussion.Files[1].Hunks[0].Header, "@@ -8,3 +8,5 @@ hi")
	checkThreadlessLines(t, diffscussion.Files[1].Hunks[0].Lines,
		" hi",
		" hi",
		" hi",
		"+this is a test",
		"+of the emergency broadcast system")
	checkHeader(t, diffscussion.Files[1].Hunks[1].Header, "@@ -11,2 +13,1 @@ bye")
	checkThreadlessLines(t, diffscussion.Files[1].Hunks[1].Lines,
		" hello",
		"- bye",
		"\\ No newline at end of file")

	if len(diffscussion.Files[2].Hunks) != 0 {
		t.Fatalf("Expected 0 hunks, found %d", len(diffscussion.Files[2].Hunks))
	}
	checkHeader(t, diffscussion.Files[2].Header,
		"diff --git a/t.jpg b/t.jpg",
		"index d2d8abc..212305d 100644",
		"Binary files a/t.jpg and b/t.jpg differ")
}

func checkNoFileOrLowerThreads(t *testing.T, diffscussion *Diffscussion) {
	for i := range diffscussion.Files {
		f := diffscussion.Files[i]
		if len(f.Threads) != 0 {
			t.Fatalf("In file %s expected 0 threads, found %d", f.Header, len(f.Threads))
		}
		checkNoHunkOrLowerThreads(t, f)
	}
}

func checkNoHunkOrLowerThreads(t *testing.T, f FileSection) {
	for i := range f.Hunks {
		h := f.Hunks[i]
		if len(h.Threads) != 0 {
			t.Fatalf("In hunk %s expected 0 threads, found %d", h.Header, len(h.Threads))
		}
		checkNoLineThreads(t, h)
	}
}

func checkNoLineThreads(t *testing.T, h HunkSection) {
	for i := range h.Lines {
		l := h.Lines[i]
		if len(l.Threads) != 0 {
			t.Fatalf("In line %s expected 0 threads, found %d", l.Text, len(l.Threads))
		}
	}
}

func checkComment(t *testing.T, comment Comment, expectedAuthor string, expectedDate string, expectedHeaders map[string]string, expectedBody []string) {
	if comment.Author != expectedAuthor {
		t.Fatalf("Expected author %s, got %s", expectedAuthor, comment.Author)
	}

	date, _ := parseDiffscussDate(expectedDate)

	if comment.MadeAt != date {
		t.Fatalf("Expected made at %s, got %s", expectedDate, comment.MadeAt)
	}

	if !reflect.DeepEqual(comment.Headers, expectedHeaders) {
		t.Fatalf("Expected header %s, got %s", expectedHeaders, comment.Headers)
	}

	if !reflect.DeepEqual(expectedBody, comment.Body) {
		t.Fatalf("Expected body %s, got %s", expectedBody, comment.Body)
	}
}

func TestParseWithOneDiffscussion(t *testing.T) {
	diffscussionFile, err := getTestFileReader("tiny-with-diffscussion.diff")
	if err != nil {
		t.Fatal(err)
	}

	diffscussion, err := Parse(diffscussionFile)
	if err != nil {
		t.Fatal(err)
	}

	checkNoFileOrLowerThreads(t, diffscussion)

	if len(diffscussion.Threads) != 1 {
		t.Fatalf("Expected 1 thread, got %d", len(diffscussion.Threads))
	}

	thread := diffscussion.Threads[0]
	if len(thread.Replies) != 0 {
		t.Fatalf("Expected 0 replies, got %d", len(thread.Replies))
	}
	expectedHeader := map[string]string{"x-custom-header": "custom value", "x-custom-header2": "custom value 2"}
	expectedBody := []string{"this is a comment", "across two lines with one blank trailing", ""}
	checkComment(t, thread.Top, "edmund", "2017-08-16T21:23:24-0400", expectedHeader, expectedBody)
}

func TestParseWithOneReply(t *testing.T) {
	diffscussionFile, err := getTestFileReader("tiny-with-reply.diff")
	if err != nil {
		t.Fatal(err)
	}

	diffscussion, err := Parse(diffscussionFile)
	if err != nil {
		t.Fatal(err)
	}

	checkNoFileOrLowerThreads(t, diffscussion)

	if len(diffscussion.Threads) != 1 {
		t.Fatalf("Expected 1 thread, got %d", len(diffscussion.Threads))
	}

	thread := diffscussion.Threads[0]
	if len(thread.Replies) != 1 {
		t.Fatalf("Expected 1 reply, got %d", len(thread.Replies))
	}
	expectedHeader := map[string]string{"x-custom-header": "custom value", "x-custom-header2": "custom value 2"}
	expectedBody := []string{"this is a comment", "across two lines with one blank trailing", ""}
	checkComment(t, thread.Top, "edmund", "2017-08-16T21:23:24-0400", expectedHeader, expectedBody)

	reply := thread.Replies[0].Top
	expectedHeaderReply := map[string]string{"x-custom-header": "reply custom value", "x-custom-header2": "reply custom value 2"}
	expectedBodyReply := []string{"this is a reply"}
	checkComment(t, reply, "edmund-reply", "2017-08-16T21:24:25-0400", expectedHeaderReply, expectedBodyReply)
}

func TestParseWithDiffscussionAtEveryLevel(t *testing.T) {
	diffscussionFile, err := getTestFileReader("tiny-with-all-diffscussion-locs.diff")
	if err != nil {
		t.Fatal(err)
	}

	diffscussion, err := Parse(diffscussionFile)
	if err != nil {
		t.Fatal(err)
	}

	if len(diffscussion.Threads) != 1 {
		t.Fatalf("Expected 1 thread, got %d", len(diffscussion.Threads))
	}
	emptyHeaders := make(map[string]string)
	checkReplylessThread(t, diffscussion.Threads[0], "edmund-top", "2017-08-16T21:23:24-0400", emptyHeaders, []string{"this is a top comment"})
	checkReplylessThread(t, diffscussion.Files[0].Threads[0], "edmund-file", "2017-08-16T21:23:25-0400", emptyHeaders, []string{"this is a file comment"})
	checkReplylessThread(t, diffscussion.Files[0].Hunks[0].Threads[0], "edmund-hunk", "2017-08-16T21:23:26-0400", emptyHeaders,
		[]string{"this is a hunk comment"})
	checkReplylessThread(t, diffscussion.Files[0].Hunks[0].Lines[3].Threads[0], "edmund-line", "2017-08-16T21:23:27-0400", emptyHeaders,
		[]string{"this is a line comment"})
}

func checkReplylessThread(t *testing.T, thread Thread, expectedAuthor string, expectedDate string, expectedHeaders map[string]string, expectedBody []string) {
	if len(thread.Replies) != 0 {
		t.Fatalf("Expected 0 replies, got %d", len(thread.Replies))
	}
	checkComment(t, thread.Top, expectedAuthor, expectedDate, expectedHeaders, expectedBody)
}
