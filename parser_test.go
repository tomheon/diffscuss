package diffscuss

import (
	"reflect"
	"testing"
)

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

	expectedHeaderReply := map[string]string{"x-custom-header": "reply custom value", "x-custom-header2": "reply custom value 2"}
	expectedBodyReply := []string{"this is a reply"}
	checkReplylessThread(t, thread.Replies[0], "edmund-reply", "2017-08-16T21:24:25-0400", expectedHeaderReply, expectedBodyReply)
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

func TestParseOptions(t *testing.T) {
	diffscussionFile, err := getTestFileReader("tiny-with-options.diff")
	if err != nil {
		t.Fatal(err)
	}

	diffscussion, err := Parse(diffscussionFile)
	if err != nil {
		t.Fatal(err)
	}

	expectedOptions := map[string]string{
		"mode":   "github",
		"custom": "hello",
	}
	if !reflect.DeepEqual(expectedOptions, diffscussion.Options) {
		t.Fatalf("Expected options %s got %s", expectedOptions, diffscussion.Options)
	}
}
