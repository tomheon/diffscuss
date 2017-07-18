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

	checkHeader(t, diffscussion.Files[2].Header,
		"diff --git a/t.jpg b/t.jpg",
		"index d2d8abc..212305d 100644",
		"Binary files a/t.jpg and b/t.jpg differ")
}

// TODO one deep test with diffscussion comments, then round trip tests rather
// than specific deep tests.
