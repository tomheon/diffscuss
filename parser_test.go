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

	firstFile := diffscussion.Files[0]

	expectedFirstFileHeader := []string{
		"diff --git a/file3.txt b/file3.txt",
		"index 5db4c05..81212b6 100644",
		"--- a/file3.txt",
		"+++ b/file3.txt",
	}

	if !reflect.DeepEqual(expectedFirstFileHeader, firstFile.Header) {
		t.Fatalf("Expected header lines %s, got %s", expectedFirstFileHeader, firstFile.Header)
	}
}

// TODO one deep test with diffscussion comments, then round trip tests rather
// than specific deep tests.
