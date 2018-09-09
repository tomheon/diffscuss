package diffscuss

import (
	"bytes"
	"github.com/andreyvit/diff"
	"testing"
)

// test these as round trips, which incidentally relies on the parser, but that's ok

func TestRoundTrips(t *testing.T) {
	testFiles := []string{"tiny.diff", "tiny-with-diffscussion.diff"}

	for _, testFile := range testFiles {

		diffFile, err := getTestFileReader(testFile)

		if err != nil {
			t.Fatal(err)
		}

		diffscussion, err := Parse(diffFile)
		if err != nil {
			t.Fatal(err)
		}

		var buf bytes.Buffer

		Render(diffscussion, &buf)

		actualBytes := buf.Bytes()
		expectedBytes, err := getTestFileBytes(testFile)
		if err != nil {
			t.Fatal(err)
		}

		if !bytes.Equal(actualBytes, expectedBytes) {
			t.Fatalf("%s differed on round trip %s", testFile, diff.LineDiff(string(expectedBytes), string(actualBytes)))
		}
	}
}
