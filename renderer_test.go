package diffscuss_test

import (
	"bytes"
	"diffscuss.com/diffscuss"
	"github.com/andreyvit/diff"
	"testing"
)

// test these as round trips, which incidentally relies on the parser, but that
// feels pragmatic, as the parser should also fail if there's something wrong
// with it (or if it hasn't, we can strengthen those tests)

func TestRoundTrips(t *testing.T) {
	testFiles := []string{
		"tiny.diff",
		"tiny-with-diffscussion.diff",
		"tiny-with-reply.diff",
		"tiny-with-all-diffscussion-locs.diff",
		"tiny-with-all-diffscussion-locs-and-replies.diff",
		"tiny-with-options.diff",
	}

	for _, testFile := range testFiles {

		diffFile, err := getTestFileReader(testFile)

		if err != nil {
			t.Fatal(err)
		}

		diffscussion, err := diffscuss.Parse(diffFile)
		if err != nil {
			t.Fatal(err)
		}

		var buf bytes.Buffer

		diffscuss.Render(diffscussion, &buf)

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
