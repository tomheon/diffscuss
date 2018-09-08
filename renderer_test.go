package diffscuss

import (
	"bytes"
	"testing"
)

// test these as round trips, which incidentally relies on the parser, but that's ok

func TestRoundTripSimpleDiff(t *testing.T) {
	diffFile, err := getTestFileReader("tiny.diff")

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
	expectedBytes, err := getTestFileBytes("tiny.diff")
	if err != nil {
		t.Fatal(err)
	}

	if !bytes.Equal(actualBytes, expectedBytes) {
		t.Fatalf("Bytes not equal %s\n%s", actualBytes, expectedBytes)
	}
}
