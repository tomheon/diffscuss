package diffscuss

import (
	"fmt"
	"io"
	"os"
	"path"
	"testing"
)

func getTestFileReader(diffName string) (io.Reader, error) {
	filePath := path.Join("testfiles", "parser", diffName)
	return os.Open(filePath)
}

func TestParser(t *testing.T) {
	diffFile, err := getTestFileReader("tiny.diff")
	if err != nil {
		t.Fatal(err)
	}

	diffscussion, err := Parse(diffFile)
	if err != nil {
		t.Fatal(err)
	}

	fmt.Println(diffscussion.Files)

}
