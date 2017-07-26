package diffscuss

import (
	"bytes"
	"fmt"
	"strings"
	"time"
)

type KeyValuePair struct {
	Key   string
	Value string
}

type Line struct {
	Text    string
	Threads []Thread
}

func newLine() *Line {
	return &Line{Threads: make([]Thread, 0)}
}

type HunkSection struct {
	Header  []string
	Threads []Thread
	Lines   []Line
}

type FileSection struct {
	Header  []string
	Threads []Thread
	Hunks   []HunkSection
}


func (fileSection *FileSection) OriginalPath() (string, error) {
	for _, header := range fileSection.Header {
		if strings.HasPrefix(header, originalFilePrefix) {
			withoutFilePrefix := strings.TrimPrefix(header, originalFilePrefix)
			withoutSpaces := strings.TrimLeft(withoutFilePrefix, " ")
			return strings.TrimPrefix(withoutSpaces, "a/"), nil
		}
	}

	return "", fmt.Errorf("Could not find original file in header for file section %s", fileSection.Header)
}

func (fileSection *FileSection) NewPath() (string, error) {
	for _, header := range fileSection.Header {
		if strings.HasPrefix(header, newFilePrefix) {
			withoutFilePrefix := strings.TrimPrefix(header, newFilePrefix)
			withoutSpaces := strings.TrimLeft(withoutFilePrefix, " ")
			return strings.TrimPrefix(withoutSpaces, "b/"), nil
		}
	}

	return "", fmt.Errorf("Could not find new file in header for file section %s", fileSection.Header)
}

type Comment struct {
	Author  string
	MadeAt  time.Time
	Headers []KeyValuePair
	Body    []string
}

type Thread struct {
	Top     Comment
	Replies []Thread
}

func newThread() *Thread {
	return &Thread{Top: Comment{Headers: make([]KeyValuePair, 0)}, Replies: make([]Thread, 0)}
}

type Diffscussion struct {
	LeadingLines []string
	Options      map[string]string
	Threads      []Thread
	Files        []FileSection
}

func NewDiffscussion() *Diffscussion {
	return &Diffscussion{LeadingLines: make([]string, 0), Options: make(map[string]string), Threads: make([]Thread, 0), Files: make([]FileSection, 0)}
}

func FromBytes(bs []byte) (*Diffscussion, error) {
	reader := bytes.NewReader(bs)
	return Parse(reader)
}

// func (diffscussion *Diffscussion) AddComment(comment Comment, lineNum int) error {
// }
