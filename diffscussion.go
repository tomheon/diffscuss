package diffscuss

import (
	"bytes"
	"time"
)

type Option struct {
	Name  string
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

type Comment struct {
	Author  string
	MadeAt  time.Time
	Headers map[string]string
	Body    string
}

type Thread struct {
	Top     Comment
	Replies []Thread
}

type Diffscussion struct {
	LeadingLines []string
	Options      []Option
	Threads      []Thread
	Files        []FileSection
}

func NewDiffscussion() *Diffscussion {
	return &Diffscussion{LeadingLines: make([]string, 0), Options: make([]Option, 0), Threads: make([]Thread, 0), Files: make([]FileSection, 0)}
}

func FromBytes(bs []byte) (*Diffscussion, error) {
	return nil, nil
	reader := bytes.NewReader(bs)
	return Parse(reader)
}
