package diffscuss

import (
	"time"
)

type Option struct {
	Name string
	Value string
}

type Line struct {
	Text string
	Threads []Thread
}

type HunkSection struct {
	Header []string
	Lines []Line
}

type FileSection struct {
	Header []string
	Threads []Thread
	Hunks []HunkSection
}

type Comment struct {
	Author string
	MadeAt time.Time
	Headers map[string]string
	Body string
}

type Thread struct {
	Top Comment
	Replies []Thread
}

type Diffscussion struct {
	LeadingLines []string
	Options []Option
	Threads []Thread
	Files []FileSection
}
