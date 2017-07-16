package diffscuss

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"strings"
)

type parseState int

const (
	initial parseState = iota
	inTopMatter
	inOptions
	inFileHeader
	inHunkHeader
	inHunk
	inDiffscussHeader
	inDiffscussBody
	done
)

type parseWorkingState struct {
	curState parseState
	// set when we start a diffscuss thread so we know where we were when it
	// finishes
	deferredState parseState
	diffscussion *Diffscussion
	curThreads *[]Thread
	err error
}

func initialState(diffscussion *Diffscussion) *parseWorkingState {
	s := &parseWorkingState{}
	s.curState = initial
	s.deferredState = initial
	s.diffscussion = diffscussion
	return s
}

type initStateFunc func (*parseWorkingState, string)
type continueStateFunc func (*parseWorkingState, string)
type closeStateFunc func (*parseWorkingState)

func findLastFile(diffscussion *Diffscussion) *FileSection {
	return &diffscussion.Files[len(diffscussion.Files) - 1]
}

func findLastHunk(diffscussion *Diffscussion) *HunkSection {
	fileSection := findLastFile(diffscussion)
	return &fileSection.Hunks[len(fileSection.Hunks) - 1]
}

func addFileHeaderLine(diffscussion *Diffscussion, line string) {
	lastFile := findLastFile(diffscussion)
	lastFile.Header = append(lastFile.Header, line)
}

func addHunkHeaderLine(diffscussion *Diffscussion, line string) {
	lastHunk := findLastHunk(diffscussion)
	lastHunk.Header = append(lastHunk.Header, line)
}

// inits

func initInitialState(workingState *parseWorkingState, line string) {
	workingState.curThreads = &workingState.diffscussion.Threads
}

func initInTopMatterState(workingState *parseWorkingState, line string) {
	// this space inentionally left blank
}

func initInOptionsState(workingState *parseWorkingState, line string) {
	// this space inentionally left blank
}

func initInFileHeaderState(workingState *parseWorkingState, line string) {
	workingState.diffscussion.Files = append(workingState.diffscussion.Files, FileSection{})
	addFileHeaderLine(workingState.diffscussion, line)
}

func initInHunkHeaderState(workingState *parseWorkingState, line string) {
	fileSection := findLastFile(workingState.diffscussion)
	fileSection.Hunks = append(fileSection.Hunks, HunkSection{})
	addHunkHeaderLine(workingState.diffscussion, line)
}

func initInHunkState(workingState *parseWorkingState, line string) {
	newLine := newLine()
	newLine.Text = line
	lastHunk := findLastHunk(workingState.diffscussion)
	lastHunk.Lines = append(lastHunk.Lines, *newLine)
}

func initInDiffscussHeaderState(workingState *parseWorkingState, line string) {
}

func initInDiffscussBodyState(workingState *parseWorkingState, line string) {
}

func initInDoneState(workingState *parseWorkingState, line string) {
}

// continues

func continueInitialState(workingState *parseWorkingState, line string) {
}

func continueInTopMatterState(workingState *parseWorkingState, line string) {
}

func continueInOptionsState(workingState *parseWorkingState, line string) {
}

func continueInFileHeaderState(workingState *parseWorkingState, line string) {
	addFileHeaderLine(workingState.diffscussion, line)
}

func continueInHunkHeaderState(workingState *parseWorkingState, line string) {
	addHunkHeaderLine(workingState.diffscussion, line)
}

func continueInHunkState(workingState *parseWorkingState, line string) {
	newLine := newLine()
	newLine.Text = line
	lastHunk := findLastHunk(workingState.diffscussion)
	lastHunk.Lines = append(lastHunk.Lines, *newLine)
}

func continueInDiffscussHeaderState(workingState *parseWorkingState, line string) {
}

func continueInDiffscussBodyState(workingState *parseWorkingState, line string) {
}

func continueInDoneState(workingState *parseWorkingState, line string) {
}

// closes

func closeInitialState(workingState *parseWorkingState) {
}

func closeInTopMatterState(workingState *parseWorkingState) {
}

func closeInOptionsState(workingState *parseWorkingState) {
}

func closeInFileHeaderState(workingState *parseWorkingState) {
}

func closeInHunkHeaderState(workingState *parseWorkingState) {
}

func closeInHunkState(workingState *parseWorkingState) {
}

func closeInDiffscussHeaderState(workingState *parseWorkingState) {
}

func closeInDiffscussBodyState(workingState *parseWorkingState) {
}

func closeInDoneState(workingState *parseWorkingState) {
}

type parser struct {
	transitions map[parseState][]parseState
	initStateFuncs map[parseState]initStateFunc
	continueStateFuncs map[parseState]continueStateFunc
	closeStateFuncs map[parseState]closeStateFunc
}

// closes will be the ones checking logic of specific parts, e.g. that enough
// lines in the file header, etc.

func newParser() *parser {
	p := &parser{}

	p.transitions = map[parseState][]parseState {
		initial: []parseState{inTopMatter, inOptions, inFileHeader, inDiffscussHeader, done},
		inTopMatter: []parseState{inOptions, inFileHeader, inDiffscussHeader, done},
		inOptions: []parseState{inFileHeader, inDiffscussHeader, done},
		inFileHeader: []parseState{inHunkHeader, inDiffscussHeader, done},
		inHunkHeader: []parseState{inHunk, inDiffscussHeader, done},
		inHunk: []parseState{inDiffscussHeader, inHunkHeader, inFileHeader, done},
		inDiffscussHeader: []parseState{inDiffscussBody, done},
		inDiffscussBody: []parseState{inDiffscussHeader, inHunkHeader, inHunk, inFileHeader, done},
		done: []parseState{},
	}

	p.initStateFuncs = map[parseState]initStateFunc {
		initial: initInitialState,
		inTopMatter: initInTopMatterState,
		inOptions: initInOptionsState,
		inFileHeader: initInFileHeaderState,
		inHunkHeader: initInHunkHeaderState,
		inHunk: initInHunkState,
		inDiffscussHeader: initInDiffscussHeaderState,
		inDiffscussBody: initInDiffscussBodyState,
		done: initInDoneState,
	}

	p.continueStateFuncs = map[parseState]continueStateFunc {
		initial: continueInitialState,
		inTopMatter: continueInTopMatterState,
		inOptions: continueInOptionsState,
		inFileHeader: continueInFileHeaderState,
		inHunkHeader: continueInHunkHeaderState,
		inHunk: continueInHunkState,
		inDiffscussHeader: continueInDiffscussHeaderState,
		inDiffscussBody: continueInDiffscussBodyState,
		done: continueInDoneState,
	}

	p.closeStateFuncs = map[parseState]closeStateFunc {
		initial: closeInitialState,
		inTopMatter: closeInTopMatterState,
		inOptions: closeInOptionsState,
		inFileHeader: closeInFileHeaderState,
		inHunkHeader: closeInHunkHeaderState,
		inHunk: closeInHunkState,
		inDiffscussHeader: closeInDiffscussHeaderState,
		inDiffscussBody: closeInDiffscussBodyState,
		done: closeInDoneState,
	}

	return p
}

func shouldCheckDeferredState(curState parseState, newState parseState) bool {
	return curState == inDiffscussBody && newState != inDiffscussHeader
}

func shouldStoreDeferredState(curState parseState, newState parseState) bool {
	return curState != inDiffscussBody && newState == inDiffscussHeader
}

func hasState(state parseState, states []parseState) bool {
	for _, s := range states {
		if s == state {
			return true
		}
	}
	return false
}

func (p *parser) checkLegalTransition(parseWorkingState *parseWorkingState, lineState parseState) error {
	legalStates := p.transitions[parseWorkingState.curState]

	if !hasState(lineState, legalStates) {
		return errors.New("bad state write better message")
	}

	if shouldCheckDeferredState(parseWorkingState.curState, lineState) {
		legalStatesFromDeferred := p.transitions[parseWorkingState.deferredState]

		if !hasState(lineState, legalStatesFromDeferred) {
			return errors.New("bad state write better message")
		}
	}

	return nil
}

const (
	optionPrefix = "#@"
	diffscussHeaderPrefix = "#*"
	diffscussBodyPrefix = "#-"
	commentPrefix = "#"

	originalFilePrefix = "---"
	newFilePrefix = "+++"
	diffCmdPrefix = "diff"
	indexPrefix = "index"

	noNewlinePrefix = "\\"
	addedLinePrefix = "+"
	removedLinePrefix = "-"
	contextLinePrefix = " "

	hunkHeaderPrefix = "@"
)

func inferState(line string) (parseState, error) {
	// there is some order dependency here, in that if line prefixes themselves
	// share a prefix (e.g. commentPrefix and optionPrefix), the longer one
	// should be checked first
	if strings.HasPrefix(line, optionPrefix) {
		return inOptions, nil
	} else if strings.HasPrefix(line, diffscussHeaderPrefix) {
		return inDiffscussHeader, nil
	} else if strings.HasPrefix(line, diffscussBodyPrefix) {
		return inDiffscussBody, nil
	} else if strings.HasPrefix(line, commentPrefix) {
		return inTopMatter, nil
	} else if strings.HasPrefix(line, originalFilePrefix) ||
		strings.HasPrefix(line, newFilePrefix) ||
		strings.HasPrefix(line, diffCmdPrefix) ||
		strings.HasPrefix(line, indexPrefix) {
		return inFileHeader, nil
	} else if strings.HasPrefix(line, noNewlinePrefix) ||
		strings.HasPrefix(line, addedLinePrefix) ||
		strings.HasPrefix(line, removedLinePrefix) ||
		strings.HasPrefix(line, contextLinePrefix) {
		return inHunk, nil
	} else if strings.HasPrefix(line, hunkHeaderPrefix) {
		return inHunkHeader, nil
	}

	return done, fmt.Errorf("Couldn't infer state of %s", line)
}

func (p *parser) parseNext(line string, workingState *parseWorkingState) {
	lineState, err := inferState(line)
	if err != nil {
		workingState.err = err
		return
	}

	if lineState == workingState.curState {
		p.continueStateFuncs[lineState](workingState, line)
	} else {
		if err := p.checkLegalTransition(workingState, lineState); err != nil {
			workingState.err = err
			return
		}

		p.closeStateFuncs[workingState.curState](workingState)

		if shouldStoreDeferredState(workingState.curState, lineState) {
			workingState.deferredState = workingState.curState
		}

		p.initStateFuncs[lineState](workingState, line)
		workingState.curState = lineState
	}
}

func Parse(reader io.Reader) (*Diffscussion, error) {
	scanner := bufio.NewScanner(reader)

	// todo figure out "binary files differ"

	diffscussion := NewDiffscussion()
	parseWorkingState := initialState(diffscussion)
	parser := newParser()

	for scanner.Scan() {
		line := scanner.Text()
		parser.parseNext(line, parseWorkingState)
		if parseWorkingState.err != nil {
			return nil, parseWorkingState.err
		}
	}

	if scanner.Err() != nil {
		return nil, scanner.Err()
	}

	return diffscussion, nil
}
