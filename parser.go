package diffscuss

import (
	"bufio"
	"io"
)

type scannedLinePair struct {
	first string
	second string
	hasSecond bool
	err error
}

func scanLinePairs(scanner *bufio.Scanner, scannedLinePairs chan<- *scannedLinePair) {
	defer close(scannedLinePairs)

	if !scanner.Scan() {
		if scanner.Err() != nil {
			scannedLinePairs <- &scannedLinePair{err: scanner.Err()}
		}
		return
	}

	firstLine := scanner.Text()

	for scanner.Scan() {
		nextLine := scanner.Text()
		scannedLinePairs <- &scannedLinePair{first: firstLine, second: nextLine, hasSecond: true}
		firstLine = nextLine
	}

	if scanner.Err() != nil {
		scannedLinePairs <- &scannedLinePair{err: scanner.Err()}
		return
	}

	scannedLinePairs <- &scannedLinePair{first: firstLine}
}

func Parse(reader io.Reader) (*Diffscussion, error) {
	// scanner := bufio.NewScanner(reader)
	// scannedLinePairs := make(chan *scannedLinePair)
	// go scanLinePairs(scanner, scannedLinePairs)
	return nil, nil
}
