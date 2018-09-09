package diffscuss

import (
	"fmt"
	"io"
	"strings"
)

func writeCommentLine(line string, padder string, writer io.Writer, level int) error {
	_, err := writer.Write([]byte("#"))
	if err != nil {
		return err
	}

	_, err = writer.Write([]byte(strings.Repeat(padder, level)))
	if err != nil {
		return err
	}

	if len(line) > 0 {
		_, err = writer.Write([]byte(" "))
		if err != nil {
			return err
		}

		_, err = writer.Write([]byte(line))
		if err != nil {
			return err
		}
	}

	_, err = writer.Write([]byte("\n"))
	if err != nil {
		return err
	}

	return nil
}

func writeHeaderLine(line string, writer io.Writer, level int) error {
	return writeCommentLine(line, "*", writer, level)
}

func writeBodyLine(line string, writer io.Writer, level int) error {
	return writeCommentLine(line, "-", writer, level)
}

func RenderOptions(options []KeyValuePair, writer io.Writer) error {
	for _, kv := range options {
		_, err := writer.Write([]byte(fmt.Sprintf("#@ %s: %s\n", kv.Key, kv.Value)))
		if err != nil {
			return err
		}
	}

	return nil
}

func RenderComment(comment Comment, writer io.Writer, level int) error {
	err := writeHeaderLine("", writer, level)
	if err != nil {
		return err
	}

	authorLine := fmt.Sprintf("author: %s", comment.Author)
	err = writeHeaderLine(authorLine, writer, level)
	if err != nil {
		return err
	}

	madeAtLine := fmt.Sprintf("date: %s", comment.MadeAt.Format(DiffscussTimeFormat))
	err = writeHeaderLine(madeAtLine, writer, level)
	if err != nil {
		return err
	}

	for _, kv := range comment.Headers {
		line := ""
		if kv.Key != "" {
			line = fmt.Sprintf("%s: %s", kv.Key, kv.Value)
		}
		err = writeHeaderLine(line, writer, level)
		if err != nil {
			return err
		}
	}

	err = writeHeaderLine("", writer, level)
	if err != nil {
		return err
	}

	for _, bodyLine := range comment.Body {
		err = writeBodyLine(bodyLine, writer, level)
		if err != nil {
			return err
		}
	}

	return nil
}

func RenderThreads(threads []Thread, writer io.Writer, level int) error {
	for _, t := range threads {
		err := RenderComment(t.Top, writer, level)
		if err != nil {
			return err
		}
		err = RenderThreads(t.Replies, writer, level+1)
		if err != nil {
			return err
		}
	}
	return nil
}

func RenderLine(line Line, writer io.Writer) error {
	_, err := writer.Write([]byte(line.Text))
	if err != nil {
		return err
	}

	_, err = writer.Write([]byte("\n"))
	if err != nil {
		return err
	}

	err = RenderThreads(line.Threads, writer, 1)
	if err != nil {
		return err
	}

	return nil
}
func RenderHunk(hunk HunkSection, writer io.Writer) error {
	err := writeRawLines(hunk.Header, writer)
	if err != nil {
		return err
	}

	err = RenderThreads(hunk.Threads, writer, 1)
	if err != nil {
		return err
	}

	for _, line := range hunk.Lines {
		err = RenderLine(line, writer)
		if err != nil {
			return err
		}
	}

	return nil
}

func RenderFile(fs FileSection, writer io.Writer) error {
	err := writeRawLines(fs.Header, writer)
	if err != nil {
		return err
	}

	err = RenderThreads(fs.Threads, writer, 1)
	if err != nil {
		return err
	}

	for _, hunk := range fs.Hunks {
		err = RenderHunk(hunk, writer)
		if err != nil {
			return err
		}
	}

	return nil
}

func writeRawLines(lines []string, writer io.Writer) error {
	for _, line := range lines {
		_, err := writer.Write([]byte(line))
		if err != nil {
			return err
		}
		_, err = writer.Write([]byte("\n"))
		if err != nil {
			return err
		}
	}
	return nil
}

func Render(diffscussion *Diffscussion, writer io.Writer) error {
	err := writeRawLines(diffscussion.LeadingLines, writer)
	if err != nil {
		return err
	}

	err = RenderOptions(diffscussion.Options, writer)
	if err != nil {
		return err
	}

	err = RenderThreads(diffscussion.Threads, writer, 1)
	if err != nil {
		return err
	}

	for _, f := range diffscussion.Files {
		err = RenderFile(f, writer)

		if err != nil {
			return err
		}
	}

	return nil
}
