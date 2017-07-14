package diffscuss

import (
	"errors"
	"sort"
)

// Sorting

type ThreadSortable interface {
	SortThreads()
}

func (diffscussion *Diffscussion) SortThreads() {
	sort.Sort(ByMadeAt(diffscussion.Threads))

	for i := range diffscussion.Threads {
		diffscussion.Threads[i].SortThreads()
	}

	for i := range diffscussion.Files {
		diffscussion.Files[i].SortThreads()
	}
}

func (fileSection *FileSection) SortThreads() {
	sort.Sort(ByMadeAt(fileSection.Threads))

	for i := range fileSection.Threads {
		fileSection.Threads[i].SortThreads()
	}

	for i := range fileSection.Hunks {
		fileSection.Hunks[i].SortThreads()
	}
}

func (hunkSection *HunkSection) SortThreads() {
	sort.Sort(ByMadeAt(hunkSection.Threads))

	for i := range hunkSection.Threads {
		hunkSection.Threads[i].SortThreads()
	}

	for i := range hunkSection.Lines {
		hunkSection.Lines[i].SortThreads()
	}
}

func (line *Line) SortThreads() {
	sort.Sort(ByMadeAt(line.Threads))

	for i := range line.Threads {
		line.Threads[i].SortThreads()
	}
}

func (thread *Thread) SortThreads() {
	sort.Sort(ByMadeAt(thread.Replies))

	for i := range thread.Replies {
		thread.Replies[i].SortThreads()
	}
}

type ByMadeAt []Thread

func (t ByMadeAt) Len() int {
	return len(t)
}

func (t ByMadeAt) Swap(i int, j int) {
	t[i], t[j] = t[j], t[i]
}

func (t ByMadeAt) Less(i int, j int) bool {
	return t[i].Top.MadeAt.Before(t[j].Top.MadeAt)
}

// Rethreading

func (thread *Thread) Rethread(curDepth int, maxDepth int) []Thread {
	unthreadedReplies := make([]Thread, 0)
	for i := range thread.Replies {
		unthreadedReplies = append(unthreadedReplies, thread.Replies[i].Rethread(curDepth+1, maxDepth)...)
	}

	if curDepth < maxDepth {
		return []Thread{Thread{Top: thread.Top, Replies: unthreadedReplies}}
	} else {
		return append(unthreadedReplies, Thread{Top: thread.Top, Replies: make([]Thread, 0)})
	}
}

func (line *Line) Rethread(maxDepth int) {
	newThreads := make([]Thread, 0)
	for _, thread := range line.Threads {
		newThreads = append(newThreads, thread.Rethread(1, maxDepth)...)
	}

	line.Threads = newThreads
}

func (hunkSection *HunkSection) Rethread(maxDepth int) {
	newThreads := make([]Thread, 0)
	for _, thread := range hunkSection.Threads {
		newThreads = append(newThreads, thread.Rethread(1, maxDepth)...)
	}

	hunkSection.Threads = newThreads

	for i := range hunkSection.Lines {
		hunkSection.Lines[i].Rethread(maxDepth)
	}
}

func (fileSection *FileSection) Rethread(maxDepth int) {
	newThreads := make([]Thread, 0)
	for _, thread := range fileSection.Threads {
		newThreads = append(newThreads, thread.Rethread(1, maxDepth)...)
	}

	fileSection.Threads = newThreads

	for i := range fileSection.Hunks {
		fileSection.Hunks[i].Rethread(maxDepth)
	}
}

func (diffscussion *Diffscussion) Rethread(maxDepth int) error {
	if maxDepth < 1 {
		return errors.New("Threads require a positive depth")
	}

	newThreads := make([]Thread, 0)
	for _, thread := range diffscussion.Threads {
		newThreads = append(newThreads, thread.Rethread(1, maxDepth)...)
	}

	diffscussion.Threads = newThreads

	for i := range diffscussion.Files {
		diffscussion.Files[i].Rethread(maxDepth)
	}

	diffscussion.SortThreads()

	return nil
}
