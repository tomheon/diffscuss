package diffscuss_test

import (
	"diffscuss.com/diffscuss"
	"sort"
	"testing"
	"time"
)

func setThreadTimes(threads []diffscuss.Thread, times []time.Time) {
	for i := range threads {
		threads[i].Top.MadeAt = times[i]
	}

	for i := range threads {
		setThreadTimes(threads[i].Replies, times)
	}
}

func setAllThreadTimes(diffscussion *diffscuss.Diffscussion, times []time.Time) {
	setThreadTimes(diffscussion.Threads, times)

	for f := range diffscussion.Files {
		setThreadTimes(diffscussion.Files[f].Threads, times)

		for h := range diffscussion.Files[f].Hunks {
			setThreadTimes(diffscussion.Files[f].Hunks[h].Threads, times)

			for l := range diffscussion.Files[f].Hunks[h].Lines {
				setThreadTimes(diffscussion.Files[f].Hunks[h].Lines[l].Threads, times)
			}
		}
	}
}

func checkSortedThreads(t *testing.T, threads []diffscuss.Thread, sorted bool) {
	if sort.IsSorted(diffscuss.ByMadeAt(threads)) != sorted {
		t.Fatalf("Expected IsSorted = %t, got %t", sorted, sort.IsSorted(diffscuss.ByMadeAt(threads)))
	}

	for i := range threads {
		if len(threads[i].Replies) != 0 {
			checkSortedThreads(t, threads[i].Replies, sorted)
		}
	}
}

func checkSorted(t *testing.T, diffscussion *diffscuss.Diffscussion, sorted bool) {
	checkSortedThreads(t, diffscussion.Threads, sorted)

	for f := range diffscussion.Files {
		checkSortedThreads(t, diffscussion.Files[f].Threads, sorted)

		for h := range diffscussion.Files[f].Hunks {
			checkSortedThreads(t, diffscussion.Files[f].Hunks[h].Threads, sorted)

			for l := range diffscussion.Files[f].Hunks[h].Lines {
				checkSortedThreads(t, diffscussion.Files[f].Hunks[h].Lines[l].Threads, sorted)
			}
		}
	}
}

func TestSorting(t *testing.T) {
	params := newTestDiffscussionParams()
	params.numThreads = 3
	diffscussion := createTestDiffscussion(params)
	checkSorted(t, diffscussion, true)

	setAllThreadTimes(diffscussion, []time.Time{defaultMadeAtTime, latestMadeAtTime, laterMadeAtTime})
	checkSorted(t, diffscussion, false)
	diffscussion.SortThreads()
	checkSorted(t, diffscussion, true)

	setAllThreadTimes(diffscussion, []time.Time{latestMadeAtTime, laterMadeAtTime, defaultMadeAtTime})
	checkSorted(t, diffscussion, false)
	diffscussion.SortThreads()
	checkSorted(t, diffscussion, true)

	// and of course, you should be able to sort 2x.
	checkSorted(t, diffscussion, true)
	diffscussion.SortThreads()
	checkSorted(t, diffscussion, true)
}
