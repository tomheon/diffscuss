package diffscuss

import (
	"fmt"
	"strings"
)

func printThreadsAtLevel(threads []Thread, curLevel int) {
	for i := range threads {
		thread := threads[i]
		fmt.Printf("%s%+v\n", strings.Repeat("*", curLevel), thread)
		printThreadsAtLevel(thread.Replies, curLevel+1)
	}
}

func printThreads(threads []Thread) {
	printThreadsAtLevel(threads, 0)
}
