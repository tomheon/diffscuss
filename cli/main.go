package main

// import "bytes"
// import "flag"
// import "fmt"
// import "io/ioutil"
// import "log"
// import "net/http"
// import "strings"
// import "time"

// import "diffscuss.com/diffscuss"

import (
	"log"
	"flag"
	"os"

	"diffscuss.com/diffscuss"
)

func main() {
	flag.Parse()
	diffscussFile := flag.Arg(0)
	fil, err := os.Open(diffscussFile)
	if err != nil {
		log.Fatalf("Error opening file %s", err)
	}
	diffscuss.Parse(fil)
}


// func main() {
// 	username := flag.String("username", "gh-username", "Your github user name")
// 	tokenfile := flag.String("tokenfile", "tokenfile.txt", "A file containing a github token")

// 	flag.Parse()

// 	tokenbytes, err := ioutil.ReadFile(*tokenfile)
// 	if err != nil {
// 		log.Fatal(err)
// 	}

// 	token := strings.TrimSpace(string(tokenbytes))

// 	client := &http.Client{}
// 	client.Timeout = time.Duration(5 * time.Second)
// 	diffscussion, err := diffscuss.FromGithubPR("tomheon/scratch", 1, client, *username, token)
// 	if err != nil {
// 		log.Fatal(err)
// 	}

// 	buf := new(bytes.Buffer)
// 	err = diffscuss.Render(diffscussion, buf)
// 	if err != nil {
// 		log.Fatal(err)
// 	}

// 	fmt.Println(buf.String())
// }
