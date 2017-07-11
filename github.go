package diffscuss

import (
	"errors"
	"fmt"
	"encoding/json"
	"net/http"
	"io/ioutil"
	"strings"
	"time"
)

const urlBase = "https://api.github.com"

type rawPR struct {
	Head struct {
		Sha string
		Repo struct {
			FullName string `json:"full_name"`
		}
	}
	Number int
	IssueCommentsUrl string `json:"comments_url"`
	ReviewCommentsUrl string `json:"review_comments_url"`
}

type comment struct {
	Author string
	Body string
	GithubUrl string
	CreatedAt time.Time
	ExtraHeaders map[string]string
}

type fullPR struct {
	RepoFullName string
	Sha string
	Number int
	Reviews []comment
	ReviewComments []comment
	IssueComments []comment
	Diff string
}

type paginatedBytes struct {
	Bytes []byte
	Err error
}

func parseNextPage(resp *http.Response) (string, error) {
	links, ok := resp.Header["Link"]
	if ok && len(links) > 0 {
		for _, link := range links {
			entries := strings.Split(strings.TrimSpace(link), ",")
			for _, entry := range entries {
				segments := strings.Split(strings.TrimSpace(entry), ";")
				if len(segments) < 2 {
					continue
				}
				url := strings.TrimSpace(segments[0])
				rel := strings.TrimSpace(segments[1])
				if rel == "rel=\"next\"" {
					// remove the leading < and trailing >
					return url[1:len(url)-1], nil
				}
			}
		}
	}

	return "", nil
}

func getOnePageBytes(url string, client *http.Client, username string, token string) ([]byte, string, error) {
	req, err := generateRequest(url, username, token)
	if err != nil {
		return nil, "", err
	}

	resp, err := client.Do(req)
	if err != nil {
		return nil, "", err
	}

	bytes, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return nil, "", err
	}

	nextUrl, err := parseNextPage(resp)
	if err != nil {
		return nil, "", err
	}

	return bytes, nextUrl, nil
}

func getPaginatedBytes(url string, client *http.Client, username string, token string, results chan<- *paginatedBytes) {
	defer close(results)

	nextUrl := url
	var err error = nil
	var bytes []byte

	for nextUrl != ""  {
		bytes, nextUrl, err = getOnePageBytes(nextUrl, client, username, token)
		results <- &paginatedBytes{bytes, err}
		if err != nil {
			break
		}
	}
}

func generateRequest(url string, username string, token string) (*http.Request, error) {
	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		return nil, err
	}

	req.SetBasicAuth(username, token)
	return req, nil
}

func getResponseBytes(url string, client *http.Client, username string, token string) ([]byte, error) {
	req, err := generateRequest(url, username, token)

	resp, err := client.Do(req)
	if err != nil {
		return nil, err
	}

	return ioutil.ReadAll(resp.Body)
}

func generateApiUrl(uri string) string {
	return fmt.Sprintf("%s/%s", urlBase, uri)
}

func getApiBytes(uri string, client *http.Client, username string, token string) ([]byte, error) {
	url := generateApiUrl(uri)
	body, err := getResponseBytes(url, client, username, token)
	return body, err
}

func getRawPR(repo string, pullRequestId int, client *http.Client, username string, token string) (*rawPR, error) {
	bytes, err := getApiBytes(fmt.Sprintf("repos/%s/pulls/%d", repo, pullRequestId), client, username, token)
	if err != nil {
		return nil, err
	}
	rawPR := &rawPR{}
	err = json.Unmarshal(bytes, &rawPR)
	if err != nil {
		return nil, err
	}
	return rawPR, nil
}

func generateReviewsUrl(pr *rawPR) string {
	return generateApiUrl(fmt.Sprintf("repos/%s/pulls/%d/reviews", pr.Head.Repo.FullName, pr.Number))
}

func getFullPR(rawPR *rawPR, client *http.Client, username string, token string) (*fullPR, error) {
	issueCommentBytes := make(chan *paginatedBytes, 10)
	go getPaginatedBytes(rawPR.IssueCommentsUrl, client, username, token, issueCommentBytes)

	reviewCommentBytes := make(chan *paginatedBytes, 10)
	go getPaginatedBytes(rawPR.ReviewCommentsUrl, client, username, token, reviewCommentBytes)

	reviewsBytes := make(chan *paginatedBytes, 10)
	reviewsUrl := generateReviewsUrl(rawPR)
	go getPaginatedBytes(reviewsUrl, client, username, token, reviewsBytes)

	for issueCommentResult := range issueCommentBytes {
		if issueCommentResult.Err != nil {
			return nil, issueCommentResult.Err
		}
		fmt.Println(len(issueCommentResult.Bytes))
		fmt.Println("*******EWJ*****")
	}

	for reviewCommentResult := range reviewCommentBytes {
		if reviewCommentResult.Err != nil {
			return nil, reviewCommentResult.Err
		}
		fmt.Println(len(reviewCommentResult.Bytes))
	}

	for reviewResult := range reviewsBytes {
		if reviewResult.Err != nil {
			return nil, reviewResult.Err
		}
		fmt.Println(len(reviewResult.Bytes))
	}

	return nil, nil
}

func FromGithubPR(repo string, pullRequestId int, client *http.Client, username string, token string) (*Diffscussion, error) {
	rawPR, err := getRawPR(repo, pullRequestId, client, username, token)
	if err != nil {
		return nil, err
	}

	fullPR, err := getFullPR(rawPR, client, username, token)
	if err != nil {
		return nil, err
	}

	checkRawPR, err := getRawPR(repo, pullRequestId, client, username, token)
	if err != nil {
		return nil, err
	}
	if rawPR.Head.Sha != checkRawPR.Head.Sha {
		return nil, errors.New(fmt.Sprintf("New commit pushed to PR while retrieving (%s => %s)", rawPR.Head.Sha, checkRawPR.Head.Sha))
	}

	fmt.Println(fullPR)

	return nil, err
}
