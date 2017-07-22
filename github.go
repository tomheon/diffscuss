package diffscuss

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"net/http"
	"strings"
	"time"
)

const (
	urlBase       = "https://api.github.com"
	ghTimeFormat  = "2006-01-02T15:04:05Z"
	diffMediaType = "application/vnd.github.v3.diff"
)

type LimitedHttpClient interface {
	Do(req *http.Request) (*http.Response, error)
}

type rawPR struct {
	Url  string
	Head struct {
		Sha  string
		Repo struct {
			FullName string `json:"full_name"`
		}
	}
	Number            int
	IssueCommentsUrl  string `json:"comments_url"`
	ReviewCommentsUrl string `json:"review_comments_url"`
}

type comment struct {
	Author      string
	Body        string
	GithubUrl   string
	CreatedAt   time.Time
	Path        string
	Position    int
	ReviewState string
}

type fullPR struct {
	RepoFullName   string
	Sha            string
	Number         int
	Reviews        []comment
	ReviewComments []comment
	IssueComments  []comment
	Diff           []byte
}

type paginatedBytes struct {
	Bytes []byte
	Err   error
}

type commentsMaker func([]byte) ([]comment, error)

type rawIssueComment struct {
	HtmlUrl string `json:"html_url"`
	User    struct {
		Login string
	}
	CreatedAt string `json:"created_at"`
	Body      string
}

type rawReviewComment struct {
	HtmlUrl string `json:"html_url"`
	User    struct {
		Login string
	}
	CreatedAt string `json:"created_at"`
	Body      string
	Path      string
	// TODO clarify position vs. original position, but original_position is
	// what seems to be right in the case you're commenting on areas of the
	// diff that aren't changed as well
	Position int `json:"original_position"`
}

type rawReview struct {
	HtmlUrl string `json:"html_url"`
	User    struct {
		Login string
	}
	CreatedAt   string `json:"created_at"`
	Body        string
	State       string
	SubmittedAt string `json:"submitted_at"`
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
					return url[1 : len(url)-1], nil
				}
			}
		}
	}

	return "", nil
}

func getOnePageBytes(url string, client LimitedHttpClient, username string, token string) ([]byte, string, error) {
	req, err := generateRequest(url, username, token)
	if err != nil {
		return nil, "", err
	}

	bytes, resp, err := checkedClientReq(client, req)
	if err != nil {
		return nil, "", err
	}

	nextUrl, err := parseNextPage(resp)
	if err != nil {
		return nil, "", err
	}

	return bytes, nextUrl, nil
}

func getPaginatedBytes(url string, client LimitedHttpClient, username string, token string, results chan<- *paginatedBytes) {
	defer close(results)

	nextUrl := url
	var err error = nil
	var bytes []byte

	for nextUrl != "" {
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

func getResponseBytes(url string, client LimitedHttpClient, username string, token string) ([]byte, error) {
	req, err := generateRequest(url, username, token)

	bytes, _, err := checkedClientReq(client, req)
	if err != nil {
		return nil, err
	}

	return bytes, nil
}

func generateApiUrl(uri string) string {
	return fmt.Sprintf("%s/%s", urlBase, uri)
}

func getApiBytes(uri string, client LimitedHttpClient, username string, token string) ([]byte, error) {
	url := generateApiUrl(uri)
	body, err := getResponseBytes(url, client, username, token)
	return body, err
}

func getRawPR(repo string, pullRequestId int, client LimitedHttpClient, username string, token string) (*rawPR, error) {
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

func timeFromGithub(ghTime string) (time.Time, error) {
	return time.Parse(ghTimeFormat, ghTime)
}

func makeIssueComments(bytes []byte) ([]comment, error) {
	rawIssueComments := make([]rawIssueComment, 0)
	json.Unmarshal(bytes, &rawIssueComments)
	comments := make([]comment, len(rawIssueComments))
	for i, raw := range rawIssueComments {
		createdAt, err := timeFromGithub(raw.CreatedAt)
		if err != nil {
			return nil, err
		}
		comments[i] = comment{Author: raw.User.Login, Body: raw.Body, GithubUrl: raw.HtmlUrl, CreatedAt: createdAt}
	}
	return comments, nil
}

func makeReviewComments(bytes []byte) ([]comment, error) {
	rawReviewComments := make([]rawReviewComment, 0)
	json.Unmarshal(bytes, &rawReviewComments)
	comments := make([]comment, len(rawReviewComments))
	for i, raw := range rawReviewComments {
		createdAt, err := timeFromGithub(raw.CreatedAt)
		if err != nil {
			return nil, err
		}
		comments[i] = comment{Author: raw.User.Login, Body: raw.Body, GithubUrl: raw.HtmlUrl, Path: raw.Path, Position: raw.Position, CreatedAt: createdAt}
	}
	return comments, nil
}

func makeReviews(bytes []byte) ([]comment, error) {
	rawReviews := make([]rawReview, 0)
	json.Unmarshal(bytes, &rawReviews)
	comments := make([]comment, len(rawReviews))
	for i, raw := range rawReviews {
		createdAt, err := timeFromGithub(raw.SubmittedAt)
		if err != nil {
			return nil, err
		}
		comments[i] = comment{Author: raw.User.Login, Body: raw.Body, GithubUrl: raw.HtmlUrl, ReviewState: raw.State, CreatedAt: createdAt}
	}
	return comments, nil
}

func mapComments(commentBytes <-chan *paginatedBytes, makeComments commentsMaker) ([]comment, error) {
	comments := make([]comment, 0)

	for commentResult := range commentBytes {
		if commentResult.Err != nil {
			return nil, commentResult.Err
		}
		newComments, err := makeComments(commentResult.Bytes)
		if err != nil {
			return nil, err
		}
		comments = append(comments, newComments...)
	}

	return comments, nil
}

func checkedClientReq(client LimitedHttpClient, req *http.Request) ([]byte, *http.Response, error) {
	resp, err := client.Do(req)
	if err != nil {
		return nil, nil, err
	}
	defer resp.Body.Close()

	// TODO better logging
	if resp.StatusCode != 200 {
		return nil, nil, errors.New(fmt.Sprintf("Status code %d", resp.StatusCode))
	}

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return nil, nil, err
	}

	return body, resp, nil
}

func getDiff(rawPR *rawPR, client LimitedHttpClient, username string, token string) ([]byte, error) {
	req, err := generateRequest(rawPR.Url, username, token)
	if err != nil {
		return nil, err
	}

	req.Header.Set("Accept", diffMediaType)

	body, _, err := checkedClientReq(client, req)
	if err != nil {
		return nil, err
	}

	return body, err
}

func getFullPR(rawPR *rawPR, client LimitedHttpClient, username string, token string) (*fullPR, error) {
	issueCommentBytes := make(chan *paginatedBytes, 10)
	go getPaginatedBytes(rawPR.IssueCommentsUrl, client, username, token, issueCommentBytes)

	reviewCommentBytes := make(chan *paginatedBytes, 10)
	go getPaginatedBytes(rawPR.ReviewCommentsUrl, client, username, token, reviewCommentBytes)

	reviewsBytes := make(chan *paginatedBytes, 10)
	reviewsUrl := generateReviewsUrl(rawPR)
	go getPaginatedBytes(reviewsUrl, client, username, token, reviewsBytes)

	diff, err := getDiff(rawPR, client, username, token)
	if err != nil {
		return nil, err
	}

	issueComments, err := mapComments(issueCommentBytes, makeIssueComments)
	if err != nil {
		return nil, err
	}

	reviewComments, err := mapComments(reviewCommentBytes, makeReviewComments)
	if err != nil {
		return nil, err
	}

	reviews, err := mapComments(reviewsBytes, makeReviews)
	if err != nil {
		return nil, err
	}

	return &fullPR{RepoFullName: rawPR.Head.Repo.FullName, Sha: rawPR.Head.Sha, Number: rawPR.Number, Reviews: reviews, ReviewComments: reviewComments, IssueComments: issueComments, Diff: diff}, nil
}

func fromFullPR(fullPR *fullPR) (*Diffscussion, error) {
	diffscussion, err := FromBytes(fullPR.Diff)
	if err != nil {
		return nil, err
	}
	// todo assert no existing threads, as that would screw everything up
	// todo overlay threads
	return diffscussion, err
}

func FromGithubPR(repo string, pullRequestId int, client LimitedHttpClient, username string, token string) (*Diffscussion, error) {
	rawPR, err := getRawPR(repo, pullRequestId, client, username, token)
	if err != nil {
		return nil, err
	}

	fullPR, err := getFullPR(rawPR, client, username, token)
	if err != nil {
		return nil, err
	}

	// If the pull request has received another commit since we started pulling
	// from the API, our positions etc. for placing the comments in the diff
	// can be off.  So check what the current sha is now that we've gathered
	// the PR, and if it has changed, error out.
	checkRawPR, err := getRawPR(repo, pullRequestId, client, username, token)
	if err != nil {
		return nil, err
	}
	if rawPR.Head.Sha != checkRawPR.Head.Sha {
		return nil, errors.New(fmt.Sprintf("New commit pushed to PR while retrieving (%s => %s)", rawPR.Head.Sha, checkRawPR.Head.Sha))
	}

	return fromFullPR(fullPR)
}

// func (fileSection *FileSection) MatchesGithubPath(path string) bool {

// }

// func LineNumFromGithubPos(diffscussion *Diffscussion, path string, position int) (int, error) {
// 	linesSoFar := 0

// 	for i, fileSection := range diffscussion.Files {
// 		if fileSection.MatchesGithubPath(path) {

// 		} else {
// 			linesSoFar += fileSection.AllLineCount()
// 		}
// 	}
// }
