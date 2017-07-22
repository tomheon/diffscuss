package diffscuss

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"net/http"
	"path"
	"path/filepath"
	"sync"
	"testing"
)

type RequestSpec struct {
	ExpectedUsername string
	ExpectedToken    string
	ExpectedHeaders  map[string]string
	ExpectedMethod   string

	ResponseHeaders    map[string]string
	ResponseStatusCode int
	ResponseBasePath   string
	ResponseError      error
	ResponseSuffix     string
}

func NewRequestSpec(username string, token string, basePath string) *RequestSpec {
	return &RequestSpec{ExpectedHeaders: make(map[string]string), ResponseHeaders: make(map[string]string), ResponseBasePath: basePath, ExpectedUsername: username, ExpectedToken: token, ResponseStatusCode: 200, ResponseSuffix: ".json", ExpectedMethod: "GET"}
}

func (reqSpec *RequestSpec) Match(req *http.Request, t *testing.T) (*http.Response, error) {
	if reqSpec.ExpectedMethod != req.Method {
		t.Fatalf("Expected http method %s, got %s", reqSpec.ExpectedMethod, req.Method)
	}

	username, password, _ := req.BasicAuth()
	if reqSpec.ExpectedUsername != username {
		t.Fatalf("Expected username %s, got %s", reqSpec.ExpectedUsername, username)
	}

	if reqSpec.ExpectedToken != password {
		t.Fatalf("Expected token %s, got %s", reqSpec.ExpectedToken, password)
	}

	responseFile := path.Join(reqSpec.ResponseBasePath, req.URL.Path) + reqSpec.ResponseSuffix
	bs, err := ioutil.ReadFile(responseFile)
	if err != nil {
		t.Fatal(err)
	}

	for header, value := range reqSpec.ExpectedHeaders {
		actual, ok := req.Header[header]
		if !ok {
			t.Fatalf("Expected request header %s to be %s but it was not set", header, value)
		}
		if actual[0] != value {
			t.Fatalf("Expected request header %s to be %s but it was %s", header, value, actual[0])
		}
	}

	resp := &http.Response{Header: make(http.Header)}
	resp.StatusCode = reqSpec.ResponseStatusCode
	resp.Body = ioutil.NopCloser(bytes.NewReader(bs))
	for header, value := range reqSpec.ResponseHeaders {
		resp.Header.Set(header, value)
	}

	return resp, reqSpec.ResponseError
}

func (matcher *RequestMatcher) Match(req *http.Request, t *testing.T) (*http.Response, error) {
	matcher.Mutex.Lock()
	defer matcher.Mutex.Unlock()

	url := req.URL.String()
	callCount, ok := matcher.RequestCounts[url]
	if !ok {
		callCount = 0
	}
	callCount = callCount + 1

	requestSpecs, ok := matcher.RequestSpecs[url]
	if !ok {
		t.Fatalf("Unexpected request to %s", url)
	}

	if len(requestSpecs) < callCount {
		t.Fatalf("Expected no more than %d requests to %s, got %d", len(requestSpecs), url, callCount)
	}

	matcher.RequestCounts[url] = callCount

	return requestSpecs[callCount-1].Match(req, t)
}

func (matcher *RequestMatcher) CheckCounts(t *testing.T) {
	matcher.Mutex.Lock()
	defer matcher.Mutex.Unlock()

	for url, specs := range matcher.RequestSpecs {
		callCount, ok := matcher.RequestCounts[url]
		if !ok {
			callCount = 0
		}
		if callCount != len(specs) {
			t.Errorf("Expected %d requests to %s, got %d", len(specs), url, callCount)
		}
	}
}

type RequestMatcher struct {
	RequestCounts map[string]int
	RequestSpecs  map[string][]*RequestSpec
	Mutex         *sync.Mutex
}

func NewRequestMatcher() *RequestMatcher {
	return &RequestMatcher{RequestCounts: make(map[string]int), RequestSpecs: make(map[string][]*RequestSpec), Mutex: &sync.Mutex{}}
}

type TestClient struct {
	Matcher *RequestMatcher
	T       *testing.T
}

func (client *TestClient) Do(req *http.Request) (*http.Response, error) {
	return client.Matcher.Match(req, client.T)
}

func generateCommentUrl(repo string, prId int, topType string, commentType string, page int) string {
	pageParam := ""
	if page > 1 {
		pageParam = fmt.Sprintf("?page=%d", page)
	}
	return fmt.Sprintf("https://api.github.com/repos/%s/%s/%d/%s%s", repo, topType, prId, commentType, pageParam)
}

func generatePaginatedSpecs(username string, token string, repo string, prId int, basePath string, topType string, commentType string) (map[string][]*RequestSpec, error) {
	// specs := []*RequestSpec{NewRequestSpec(username, token, basePath)}
	jsonDir := path.Join(basePath, "repos", repo, topType, fmt.Sprint(prId), commentType)
	glob := fmt.Sprintf("%s.*", jsonDir)

	matches, err := filepath.Glob(glob)
	if err != nil {
		return nil, err
	}

	numSpecs := len(matches)
	specs := make(map[string][]*RequestSpec)
	workingSpecs := make([]*RequestSpec, numSpecs)

	for i := 0; i < numSpecs; i++ {
		spec := NewRequestSpec(username, token, basePath)
		workingSpecs[i] = spec
		specs[generateCommentUrl(repo, prId, topType, commentType, i+1)] = []*RequestSpec{spec}
	}

	for i := 1; i < numSpecs; i++ {
		lastSpec := workingSpecs[i-1]
		thisSpec := workingSpecs[i]
		lastSpec.ResponseHeaders["Link"] = generateLinkHeader(repo, prId, topType, commentType, i+1, numSpecs)
		thisSpec.ResponseSuffix = fmt.Sprintf(".%d.json", i+1)
	}

	return specs, nil
}

func generateLinkHeader(repo string, prId int, topType string, commentType string, nextPage int, lastPage int) string {
	nextUrl := generateCommentUrl(repo, prId, topType, commentType, nextPage)
	lastUrl := generateCommentUrl(repo, prId, topType, commentType, lastPage)
	return fmt.Sprintf("<%s>; rel=\"next\", <%s>; rel=\"last\"", nextUrl, lastUrl)
}

func createPRTestClient(t *testing.T, repo string, prId int, username string, token string, basePath string) *TestClient {
	client := &TestClient{T: t}
	matcher := NewRequestMatcher()

	pullSpec := NewRequestSpec(username, token, basePath)

	diffSpec := NewRequestSpec(username, token, basePath)
	diffSpec.ExpectedHeaders["Accept"] = "application/vnd.github.v3.diff"
	diffSpec.ResponseSuffix = ".diff"

	reviewsSpecs, err := generatePaginatedSpecs(username, token, repo, prId, basePath, "pulls", "reviews")
	if err != nil {
		t.Fatal(err)
	}

	pullsCommentsSpecs, err := generatePaginatedSpecs(username, token, repo, prId, basePath, "pulls", "comments")
	if err != nil {
		t.Fatal(err)
	}

	issueCommentsSpecs, err := generatePaginatedSpecs(username, token, repo, prId, basePath, "issues", "comments")
	if err != nil {
		t.Fatal(err)
	}

	matcher.RequestSpecs[fmt.Sprintf("https://api.github.com/repos/%s/pulls/%d", repo, prId)] = []*RequestSpec{pullSpec, diffSpec, pullSpec}

	for url, specs := range reviewsSpecs {
		matcher.RequestSpecs[url] = specs
	}

	for url, specs := range pullsCommentsSpecs {
		matcher.RequestSpecs[url] = specs
	}

	for url, specs := range issueCommentsSpecs {
		matcher.RequestSpecs[url] = specs
	}

	client.Matcher = matcher

	return client
}

func checkedFromGithubPR(t *testing.T, repo string, prId int, username string, token string, basePath string) (*Diffscussion, error) {
	client := createPRTestClient(t, repo, prId, username, token, basePath)

	diffscussion, err := FromGithubPR(repo, prId, client, username, token)

	client.Matcher.CheckCounts(t)

	return diffscussion, err
}

func TestSimplePull(t *testing.T) {
	username := "someuser"
	token := "sometoken"
	basePath := path.Join("testfiles", "simple_pull")
	repo := "tomheon/scratch"
	prId := 1

	diffscussion, err := checkedFromGithubPR(t, repo, prId, username, token, basePath)
	if err != nil {
		t.Fatalf("Expected nil error, got %s", err)
	}

	if diffscussion == nil {
		t.Fatal("Got nil diffscussion")
	}
}

func TestPullWithMove(t *testing.T) {
	username := "someuser"
	token := "sometoken"
	basePath := path.Join("testfiles", "pull_with_move")
	repo := "tomheon/scratch"
	prId := 3

	diffscussion, err := checkedFromGithubPR(t, repo, prId, username, token, basePath)
	if err != nil {
		t.Fatalf("Expected nil error, got %s", err)
	}

	if diffscussion == nil {
		t.Fatal("Got nil diffscussion")
	}

}
