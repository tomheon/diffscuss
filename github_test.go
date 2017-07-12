package diffscuss

import (
	"bytes"
	"io/ioutil"
	"net/http"
	"path"
	"testing"
)

type RequestSpec struct {
	ExpectedUsername string
	ExpectedToken string
	ExpectedHeaders map[string]string

	ResponseHeaders map[string]string
	ResponseStatusCode int
	ResponseBasePath string
	ResponseError error
	ResponseSuffix string
}

func NewRequestSpec(username string, token string, basePath string) *RequestSpec {
	return &RequestSpec{ExpectedHeaders: make(map[string]string), ResponseHeaders: make(map[string]string), ResponseBasePath: basePath, ExpectedUsername: username, ExpectedToken: token, ResponseStatusCode: 200, ResponseSuffix: ".json"}
}

func (reqSpec *RequestSpec) Match(req *http.Request, t *testing.T) (*http.Response, error) {
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

	return requestSpecs[callCount - 1].Match(req, t)
}

func (matcher *RequestMatcher) CheckCounts(t *testing.T) {
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
	RequestSpecs map[string][]*RequestSpec
}

func NewRequestMatcher() *RequestMatcher {
	return &RequestMatcher{RequestCounts: make(map[string]int), RequestSpecs: make(map[string][]*RequestSpec)}
}

type TestClient struct {
	Matcher *RequestMatcher
	T *testing.T
}

func (client *TestClient) Do(req *http.Request) (*http.Response, error) {
	return client.Matcher.Match(req, client.T)
}

func TestSomething(t *testing.T) {
	user := "someuser"
	token := "sometoken"
	basePath := path.Join("testfiles", "simple_pull")
	client := &TestClient{T: t}
	matcher := NewRequestMatcher()

	pullSpec := NewRequestSpec(user, token, basePath)

	diffSpec := NewRequestSpec(user, token, basePath)
	diffSpec.ExpectedHeaders["Accept"] = "application/vnd.github.v3.diff"
	diffSpec.ResponseSuffix = ".diff"

	reviewsSpec := NewRequestSpec(user, token, basePath)

	pullsCommentsSpec := NewRequestSpec(user, token, basePath)

	issueCommentsSpec := NewRequestSpec(user, token, basePath)
	issueCommentsSpec.ResponseHeaders["Link"] = "<https://api.github.com/repos/tomheon/scratch/issues/1/comments?page=2>; rel=\"next\", <https://api.github.com/repos/tomheon/scratch/issues/1/comments?page=2>; rel=\"last\""

	issueComments2Spec := NewRequestSpec(user, token, basePath)
	issueComments2Spec.ResponseSuffix = ".2.json"

	pullSpecs := []*RequestSpec{pullSpec, diffSpec, pullSpec}
	reviewsSpecs := []*RequestSpec{reviewsSpec}
	pullsCommentsSpecs := []*RequestSpec{pullsCommentsSpec}
	issueCommentsSpecs := []*RequestSpec{issueCommentsSpec}
	issueComments2Specs := []*RequestSpec{issueComments2Spec}

	matcher.RequestSpecs["https://api.github.com/repos/tomheon/scratch/pulls/1"] = pullSpecs
	matcher.RequestSpecs["https://api.github.com/repos/tomheon/scratch/pulls/1/reviews"] = reviewsSpecs
	matcher.RequestSpecs["https://api.github.com/repos/tomheon/scratch/pulls/1/comments"] = pullsCommentsSpecs
	matcher.RequestSpecs["https://api.github.com/repos/tomheon/scratch/issues/1/comments"] = issueCommentsSpecs
	matcher.RequestSpecs["https://api.github.com/repos/tomheon/scratch/issues/1/comments?page=2"] = issueComments2Specs

	client.Matcher = matcher
	FromGithubPR("tomheon/scratch", 1, client, "someuser", "sometoken")

	matcher.CheckCounts(t)
}
