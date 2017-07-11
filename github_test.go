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

func NewRequestSpec() *RequestSpec {
	return &RequestSpec{ExpectedHeaders: make(map[string]string), ResponseHeaders: make(map[string]string)}
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
	basePath := path.Join("testfiles", "simple_pull")
	client := &TestClient{T: t}
	matcher := NewRequestMatcher()

	pullSpec := NewRequestSpec()
	pullSpec.ExpectedUsername = "someuser"
	pullSpec.ExpectedToken = "sometoken"
	pullSpec.ResponseStatusCode = 200
	pullSpec.ResponseBasePath = basePath
	pullSpec.ResponseSuffix = ".json"

	diffSpec := NewRequestSpec()
	diffSpec.ExpectedUsername = "someuser"
	diffSpec.ExpectedToken = "sometoken"
	diffSpec.ExpectedHeaders["Accept"] = "application/vnd.github.v3.diff"
	diffSpec.ResponseStatusCode = 200
	diffSpec.ResponseBasePath = basePath
	diffSpec.ResponseSuffix = ".diff"

	reviewsSpec := NewRequestSpec()
	reviewsSpec.ExpectedUsername = "someuser"
	reviewsSpec.ExpectedToken = "sometoken"
	reviewsSpec.ResponseStatusCode = 200
	reviewsSpec.ResponseBasePath = basePath
	reviewsSpec.ResponseSuffix = ".json"

	pullsCommentsSpec := NewRequestSpec()
	pullsCommentsSpec.ExpectedUsername = "someuser"
	pullsCommentsSpec.ExpectedToken = "sometoken"
	pullsCommentsSpec.ResponseStatusCode = 200
	pullsCommentsSpec.ResponseBasePath = basePath
	pullsCommentsSpec.ResponseSuffix = ".json"

	issueCommentsSpec := NewRequestSpec()
	issueCommentsSpec.ExpectedUsername = "someuser"
	issueCommentsSpec.ExpectedToken = "sometoken"
	issueCommentsSpec.ResponseStatusCode = 200
	issueCommentsSpec.ResponseBasePath = basePath
	issueCommentsSpec.ResponseHeaders["Link"] = "<https://api.github.com/repos/tomheon/scratch/issues/1/comments?page=2>; rel=\"next\", <https://api.github.com/repos/tomheon/scratch/issues/1/comments?page=2>; rel=\"last\""
	issueCommentsSpec.ResponseSuffix = ".json"

	issueComments2Spec := NewRequestSpec()
	issueComments2Spec.ExpectedUsername = "someuser"
	issueComments2Spec.ExpectedToken = "sometoken"
	issueComments2Spec.ResponseStatusCode = 200
	issueComments2Spec.ResponseBasePath = basePath
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
