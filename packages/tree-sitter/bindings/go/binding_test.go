package tree_sitter_bonzai_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_bonzai "github.com/bonzai-lang/bonzai/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_bonzai.Language())
	if language == nil {
		t.Errorf("Error loading Bonzai grammar")
	}
}
