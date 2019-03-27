.DEFAULT_GOAL := generate-executable
DOCS_DIR  ?= docs

PANDOC = pandoc
PANDOC_FLAGS = --metadata date="`date +'%b %d, %Y'`" --toc

docs: $(addsuffix .pdf, $(basename $(wildcard $(DOCS_DIR)/*.md)))

$(DOCS_DIR)/%.pdf: $(DOCS_DIR)/%.md
	$(PANDOC) $(PANDOC_FLAGS) $< -o $@


generate-executable:
	# sbt clean
	sbt compile
	sbt package


.PHONY: generate-executable
