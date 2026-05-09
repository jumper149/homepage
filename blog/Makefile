SOURCE = source
BUILD = build

ARTICLES = $(patsubst $(SOURCE)/%.adoc, %, $(wildcard $(SOURCE)/*.adoc))
BUILT_ADOC_ARTICLES = $(patsubst %, $(BUILD)/%.adoc, $(ARTICLES))
BUILT_HTML_ARTICLES = $(patsubst %, $(BUILD)/%.html, $(ARTICLES))
BUILT_PDF_ARTICLES = $(patsubst %, $(BUILD)/%.pdf, $(ARTICLES))

ifndef AUTHOR
$(error AUTHOR is not set)
endif

ifndef BASEURL
$(error BASEURL is not set)
endif

ifndef EMAIL
$(error EMAIL is not set)
endif

ifndef HOMEPAGE
$(error HOMEPAGE is not set)
endif

ifndef REVNUMBER
$(error REVNUMBER is not set)
endif

ASCIIDOCTOR_FLAGS = $\
	--doctype article $\
	--safe-mode server $\
	--attribute source-highlighter=rouge $\
	--attribute prewrap! $\
	--attribute author="$(AUTHOR)" $\
	--attribute email=$(EMAIL) $\
	--attribute homepage="$(HOMEPAGE)" $\
	--attribute revnumber=$(REVNUMBER)

ASCIIDOCTOR_HTML_FLAGS = $\
	--out-file "$(BUILD)/$*.html" $\
	--attribute imagesdir="$(BASEURL)/blog/raw/$*" $\
	--backend html5 $\
	--attribute nofooter $\
	--attribute stylesheet="html-stylesheet.css"

ASCIIDOCTOR_PDF_FLAGS = $\
	--out-file "$(BUILD)/$*.pdf" $\
	--attribute imagesdir="$*" $\
	--backend pdf $\
	--attribute pdf-theme="pdf-theme.yml"

$(BUILT_ADOC_ARTICLES): $(BUILD)/%.adoc: $(SOURCE)/%.adoc $(BUILD)
	cp "$<" "$(BUILD)/$*.adoc"
	if [ -d "$(SOURCE)/$*" ]; then cp --recursive "$(SOURCE)/$*" "$(BUILD)/$*"; fi

# TODO: This has an optional pre-requisite `$(SOURCE)/%` producing the optional target `$(BUILD)/%`.
$(BUILT_HTML_ARTICLES): $(BUILD)/%.html: $(BUILD)/%.adoc
	asciidoctor "$<" $(ASCIIDOCTOR_FLAGS) $(ASCIIDOCTOR_HTML_FLAGS)
	sed -i 's/^<head>$$/<head>\n<base target="_parent">/' "$(BUILD)/$*.html"

$(BUILT_PDF_ARTICLES): $(BUILD)/%.pdf: $(BUILD)/%.adoc
	asciidoctor-pdf "$<" $(ASCIIDOCTOR_FLAGS) $(ASCIIDOCTOR_PDF_FLAGS)

$(BUILD):
	mkdir -p "$(BUILD)"
	cp "custom-stylesheet/custom-stylesheet.css" "$(BUILD)/html-stylesheet.css"

.PHONY: clean
clean:
	rm -r $(BUILD)
