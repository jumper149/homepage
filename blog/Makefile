SOURCE = source
BUILD = build
ARTICLES = $(wildcard $(SOURCE)/*.adoc)

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
	--attribute webfonts!

ASCIIDOCTOR_PDF_FLAGS = $\
	--out-file "$(BUILD)/$*.pdf" $\
	--attribute imagesdir="$*" $\
	--attribute pdf-theme="pdf-theme.yml"

$(BUILD)/%.html: $(SOURCE)/%.adoc
	mkdir -p $(BUILD)
	asciidoctor "$(SOURCE)/$*.adoc" $(ASCIIDOCTOR_FLAGS) $(ASCIIDOCTOR_HTML_FLAGS)
	sed -i 's/^<head>$$/<head>\n<base target="_parent">/' "$(BUILD)/$*.html"
	if [ -d "$(SOURCE)/$*" ]; then cp --recursive "$(SOURCE)/$*" "$(BUILD)/$*"; fi

$(BUILD)/%.pdf: $(SOURCE)/%.adoc
	mkdir -p $(BUILD)
	asciidoctor-pdf "$(SOURCE)/$*.adoc" $(ASCIIDOCTOR_FLAGS) $(ASCIIDOCTOR_PDF_FLAGS)

.PHONY: clean
clean:
	rm -r $(BUILD)
