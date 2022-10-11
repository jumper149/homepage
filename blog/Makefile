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

ASCIIDOC_COMPILER = asciidoctor
ASCIIDOCTOR_FLAGS = $\
	--doctype article $\
	--safe-mode server $\
	--attribute source-highlighter=rouge $\
	--attribute prewrap! $\
	--attribute author="$(AUTHOR)" $\
	--attribute email=$(EMAIL) $\
	--attribute homepage="$(HOMEPAGE)" $\
	--attribute revnumber=$(REVNUMBER)

$(BUILD)/%.html: $(SOURCE)/%.adoc
	mkdir -p $(BUILD)
	asciidoctor "$(SOURCE)/$*.adoc" --out-file "$(BUILD)/$*.html" $(ASCIIDOCTOR_FLAGS) --attribute imagesdir="$(BASEURL)/blog/raw/$*" --backend html5 --attribute nofooter --attribute webfonts!
	sed -i 's/^<head>$$/<head>\n<base target="_parent">/' "$(BUILD)/$*.html"

$(BUILD)/%.pdf: $(SOURCE)/%.adoc
	mkdir -p $(BUILD)
	asciidoctor-pdf "$(SOURCE)/$*.adoc" --out-file "$(BUILD)/$*.pdf" $(ASCIIDOCTOR_FLAGS) --attribute imagesdir="$*" --attribute pdf-theme="pdf-theme.yml"

clean:
	rm -r $(BUILD)
