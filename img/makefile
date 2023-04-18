DOTSRC=$(wildcard dot/*.dot)
SVGOUT=$(DOTSRC:dot/%.dot=svg/%.svg)
all: $(SVGOUT)

svg/%.svg: dot/%.dot
	dot -Tsvg -o $@ $<