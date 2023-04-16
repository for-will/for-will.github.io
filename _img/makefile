DOTSRC=$(wildcard *.dot)
SVGOUT=$(DOTSRC:%.dot=%.svg)
all: $(SVGOUT)

%.svg: %.dot
	dot -Tsvg -o $@ $<