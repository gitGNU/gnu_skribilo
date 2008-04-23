top_srcdir = ..

SKRIBILO := $(top_srcdir)/src/pre-inst-skribilo -v2

docs := index.skb
products := doc $(docs:%.skb=%.html)


all: $(products)

# We need this dummy directory to have correct SUI references.
doc:
	ln -s $(top_srcdir)/doc/user doc

%.html: %.skb
	$(SKRIBILO) --target=html -o $@ $^

clean:
	-rm -f $(products)


.PHONY: all clean

# arch-tag: c5c7c80c-5cb1-47f2-a93d-53c690e30bd6
