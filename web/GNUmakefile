SKRIBILO := skribilo -v2

docs := index.skb
products := $(docs:%.skb=%.html)


all: $(products)

%.html: %.skb
	$(SKRIBILO) --target=html -o $@ $^

clean:
	-rm -f $(products)


.PHONY: all clean

# arch-tag: c5c7c80c-5cb1-47f2-a93d-53c690e30bd6
