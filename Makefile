NAME	:= pi-stream

all: run

run: $(NAME).ss
	scheme --script $<

edit:
	vim -c 'set nu et fdm=marker bg=dark' $(NAME).ss

.PHONY: all run edit
