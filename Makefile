all:
	mkdir -p target/c && mkdir -p target/exe
	dune build && dune build @fmt --auto-promote
	ln -sf _build/default/bin/main.exe mini-go
	cd tgc && $(MAKE)

clean:
	rm -rf _build
	rm -f mini-go
	rm -rf target
	cd tgc && $(MAKE) clean