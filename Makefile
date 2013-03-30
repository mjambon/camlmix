# Type `make' for native code
# Type `make byte' for byte code instead of native code

VERSION = 1.3.1
export VERSION

SOURCES = \
  version.ml lexer.mli lexer.mll \
  parser_directive.mly directive.ml main.ml
RESULT = camlmix

.PHONY: default native byte all opt install www version force clean veryclean

default: native
native: version nc
byte: version bc

ifndef PREFIX
  PREFIX := $(shell dirname `which ocaml`)/..
endif
export PREFIX

all: byte
opt: native
install:
	install -m 0755 $(addsuffix $(EXE), \
			$(sort $(BCRESULT) $(NCRESULT))) $(PREFIX)/bin/

www: all test install archive

version:
	contents='let version = "$(VERSION)"'; \
	  if test "`cat version.ml`" != "$$contents"; then \
	    echo "$$contents" > version.ml;\
	  fi

.PHONY: examples
examples:
	cd examples && $(MAKE)

force:
	$(MAKE) clean
	$(MAKE)
	$(MAKE) archive

TRASH = META.mlx.ml langmix.* camlmix.exe *~

clean::
	cd examples && $(MAKE) clean


LIBS = unix

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
