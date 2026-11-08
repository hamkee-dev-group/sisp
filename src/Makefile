srcdir = .


DESTDIR =
prefix = /usr/local
exec_prefix = ${prefix}

bindir = ${exec_prefix}/bin

INSTALL_PROGRAM = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644
INSTALL = /usr/bin/install -c

CC = cc
CPPFLAGS = -D_POSIX_C_SOURCE=200809L
# Keep debug symbols in optimized builds; `make debug` selects -O0.
OPTFLAGS = -O2
CFLAGS = -std=c11 -g $(OPTFLAGS) -Wall -Wextra -Wpedantic -Wshadow \
	-Wstrict-prototypes -Wmissing-prototypes
DEPFLAGS = -MMD -MP
LDFLAGS =
LIBS = -lm


PROG = sisp
OBJS = main.o pool.o reader.o lexer.o parser.o object.o numeric.o gc.o print.o equal.o comprehension.o \
       builtins-set.o builtins-math.o builtins-matrix.o builtins.o special.o eval.o process.o \
       doc.o
DEPS = $(OBJS:.o=.d)

%.o: %.c
	$(CC) -c -fno-common -I$(srcdir) $(CPPFLAGS) $(CFLAGS) $(DEPFLAGS) $<

all: $(PROG)

debug:
	$(MAKE) clean
	$(MAKE) OPTFLAGS='-O0' CPPFLAGS='$(CPPFLAGS) -DSISP_DEBUG' all

# Embed the prelude as per-line C literals, joined by main.c at startup.
# The executable does not need prelude.lsp at runtime.
embed: embed.c
	$(CC) -o $@ -I$(srcdir) $(CPPFLAGS) $(CFLAGS) embed.c

libsisp.h: prelude.lsp embed
	./embed prelude.lsp > libsisp.h

$(PROG): $(OBJS)
	$(CC) -o $(PROG) $(OBJS) $(LDFLAGS) $(LIBS)

main.o: libsisp.h

-include $(DEPS)

.PHONY: debug install uninstall clean realclean distclean mostlyclean

install: $(PROG)
	$(INSTALL) -d $(DESTDIR)$(bindir)
	$(INSTALL_PROGRAM) $(PROG) $(DESTDIR)$(bindir)

uninstall:
	rm -f $(DESTDIR)$(bindir)/$(PROG)

clean:
	-rm -f $(PROG) embed *.*~ $(OBJS) $(DEPS) libsisp.h


distclean: clean

realclean: distclean

mostlyclean: clean
