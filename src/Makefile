srcdir = .


DESTDIR =
prefix = /usr/local
exec_prefix = ${prefix}

bindir = ${exec_prefix}/bin

INSTALL_PROGRAM = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644
INSTALL = /usr/bin/install -c

CC = cc
CFLAGS = -g -Wall -pedantic -Wextra
LDFLAGS =
LIBS = -lpthread -lm


PROG = sisp
OBJS = main.o  vmm.o lexer.o parser.o object.o funcs-set.o funcs-math.o funcs.o misc.o eval.o  process.o

.c.o:
	$(CC) -c -fcommon -DGCTHREADS -I$(srcdir) $(CFLAGS) $<

all: $(PROG)

$(PROG): $(OBJS)
	$(CC) -DGCTHREADS -o $(PROG) $(OBJS) $(LDFLAGS) $(LIBS)

.PHONY: install uninstall clean realclean distclean mostlyclean

install: $(PROG)
	$(INSTALL) -d $(DESTDIR)$(bindir)
	$(INSTALL_PROGRAM) $(PROG) $(DESTDIR)$(bindir)

uninstall:
	rm -f $(DESTDIR)$(bindir)/$(PROG)

clean: 
	-rm -f $(PROG) *.*~ $(OBJS)


distclean: clean 
	-rm -f Makefile

realclean: distclean

mostlyclean: clean

$(OBJS): sisp.h libsisp.h extern.h 
