.SUFFIXES: .czw .pdf .ss .tex

.czw.tex:
	chezweave $<

.tex.pdf:
	pdftex $*

.czw.ss:
	cheztangle $<
