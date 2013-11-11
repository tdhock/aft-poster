HOCKING-AFT.pdf: HOCKING-AFT.tex figure-loss.tex
	pdflatex HOCKING-AFT
figure-loss.tex: figure-loss.R
	R --no-save < $<
