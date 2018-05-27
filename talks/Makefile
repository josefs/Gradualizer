talk.pdf: talk.tex
	pdflatex talk.tex

talk.tex: talk.md Makefile
	pandoc talk.md -s -t beamer -o talk.tex --slide-level=2 --tab-stop=2 --highlight-style=zenburn
