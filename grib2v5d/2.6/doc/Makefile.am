

# bin_PROGRAMS = grib2v5d.ps

# grib2v5d_ps_SOURCES = grib2v5d.tex

dist_pkgdoc_DATA = grib2v5d.pdf

.tex.dvi:
	latex $<

.dvi.ps:
	dvips -o $@ $<

.ps.pdf:
	ps2pdf $< $@

clean-local:
	rm -f grib2v5d.aux grib2v5d.dvi grib2v5d.log grib2v5d.ps grib2v5d.toc

# html/grib2v5d.html: grib2v5d.tex
# 	latex2html -split 3 -no_footnode grib2v5d.tex
# 	cp -f grib2v5d/*.html grib2v5d/*.css grib2v5d/*.png html
