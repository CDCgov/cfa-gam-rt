outputs/convolve.md: convolve.qmd convolve_funs.R
	mkdir -p outputs
	mkdir -p $(basename $@)_files
	quarto render $<
	mv $(notdir $@) outputs
	cp -r $(notdir $(basename $@))_files/* $(basename $@)_files

convolve.html:
	quarto render $<

view-out: outputs/convolve.md
	cd outputs; preview convolve.md --browser firefox &
