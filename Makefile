dyndocs = 01-regression.qmd 02-glm.qmd


.PHONY:default
default: $(dyndocs)
	quarto render

.PHONY:stata
stata: $(dyndocs)
	@echo > /dev/null

$(dyndocs): %.qmd: %.dyndoc
	/Applications/Stata/StataSE.app/Contents/MacOS/stata-se -b 'dyntext "$<", saving("$@") replace nostop'

.PHONY:open
open:
	@open docs/index.html

.PHONY:preview
preview:
	quarto preview
