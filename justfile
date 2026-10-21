set default-list

# Run analysis at analysis/{{ path }}.qmd
analyze path="analysis" *flags: (render "analysis/" + path + ".qmd" flags)

# Clear quarto caches in analysis/
clean:
    @rm -rf analysis/*_cache

alias fmt-qmd := format-qmd
# Format markdown with panache
format-qmd path *flags:
    @panache format {{ path }} {{ flags }}

alias fmt-r := format-r
# Format R script with air
format-r path *flags:
    @air format {{ path }} {{ flags }}

# Lint R with jarl
lint path *flags:
    @jarl check {{ path }} {{ flags }}

# Render markdown with quarto
render path *flags:
    @quarto render {{ path }} {{ flags }}

# Source R with Rscript
run path *cmdargs:
    @Rscript {{ path }} {{ cmdargs }}

# Get versions of system dependencies
version:
    @air --version
    @jarl --version
    @panache --version
    @echo "quarto $(quarto --version)"

# Render manuscript at manuscript/{{ path }}.qmd
write path="manuscript" *flags: (render "manuscript/" + path + ".qmd" flags)
