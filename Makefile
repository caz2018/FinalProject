## caz
##
## Some Unix utilities to clean up files
## The commands are embedded in this Makefile, so $ in the command strings has to
## replaced with $$

TARGETS ?= $(addprefix src/, sales.csv gbp-usd.csv london.csv usd-gbp.csv)

tfile := $(shell tempfile)
tfile1 := $(shell tempfile)

all: dirs $(TARGETS) xtmp

xtmp:
	$(RM) $(tfile) $(tfile1)

dirs:
	test -d src || mkdir src

## Different formats

## This is UCS-2 (BOM) with DOS line-feed
src/sales.csv: bak/AllHistoricalData.csv
	recode u2/cl..u2/cr < $< > $(tfile)
	dos2unix < $(tfile) > $@

## UCS-2 (BOM)
## This has quotes and a % sign
## The month year is an odd string
src/gbp-usd.csv: bak/GBP_USD.csv
	dos2unix < $< > $(tfile)
	sed -e 's/"//g' -e 's/%//g' < $(tfile) | sed 's/^\([A-Za-z]*\) \(.*\)$$/\1,\2/g' | sed -e 's/ //g' -e 's/,0/,/g' | awk -F, 'BEGIN { OFS=","} NR == 1 { $$1="Year"; $$0="Month,Day,"$$0; } { print }' > $@

src/usd-gbp.csv: bak/daily_csv.csv
	dos2unix < $< > $(tfile)
	awk -F, 'BEGIN { OFS="," } NR == 1 { print; next } $$2 ~ /United Kingdom/ { print }' $(tfile) > $@

## UCS-2 (BOM)
## A load of blanks
src/london.csv: bak/LondonWeather2013-2018.csv
	sed -e '/^,,*$$/d'	$< | sed -e 's/HEATHROW, /HEATHROW /g' > $@