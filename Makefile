## caz
##
## Some Unix utilities to clean up files
## The commands are embedded in this Makefile, so $ in the command strings has to
## replaced with $$

TARGETS ?= $(addprefix src/, sales.csv gbp-usd.csv london.csv gbp-usd2.csv)

tfile := $(shell tempfile)
tfile1 := $(shell tempfile)

all: dirs $(TARGETS) xtmp all.Rout all.csv

clean::
	$(RM) $(wildcard *.jpeg)

all.Rout: gam0.R gam00.R
	Rscript gam0.R src/sales0-M.csv sales cp

all.csv: all.Rout src/gam-summary.awk
	awk -f src/gam-summary.awk all.Rout > $@

all.csv all-names.csv: all.Rout src/gam-summary.awk
	awk -v names=$@ -f src/gam-summary.awk all.Rout > all.csv


xtmp:
	$(RM) $(tfile) $(tfile1)

dirs:
	test -d src || mkdir src

## Monthly data from the MetOffice

src/weather.txt:
	wget --quiet -O $@ https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/heathrowdata.txt
	dos2unix $@
	sed -i '1,5d' $@
	sed -i '2d' $@

src/weather.csv: src/weather.txt
	cat $+ | awk '{ for (i=1; i<=NF; i++) printf("%s,", $$i); printf("\n") }' | sed -e 's/Provisional,$$//g' | sed -e 's/[*#]//g' -e 's/---//g' -e 's/,$$//g' > $@


## Different encodings

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

src/gbp-usd2.csv: bak/daily_csv.csv
	dos2unix < $< > $@

## UCS-2 (BOM)
## A load of blanks
src/london.csv: bak/LondonWeather2013-2018.csv
	sed -e '/^,,*$$/d'	$< | sed -e 's/HEATHROW, /HEATHROW /g' > $@
