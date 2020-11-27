SHELL != which bash

main: code csvs

# Invoke as "make -k csvs" if you want make to continue after a fail on a mp3
audio-dir := ../../1_Input/1_Generating_Dataframe/Audio
csv-dir := ../../3_Output/1_Generating_Dataframe/CSV
mp3s := $(wildcard $(audio-dir)/*.mp3)
csvs := $(mp3s:$(audio-dir)/%.mp3=$(csv-dir)/%.csv)
csvs: $(csvs)
.PHONY: csvs

birds.head := ../../1_Input/1_Generating_Dataframe/birds.head
all.csv := ../../3_Output/1_Generating_Dataframe/all.csv
$(all.csv): $(csvs)
	(cut -d, -f 2- $(birds.head); ls $(csv-dir)/*.csv | xargs -i sed -n 2p {} | cut -d, -f 2-) > $@
all: $(all.csv)
.PHONY: all

CodeDF.R: call-purl.R
code: CodeDF.R

%.R: %.md;
	./call-purl.R $< 2> /dev/null
	@echo -e '1i\n#!/usr/bin/Rscript --vanilla\n.\nwq' | ed $@ 2> /dev/null
	@chmod +x $@

$(csv-dir)/%.csv: $(audio-dir)/%.mp3; ./CodeDF.R $< |& col -b > $(basename $@).out
