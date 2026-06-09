
library(tidyverse)

## Create a dataframe
ss <- data.frame(Sample = c("Control_1","Control_2","Control_3","MOV10_OE_1","MOV10_OE_2","MOV10_OE_3","MOV10_KD_2","MOV10_KD_3"),
                 Group = factor(c("Control","Control","Control","MOV10_OE","MOV10_OE","MOV10_OE","MOV10_KD","MOV10_KD"), levels = c("Control","MOV10_OE","MOV10_KD")),
                 Replicate = factor(c(1,2,3,1,2,3,2,3))
)

## Or read in a sample sheet.
## col_types argument specifies the data type for each column. The Sample name is a character and the Group and Replicate columns are factors.
ss <- read_tsv("data/salmon/samples.tsv", col_names = T, col_types = "cff")

## Look at our sample sheet
ss

library(GenomicFeatures)

## Get the tx2gene map file
tx2gene <- read_tsv("data/salmon/salmon_tx2gene.tsv",col_names = c("TranscriptID","GeneID","GeneSymbol"))

library(tximport)

## List all of our salmon quant.sf files
files <- file.path("data/salmon",ss$Sample,"quant.sf")
names(files) <- ss$Sample

## Import the transcript counts and summarise to gene counts
txi <- tximport(files, type = "salmon", tx2gene = tx2gene)

str(txi)

library(DESeq2)

## Create the DESeq dataset object
dds <- DESeqDataSetFromTximport(txi = txi, colData = ss, design = ~ Group)
dds
