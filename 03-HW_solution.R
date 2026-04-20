
############################################################
# 🧬 ChIP-seq Peak Analysis Homework Script
# Import → Overlaps → Annotation → Plotting
############################################################

# ----------------------------
# 📦 Load libraries
# ----------------------------
library(GenomicRanges)
library(genomation)
library(rtracklayer)
library(ggplot2)

# Bioconductor annotation packages
library(biomaRt)

# ----------------------------
# 📁 Part 1 — Get Ensembl mm10 genes from biomaRT
# ----------------------------

## Connect to biomart
mart <- useMart("ensembl", dataset = "hsapiens_gene_ensembl")

## Get gene annotations with the following attributes:
## - ensembl_gene_id
## - hgnc_symbol
## - chromosome_name
## - start_position
## - end_position
## - strand
## - gene_biotype

genes <- getBM(
  attributes = c("ensembl_gene_id", "hgnc_symbol", "chromosome_name", "start_position", "end_position", "strand", "gene_biotype"),
  mart = mart
)

## Inspect
head(genes)
dim(genes)

## Get protein coding genes and change strand to - / +
pc_genes <- genes |> filter(gene_biotype == "protein_coding") |>
  mutate(strand = ifelse(strand == 1, "+", "-")) |>
  mutate(chromosome_name = ifelse(chromosome_name %in% c("X", "Y"), chromosome_name, paste0("chr", chromosome_name)))

# Inspect
head(pc_genes)
dim(pc_genes)

## Convert to GRanges
genes_gr <- makeGRangesFromDataFrame(pc_genes,
                             seqnames.field = "chromosome_name",
                             start.field = "start_position",
                             end.field = "end_position",
                             strand.field = "strand",
                             keep.extra.columns = TRUE)

# Chromosome distribution
table(seqnames(genes_gr))

# ----------------------------
# 🧪 Part 2 — Get promoter regions
# ----------------------------

# Transcription Start Sites (TSS)
promoters_gr <- promoters(genes_gr, upstream = 3000, downstream = 3000)

# ----------------------------
# 🔍 Part 3 — Get peak files
# ----------------------------

# Read peaks with genomation (readGeneric)
# You need to tell the function that the data has a header and to keep metadata

h3k4me3_wt_1 <- readGeneric("https://bifx-core3.bio.ed.ac.uk/training/DSB/data/03_HW/wt_LPS_aH3K4me3_rep1.peaks.txt", keep.all.metadata = T, header = T)
h3k4me3_wt_2 <- readGeneric("https://bifx-core3.bio.ed.ac.uk/training/DSB/data/03_HW/wt_LPS_aH3K4me3_rep2.peaks.txt", keep.all.metadata = T, header = T)
h3k4me3_setd1_1 <- readGeneric("https://bifx-core3.bio.ed.ac.uk/training/DSB/data/03_HW/setd1_del_LPS_aH3K4me3_rep1.peaks.txt",keep.all.metadata = T, header = T)
h3k4me3_setd1_2 <- readGeneric("https://bifx-core3.bio.ed.ac.uk/training/DSB/data/03_HW/setd1_del_LPS_aH3K4me3_rep2.peaks.txt",keep.all.metadata = T, header = T)


# ----------------------------
# 🔍 Part 4 — Investigate peak overlaps with genes and promoters 
# ----------------------------

## You could use the findOverlaps function 
## or a package like ChIPseeker to annotate peaks with overlapping genes


# ----------------------------
# 🌐 Part 5 — Average profile of bigWigs over gene promoters
# ----------------------------

## You can use the soGGi package to do this
library(soGGi)

## Get bigwig files
bw_files <- c(
  "https://bifx-core3.bio.ed.ac.uk/training/DSB/data/03_HW/wt_LPS_aH3K4me3_rep1.bw",
  "https://bifx-core3.bio.ed.ac.uk/training/DSB/data/03_HW/wt_LPS_aH3K4me3_rep2.bw",
  "https://bifx-core3.bio.ed.ac.uk/training/DSB/data/03_HW/setd1_del_LPS_aH3K4me3_rep1.bw",
  "https://bifx-core3.bio.ed.ac.uk/training/DSB/data/03_HW/setd1_del_LPS_aH3K4me3_rep2.bw"
)

## Download files locally
bw_files |> map(~download.file(.x, destfile = paste0("data/",basename(.x))))

bws <- c(
  "data/wt_LPS_aH3K4me3_rep1.bw",
  "data/wt_LPS_aH3K4me3_rep2.bw",
  "data/setd1_del_LPS_aH3K4me3_rep1.bw",
  "data/setd1_del_LPS_aH3K4me3_rep2.bw"
)

## Create matrix for each bigwig over the promoters
matrices <- bws |> map(~ regionPlot(bamFile = .x,
                                               testRanges = promoters_gr,
                                               format = "bigwig",
                                               style = "percentOfRegion")
)

## join items in a list using c()
rpc <- do.call(c, matrices)

## Plot the chipProfiles
plotRegion(rpc, colourBy = "Sample")


