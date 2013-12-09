### R code from vignette source 'A01_Introduction.Rnw'

### R code from vignette source 'A11_R.Rnw'

###################################################
### code chunk number 1: setup
###################################################
library(SequenceAnalysisData)


###################################################
### code chunk number 2: prompt
###################################################
## assign values 5, 4, 3, 2, 1 to variable 'x'
x <- c(5, 4, 3, 2, 1)
x


###################################################
### code chunk number 3: colon
###################################################
x[2:4]


###################################################
### code chunk number 4: log
###################################################
log(x)


###################################################
### code chunk number 5: types
###################################################
c(1.1, 1.2, 1.3)         # numeric
c(FALSE, TRUE, FALSE)    # logical
c("foo", "bar", "baz")   # character, single or double quote ok
as.character(x)          # convert 'x' to character
typeof(x)                # the number 5 is numeric, not integer
typeof(2L)               # append 'L' to force integer
typeof(2:4)              # ':' produces a sequence of integers


###################################################
### code chunk number 6: factor
###################################################
sex <- factor(c("Male", "Female", NA), levels=c("Female", "Male"))
sex


###################################################
### code chunk number 7: lists
###################################################
lst <- list(a=1:3, b=c("foo", "bar"), c=sex)
lst


###################################################
### code chunk number 8: list-subset
###################################################
lst[c(3, 1)]             # another list
lst[["a"]]               # the element itself, selected by name


###################################################
### code chunk number 9: data.frame
###################################################
df <- data.frame(age=c(27L, 32L, 19L),
                 sex=factor(c("Male", "Female", "Male")))
df
df[c(1, 3),]
df[df$age > 20,]


###################################################
### code chunk number 10: matrix
###################################################
m <- matrix(1:12, nrow=3)
m
m[c(1, 3), c(2, 4)]


###################################################
### code chunk number 11: matrix-subset
###################################################
m[, 3]
m[, 3, drop=FALSE]


###################################################
### code chunk number 12: lm
###################################################
x <- rnorm(1000, sd=1)
y <- x + rnorm(1000, sd=.5)
fit <- lm(y ~ x)       # formula describes linear regression 
fit                    # an 'S3' object
anova(fit)
sqrt(var(resid(fit)))  # residuals accessor and subsequent transforms
class(fit)


###################################################
### code chunk number 13: function-args
###################################################
y <- 5:1
log(y)
args(log)        # arguments 'x' and 'base'; see ?log
log(y, base=2)   # 'base' is optional, with default value
try(log())       # 'x' required; 'try' continues even on error
args(data.frame) # ... represents variable number of arguments


###################################################
### code chunk number 14: named-args
###################################################
log(base=2, y)   # match argument 'base' by name, 'x' by position


###################################################
### code chunk number 15: S3-method-args
###################################################
args(anova)
args(anova.glm)


###################################################
### code chunk number 16: rbioc-pdata
###################################################
pdataFile <- system.file(package="SequenceAnalysisData", "extdata", 
                         "pData.csv")


###################################################
### code chunk number 17: rbioc-pdata-csv
###################################################
pdata <- read.table(pdataFile)  
dim(pdata)
names(pdata)
summary(pdata)


###################################################
### code chunk number 18: rbioc-pdata-subset
###################################################
head(pdata[,"sex"], 3)
head(pdata$sex, 3)
head(pdata[["sex"]], 3)
sapply(pdata, class)


###################################################
### code chunk number 19: rbioc-pdata-sextab
###################################################
table(pdata$sex, useNA="ifany")


###################################################
### code chunk number 20: rbioc-pdata-molbiol
###################################################
with(pdata, table(mol.biol, useNA="ifany"))


###################################################
### code chunk number 21: rbbioc-pdata-bcrabl
###################################################
ridx <- pdata$mol.biol %in% c("BCR/ABL", "NEG")


###################################################
### code chunk number 22: rbioc-pdata-molbiol-selected
###################################################
table(ridx)
sum(ridx)


###################################################
### code chunk number 23: rbioc-pdata-subset
###################################################
pdata1 <- pdata[ridx,]


###################################################
### code chunk number 24: rbioc-pdata-subset-levels
###################################################
levels(pdata1$mol.biol)


###################################################
### code chunk number 25: rbioc-pdata-subset-recode
###################################################
pdata1$mol.biol <- factor(pdata1$mol.biol)
table(pdata1$mol.biol)


###################################################
### code chunk number 26: rbioc-pdata-age-molbiol
###################################################
with(pdata1, t.test(age ~ mol.biol))


###################################################
### code chunk number 27: rbioc-pdata-boxplot (eval = FALSE)
###################################################
## ## not evaluated
## boxplot(age ~ mol.biol, pdata1)


###################################################
### code chunk number 28: fail
###################################################
f <- function(i) {
    if (i < 0)
        stop("i is negative")
    rnorm(i)
}
lapply(0:1, f)


###################################################
### code chunk number 29: tryCatch
###################################################
lapply(-1:1, function(i) {
    tryCatch({
        f(i)
    }, error=function(err) {
        ## return 'NA' when error occurs, instead of stopping
        NA_real_
    })
})


###################################################
### code chunk number 30: io-sketch (eval = FALSE)
###################################################
## ## not evaluated
## colClasses <-
##   c("NULL", "integer", "numeric", "NULL")
## df <- read.table("myfile", colClasses=colClasses)


###################################################
### code chunk number 31: iterative
###################################################
x <- runif(100000); x2 <- x^2
m <- matrix(x2, nrow=1000); y <- rowSums(m)


###################################################
### code chunk number 32: preallocate-and-fill-sketch (eval = FALSE)
###################################################
## ## not evaluated
## result <- numeric(nrow(df))
## for (i in seq_len(nrow(df)))
##   result[[i]] <- some_calc(df[i,])


###################################################
### code chunk number 33: unnecessary-names
###################################################
unlist(list(a=1:2)) # name 'a' becomes 'a1', 'a2'
unlist(list(a=1:2), use.names=FALSE)   # no names


###################################################
### code chunk number 34: inappropriate-functions-2 (eval = FALSE)
###################################################
## ## not evaluated
## library(limma) # microarray linear models
## fit <- lmFit(eSet, design)


###################################################
### code chunk number 35: algo-poly
###################################################
x <- 1:100; s <- sample(x, 10)
inS <- x %in% s


###################################################
### code chunk number 36: system.time
###################################################
m <- matrix(runif(200000), 20000)
system.time(apply(m, 1, sum))


###################################################
### code chunk number 37: rbenchmark
###################################################
library(rbenchmark)
f0 <- function(x) apply(x, 1, sum)
f1 <- function(x) rowSums(x)
benchmark(f0(m), f1(m), 
          columns=c("test", "elapsed", "relative"), 
          replications=5)


###################################################
### code chunk number 38: identical
###################################################
res1 <- apply(m, 1, sum)
res2 <- rowSums(m)
identical(res1, res2)
identical(c(1, -1), c(x=1, y=-1))
all.equal(c(1, -1), c(x=1, y=-1),
          check.attributes=FALSE)


###################################################
### code chunk number 39: lattice
###################################################
library(lattice)
plt <- dotplot(variety ~ yield | site, data = barley, groups = year,
               xlab = "Barley Yield (bushels/acre)" , ylab=NULL,
               key = simpleKey(levels(barley$year), space = "top", 
                 columns=2),
               aspect=0.5, layout = c(2,3))
print(plt)


###################################################
### code chunk number 40: search
###################################################
length(search())
search()


###################################################
### code chunk number 41: double-colon
###################################################
base::log(1:3)


###################################################
### code chunk number 42: package (eval = FALSE)
###################################################
## library(IntermediateSequenceAnalysis2013)
## sessionInfo()


###################################################
### code chunk number 43: help-start (eval = FALSE)
###################################################
## help.start()


###################################################
### code chunk number 44: help (eval = FALSE)
###################################################
## ?data.frame
## ?lm
## ?anova             # a generic function
## ?anova.lm          # an S3 method, specialized for 'lm' objects


###################################################
### code chunk number 45: S3-interactive
###################################################
methods(anova)
methods(class="glm")


###################################################
### code chunk number 46: S3-view (eval = FALSE)
###################################################
## anova.lm
## getAnywhere("anova.loess")


###################################################
### code chunk number 47: head-src
###################################################
utils::head
methods(head)
head(head.matrix)


###################################################
### code chunk number 48: vignette (eval = FALSE)
###################################################
## vignette(package="IntermediateSequenceAnalysis2013")


### R code from vignette source 'A12_Bioconductor.Rnw'

###################################################
### code chunk number 1: S4-view (eval = FALSE)
###################################################
## selectMethod(countOverlaps, c("GRanges", "GRanges"))


###################################################
### code chunk number 2: GRanges
###################################################
library(GenomicRanges)
getClass("GRanges")


###################################################
### code chunk number 3: countOverlaps
###################################################
showMethods("countOverlaps")


###################################################
### code chunk number 4: countOverlapsWorks
###################################################
gr0 <- GRanges("chr1", IRanges(start=c(10, 20), width = 5), "+")
gr1 <- GRanges("chr1", IRanges(start=12, end=18), "+")
countOverlaps(gr0, gr1)


###################################################
### code chunk number 5: S4
###################################################
library(Biostrings)
showMethods(complement)


###################################################
### code chunk number 6: S4-showMethods (eval = FALSE)
###################################################
## showMethods(class="DNAStringSet", where=search())


###################################################
### code chunk number 7: S4-help (eval = FALSE)
###################################################
## class ? DNAStringSet
## method ? "complement,DNAStringSet"


### R code from vignette source 'A13_SequenceAnalysis.Rnw'

### R code from vignette source 'A14_ReadsAndStrings.Rnw'

###################################################
### code chunk number 1: gc-genome
###################################################
library(BSgenome.Dmelanogaster.UCSC.dm3)
Dmelanogaster
library(SequenceAnalysisData)
data(ex)
ex[1]
nm <- "chr3L"
chr <- Dmelanogaster[[nm]]
v <- Views(chr, start=start(ex[[1]]), end=end(ex[[1]]))


###################################################
### code chunk number 2: gcFunction-definition
###################################################
gcFunction <- 
    function(x)
{
    alf <- alphabetFrequency(x, as.prob=TRUE)
    rowSums(alf[,c("G", "C")])
}


###################################################
### code chunk number 3: gcFunction-genome (eval = FALSE)
###################################################
## subjectGC <- gcFunction(v)


###################################################
### code chunk number 4: fastq-format
###################################################
bigdata <- system.file("bigdata", package="SequenceAnalysisData")
fls <- dir(file.path(bigdata, "fastq"), full=TRUE)
cat(noquote(readLines(fls[[1]], 4)), sep="\n")


###################################################
### code chunk number 5: ascii
###################################################
cat(rawToChar(as.raw(32+1:47)),
    rawToChar(as.raw(32+48:94)), sep="\n")


###################################################
### code chunk number 6: readFastq
###################################################
library(ShortRead)
bigdata <- system.file("bigdata", package="SequenceAnalysisData")
fastqDir <- file.path(bigdata, "fastq")
fastqFiles <- dir(fastqDir, full=TRUE)
fq <- readFastq(fastqFiles[1])
fq


###################################################
### code chunk number 7: sread
###################################################
head(sread(fq), 3)
head(quality(fq), 3)
head(id(fq), 3)


###################################################
### code chunk number 8: getClasse
###################################################
getClass("ShortReadQ")


###################################################
### code chunk number 9: showMethods-ShortRead (eval = FALSE)
###################################################
## showMethods(class="ShortRead", where="package:ShortRead")


###################################################
### code chunk number 10: width-ShortRead
###################################################
table(width(fq))


###################################################
### code chunk number 11: width-ShortReadQ
###################################################
abc <- alphabetByCycle(sread(fq))
abc[1:4, 1:8]


###################################################
### code chunk number 12: FastqSampler
###################################################
sampler <- FastqSampler(fastqFiles[1], 1000000)
yield(sampler) # sample of 1000000 reads


###################################################
### code chunk number 13: qa (eval = FALSE)
###################################################
## qas0 <- Map(function(fl, nm) {
##     fq <- FastqSampler(fl)
##     qa(yield(fq), nm)
## }, fastqFiles,
##    sub("_subset.fastq", "", basename(fastqFiles)))
## qas <- do.call(rbind, qas0)
## rpt <- report(qas, dest=tempfile())
## browseURL(rpt)


###################################################
### code chunk number 14: report (eval = FALSE)
###################################################
## rpt <- system.file("GSM461176_81_qa_report", "index.html",
##                    package="SequenceAnalysisData")
## browseURL(rpt)


###################################################
### code chunk number 15: fastq-discovery
###################################################
dir(bigdata)
fls <- dir(file.path(bigdata, "fastq"), full=TRUE)


###################################################
### code chunk number 16: fastq-input-gc
###################################################
rm(fq); invisible(gc())


###################################################
### code chunk number 17: fastq-input
###################################################
fq <- readFastq(fls[1])


###################################################
### code chunk number 18: gcC
###################################################
alf0 <- alphabetFrequency(sread(fq), as.prob=TRUE, collapse=TRUE)
sum(alf0[c("G", "C")])


###################################################
### code chunk number 19: gc-reads
###################################################
gc <- gcFunction(sread(fq))
hist(gc)


###################################################
### code chunk number 20: abc
###################################################
abc <- alphabetByCycle(sread(fq))
matplot(t(abc[c("A", "C", "G", "T"),]), type="l")


###################################################
### code chunk number 21: abc-mclapply (eval = FALSE)
###################################################
## library(parallel)
## gc0 <- mclapply(fls, function(fl) {
##   fq <- readFastq(fl)
##   gc <- gcFunction(sread(fq))
##   table(cut(gc, seq(0, 1, .05)))
## })
## ## simplify list of length 2 to 2-D array
## gc <- simplify2array(gc0)
## matplot(gc, type="s")


###################################################
### code chunk number 22: quality
###################################################
head(quality(fq))
qual <- as(quality(fq), "matrix")
dim(qual)
plot(colMeans(qual), type="b")


### R code from vignette source 'A15_AlignmentsAndRanges.Rnw'

###################################################
### code chunk number 1: setup
###################################################
library(GenomicFeatures)
plotRanges <- function(x, xlim = x, main = deparse(substitute(x)),
                       col = "black", sep = 0.5, ...)
{
    height <- 1
    if (is(xlim, "Ranges"))
        xlim <- c(min(start(xlim)), max(end(xlim)))
    bins <- disjointBins(IRanges(start(x), end(x) + 1))
    plot.new()
    par(mai=c(0.6, 0.2, 0.6, 0.2))
    plot.window(xlim, c(0, max(bins)*(height + sep)))
    ybottom <- bins * (sep + height) - height
    rect(start(x)-0.5, ybottom, end(x)+0.5, ybottom + height, col = col, ...)
    title(main, cex.main=2.8, font.main=1)
    axis(1)
}


###################################################
### code chunk number 2: GRanges-genes
###################################################
genes <- GRanges(seqnames=c("3R", "X"),
                 ranges=IRanges(
                     start=c(19967117, 18962306),
                     end=c(19973212, 18962925)),
                 strand=c("+", "-"),
                 seqlengths=c(`3R`=27905053L, `X`=22422827L))


###################################################
### code chunk number 3: GRanges-display
###################################################
genes


###################################################
### code chunk number 4: GRanges-help (eval = FALSE)
###################################################
## ?GRanges


###################################################
### code chunk number 5: GRanges-vignettes (eval = FALSE)
###################################################
## vignette(package="GenomicRanges")


###################################################
### code chunk number 6: GRanges-basics
###################################################
genes[2]
strand(genes)
width(genes)
length(genes)
names(genes) <- c("FBgn0039155", "FBgn0085359")
genes  # now with names


###################################################
### code chunk number 7: ranges-ir
###################################################
ir <- IRanges(start=c(7, 9, 12, 14, 22:24),
              end=c(15, 11, 12, 18, 26, 27, 28))


###################################################
### code chunk number 8: ranges-ir-plot
###################################################
png("ranges-ir-plot.png", width=800, height=160)
plotRanges(ir, xlim=c(5, 35), main="Original")
dev.off()
png("ranges-shift-ir-plot.png", width=800, height=160)
plotRanges(shift(ir, 5), xlim=c(5, 35), main="Shift")
dev.off()
png("ranges-reduce-ir-plot.png", width=800, height=160)
plotRanges(reduce(ir), xlim=c(5, 35), main="Reduce")
dev.off()


###################################################
### code chunk number 9: ranges-shift-ir
###################################################
shift(ir, 5)


###################################################
### code chunk number 10: ranges-shift-ir-plot
###################################################
png("ranges-shift-ir-plot.png", width=800, height=160)
plotRanges(shift(ir, 5), xlim=c(5, 35), main="Shift")
dev.off()


###################################################
### code chunk number 11: ranges-reduce-ir
###################################################
reduce(ir)


###################################################
### code chunk number 12: ranges-reduce-ir-plot
###################################################
png("ranges-reduce-ir-plot.png", width=800, height=160)
plotRanges(reduce(ir), xlim=c(5, 35), main="Reduce")
dev.off()


###################################################
### code chunk number 13: coverage
###################################################
coverage(ir)


###################################################
### code chunk number 14: GRanges-mcols
###################################################
mcols(genes) <- DataFrame(EntrezId=c("42865", "2768869"),
                          Symbol=c("kal-1", "CG34330"))


###################################################
### code chunk number 15: GRanges-metadata
###################################################
metadata(genes) <-
    list(CreatedBy="A. User", Date=date())


###################################################
### code chunk number 16: GRangesList-eg-setup
###################################################
library(TxDb.Dmelanogaster.UCSC.dm3.ensGene)
txdb <- TxDb.Dmelanogaster.UCSC.dm3.ensGene # alias
fbgn <- exonsBy(txdb, "gene")["FBgn0039155"]
seqlevels(fbgn) <- "chr3R"


###################################################
### code chunk number 17: GRangesList-eg
###################################################
fbgn


###################################################
### code chunk number 18: txdb
###################################################
library(TxDb.Dmelanogaster.UCSC.dm3.ensGene)
txdb <- TxDb.Dmelanogaster.UCSC.dm3.ensGene # alias
ex0 <- exonsBy(txdb, "gene")
head(table(elementLengths(ex0)))
ids <- c("FBgn0002183", "FBgn0003360", "FBgn0025111", "FBgn0036449")
ex <- reduce(ex0[ids])


###################################################
### code chunk number 19: SAM
###################################################
fl <- system.file("extdata", "ex1.sam", package="Rsamtools")
strsplit(readLines(fl, 1), "\t")[[1]]


###################################################
### code chunk number 20: readGappedAlignments
###################################################
alnFile <- system.file("extdata", "ex1.bam", package="Rsamtools")
aln <- readGappedAlignments(alnFile)
head(aln, 3)


###################################################
### code chunk number 21: GappedAlignments-accessors
###################################################
table(strand(aln))
table(width(aln))
head(sort(table(cigar(aln)), decreasing=TRUE))


###################################################
### code chunk number 22: bam-ex-fls
###################################################
bigdata <- system.file("bigdata", package="SequenceAnalysisData")
fls <- dir(file.path(bigdata, "bam"), ".bam$", full=TRUE) #$
names(fls) <- sub("_.*", "", basename(fls))


###################################################
### code chunk number 23: bam-ex-input
###################################################
## input
aln <- readGappedAlignments(fls[1])
xtabs(~seqnames + strand, as.data.frame(aln))


###################################################
### code chunk number 24: bam-ex-roi
###################################################
data(ex)             # from an earlier exercise


###################################################
### code chunk number 25: bam-ex-strand
###################################################
strand(aln) <- "*"   # protocol not strand-aware


###################################################
### code chunk number 26: bam-ex-hits
###################################################
hits <- countOverlaps(aln, ex)
table(hits)


###################################################
### code chunk number 27: bam-ex-cnt
###################################################
cnt <- countOverlaps(ex, aln[hits==1])


###################################################
### code chunk number 28: bam-count-fun
###################################################
counter <- 
    function(filePath, range)
{
    aln <- readGappedAlignments(filePath)
    strand(aln) <- "*"
    hits <- countOverlaps(aln, range)
    countOverlaps(range, aln[hits==1])
}


###################################################
### code chunk number 29: bam-count-all
###################################################
counts <- sapply(fls, counter, ex)


###################################################
### code chunk number 30: bam-count-mclapply (eval = FALSE)
###################################################
## if (require(parallel))
##     simplify2array(mclapply(fls, counter, ex))


### R code from vignette source 'A17_LargeData.Rnw'

###################################################
### code chunk number 1: setup
###################################################
library(SequenceAnalysisData)
library(RNAseqData.HeLa.bam.chr14)
library(TxDb.Dmelanogaster.UCSC.dm3.ensGene)
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
library(Rsamtools)
library(ShortRead)
library(parallel); options(mc.cores=detectCores())
library(lattice)


###################################################
### code chunk number 2: restriction-what
###################################################
param <- ScanBamParam(what=c("rname", "pos", "cigar"))


###################################################
### code chunk number 3: restriction-which
###################################################
library(TxDb.Dmelanogaster.UCSC.dm3.ensGene)
exByGn <- exonsBy(TxDb.Dmelanogaster.UCSC.dm3.ensGene, "gene")
seqlevels(exByGn, force=TRUE) <- "chr3L"
gns <- unlist(range(exByGn))
param <- ScanBamParam(which=gns)


###################################################
### code chunk number 4: restrictions-several
###################################################
param <- ScanBamParam(what=c("rname", "pos", "cigar"), which=gns)


###################################################
### code chunk number 5: bigdata
###################################################
bigdata <- system.file("bigdata", package="SequenceAnalysisData")
bamfls <- dir(file.path(bigdata, "bam"), ".bam$", full=TRUE) #$
names(bamfls) <- sub("_.*", "", basename(bamfls))


###################################################
### code chunk number 6: scanBam-restriction
###################################################
param <- ScanBamParam(what="rname")
bam <- scanBam(bamfls[1], param=param)[[1]]
table(bam$rname)


###################################################
### code chunk number 7: readGappedAlignments-restriciton
###################################################
gal <- readGappedAlignments(bamfls[1])
head(gal, 3)
param <- ScanBamParam(what="seq")  ## also input sequence
gal <- readGappedAlignments(bamfls[1], param=param)
head(mcols(gal)$seq)


###################################################
### code chunk number 8: GC-sampling
###################################################
gcFunction <- 
    function(x)
{
    alf <- alphabetFrequency(x, as.prob=TRUE)
    rowSums(alf[,c("G", "C")])
}


###################################################
### code chunk number 9: fastq-files
###################################################
bigdata <- system.file("bigdata", package="SequenceAnalysisData")
fqfl <- dir(file.path(bigdata, "fastq"), ".fastq$", full=TRUE) #$


###################################################
### code chunk number 10: fastq-sampling
###################################################
sampler <- FastqSampler(fqfl, 100000)
fq <- yield(sampler)                    # 100,000 reads
lattice::densityplot(gcFunction(sread(fq)), plot.points=FALSE)
fq <- yield(sampler)                    # a different 100,000 reads


###################################################
### code chunk number 11: fastq-paired-end (eval = FALSE)
###################################################
## ## NOT RUN
## set.seed(123)
## end1 <- yield(FastqSampler("end_1.fastq"))
## set.seed(123)
## end2 <- yield(FastqSampler("end_2.fastq"))


###################################################
### code chunk number 12: readLines-chunks (eval = FALSE)
###################################################
## ## NOT RUN
## con <- file("<hypothetical-file>.txt")
## open(f)
## while (length(x <- readLines(f, n=10000))) {
##     ## work on character vector 'x'
## }
## close(f)


###################################################
### code chunk number 13: iteration-bam
###################################################
library(RNAseqData.HeLa.bam.chr14)
bamfl <- RNAseqData.HeLa.bam.chr14_BAMFILES[1]
countBam(bamfl)
bf <- BamFile(bamfl, yieldSize=200000) # could be larger, e.g., 2 million


###################################################
### code chunk number 14: BamFile-iter (eval = FALSE)
###################################################
## ## initialize, e.g., for step 3 ...
## open(bf)
## while (length(gal <- readGappedAlignments(bf))) {
##     ## step 2: do work...
##     ## step 3: aggregate results...
## }
## close(bf)


###################################################
### code chunk number 15: counter-iter
###################################################
counter <- 
    function(query, subject, ..., ignore.strand=TRUE)
    ## query: GRanges or GRangesList
    ## subject: GappedAlignments
{
    if (ignore.strand)
        strand(subject) <- "*"
    hits <- countOverlaps(subject, query)
    countOverlaps(query, subject[hits==1])
}


###################################################
### code chunk number 16: counter-roi
###################################################
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
query <- exonsBy(TxDb.Hsapiens.UCSC.hg19.knownGene, "gene")


###################################################
### code chunk number 17: counter-work (eval = FALSE)
###################################################
## ## initialize, e.g., for step 3 ...
## open(bf)
## while (length(gal <- readGappedAlignments(bf))) {
##     ## step 2: do work...
##     count0 <- counter(query, gal, ignore.strand=TRUE)
##     ## step 3: aggregate results...
## }
## close(bf)


###################################################
### code chunk number 18: counter-work
###################################################
counter1 <- 
    function(bf, query, ...)
{
    ## initialize, e.g., for step 3 ...
    counts <- integer(length(query))
    open(bf)
    while (length(gal <- readGappedAlignments(bf, ...))) {
        ## step 2: do work...
        count0 <- counter(query, gal, ignore.strand=TRUE)
        ## step 3: aggregate results...
        counts <- counts + count0
    }
    close(bf)
    counts
}


###################################################
### code chunk number 19: counter1
###################################################
bf <- BamFile(bamfl, yieldSize=500000)
counts <- counter1(bf, query)


###################################################
### code chunk number 20: count-bfl-make
###################################################
fls <- RNAseqData.HeLa.bam.chr14_BAMFILES # 8 BAM files
bamfls <- BamFileList(fls, yieldSize=500000) # yieldSize can be larger


###################################################
### code chunk number 21: count-bams (eval = FALSE)
###################################################
## counts <- simplify2array(lapply(bamfls, counter1, query))


###################################################
### code chunk number 22: count-bams-mclapply
###################################################
options(mc.cores=detectCores())         # use all cores
counts <- simplify2array(mclapply(bamfls, counter1, query))
head(counts[rowSums(counts) != 0,], 3)


### R code from vignette source 'A21_RNASeqData.Rnw'

###################################################
### code chunk number 1: setup
###################################################
options(digits=2)


### R code from vignette source 'A225_RNASeqCounting.Rnw'

###################################################
### code chunk number 1: setup
###################################################
options(max.print=200)
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
library(RNAseqData.HeLa.bam.chr14)
library(Rsamtools)
library(ShortRead)
library(parallel); options(mc.cores=detectCores())
library(lattice)


###################################################
### code chunk number 2: count (eval = FALSE)
###################################################
## ## 'Annotation' describing known gene models
## library(TxDb.Hsapiens.UCSC.hg19.knownGene)
## exByGn <- exonsBy(TxDb.Hsapiens.UCSC.hg19.knownGene, "gene")
## 
## ## reads sorted by qname
## fls <- dir("~/some/dir", pattern="bam$")
## bamfls <- BamFileList(fls, index=character(), 
##                obeyQname=TRUE, yieldSize=1000000)
## 
## ## restrict to primary alignment of paired reads
## flag <- scanBamFlag(isNotPrimaryRead=FALSE, isProperPair=TRUE)
## param <- ScanBamParam(flag=flag)
## 
## ## count reads overlapping a gene model; each read counted 
## ## at most once; strand-unaware assay; paired-end reads; 
## cnts <- summarizeOverlaps(exByGn, bamfls, mode=Union,
##     ignore.strand=TRUE, single.end=FALSE, param=param)


###################################################
### code chunk number 3: hela-metadata
###################################################
url <- sprintf("%s/%s",
    "http://www.ebi.ac.uk/arrayexpress/files",
    "E-MTAB-1147/E-MTAB-1147.sdrf.txt")
pdata <- read.delim(url, sep="\t", stringsAsFactors=FALSE)


###################################################
### code chunk number 4: pdata-explore
###################################################
pdata[1:3, grep("Characteristics", names(pdata))]
pdata[1:3, grep("Comment", names(pdata))]
pdata$Factor.Value.RNAI.


###################################################
### code chunk number 5: pdata-simplify
###################################################
drop <- c("Scan.Name", "Comment.SUBMITTED_FILE_NAME.",
          "Comment.FASTQ_URI.")
pdata <- unique(pdata[,!names(pdata) %in% drop])


###################################################
### code chunk number 6: package-description
###################################################
packageDescription("RNAseqData.HeLa.bam.chr14")


###################################################
### code chunk number 7: package-help (eval = FALSE)
###################################################
## package?RNAseqData.HeLa.bam.chr14


###################################################
### code chunk number 8: package-scripts (eval = FALSE)
###################################################
## README_file <- system.file("scripts", "README.TXT",
##                            package="RNAseqData.HeLa.bam.chr14")
## cat(readLines(README_file), sep="\n")


###################################################
### code chunk number 9: bam-files
###################################################
library(RNAseqData.HeLa.bam.chr14)
library(Rsamtools)
fls <- RNAseqData.HeLa.bam.chr14_BAMFILES
bfl <- BamFileList(fls)
bfl
bf <- bfl[[1]]; bf


###################################################
### code chunk number 10: seqinfo
###################################################
head(seqlengths(bfl[[1]]))


###################################################
### code chunk number 11: hdr
###################################################
hdr <- scanBamHeader(bfl[[1]])


###################################################
### code chunk number 12: hdr-data
###################################################
hdr[["text"]][["@PG"]][[1]]
substr(hdr[["text"]][["@PG"]][[3]], 1, 60)


###################################################
### code chunk number 13: countBam
###################################################
countBam(bfl)


###################################################
### code chunk number 14: quickCountBam
###################################################
quickCountBam(bf)


###################################################
### code chunk number 15: bamsampler
###################################################
sampler <- BamSampler(fls[1], yieldSize=100000)
param <- ScanBamParam(what=c("seq", "mapq"))
gal <- readGappedAlignments(sampler, param=param)
table(qwidth(gal))     # width of reads: all 72mers
range(width(gal))      # width of _alignments_
table(mcols(gal)$mapq)


###################################################
### code chunk number 16: plot-reads
###################################################
seq <- mcols(gal)$seq
idx <- as.logical(strand(gal) == "-")
seq[idx] <- reverseComplement(seq[idx])
abc <- ShortRead::alphabetByCycle(seq)[1:4,]
matplot(t(abc), type="l", lty=1, lwd=2)
legend("topright", legend=rownames(abc)[1:4], col=1:4, lty=1, lwd=2)


###################################################
### code chunk number 17: txdb
###################################################
library(TxDb.Hsapiens.UCSC.hg19.knownGene)


###################################################
### code chunk number 18: exByGn
###################################################
exByGn <- exonsBy(TxDb.Hsapiens.UCSC.hg19.knownGene, "gene")
seqlevels(exByGn, force=TRUE) <- "chr14"


###################################################
### code chunk number 19: sort
###################################################
destdir <- "~/SequenceData/RNAseqData.HeLa.bam.chr14"
if (!file.exists(destdir)) {
    dir.create(destdir)
    dest0 <- sub(".bam$", "_sorted", basename(fls))
    dest <- mcmapply(sortBam, fls, file.path(destdir, dest0),
                 MoreArgs=list(byQname=TRUE))
} else {
    dest <- dir(destdir, "bam$", full=TRUE)
    names(dest) <- sub("_.*", "", basename(dest))
}
sorted <- BamFileList(dest, index=character(), obeyQname=TRUE, 
                      yieldSize=500000)


###################################################
### code chunk number 20: counting-param
###################################################
flag <- scanBamFlag(isNotPrimaryRead=FALSE, isProperPair=TRUE)
param <- ScanBamParam(flag=flag)


###################################################
### code chunk number 21: summarizeOverlaps
###################################################
gnCnt <- summarizeOverlaps(exByGn, sorted, "Union", TRUE,
    singleEnd=FALSE, param=param)


###################################################
### code chunk number 22: se-explore
###################################################
dim(gnCnt)
hist(asinh(assay(gnCnt)))
table(rowSums(assay(gnCnt)) == 0)
rowData(gnCnt)[[1]]      # model on which first gene counts were based


###################################################
### code chunk number 23: se-phenoData
###################################################
o <- match(pdata[["Comment.ENA_RUN."]], colnames(gnCnt))
colData <- cbind(colData(gnCnt), as(pdata[o,], "DataFrame"))
rownames(colData) <- rownames(colData(gnCnt))
colData(gnCnt) <- colData


###################################################
### code chunk number 24: se-subset
###################################################
colData(gnCnt)$Factor.Value.RNAI.
gnCnt[, colData(gnCnt)$Factor.Value.RNAI. == "control"]


###################################################
### code chunk number 25: se-counts
###################################################
colSums(assay(gnCnt))


###################################################
### code chunk number 26: se-count-distr
###################################################
library(lattice)
scales <- list(x=list(rot=45))
o <- tail(order(rowMeans(assay(gnCnt))), 30)
levelplot(asinh(t(assay(gnCnt)[o,])), scales=scales)


###################################################
### code chunk number 27: se-dist
###################################################
d <- dist(t(assay(gnCnt)))
levelplot(as.matrix(d), scales=scales)


###################################################
### code chunk number 28: se-downstream (eval = FALSE)
###################################################
## f <- colData(gnCnt)$Factor.Value.RNAI. 
## library(DESeq)
## deseq <- newCountDataSet(assays(se)$counts, f)
## 
## library(edgeR)
## edger <- DGEList(assays(se)$counts, group=f)


###################################################
### code chunk number 29: gene-ranges
###################################################
gn <- unlist(range(exByGn))


###################################################
### code chunk number 30: exByTx
###################################################
exByTx <- exonsBy(TxDb.Hsapiens.UCSC.hg19.knownGene, "tx")
seqlevels(exByTx, force=TRUE) <- "chr14"


###################################################
### code chunk number 31: AnnotationHub
###################################################
library(AnnotationHub)
hub <- AnnotationHub()
hub


###################################################
### code chunk number 32: hub-tab (eval = FALSE)
###################################################
## anno <- hub$ensembl.release.70.gtf.


###################################################
### code chunk number 33: ah-metadata
###################################################
m <- metadata(hub)
names(m)


### R code from vignette source 'A22_StatisticalConsiderations.Rnw'

### R code from vignette source 'A23_DESeqWorkflow.Rnw'

### R code from vignette source 'A24_AdditionalWorkflows.Rnw'

### R code from vignette source 'A31_VariantData.Rnw'

###################################################
### code chunk number 1: LungCancerLines
###################################################
library(LungCancerLines)
library(ShortRead)
FastqFileList(LungCancerFastqFiles())
LungCancerBamFiles()


###################################################
### code chunk number 2: LungCancerLines-vignette (eval = FALSE)
###################################################
## vignette(package="VariantTools", "VariantTools")


### R code from vignette source 'A33_VariantToolsWorkflow.Rnw'

### R code from vignette source 'A34_VariantAnnotationWorkflow.Rnw'

###################################################
### code chunk number 1: setup
###################################################
options(digits=2)
library(SequenceAnalysisData)
library(VariantAnnotation)
library(ggplot2)
library(SNPlocs.Hsapiens.dbSNP.20101109)
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
library(BSgenome.Hsapiens.UCSC.hg19)
library(SIFT.Hsapiens.dbSNP132)


###################################################
### code chunk number 2: readVcf
###################################################
library(VariantAnnotation)
fl <- system.file("extdata", "chr22.vcf.gz", package="VariantAnnotation")
(hdr <- scanVcfHeader(fl))
info(hdr)[c("VT", "RSQ"),]


###################################################
### code chunk number 3: readVcf
###################################################
(vcf <- readVcf(fl, "hg19"))
head(rowData(vcf), 3)


###################################################
### code chunk number 4: renameSeqlevels
###################################################
seqlevels(vcf, force=TRUE)  <- c("22"="ch22")


###################################################
### code chunk number 5: isInDbSNP
###################################################
.isInDbSNP <- 
    function(vcf, seqname, rsid=TRUE) 
{
    snpLocs <- getSNPlocs(seqname)
    idx <-                    # correct seqname, width of variant == 1
        ((seqnames(vcf) == seqname) & (width(rowData(vcf)) == 1L))
    idx <- as.vector(idx)
    snps <- rowData(vcf)[idx]
    result <- rep(NA, nrow(vcf))
    result[idx] <- if (rsid) {
        sub("rs", "", names(snps)) %in% snpLocs[["RefSNP_id"]]
    } else {
        start(snps) %in% snpLocs[["loc"]]
    }
    result
}


###################################################
### code chunk number 6: dbSNP (eval = FALSE)
###################################################
## library(SNPlocs.Hsapiens.dbSNP.20101109)
## inDbSNP <- .isInDbSNP(vcf, "ch22")
## table(inDbSNP)


###################################################
### code chunk number 7: SNP-quality (eval = FALSE)
###################################################
## metrics <-
##     data.frame(inDbSNP=inDbSNP, RSQ=info(vcf)$RSQ)


###################################################
### code chunk number 8: RSQ-plot (eval = FALSE)
###################################################
## library(ggplot2)
## ggplot(metrics, aes(RSQ, fill=inDbSNP)) +
##     geom_density(alpha=0.5) +
##     scale_x_continuous(name="MaCH / Thunder Imputation Quality") +
##     scale_y_continuous(name="Density") +
##     theme(legend.position="top")


###################################################
### code chunk number 9: seqlevels_rename
###################################################
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene 

fl <- system.file("extdata", "chr22.vcf.gz", package="VariantAnnotation")
vcf <- readVcf(fl, "hg19")
seqlevels(vcf, force=TRUE) <- c("22"="chr22")


###################################################
### code chunk number 10: locateVariants_find
###################################################
rd <- rowData(vcf)
loc <- locateVariants(rd, txdb, CodingVariants())
head(loc, 3)


###################################################
### code chunk number 11: locateVariants_example
###################################################
## Did any coding variants match more than one gene?
splt <- split(loc$GENEID, loc$QUERYID)
table(sapply(splt, function(x) length(unique(x)) > 1))

## Summarize the number of coding variants by gene ID
splt <- split(loc$QUERYID, loc$GENEID)
head(sapply(splt, function(x) length(unique(x))), 3)


###################################################
### code chunk number 12: predictCoding
###################################################
library(BSgenome.Hsapiens.UCSC.hg19)
coding <- predictCoding(vcf, txdb, seqSource=Hsapiens)
coding[5:9]


###################################################
### code chunk number 13: predictCoding_frameshift
###################################################
coding[coding$CONSEQUENCE == "frameshift"]


###################################################
### code chunk number 14: nonsynonymous
###################################################
nms <- names(coding)
idx <- coding$CONSEQUENCE == "nonsynonymous"
nonsyn <- coding[idx]
names(nonsyn) <- nms[idx]
rsids <- unique(names(nonsyn)[grep("rs", names(nonsyn), fixed=TRUE)])


###################################################
### code chunk number 15: sift
###################################################
library(SIFT.Hsapiens.dbSNP132)

## rsids in the package 
head(keys(SIFT.Hsapiens.dbSNP132), 3)
## list available columns
cols(SIFT.Hsapiens.dbSNP132)
## select a subset of columns
## a warning is thrown when a key is not found in the database
subst <- c("RSID", "PREDICTION", "SCORE", "AACHANGE", "PROTEINID")
sift <- select(SIFT.Hsapiens.dbSNP132, keys=rsids, cols=subst)
head(sift, 3)


###################################################
### code chunk number 16: ensemblVEP
###################################################
library(ensemblVEP)


###################################################
### code chunk number 17: vcf-AMI (eval = FALSE)
###################################################
## vcfDir <- "~/Seattle-May-2013/CompleteGenomics"
## dir(vcfDir)
## vcfFile <- dir(vcfDir, ".gz$", full=TRUE)


###################################################
### code chunk number 18: A34_VariantAnnotationWorkflow.Rnw:399-400 (eval = FALSE)
###################################################
## hdr <- scanVcfHeader(vcfFile)


###################################################
### code chunk number 19: A34_VariantAnnotationWorkflow.Rnw:406-408 (eval = FALSE)
###################################################
## info(hdr)
## geno(hdr)


###################################################
### code chunk number 20: chunks (eval = FALSE)
###################################################
## tbx <- TabixFile(vcfFile, yieldSize=100000)
## open(tbx)
## while (len <- nrow(readVcf(tbx, "hg19")))
##     cat("read", len, "rows\n")


###################################################
### code chunk number 21: germline-1 (eval = FALSE)
###################################################
## grepl("Germline", x, fixed=TRUE)


###################################################
### code chunk number 22: FitlerRules
###################################################
isGermline <- function(x)
    grepl("Germline", x, fixed=TRUE)
filters <- FilterRules(list(isGermline=isGermline))


###################################################
### code chunk number 23: prefilter (eval = FALSE)
###################################################
## destination <- tempfile()               # temporary location
## filterVcf(vcfFile, "hg19", destination, prefilters=filters)


###################################################
### code chunk number 24: allelicDepth
###################################################
allelicDepth <- function(x) 
{
    ##  ratio of AD of the 'alternate allele' for the tumor sample
    ##  OR 'reference allele' for normal samples to total reads for
    ##  the sample should be greater than some threshold (say 0.1,
    ##  that is: at least 10% of the sample should have the allele
    ##  of interest)
    ad <- geno(x)[["AD"]]
    tumorPct <- ad[,1,2,drop=FALSE] / rowSums(ad[,1,,drop=FALSE])
    normPct <- ad[,2,1, drop=FALSE] / rowSums(ad[,2,,drop=FALSE])
    test <- (tumorPct > 0.1) | (normPct > 0.1)
    !is.na(test) & test
}


###################################################
### code chunk number 25: filters
###################################################
filters <- FilterRules(list(isGermline=isGermline, 
                            allelicDepth=allelicDepth))


###################################################
### code chunk number 26: filterVcf (eval = FALSE)
###################################################
## destination <- tempfile()
## filterVcf(vcfFile, "hg19", destination, prefilters=filters[1],
##           filters=filters[2])


###################################################
### code chunk number 27: filterVariant-final (eval = FALSE)
###################################################
## vcf <- readVcf(destination, "hg19")
## all(info(vcf)$SS == "Germline")
## table(allelicDepth(vcf))


### R code from vignette source 'A41_GeneAnnotations.Rnw'

###################################################
### code chunk number 1: setup
###################################################
options(digits=2)
library(SequenceAnalysisData)
library(org.Dm.eg.db)


###################################################
### code chunk number 2: select
###################################################
library(org.Dm.eg.db)
cols(org.Dm.eg.db)
keytypes(org.Dm.eg.db)
uniprotKeys <- head(keys(org.Dm.eg.db, keytype="UNIPROT"))
cols <- c("SYMBOL", "PATH")
select(org.Dm.eg.db, keys=uniprotKeys, cols=cols, keytype="UNIPROT")


###################################################
### code chunk number 3: select-kegg
###################################################
kegg <- select(org.Dm.eg.db, "00310", c("UNIPROT", "SYMBOL"), "PATH")
nrow(kegg)
head(kegg, 3)


###################################################
### code chunk number 4: tt-to-eg
###################################################
library(SequenceAnalysisData)
library(edgeR)
library(org.Dm.eg.db)
data(lrTest)
tt <- as.data.frame(topTags(lrTest))


###################################################
### code chunk number 5: tt-to-eg2
###################################################
fbids <- rownames(tt)
cols <- c("ENTREZID", "SYMBOL")
anno <- select(org.Dm.eg.db, fbids, cols, "FLYBASE")
ttanno <- merge(tt, anno, by.x=0, by.y="FLYBASE")
dim(ttanno)
head(ttanno, 3)


###################################################
### code chunk number 6: biomaRt1 (eval = FALSE)
###################################################
## library(biomaRt)
## head(listMarts(), 3)                      ## list the marts
## head(listDatasets(useMart("ensembl")), 3) ## mart datasets
## ensembl <-                                ## fully specified mart
##     useMart("ensembl", dataset = "hsapiens_gene_ensembl")
## 
## head(listFilters(ensembl), 3)             ## filters
## myFilter <- "chromosome_name"
## head(filterOptions(myFilter, ensembl), 3) ## return values
## myValues <- c("21", "22")
## head(listAttributes(ensembl), 3)          ## attributes
## myAttributes <- c("ensembl_gene_id","chromosome_name")
## 
## ## assemble and query the mart
## res <- getBM(attributes =  myAttributes, filters =  myFilter,
##              values =  myValues, mart = ensembl)


### R code from vignette source 'A42_GenomicAnnotations.Rnw'

###################################################
### code chunk number 1: txdb
###################################################
library(TxDb.Dmelanogaster.UCSC.dm3.ensGene)
txdb <- TxDb.Dmelanogaster.UCSC.dm3.ensGene


###################################################
### code chunk number 2: de
###################################################
library(SequenceAnalysisData)
data(lrTest)
fbids <- rownames(topTags(lrTest))


###################################################
### code chunk number 3: flybase-tx-gn
###################################################
txnm <- select(txdb, fbids, "TXNAME", "GENEID")
nrow(txnm)
head(txnm, 3)


###################################################
### code chunk number 4: cdsBy
###################################################
cds <- cdsBy(txdb, "tx", use.names=TRUE)[txnm$TXNAME]
length(cds)
cds[1]


###################################################
### code chunk number 5: BSgenome
###################################################
library(BSgenome.Dmelanogaster.UCSC.dm3)
txx <- extractTranscriptsFromGenome(Dmelanogaster, cds)
length(txx)
head(txx, 3)
head(translate(txx), 3)


###################################################
### code chunk number 6: makeTranscriptDbFromUCSC-discover
###################################################
library(rtracklayer)
library(GenomicFeatures)

## genomes
gnms <- ucscGenomes()
nrow(gnms)
gnms[grep("elegans", gnms$species),]

## tables
tbls <- supportedUCSCtables()
nrow(tbls)
head(tbls)


###################################################
### code chunk number 7: makeTranscriptDbFromUCSC-make (eval = FALSE)
###################################################
## ## Not run
## txdb <- makeTranscriptDbFromUCSC("ce10", "refGene")
## saveDb(txdb, file="/path/to/file.sqlite")


### R code from vignette source 'A43_Visualization.Rnw'

###################################################
### code chunk number 1: Gviz
###################################################
library(Gviz)
data(cpgIslands)
chr <- "chr7"
genome <- "hg19"


###################################################
### code chunk number 2: AnnotationTrack
###################################################
atrack <- AnnotationTrack(cpgIslands, name="CpG")
plotTracks(atrack)


###################################################
### code chunk number 3: GenomeAxisTrack
###################################################
gtrack <- GenomeAxisTrack()
plotTracks(list(gtrack, atrack))


###################################################
### code chunk number 4: IdeogramTrack
###################################################
itrack <- IdeogramTrack(genome=genome, chromosome=chr)
plotTracks(list(itrack, gtrack, atrack))


###################################################
### code chunk number 5: GeneRegionTrack
###################################################
data(geneModels)
grtrack <- 
    GeneRegionTrack(geneModels, genome=genome,
                    chromosome=chr, name="Gene Model")
tracks <- list(itrack, gtrack, atrack, grtrack)
plotTracks(tracks)


###################################################
### code chunk number 6: Gviz-zoom
###################################################
plotTracks(tracks, from=2.5e7, to=2.8e7)


###################################################
### code chunk number 7: Gviz-sequence
###################################################
library(BSgenome.Hsapiens.UCSC.hg19)
strack <- SequenceTrack(Hsapiens, chromosome=chr)
plotTracks(c(tracks, strack), from=26450430, to=26450490, cex=.8)


###################################################
### code chunk number 8: Gviz-data
###################################################
## some data
lim <- c(26700000, 26900000)
coords <- seq(lim[1], lim[2], 101)
dat <- runif(length(coords) - 1, min=-10, max=10)

## DataTrack
dtrack <- 
    DataTrack(data=dat, start=coords[-length(coords)],
              end= coords[-1], chromosome=chr, genome=genome,
              name="Uniform Random")
plotTracks(c(tracks, dtrack))


###################################################
### code chunk number 9: Gviz-figure
###################################################
pdf("GvizFigure.pdf")
plotTracks(c(tracks, dtrack))
xx <- capture.output(dev.off())


###################################################
### code chunk number 10: shiny-demo-1 (eval = FALSE)
###################################################
## source("http://bioconductor.org/scratch-repos/pkgInstall.R")
## demo1()


###################################################
### code chunk number 11: shiny-demo-2 (eval = FALSE)
###################################################
## demo2()


### R code from vignette source 'A44_Future.Rnw'

