
outputdir <- "output/cces_processed"
# Save all datasets as .rds files 
saveRDS(cces2006, file.path(args$outdir, "cces", "cces2006.rds"))
saveRDS(cces2008, file.path(args$outdir, "cces", "cces2008.rds"))
saveRDS(cces2010, file.path(args$outdir, "cces", "cces2010.rds"))
saveRDS(cces2012, file.path(args$outdir, "cces", "cces2012.rds"))
saveRDS(cces2014, file.path(args$outdir, "cces", "cces2014.rds"))
saveRDS(cces2016, file.path(args$outdir, "cces", "cces2016.rds"))