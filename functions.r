library(ggplot2)

readData = function(filepath = "~/Dropbox/mcintyreMigrationProject/data/compiled data.csv") {
    return(read.csv(filepath))
}

freqOverTimePlot_bySpecies = function(df, colorvar, sizevar)
    uniqueSpeciesCodes = unique(df$SPECIES)
    df$ASSESSMENT_DATE = strptime(df$ASSESSMENT_DATE, format="%m/%d/%Y")
    for (uniqueSpecies in uniqueSpeciesCodes) {
        df_sp = df[which(df$SPECIES == uniqueSpecies),]
        d = ggplot(df_sp, aes(ASSESSMENT_DATE$yday, log(TotalFrequency))) + 
            xlab("Day of Year") + 
            ylab("Ln(Frequency)") + 
            ggtitle(df_sp[1,"SPECIES_NAME"]) +
            geom_hex() +
            scale_fill_gradientn(colours=c("white","blue"), name = "Frequency") +
            stat_smooth()
        ggsave(plot=d,filename="~/Dropbox/mcintyreMigrationProject/plots/test.pdf",height=6,width=8)
    }
}
    
    