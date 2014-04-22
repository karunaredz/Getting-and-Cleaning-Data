### Assume that the following zip### 
### https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
### is unzipped in the work directory
require(plyr)
library('plyr') # faster with data.table...

gather_data <- function(test_or_train = 'test'){
    ### Merge X and Y from folder_path
    ### Argument is test or train
    ### The purpose of this function is to avoid repetition in the code
    tt <- test_or_train
    folder_name <- 'UCI HAR Dataset/'
    X_path <- paste0(folder_name, tt, '/X_', tt, '.txt')
    y_path <- paste0(folder_name, tt, '/y_', tt, '.txt')
    subject_path <-paste0(folder_name, tt, '/subject_', tt, '.txt')

    X <- read.table(X_path, header = F)
    y <- read.table(y_path, header = F,  sep = ' ')

    subject <- read.table(subject_path, header = F,  sep=' ')
    colnames(subject) <- 'subject'

    x_colnames <-
        read.csv2(paste0(folder_name, 'features.txt'), sep=' ', header = F)

    colnames(X) <- x_colnames[, 2]
    colnames(y) <- 'y'
    
    return(data.frame(y = y, subject = subject, X))
}

### 1)
test_set <- gather_data('test')
train_set <- gather_data('train')
dat <- rbind(test_set, train_set)
dim(dat)
head(dat)

### 2)
dat_col <- colnames(dat)
idx_mean <- grep('mean[^F]|std', dat_col, ignore.case = T)
dat2 <- dat[, c(1, 2, idx_mean[1:66])]
### ignore the angle features
### colnames(dat2)

### 5) 

dat3 <- ddply(dat2, .(subject, y), function(x){
    unlist(lapply(x, mean, na.rm = T))
    })

### 3) and 4) # Easier to make this here because of ddply that does not like factors
### Transform y label to something humanly readable
activity <- read.csv('UCI HAR Dataset/activity_labels.txt',
                     sep = ' ', header = F)
dat3$y <- activity[dat3$y, 2]
colnames(dat3)[1] <- 'activity'
write.csv(dat3, file = 'tidy_SamsungData.csv')
