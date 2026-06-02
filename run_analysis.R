# =========================
# STEP 1: READ DATA
# =========================

features <- read.table("features.txt")
activities <- read.table("activity_labels.txt")

x_train <- read.table("train/X_train.txt")
y_train <- read.table("train/y_train.txt")
subject_train <- read.table("train/subject_train.txt")

x_test <- read.table("test/X_test.txt")
y_test <- read.table("test/y_test.txt")
subject_test <- read.table("test/subject_test.txt")

# =========================
# STEP 2: MERGE DATA
# =========================

x <- rbind(x_train, x_test)
y <- rbind(y_train, y_test)
subject <- rbind(subject_train, subject_test)

# =========================
# STEP 3: LABEL COLUMNS
# =========================

colnames(x) <- features[, 2]
colnames(y) <- "activity"
colnames(subject) <- "subject"

# =========================
# STEP 4: EXTRACT MEAN & STD
# =========================

mean_std <- grep("mean\\(\\)|std\\(\\)", features[, 2])
x <- x[, mean_std]

# =========================
# STEP 5: COMBINE DATA
# =========================

data <- cbind(subject, y, x)

# =========================
# STEP 6: ACTIVITY NAMES
# =========================

data$activity <- factor(
  data$activity,
  levels = activities[, 1],
  labels = activities[, 2]
)

# =========================
# STEP 7: CLEAN NAMES
# =========================

names(data) <- gsub("\\()", "", names(data))
names(data) <- gsub("^t", "time", names(data))
names(data) <- gsub("^f", "frequency", names(data))
names(data) <- gsub("Acc", "Accelerometer", names(data))
names(data) <- gsub("Gyro", "Gyroscope", names(data))
names(data) <- gsub("Mag", "Magnitude", names(data))
names(data) <- gsub("BodyBody", "Body", names(data))

# =========================
# STEP 8: TIDY DATA (AVERAGE)
# =========================

tidy_data <- aggregate(. ~ subject + activity, data = data, FUN = mean)

# =========================
# STEP 9: WRITE OUTPUT
# =========================

write.table(tidy_data, "tidy_data.txt", row.name = FALSE)

print("DONE: tidy_data.txt created")