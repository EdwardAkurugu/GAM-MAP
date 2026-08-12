
#=================================================================================================
# GENERALISED ADDITIVE MODEL (GAM) 
#=================================================================================================

# ---------- Upper East (UE) ----------
model_UE_1 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 13, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Upper East"),
                  family = "nb", method = "REML")

model_UE_2 <- gam(uncom ~ offset(log_pop_offset) +
                    s(months, k = 12, bs = "cc") +
                    s(rainfall, k = 22, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, k = c(4, 12), bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Upper East"),
                  family = "nb", method = "REML")

model_UE_3 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 22, bs = "cr") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, k = c(4, 12), bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Upper East"),
                  family = "nb", method = "REML")

model_UE_4 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 11, bs = "cr") +
                    s(months, bs = "cc") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Upper East"),
                  family = "nb", method = "REML")

model_UE_5 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 11, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Upper East"),
                  family = "nb", method = "REML")

model_UE_6 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 25, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Upper East"),
                  family = "nb", method = "REML")

model_UE_7 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 11, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")),
                  data = subset(data, region == "Upper East"),
                  family = "nb", method = "REML")

model_UE_8 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 27, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr"),
                  data = subset(data, region == "Upper East"),
                  family = "nb", method = "REML")

saveRDS(model_UE_1, file = "model_UE_1.rds")
saveRDS(model_UE_2, file = "model_UE_2.rds")
saveRDS(model_UE_3, file = "model_UE_3.rds")
saveRDS(model_UE_4, file = "model_UE_4.rds")
saveRDS(model_UE_5, file = "model_UE_5.rds")
saveRDS(model_UE_6, file = "model_UE_6.rds")
saveRDS(model_UE_7, file = "model_UE_7.rds")
saveRDS(model_UE_8, file = "model_UE_8.rds")

model_UE_1 <- readRDS("model_UE_1.rds")
model_UE_2 <- readRDS("model_UE_2.rds")
model_UE_3 <- readRDS("model_UE_3.rds")
model_UE_4 <- readRDS("model_UE_4.rds")
model_UE_5 <- readRDS("model_UE_5.rds")
model_UE_6 <- readRDS("model_UE_6.rds")
model_UE_7 <- readRDS("model_UE_7.rds")
model_UE_8 <- readRDS("model_UE_8.rds")


# ---------- Upper West (UW) ----------
model_UW_1 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 34, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Upper West"),
                  family = "nb", method = "REML")

model_UW_2 <- gam(uncom ~ offset(log_pop_offset) +
                    s(months, k = 12, bs = "cc") +
                    s(rainfall, k = 16, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, k = c(16, 12), bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Upper West"),
                  family = "nb", method = "REML")

model_UW_3 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 16, bs = "cr") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, k = c(16, 12), bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Upper West"),
                  family = "nb", method = "REML")

model_UW_4 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 34, bs = "cr") +
                    s(months, bs = "cc") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Upper West"),
                  family = "nb", method = "REML")

model_UW_5 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 33, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Upper West"),
                  family = "nb", method = "REML")

model_UW_6 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 33, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Upper West"),
                  family = "nb", method = "REML")

model_UW_7 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 34, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")),
                  data = subset(data, region == "Upper West"),
                  family = "nb", method = "REML")

model_UW_8 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 33, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr"),
                  data = subset(data, region == "Upper West"),
                  family = "nb", method = "REML")

saveRDS(model_UW_1, file = "model_UW_1.rds")
saveRDS(model_UW_2, file = "model_UW_2.rds")
saveRDS(model_UW_3, file = "model_UW_3.rds")
saveRDS(model_UW_4, file = "model_UW_4.rds")
saveRDS(model_UW_5, file = "model_UW_5.rds")
saveRDS(model_UW_6, file = "model_UW_6.rds")
saveRDS(model_UW_7, file = "model_UW_7.rds")
saveRDS(model_UW_8, file = "model_UW_8.rds")

model_UW_1 <- readRDS("model_UW_1.rds")
model_UW_2 <- readRDS("model_UW_2.rds")
model_UW_3 <- readRDS("model_UW_3.rds")
model_UW_4 <- readRDS("model_UW_4.rds")
model_UW_5 <- readRDS("model_UW_5.rds")
model_UW_6 <- readRDS("model_UW_6.rds")
model_UW_7 <- readRDS("model_UW_7.rds")
model_UW_8 <- readRDS("model_UW_8.rds")


# ---------- Northern (NO) ----------
model_NO_1 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 16, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Northern"),
                  family = "nb", method = "REML")

model_NO_2 <- gam(uncom ~ offset(log_pop_offset) +
                    s(months, k = 12, bs = "cc") +
                    s(rainfall, k = 16, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, k = c(16, 12), bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Northern"),
                  family = "nb", method = "REML")

model_NO_3 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 16, bs = "cr") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, k = c(16, 12), bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Northern"),
                  family = "nb", method = "REML")

model_NO_4 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 16, bs = "cr") +
                    s(months, bs = "cc") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Northern"),
                  family = "nb", method = "REML")

model_NO_5 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 16, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Northern"),
                  family = "nb", method = "REML")

model_NO_6 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 25, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Northern"),
                  family = "nb", method = "REML")

model_NO_7 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 16, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")),
                  data = subset(data, region == "Northern"),
                  family = "nb", method = "REML")

model_NO_8 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 25, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr"),
                  data = subset(data, region == "Northern"),
                  family = "nb", method = "REML")

saveRDS(model_NO_1, file = "model_NO_1.rds")
saveRDS(model_NO_2, file = "model_NO_2.rds")
saveRDS(model_NO_3, file = "model_NO_3.rds")
saveRDS(model_NO_4, file = "model_NO_4.rds")
saveRDS(model_NO_5, file = "model_NO_5.rds")
saveRDS(model_NO_6, file = "model_NO_6.rds")
saveRDS(model_NO_7, file = "model_NO_7.rds")
saveRDS(model_NO_8, file = "model_NO_8.rds")

model_NO_1 <- readRDS("model_NO_1.rds")
model_NO_2 <- readRDS("model_NO_2.rds")
model_NO_3 <- readRDS("model_NO_3.rds")
model_NO_4 <- readRDS("model_NO_4.rds")
model_NO_5 <- readRDS("model_NO_5.rds")
model_NO_6 <- readRDS("model_NO_6.rds")
model_NO_7 <- readRDS("model_NO_7.rds")
model_NO_8 <- readRDS("model_NO_8.rds")


# ---------- Brong Ahafo (BA) ----------
model_BA_1 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 17, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Brong Ahafo"),
                  family = "nb", method = "REML")

model_BA_2 <- gam(uncom ~ offset(log_pop_offset) +
                    s(months, k = 12, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, k = c(16, 12), bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Brong Ahafo"),
                  family = "nb", method = "REML")

model_BA_3 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, bs = "cr") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, k = c(12, 12), bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Brong Ahafo"),
                  family = "nb", method = "REML")

model_BA_4 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 17, bs = "cr") +
                    s(months, bs = "cc") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Brong Ahafo"),
                  family = "nb", method = "REML")

model_BA_5 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 17, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Brong Ahafo"),
                  family = "nb", method = "REML")

model_BA_6 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 23, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Brong Ahafo"),
                  family = "nb", method = "REML")

model_BA_7 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 17, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")),
                  data = subset(data, region == "Brong Ahafo"),
                  family = "nb", method = "REML")

model_BA_8 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 23, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr"),
                  data = subset(data, region == "Brong Ahafo"),
                  family = "nb", method = "REML")

saveRDS(model_BA_1, file = "model_BA_1.rds")
saveRDS(model_BA_2, file = "model_BA_2.rds")
saveRDS(model_BA_3, file = "model_BA_3.rds")
saveRDS(model_BA_4, file = "model_BA_4.rds")
saveRDS(model_BA_5, file = "model_BA_5.rds")
saveRDS(model_BA_6, file = "model_BA_6.rds")
saveRDS(model_BA_7, file = "model_BA_7.rds")
saveRDS(model_BA_8, file = "model_BA_8.rds")

model_BA_1 <- readRDS("model_BA_1.rds")
model_BA_2 <- readRDS("model_BA_2.rds")
model_BA_3 <- readRDS("model_BA_3.rds")
model_BA_4 <- readRDS("model_BA_4.rds")
model_BA_5 <- readRDS("model_BA_5.rds")
model_BA_6 <- readRDS("model_BA_6.rds")
model_BA_7 <- readRDS("model_BA_7.rds")
model_BA_8 <- readRDS("model_BA_8.rds")


# ---------- Ashanti (AS) ----------
model_AS_1 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 18, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Ashanti"),
                  family = "nb", method = "REML")

model_AS_2 <- gam(uncom ~ offset(log_pop_offset) +
                    s(months, k = 12, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, k = c(16, 12), bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Ashanti"),
                  family = "nb", method = "REML")

model_AS_3 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, bs = "cr") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, k = c(22, 12), bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Ashanti"),
                  family = "nb", method = "REML")

model_AS_4 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 18, bs = "cr") +
                    s(months, bs = "cc") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Ashanti"),
                  family = "nb", method = "REML")

model_AS_5 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 17, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Ashanti"),
                  family = "nb", method = "REML")

model_AS_6 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 28, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Ashanti"),
                  family = "nb", method = "REML")

model_AS_7 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 18, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")),
                  data = subset(data, region == "Ashanti"),
                  family = "nb", method = "REML")

model_AS_8 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 28, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr"),
                  data = subset(data, region == "Ashanti"),
                  family = "nb", method = "REML")

saveRDS(model_AS_1, file = "model_AS_1.rds")
saveRDS(model_AS_2, file = "model_AS_2.rds")
saveRDS(model_AS_3, file = "model_AS_3.rds")
saveRDS(model_AS_4, file = "model_AS_4.rds")
saveRDS(model_AS_5, file = "model_AS_5.rds")
saveRDS(model_AS_6, file = "model_AS_6.rds")
saveRDS(model_AS_7, file = "model_AS_7.rds")
saveRDS(model_AS_8, file = "model_AS_8.rds")

model_AS_1 <- readRDS("model_AS_1.rds")
model_AS_2 <- readRDS("model_AS_2.rds")
model_AS_3 <- readRDS("model_AS_3.rds")
model_AS_4 <- readRDS("model_AS_4.rds")
model_AS_5 <- readRDS("model_AS_5.rds")
model_AS_6 <- readRDS("model_AS_6.rds")
model_AS_7 <- readRDS("model_AS_7.rds")
model_AS_8 <- readRDS("model_AS_8.rds")


# ---------- Eastern (EA) ----------
model_EA_1 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 37, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Eastern"),
                  family = "nb", method = "REML")

model_EA_2 <- gam(uncom ~ offset(log_pop_offset) +
                    s(months, k = 12, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, k = c(12, 12), bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Eastern"),
                  family = "nb", method = "REML")

model_EA_3 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 25, bs = "cr") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Eastern"),
                  family = "nb", method = "REML")

model_EA_4 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 25, bs = "cr") +
                    s(months, bs = "cc") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Eastern"),
                  family = "nb", method = "REML")

model_EA_5 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 21, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Eastern"),
                  family = "nb", method = "REML")

model_EA_6 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 28, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Eastern"),
                  family = "nb", method = "REML")

model_EA_7 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 18, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")),
                  data = subset(data, region == "Eastern"),
                  family = "nb", method = "REML")

model_EA_8 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 25, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr"),
                  data = subset(data, region == "Eastern"),
                  family = "nb", method = "REML")

saveRDS(model_EA_1, file = "model_EA_1.rds")
saveRDS(model_EA_2, file = "model_EA_2.rds")
saveRDS(model_EA_3, file = "model_EA_3.rds")
saveRDS(model_EA_4, file = "model_EA_4.rds")
saveRDS(model_EA_5, file = "model_EA_5.rds")
saveRDS(model_EA_6, file = "model_EA_6.rds")
saveRDS(model_EA_7, file = "model_EA_7.rds")
saveRDS(model_EA_8, file = "model_EA_8.rds")

model_EA_1 <- readRDS("model_EA_1.rds")
model_EA_2 <- readRDS("model_EA_2.rds")
model_EA_3 <- readRDS("model_EA_3.rds")
model_EA_4 <- readRDS("model_EA_4.rds")
model_EA_5 <- readRDS("model_EA_5.rds")
model_EA_6 <- readRDS("model_EA_6.rds")
model_EA_7 <- readRDS("model_EA_7.rds")
model_EA_8 <- readRDS("model_EA_8.rds")


# ---------- Volta (VO) ----------
model_VO_1 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 19, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Volta"),
                  family = "nb", method = "REML")

model_VO_2 <- gam(uncom ~ offset(log_pop_offset) +
                    s(months, k = 12, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, k = c(16, 12), bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Volta"),
                  family = "nb", method = "REML")

model_VO_3 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 19, bs = "cr") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, k = c(16, 12), bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Volta"),
                  family = "nb", method = "REML")

model_VO_4 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 21, bs = "cr") +
                    s(months, bs = "cc") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Volta"),
                  family = "nb", method = "REML")

model_VO_5 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 19, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Volta"),
                  family = "nb", method = "REML")

model_VO_6 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 25, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Volta"),
                  family = "nb", method = "REML")

model_VO_7 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 19, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")),
                  data = subset(data, region == "Volta"),
                  family = "nb", method = "REML")

model_VO_8 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 25, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr"),
                  data = subset(data, region == "Volta"),
                  family = "nb", method = "REML")

saveRDS(model_VO_1, file = "model_VO_1.rds")
saveRDS(model_VO_2, file = "model_VO_2.rds")
saveRDS(model_VO_3, file = "model_VO_3.rds")
saveRDS(model_VO_4, file = "model_VO_4.rds")
saveRDS(model_VO_5, file = "model_VO_5.rds")
saveRDS(model_VO_6, file = "model_VO_6.rds")
saveRDS(model_VO_7, file = "model_VO_7.rds")
saveRDS(model_VO_8, file = "model_VO_8.rds")

model_VO_1 <- readRDS("model_VO_1.rds")
model_VO_2 <- readRDS("model_VO_2.rds")
model_VO_3 <- readRDS("model_VO_3.rds")
model_VO_4 <- readRDS("model_VO_4.rds")
model_VO_5 <- readRDS("model_VO_5.rds")
model_VO_6 <- readRDS("model_VO_6.rds")
model_VO_7 <- readRDS("model_VO_7.rds")
model_VO_8 <- readRDS("model_VO_8.rds")


# ---------- Greater Accra (GA) — unchanged from your original (already cr/cc) ----------
model_GA_1 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 18, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Greater Accra"),
                  family = "nb", method = "REML")

model_GA_2 <- gam(uncom ~ offset(log_pop_offset) +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Greater Accra"),
                  family = "nb", method = "REML")

model_GA_3 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 16, bs = "cr") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, k = c(20, 12), bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Greater Accra"),
                  family = "nb", method = "REML")

model_GA_4 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 18, bs = "cr") +
                    s(months, bs = "cc") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Greater Accra"),
                  family = "nb", method = "REML")

model_GA_5 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 18, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Greater Accra"),
                  family = "nb", method = "REML")

model_GA_6 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 28, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Greater Accra"),
                  family = "nb", method = "REML")

model_GA_7 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 18, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")),
                  data = subset(data, region == "Greater Accra"),
                  family = "nb", method = "REML")

model_GA_8 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 28, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr"),
                  data = subset(data, region == "Greater Accra"),
                  family = "nb", method = "REML")

saveRDS(model_GA_1, file = "model_GA_1.rds")
saveRDS(model_GA_2, file = "model_GA_2.rds")
saveRDS(model_GA_3, file = "model_GA_3.rds")
saveRDS(model_GA_4, file = "model_GA_4.rds")
saveRDS(model_GA_5, file = "model_GA_5.rds")
saveRDS(model_GA_6, file = "model_GA_6.rds")
saveRDS(model_GA_7, file = "model_GA_7.rds")
saveRDS(model_GA_8, file = "model_GA_8.rds")

model_GA_1 <- readRDS("model_GA_1.rds")
model_GA_2 <- readRDS("model_GA_2.rds")
model_GA_3 <- readRDS("model_GA_3.rds")
model_GA_4 <- readRDS("model_GA_4.rds")
model_GA_5 <- readRDS("model_GA_5.rds")
model_GA_6 <- readRDS("model_GA_6.rds")
model_GA_7 <- readRDS("model_GA_7.rds")
model_GA_8 <- readRDS("model_GA_8.rds")


# ---------- Central (CE) ----------
model_CE_1 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 18, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Central"),
                  family = "nb", method = "REML")

model_CE_2 <- gam(uncom ~ offset(log_pop_offset) +
                    s(months, k = 12, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, k = c(16, 12), bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Central"),
                  family = "nb", method = "REML")

model_CE_3 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 18, bs = "cr") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, k = c(12, 12), bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Central"),
                  family = "nb", method = "REML")

model_CE_4 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 18, bs = "cr") +
                    s(months, bs = "cc") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Central"),
                  family = "nb", method = "REML")

model_CE_5 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 18, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Central"),
                  family = "nb", method = "REML")

model_CE_6 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 27, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Central"),
                  family = "nb", method = "REML")

model_CE_7 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 18, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")),
                  data = subset(data, region == "Central"),
                  family = "nb", method = "REML")

model_CE_8 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 27, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr"),
                  data = subset(data, region == "Central"),
                  family = "nb", method = "REML")

saveRDS(model_CE_1, file = "model_CE_1.rds")
saveRDS(model_CE_2, file = "model_CE_2.rds")
saveRDS(model_CE_3, file = "model_CE_3.rds")
saveRDS(model_CE_4, file = "model_CE_4.rds")
saveRDS(model_CE_5, file = "model_CE_5.rds")
saveRDS(model_CE_6, file = "model_CE_6.rds")
saveRDS(model_CE_7, file = "model_CE_7.rds")
saveRDS(model_CE_8, file = "model_CE_8.rds")

model_CE_1 <- readRDS("model_CE_1.rds")
model_CE_2 <- readRDS("model_CE_2.rds")
model_CE_3 <- readRDS("model_CE_3.rds")
model_CE_4 <- readRDS("model_CE_4.rds")
model_CE_5 <- readRDS("model_CE_5.rds")
model_CE_6 <- readRDS("model_CE_6.rds")
model_CE_7 <- readRDS("model_CE_7.rds")
model_CE_8 <- readRDS("model_CE_8.rds")


# ---------- Western (WE) ----------
model_WE_1 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 19, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Western"),
                  family = "nb", method = "REML")

model_WE_2 <- gam(uncom ~ offset(log_pop_offset) +
                    s(months, k = 12, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, k = c(16, 12), bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Western"),
                  family = "nb", method = "REML")

model_WE_3 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 12, bs = "cr") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, k = c(16, 12), bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Western"),
                  family = "nb", method = "REML")

model_WE_4 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 17, bs = "cr") +
                    s(months, bs = "cc") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Western"),
                  family = "nb", method = "REML")

model_WE_5 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 22, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")) +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Western"),
                  family = "nb", method = "REML")

model_WE_6 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 22, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(avgtemp, rainfall, bs = c("cr", "cr")),
                  data = subset(data, region == "Western"),
                  family = "nb", method = "REML")

model_WE_7 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 19, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr") +
                    ti(time, months, bs = c("cr", "cc")),
                  data = subset(data, region == "Western"),
                  family = "nb", method = "REML")

model_WE_8 <- gam(uncom ~ offset(log_pop_offset) +
                    s(time, k = 25, bs = "cr") +
                    s(months, bs = "cc") +
                    s(rainfall, bs = "cr") +
                    s(avgtemp, bs = "cr"),
                  data = subset(data, region == "Western"),
                  family = "nb", method = "REML")

saveRDS(model_WE_1, file = "model_WE_1.rds")
saveRDS(model_WE_2, file = "model_WE_2.rds")
saveRDS(model_WE_3, file = "model_WE_3.rds")
saveRDS(model_WE_4, file = "model_WE_4.rds")
saveRDS(model_WE_5, file = "model_WE_5.rds")
saveRDS(model_WE_6, file = "model_WE_6.rds")
saveRDS(model_WE_7, file = "model_WE_7.rds")
saveRDS(model_WE_8, file = "model_WE_8.rds")

model_WE_1 <- readRDS("model_WE_1.rds")
model_WE_2 <- readRDS("model_WE_2.rds")
model_WE_3 <- readRDS("model_WE_3.rds")
model_WE_4 <- readRDS("model_WE_4.rds")
model_WE_5 <- readRDS("model_WE_5.rds")
model_WE_6 <- readRDS("model_WE_6.rds")
model_WE_7 <- readRDS("model_WE_7.rds")
model_WE_8 <- readRDS("model_WE_8.rds")

