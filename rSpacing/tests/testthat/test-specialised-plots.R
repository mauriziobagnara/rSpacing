make_specialised_result <- function(id = "baseline", multiplier = 1) {
  dates <- seq.Date(as.Date("2020-01-01"), by="day", length.out=6)
  daily <- data.frame(
    Date=dates, Year=2020L, DOY=1:6,
    Irrig_mm=c(0,20,0,0,15,0)*multiplier,
    FertiliserN_kgNha=c(40,0,0,80,0,0)*multiplier,
    SoilCtot_kgCha=(100000+c(0,2,4,5,8,10))*multiplier,
    `SWmm(1)`=c(40,42,41,39,43,42), `SWmm(2)`=c(60,61,60,59,62,61),
    `SoilC_kgCha(1)`=c(60000,60001,60002,60003,60004,60005),
    `SoilC_kgCha(2)`=c(40000,40001,40002,40002,40004,40005),
    check.names=FALSE
  )
  crop <- data.frame(
    Year=2020L, Crop=c("Wheat","Maize","Soybean"),
    Cultivar=c("Hartog","B_110","Generic_MG1"),
    Yield_kgha=c(6000,9000,3200)*multiplier
  )
  report <- crop
  structure(list(
    run=structure(list(run_id=id,request=list(scenario_id=id)),class="spacing_run"),
    datastore=list(), csv=list(crop_summary_test.csv=crop,daily_test.csv=daily,report_test.csv=report),
    apsimx=character(),weather=NULL,messages=list(),units=list()
  ),class="spacing_result")
}

test_that("specialised plots return ggplot objects", {
  x <- make_specialised_result()
  expect_s3_class(plot_spacing_yield(x),"ggplot")
  expect_s3_class(plot_spacing_irrigation(x),"ggplot")
  expect_s3_class(plot_spacing_fertilisation(x),"ggplot")
  expect_s3_class(plot_spacing_soil_carbon(x,"change"),"ggplot")
  expect_s3_class(plot_spacing_soil_carbon(x,"profile",thickness=c(150,150)),"ggplot")
})

test_that("atlas returns named plots and comparison combines runs", {
  x <- make_specialised_result("base",1)
  atlas <- plot_spacing_atlas(x,sources=c("daily","soil"),max_plots=3)
  expect_s3_class(atlas,"spacing_plot_atlas")
  expect_true(length(atlas)>0)
  comparison <- plot_spacing_comparison(list(x,make_specialised_result("high",1.1)),variable="Yield")
  expect_s3_class(comparison,"ggplot")
  expect_equal(length(unique(attr(comparison,"data")$scenario_id)),2L)
})
