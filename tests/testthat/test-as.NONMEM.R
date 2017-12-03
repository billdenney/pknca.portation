context("as.NONMEM testing")

test_that("get_NONMEM_name_map", {
  expect_equal(
    get_NONMEM_name_map(
      data.frame(A=structure(1, NONMEM_column="ID"),
                 B=structure(2, NONMEM_column="ADDL"),
                 C=3)),
    c(A="ID", B="ADDL"),
    info="get_NONMEM_name_map extracts the NONMEM_column values")
  expect_equal(
    get_NONMEM_name_map(
      data.frame(A=1,
                 B=2,
                 C=3)),
    c(),
    info="get_NONMEM_name_map extracts the NONMEM_column values (empty)")
})

test_that("set_NONMEM_name_map", {
  expect_equal(
    set_NONMEM_name_map(
      object=data.frame(A=1, B=2, C=3),
      name_map=c(A="ID", B="ADDL"),
      reset=TRUE),
    data.frame(A=structure(1, NONMEM_column="ID"),
               B=structure(2, NONMEM_column="ADDL"),
               C=3),
    info="Setting columns works")
  expect_error(
    set_NONMEM_name_map(
      object=data.frame(A=1, B=2, C=3),
      name_map=c(A="ID", D="ADDL"),
      reset=TRUE),
    regex="All argument names must refer to a column of .data",
    fixed=TRUE,
    info="Setting missing columns fails")
  expect_error(
    set_NONMEM_name_map(
      object=data.frame(A=1, B=2, C=3),
      name_map=c(A="ID", B="ID"),
      reset=TRUE),
    regex="Identical arguments will be set for A and B.",
    fixed=TRUE,
    info="Repeating column definitions fails (newly set)")
  expect_error(
    set_NONMEM_name_map(
      object=data.frame(A=structure(1, NONMEM_column="ID"),
                        B=structure(2, NONMEM_column="ID"),
                        C=3),
      name_map=c(C="ADDL"),
      reset=FALSE),
    regex="Identical arguments will be set for A and B.",
    fixed=TRUE,
    info="Reset works (not resetting)")
  expect_equal(
    set_NONMEM_name_map(
      object=data.frame(A=structure(1, NONMEM_column="ID"),
                        B=structure(2, NONMEM_column="ID"),
                        C=3),
      name_map=c(C="ADDL"),
      reset=TRUE),
    data.frame(A=1,
               B=2,
               C=structure(3, NONMEM_column="ADDL")),
    info="Reset works (resetting)")
  expect_error(
    set_NONMEM_name_map(
      object=data.frame(A=structure(1, NONMEM_column="ID"),
                        B=structure(2, NONMEM_column="ID"),
                        C=3),
      name_map=c(C="ADDL"),
      reset=FALSE),
    regex="Identical arguments will be set for A and B.",
    fixed=TRUE,
    info="Repeating column definitions fails (already set)")
  expect_error(
    set_NONMEM_name_map(
      object=data.frame(A=structure(1, NONMEM_column="ID"),
                        B=structure(2, NONMEM_column="ID"),
                        C=3),
      reset=FALSE),
    regex='argument "name_map" is missing, with no default',
    fixed=TRUE,
    info="name_map is required")
  expect_equal(
    set_NONMEM_name_map(
      object=data.frame(A=structure(1, NONMEM_column="ID"),
                        B=structure(2, NONMEM_column="ID"),
                        C=3),
      name_map=c(),
      reset=TRUE),
    data.frame(A=1,
               B=2,
               C=3),
    info="name_map empty allows reset")
})

context("as.NONMEMdata")

test_that("as.NONMEMdata.NULL", {
  expect_null(as.NONMEMdata.NULL(NULL),
              info="as.NONMEMdata.NULL returns NULL")
  expect_null(as.NONMEMdata.NULL(data.frame(A=1)),
              info="as.NONMEMdata.NULL returns NULL no matter what")
})

test_that("as.NONMEMdata.PKNCAconc", {
  my_conc <- PKNCAconc(CONC~TIME|SUBJECT,
                       data=data.frame(CONC=1,
                                       TIME=0,
                                       SUBJECT="A",
                                       stringsAsFactors=FALSE))
  expect_error(as.NONMEMdata.PKNCAconc(my_conc, conc_cmt_map=data.frame()),
               regex="conc_cmt_map must have a column named 'CMT'",
               fixed=TRUE,
               info="conc_cmt_map must be well formed")
  expect_error(as.NONMEMdata.PKNCAconc(my_conc, conc_cmt_map=data.frame(CMT=NA)),
               regex="CMT values cannot be NA",
               fixed=TRUE,
               info="conc_cmt_map must be well formed (no NA)")
  expect_error(as.NONMEMdata.PKNCAconc(data.frame()),
               info="Input must be a PKNCAconc object (or something very similar).")
  expect_error(as.NONMEMdata.PKNCAconc(my_conc, conc_cmt_map=data.frame(CMT=1:2)),
               regex="conc_cmt_map must either have shared names with the concentration data or be a single row",
               info="Must be able to map conc_cmt_map")
  expect_equal(as.NONMEMdata.PKNCAconc(my_conc, conc_cmt_map=data.frame(CMT=1)),
               structure(data.frame(CONC=structure(1, NONMEM_column="DV"),
                                    TIME=structure(0, NONMEM_column="TIME"),
                                    SUBJECT=structure("A", NONMEM_column="ID"),
                                    exclude=structure(NA_character_, NONMEM_column="EXCLUDETEXT"),
                                    volume=NA_real_,
                                    duration=0,
                                    CMT=structure(1, NONMEM_column="CMT"),
                                    EVID=structure(0, NONMEM_column="EVID"),
                                    stringsAsFactors=FALSE),
                         class=c("NONMEMdata_conc", "NONMEMdata", "data.frame"),
                         groups="SUBJECT"),
               info="Assigns single-row conc_cmt_map")
  expect_equal(as.NONMEMdata.PKNCAconc(my_conc,
                                       conc_cmt_map=data.frame(CMT=1:2,
                                                               SUBJECT=c("A", "B"),
                                                               stringsAsFactors=FALSE)),
               structure(data.frame(CONC=structure(1, NONMEM_column="DV"),
                                    TIME=structure(0, NONMEM_column="TIME"),
                                    SUBJECT=structure("A", NONMEM_column="ID"),
                                    exclude=structure(NA_character_, NONMEM_column="EXCLUDETEXT"),
                                    volume=NA_real_,
                                    duration=0,
                                    CMT=structure(1, NONMEM_column="CMT"),
                                    EVID=structure(0, NONMEM_column="EVID"),
                                    stringsAsFactors=FALSE),
                         class=c("NONMEMdata_conc", "NONMEMdata", "data.frame"),
                         groups="SUBJECT"),
               info="Assigns multi-row conc_cmt_map")
  expect_error(as.NONMEMdata.PKNCAconc(my_conc,
                                       conc_cmt_map=data.frame(CMT=1:2,
                                                               SUBJECT=c("C", "B"),
                                                               stringsAsFactors=FALSE)),
               regexp="Incomplete concentration compartment map, some rows are missing CMT value",
               fixed=TRUE,
               info="Assigns multi-row conc_cmt_map")
  expect_error(as.NONMEMdata.PKNCAconc(my_conc),
               regexp="Either conc_cmt_map must be provided or a CMT column must be given in the input object",
               fixed=TRUE,
               info="CMT is required")

  expect_error(
    as.NONMEMdata.PKNCAconc(
      PKNCAconc(CONC~TIME|SUBJECT,
                data=data.frame(CONC=1,
                                TIME=0,
                                SUBJECT="A",
                                CMT=NA_real_,
                                stringsAsFactors=FALSE))),
    regexp="No CMT values may be NA",
    fixed=TRUE,
    info="CMT cannot be NA in a PKNCAconc object")
  expect_error(
    as.NONMEMdata.PKNCAconc(
      PKNCAconc(CONC~TIME|SUBJECT,
                data=data.frame(CONC=1,
                                TIME=0,
                                SUBJECT="A",
                                CMT="A",
                                stringsAsFactors=FALSE))),
    regexp="CMT must be numeric (and not a factor)",
    fixed=TRUE,
    info="CMT must be a number (is character)")
  expect_error(
    as.NONMEMdata.PKNCAconc(
      PKNCAconc(CONC~TIME|SUBJECT,
                data=data.frame(CONC=1,
                                TIME=0,
                                SUBJECT="A",
                                CMT=factor("A"),
                                stringsAsFactors=FALSE))),
    regexp="CMT must be numeric (and not a factor)",
    fixed=TRUE,
    info="CMT must be a number (is factor)")
})

test_that("as.NONMEMdata.PKNCAdose", {
  my_dose <- PKNCAdose(data=data.frame(dose=1, time=0, sub="A", stringsAsFactors=FALSE),
                       formula=dose~time|sub)
  my_dose_no_time <- PKNCAdose(data=data.frame(dose=1, time=0, sub="A", stringsAsFactors=FALSE),
                               formula=dose~.|sub)
  my_dose_no_dose <- PKNCAdose(data=data.frame(dose=1, time=0, sub="A", stringsAsFactors=FALSE),
                               formula=~time|sub)
  my_dose_no_dose_2 <- PKNCAdose(data=data.frame(dose=1, time=0, sub="A", stringsAsFactors=FALSE),
                                 formula=.~time|sub)
  my_dose_amt_0 <- PKNCAdose(data=data.frame(dose=c(1, 0), time=c(0, 1), sub="A", stringsAsFactors=FALSE),
                             formula=dose~time|sub)
  
  expect_error(as.NONMEMdata.PKNCAdose(my_dose, dose_cmt_map="foo"),
               regexp="dose_cmt_map must have a column named 'CMT'",
               fixed=TRUE,
               info="dose_cmt_map requires a column named CMT")
  expect_error(as.NONMEMdata.PKNCAdose(my_dose, dose_cmt_map=data.frame()),
               regexp="dose_cmt_map must have a column named 'CMT'",
               fixed=TRUE,
               info="dose_cmt_map requires a column named CMT")
  expect_error(as.NONMEMdata.PKNCAdose(my_dose, dose_cmt_map=data.frame(CMT=NA)),
               regexp="dose_cmt_map CMT values cannot be NA",
               fixed=TRUE,
               info="dose_cmt_map requires a column named CMT")
  expect_error(as.NONMEMdata.PKNCAdose("foo", dose_cmt_map=data.frame(CMT=1)),
               regexp=tryCatch(PKNCA::parseFormula("foo"), error=function(e) e$message),
               fixed=TRUE,
               info="object must be a PKNCAdose object (or something that works like one)")
  expect_error(as.NONMEMdata.PKNCAdose(my_dose_no_dose),
               regexp="Cannot generate a NONMEMdata object when the dose amount is not given",
               fixed=TRUE,
               info="Must have dose amount (one-sided formula)")
  expect_error(as.NONMEMdata.PKNCAdose(my_dose_no_dose_2),
               regexp="Cannot generate a NONMEMdata object when the dose amount is a period ('.')",
               fixed=TRUE,
               info="Must have dose amount (dot formula)")
  expect_error(as.NONMEMdata.PKNCAdose(my_dose_no_time),
               regexp="Cannot generate a NONMEMdata object when the time is a period ('.')",
               fixed=TRUE,
               info="Must have time (dot formula)")

  expect_error(as.NONMEMdata.PKNCAdose(my_dose, dose_cmt_map=data.frame(CMT=1:2)),
               regexp="dose_cmt_map must either have shared names with the concentration data or be a single row",
               fixed=TRUE,
               info="dose_cmt_map must map with multiple rows")

  expect_equal(as.NONMEMdata.PKNCAdose(my_dose, dose_cmt_map=data.frame(CMT=1)),
               structure(
                 data.frame(dose=structure(1, NONMEM_column="AMT"),
                            time=structure(0, NONMEM_column="TIME"),
                            sub="A",
                            exclude=structure(NA_character_, NONMEM_column="EXCLUDETEXT"),
                            route="extravascular",
                            duration=0,
                            CMT=1L,
                            EVID=structure(1, NONMEM_column="EVID"), 
                            II=structure(0, NONMEM_column="II"),
                            ADDL=structure(0, NONMEM_column="ADDL"), 
                            RATE=structure(0, NONMEM_column="RATE"),
                            stringsAsFactors=FALSE),
                 class=c("NONMEMdata_dose", "NONMEMdata", "data.frame"),
                 groups="sub"),
               info="dose_cmt_map is bolted on")
  expect_equal(as.NONMEMdata.PKNCAdose(my_dose,
                                       dose_cmt_map=data.frame(CMT=1:2,
                                                               sub=c("A", "B"), stringsAsFactors=FALSE)),
               structure(
                 data.frame(dose=structure(1, NONMEM_column="AMT"),
                            time=structure(0, NONMEM_column="TIME"),
                            sub="A",
                            exclude=structure(NA_character_, NONMEM_column="EXCLUDETEXT"),
                            route="extravascular",
                            duration=0,
                            CMT=1L,
                            EVID=structure(1, NONMEM_column="EVID"), 
                            II=structure(0, NONMEM_column="II"),
                            ADDL=structure(0, NONMEM_column="ADDL"), 
                            RATE=structure(0, NONMEM_column="RATE"),
                            stringsAsFactors=FALSE),
                 class=c("NONMEMdata_dose", "NONMEMdata", "data.frame"),
                 groups="sub"),
               info="dose_cmt_map is merged")
  expect_error(as.NONMEMdata.PKNCAdose(my_dose,
                                       dose_cmt_map=data.frame(CMT=1:2,
                                                               sub=c("C", "B"), stringsAsFactors=FALSE)),
               regexp="Incomplete concentration compartment map, some rows are missing CMT value",
               fixed=TRUE,
               info="CMT must be assigned to every row")

  expect_equal(as.NONMEMdata.PKNCAdose(my_dose_amt_0, dose_cmt_map=data.frame(CMT=1)),
               structure(
                 data.frame(dose=structure(1, NONMEM_column="AMT"),
                            time=structure(0, NONMEM_column="TIME"),
                            sub="A",
                            exclude=structure(NA_character_, NONMEM_column="EXCLUDETEXT"),
                            route="extravascular",
                            duration=0,
                            CMT=1L,
                            EVID=structure(1, NONMEM_column="EVID"), 
                            II=structure(0, NONMEM_column="II"),
                            ADDL=structure(0, NONMEM_column="ADDL"), 
                            RATE=structure(0, NONMEM_column="RATE"),
                            stringsAsFactors=FALSE),
                 class=c("NONMEMdata_dose", "NONMEMdata", "data.frame"),
                 groups="sub"),
               info="dose=0 is dropped")
})

test_that("as.NONMEMdata.PKNCAdata", {
  my_data <-
    PKNCAdata(PKNCAconc(data.frame(conc=1, time=0, subject=1), conc~time|subject),
              PKNCAdose(data.frame(dose=1, time=0, subject=1), dose~time|subject),
              intervals=data.frame(start=0, end=1, cmax=TRUE))
  expect_equal(
    as.NONMEMdata.PKNCAdata(my_data,
                            conc_cmt_map=data.frame(CMT=2),
                            dose_cmt_map=data.frame(CMT=1)),
    structure(
      data.frame(conc=structure(c(NA, NA, 1), NONMEM_column="DV"),
                 time=structure(c(NA, 0, 0), NONMEM_column="TIME"),
                 subject=structure(c(1, 1, 1), NONMEM_column="ID"),
                 exclude=structure(c(NA_character_, NA_character_, NA_character_), NONMEM_column="EXCLUDETEXT"),
                 volume=c(NA_real_, NA_real_, NA_real_),
                 duration=c(NA, 0, 0),
                 CMT=structure(c(NA, 1, 2), NONMEM_column="CMT"),
                 EVID=structure(c(3, 1, 0), NONMEM_column="EVID"),
                 dose=structure(c(NA, 1, NA), NONMEM_column="AMT"),
                 route=c(NA, "extravascular", NA),
                 II=structure(c(NA, 0, NA), NONMEM_column="II"),
                 ADDL=structure(c(NA, 0, NA), NONMEM_column="ADDL"),
                 RATE=structure(c(NA, 0, NA), NONMEM_column="RATE"),
                 stringsAsFactors=FALSE),
      class=c("NONMEMdata_data", "NONMEMdata", "data.frame"),
      groups="subject"),
    info="Generating full NONMEMdata from PKNCAdata works correctly")
})

context("as.NONMEMreset")

test_that("as.NONMEMreset.NULL", {
  expect_null(as.NONMEMreset.NULL(NULL),
              info="as.NONMEMreset.NULL returns NULL")
  expect_null(as.NONMEMreset.NULL("foo"),
              info="as.NONMEMreset.NULL returns NULL no matter what")
})

test_that("as.NONMEMreset.NONMEMdata", {
  my_conc <- PKNCAconc(data.frame(conc=1, time=0, subject=1), conc~time|subject)
  my_dose <- PKNCAdose(data.frame(dose=1, time=0, subject=1), dose~time|subject)
  my_data <- PKNCAdata(my_conc, my_dose,
                       intervals=data.frame(start=0, end=1, cmax=TRUE))
  expect_equal(as.NONMEMreset.NONMEMdata(as.NONMEMdata(my_conc, conc_cmt_map=data.frame(CMT=1))),
               structure(data.frame(subject=structure(1, NONMEM_column="ID"),
                                    time=structure(NA_real_, NONMEM_column="TIME"),
                                    EVID=structure(3, NONMEM_column="EVID"),
                                    exclude=structure(NA_character_, NONMEM_column="EXCLUDETEXT"),
                                    stringsAsFactors=FALSE),
                         class=c("NONMEMdata_reset", "NONMEMdata_conc", "NONMEMdata", "data.frame"),
                         groups="subject"),
               info="NONMEMreset can be generated from PKNCAconc")
  expect_equal(as.NONMEMreset.NONMEMdata(as.NONMEMdata(my_dose, dose_cmt_map=data.frame(CMT=1))),
               structure(data.frame(subject=1,
                                    time=structure(NA_real_, NONMEM_column="TIME"),
                                    EVID=structure(3, NONMEM_column="EVID"),
                                    exclude=structure(NA_character_, NONMEM_column="EXCLUDETEXT"),
                                    stringsAsFactors=FALSE),
                         class=c("NONMEMdata_reset", "NONMEMdata_dose", "NONMEMdata", "data.frame"),
                         groups="subject"),
               info="NONMEMreset can be generated from PKNCAdose")
  expect_equal(as.NONMEMreset.NONMEMdata(as.NONMEMdata(my_data,
                                                       conc_cmt_map=data.frame(CMT=1),
                                                       dose_cmt_map=data.frame(CMT=1))),
               structure(data.frame(subject=structure(1, NONMEM_column="ID"),
                                    time=structure(NA_real_, NONMEM_column="TIME"),
                                    EVID=structure(3, NONMEM_column="EVID"),
                                    exclude=structure(NA_character_, NONMEM_column="EXCLUDETEXT"),
                                    stringsAsFactors=FALSE),
                         class=c("NONMEMdata_reset", "NONMEMdata_data", "NONMEMdata", "data.frame"),
                         groups="subject"),
               info="NONMEMreset can be generated from PKNCAdata")
})

context("as.data.frame.NONMEMdata")

test_that("as.data.frame.NONMEMdata", {
  my_conc <- PKNCAconc(CONC~TIME|SUBJECT,
                       data=data.frame(CONC=1,
                                       TIME=0,
                                       SUBJECT=3,
                                       stringsAsFactors=FALSE))
  my_conc_data <- as.NONMEMdata.PKNCAconc(my_conc, conc_cmt_map=data.frame(CMT=1))
  expect_equal(as.data.frame(my_conc_data),
               structure(data.frame(SUBJECT=structure(3, NONMEM_column="ID"), 
                                    TIME=structure(0, NONMEM_column="TIME"),
                                    EVID=structure(0, NONMEM_column="EVID"),
                                    exclude_numeric=structure(0, NONMEM_column="EXCLUDETEXT"),
                                    CMT=structure(1, NONMEM_column="CMT"),
                                    CONC=structure(1, NONMEM_column="DV"), 
                                    duration=0,
                                    volume=NA_real_,
                                    exclude=NA_character_,
                                    stringsAsFactors=FALSE),
                         class = c("NONMEMdata_conc", 
                                   "data.frame"),
                         groups = "SUBJECT"),
               info="Subject is left alone if already numeric")

  my_conc <- PKNCAconc(CONC~TIME|SUBJECT,
                       data=data.frame(CONC=1,
                                       TIME=0,
                                       SUBJECT="A",
                                       stringsAsFactors=FALSE))
  my_conc_data <- as.NONMEMdata.PKNCAconc(my_conc, conc_cmt_map=data.frame(CMT=1))
  expect_equal(as.data.frame(my_conc_data),
               structure(data.frame(SUBJECT_numeric=structure(1, NONMEM_column="ID"), 
                                    TIME=structure(0, NONMEM_column="TIME"),
                                    EVID=structure(0, NONMEM_column="EVID"),
                                    exclude_numeric=structure(0, NONMEM_column="EXCLUDETEXT"),
                                    CMT=structure(1, NONMEM_column="CMT"),
                                    CONC=structure(1, NONMEM_column="DV"), 
                                    duration=0,
                                    volume=NA_real_,
                                    exclude=NA_character_,
                                    SUBJECT="A",
                                    stringsAsFactors=FALSE),
                         class = c("NONMEMdata_conc", 
                                   "data.frame"),
                         groups = "SUBJECT"),
               info="Subject is converted to a number if needed")
  
  my_conc <- PKNCAconc(CONC~TIME|SUBJECT,
                       data=data.frame(CONC=1,
                                       TIME=0,
                                       foo=1,
                                       SUBJECT="A",
                                       stringsAsFactors=FALSE))
  my_conc_data <- as.NONMEMdata.PKNCAconc(my_conc, conc_cmt_map=data.frame(CMT=1))
  dput(as.data.frame(my_conc_data), file="c:/tmp/foo.log")
  expect_equal(as.data.frame(my_conc_data),
               structure(data.frame(SUBJECT_numeric=structure(1, NONMEM_column="ID"), 
                                    TIME=structure(0, NONMEM_column="TIME"),
                                    EVID=structure(0, NONMEM_column="EVID"),
                                    exclude_numeric=structure(0, NONMEM_column="EXCLUDETEXT"),
                                    CMT=structure(1, NONMEM_column="CMT"),
                                    CONC=structure(1, NONMEM_column="DV"), 
                                    duration=0,
                                    foo=1,
                                    volume=NA_real_,
                                    exclude=NA_character_,
                                    SUBJECT="A",
                                    stringsAsFactors=FALSE),
                         class = c("NONMEMdata_conc", 
                                   "data.frame"),
                         groups = "SUBJECT"),
               info="Logical values are converted to numbers")
})