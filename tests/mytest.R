app <- ShinyDriver$new("../")
app$snapshotInit("mytest")

app$setInputs(test_Button = "click")
app$setInputs(IDCols = "ID")
app$setInputs(IDCols = c("ID", "Name"))
app$setInputs(survfactor = "Sex")
app$snapshot()
