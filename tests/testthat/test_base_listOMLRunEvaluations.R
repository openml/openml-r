# context("listOMLRunEvaluations")

# test_that("listOMLRunEvaluations", {
#   tasks = listOMLTasks()[1:300, ]
#   error = vector("list", length(tasks$task_id))
#   for(i in 1:length(tasks$task_id)){
#     run = listOMLRuns(task.id = tasks$task_id[i])
#     run = run[is.na(run$error.message),]
#     runres = listOMLRunEvaluations(task.id = tasks$task_id[i])

#     error[[i]] = run$run.id[!run$run.id%in%runres$run.id]
#   }
#   names(error) = unique(tasks$task_id)
#   error[sapply(error, function(x) length(x) != 0)]


#   expect_equal(sort(runres$run.id), sort(run$run.id))

#   run.id = run[run$flow.id%in%flow[grepl("R", flow$external.version),"flow.id"],"run.id"]
#   rl = listOMLRunEvaluations(run.id = run.id)

#   # subset only runs without error
#   rl = rl[is.na(rl$error.message),]
#   run.id = unique(rl$run.id)
#   setup.id = unique(rl$setup.id)
#   flow.id = unique(rl$flow.id)
#   uploader.id = unique(rl$uploader)
#   # get minimum length of ids
#   min.len = min(c(length(uploader.id), length(flow.id), length(setup.id), length(run.id)))

#   runs = listOMLRunEvaluations(run.id = run.id[1:100])
#   expect_equal(sort(run.id[1:100]), sort(runs$run.id))
#   expect_equal(colnames(runs), exp.names)

#   for (i in c("run.id", "setup.id", "flow.id", "uploader.id")) {
#     id = get(i)[length(get(i))]
#     if (i == "uploader.id") i = "uploader"
#     rl = do.call("listOMLRunEvaluations", setNames(list(id), i))
#     expect_true(all(rl[,i] == id))
#     expect_true(setequal(names(rl), exp.names))
#   }

# })
