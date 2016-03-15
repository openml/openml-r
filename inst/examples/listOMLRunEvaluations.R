\dontrun{
	# get run results of task 6 (as many rows as runs for this task)
	rev_tid6 <- listOMLRunEvaluations(task.id = 6L)
	str(rev_tid6)

	# get run results of run 8 (one row)
	rev_rid8 <- listOMLRunEvaluations(run.id = 8)
	str(rev_rid8)
}
