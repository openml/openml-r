\dontrun{
	runs_ctree = listOMLRuns(flow.id = 2569)
	run1 = getOMLRun(run.id = runs_ctree$run.id[1])
	str(run1, 1)
}
